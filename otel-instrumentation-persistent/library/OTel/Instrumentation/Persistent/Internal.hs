{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module OTel.Instrumentation.Persistent.Internal
  ( -- * Disclaimer
    -- $disclaimer
    buildSqlPoolHooks
  , sqlPoolHooks
  , setNewAlterBackend
  , setNewRunBefore
  , setNewRunAfter
  , setNewRunOnException
  , modifyRunOnException
  , modifyGetStatement
  , unsafeTracedAcquire
  , persistValuesToAttrs
  , Bytes(..)
  ) where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Acquire.Internal (Acquire(..))
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist (LiteralType(..), PersistValue(..))
import Database.Persist.Sql (IsolationLevel(..), SqlBackend, Statement)
import Database.Persist.SqlBackend (SqlBackendHooks, getConnHooks, getRDBMS, setConnHooks)
import Database.Persist.SqlBackend.Internal (hookGetStatement)
import Database.Persist.SqlBackend.Internal.SqlPoolHooks (runOnException)
import Database.Persist.SqlBackend.Internal.Statement (Statement(..))
import Database.Persist.SqlBackend.SqlPoolHooks
  ( SqlPoolHooks, defaultSqlPoolHooks, modifyAlterBackend, modifyRunAfter, modifyRunBefore
  )
import OTel.API.Common (AttrsFor(..), Key(..), ToAttrVal(..), (.@), AttrsBuilder)
import OTel.API.Trace (SpanName(..), TracingT(..), SpanSpec, TracerProvider, TracingBackend)
import Prelude
import qualified OTel.API.Common as OTel
import qualified OTel.API.Trace as OTel

buildSqlPoolHooks
  :: forall m
   . (MonadIO m, MonadMask m)
  => TracerProvider
  -> IO (SqlPoolHooks m SqlBackend)
buildSqlPoolHooks tracerProvider = do
  tracingBackend <- OTel.getTracingBackend tracerProvider "otel-instrumentation-persistent"
    { OTel.instrumentationScopeVersion = Just "0.0.0" -- TODO: Automatically pull package version
    , OTel.instrumentationScopeSchemaURL = Just OTel.TRACE_SCHEMA_URL
    }
  pure $ sqlPoolHooks tracingBackend

sqlPoolHooks
  :: forall m
   . (MonadIO m, MonadMask m)
  => TracingBackend
  -> SqlPoolHooks m SqlBackend
sqlPoolHooks tracingBackend =
  setNewAlterBackend tracingBackend
    $ setNewRunBefore tracingBackend
    $ setNewRunAfter tracingBackend
    $ setNewRunOnException tracingBackend defaultSqlPoolHooks

setNewAlterBackend
  :: TracingBackend
  -> SqlPoolHooks m SqlBackend
  -> SqlPoolHooks m SqlBackend
setNewAlterBackend tracingBackend hooks = do
  modifyAlterBackend hooks \oldAlterBackend sqlBackend -> do
    oldAlterBackend $ setConnHooks (setNewBackendHooks $ getConnHooks sqlBackend) sqlBackend
  where
  setNewBackendHooks backendHooks =
    modifyGetStatement backendHooks \oldGetStatement sqlBackend sqlText statement -> do
      oldGetStatement sqlBackend sqlText statement
        { stmtQuery = \persistValues -> do
            let attrs = OTel.DB_STATEMENT .@ sqlText <> persistValuesToAttrs persistValues
            let spanSpec = mkSpanSpec (SpanName $ getRDBMS sqlBackend) attrs sqlBackend
            unsafeTracedAcquire tracingBackend spanSpec do
              stmtQuery statement persistValues
        , stmtExecute = \persistValues -> do
            let attrs = OTel.DB_STATEMENT .@ sqlText <> persistValuesToAttrs persistValues
            flip runTracingT tracingBackend do
              OTel.trace_ (mkSpanSpec (SpanName $ getRDBMS sqlBackend) attrs sqlBackend) do
                liftIO $ stmtExecute statement persistValues
        }

setNewRunBefore
  :: forall m
   . (MonadIO m, MonadMask m)
  => TracingBackend
  -> SqlPoolHooks m SqlBackend
  -> SqlPoolHooks m SqlBackend
setNewRunBefore tracingBackend hooks = do
  modifyRunBefore hooks \oldRunBefore sqlBackend mIsoLevel -> do
    flip runTracingT tracingBackend do
      let spanName = SpanName $ "BEGIN TRANSACTION" <> maybe "" isoLevelText mIsoLevel
      OTel.trace_ (mkSpanSpec spanName mempty sqlBackend) do
        lift $ oldRunBefore sqlBackend mIsoLevel
  where
  isoLevelText = \case
    Serializable -> " ISOLATION LEVEL SERIALIZABLE"
    RepeatableRead -> " ISOLATION LEVEL REPEATABLE READ"
    ReadCommitted -> " ISOLATION LEVEL READ COMMITTED"
    ReadUncommitted -> " ISOLATION LEVEL READ UNCOMMITTED"

setNewRunAfter
  :: forall m
   . (MonadIO m, MonadMask m)
  => TracingBackend
  -> SqlPoolHooks m SqlBackend
  -> SqlPoolHooks m SqlBackend
setNewRunAfter tracingBackend hooks = do
  modifyRunAfter hooks \oldRunAfter sqlBackend mIsoLevel -> do
    flip runTracingT tracingBackend do
      let attrs = mempty
      OTel.trace_ (mkSpanSpec "COMMIT TRANSACTION" attrs sqlBackend) do
        lift $ oldRunAfter sqlBackend mIsoLevel

setNewRunOnException
  :: forall m
   . (MonadIO m, MonadMask m)
  => TracingBackend
  -> SqlPoolHooks m SqlBackend
  -> SqlPoolHooks m SqlBackend
setNewRunOnException tracingBackend hooks =
  modifyRunOnException hooks \oldRunOnException sqlBackend mIsoLevel someEx -> do
    flip runTracingT tracingBackend do
      let attrs = mempty
      OTel.trace (mkSpanSpec "ROLLBACK TRANSACTION" attrs sqlBackend) \mutableSpan -> do
        OTel.updateSpan mutableSpan $ OTel.recordException someEx False OTel.TimestampSourceNow mempty
        lift $ oldRunOnException sqlBackend mIsoLevel someEx

-- TODO: This function exists in "Database.Persist.SqlBackend.SqlPoolHooks" but
-- isn't exported. Should upstream exporting it rather than inlining it here.
modifyRunOnException
  :: forall m backend
   . SqlPoolHooks m backend
  -> ((backend -> Maybe IsolationLevel -> SomeException -> m ()) -> (backend -> Maybe IsolationLevel -> SomeException -> m ()))
  -> SqlPoolHooks m backend
modifyRunOnException hooks f =
  hooks { runOnException = f $ runOnException hooks }

-- TODO: 'emptySqlBackendHooks' and 'setConnHooks' are exported from
-- @persistent@, but there is no means of setting the single function in
-- 'SqlBackendHooks'. Here, we do that using internals imports. Should upstream
-- some changes to clean this up.
modifyGetStatement
  :: SqlBackendHooks
  -> ((SqlBackend -> Text -> Statement -> IO Statement) -> SqlBackend -> Text -> Statement -> IO Statement)
  -> SqlBackendHooks
modifyGetStatement hooks f =
  hooks { hookGetStatement = f $ hookGetStatement hooks }

mkSpanSpec :: SpanName -> AttrsBuilder 'AttrsForSpan -> SqlBackend -> SpanSpec
mkSpanSpec spanSpecName additionalAttrs sqlBackend =
  OTel.defaultSpanSpec
    { OTel.spanSpecKind = OTel.SpanKindClient
    , OTel.spanSpecName
    , OTel.spanSpecAttrs =
        -- N.B. We are relying on @persistent@ implementation details here in
        -- that 'getRDBMS' will return "postgresql", "mysql", "sqlite", etc. and
        -- these happen to already line up with the trace semantic conventions.
        -- This saves us a case mapping to the 'DbSystem' enum in
        -- "OTel.API.Trace.Core.Attributes".
        OTel.DB_SYSTEM .@ getRDBMS sqlBackend <> additionalAttrs
    }

unsafeTracedAcquire :: TracingBackend -> SpanSpec -> Acquire a -> Acquire a
unsafeTracedAcquire tracingBackend spanSpec (Acquire f) =
  Acquire \restore -> do
    flip runTracingT tracingBackend do
      OTel.trace_ spanSpec do
        liftIO $ f restore

persistValuesToAttrs :: [PersistValue] -> AttrsBuilder 'AttrsForSpan
persistValuesToAttrs =
  go $ fmap (\i -> "persistent.param." <> pack (show @Int i)) [0..]
  where
  go :: [Text] -> [PersistValue] -> AttrsBuilder 'AttrsForSpan
  go keyTexts persistValues = mconcat $ zipWith zipFunc keyTexts persistValues

  zipFunc :: Text -> PersistValue -> AttrsBuilder 'AttrsForSpan
  zipFunc keyText = \case
    PersistText text -> Key keyText .@ toAttrVal text
    PersistByteString bytes -> Key keyText .@ Bytes bytes
    PersistInt64 int -> Key keyText .@ toAttrVal int
    PersistDouble double -> Key keyText .@ toAttrVal double
    PersistRational rational -> Key keyText .@ toAttrVal rational
    PersistBool bool -> Key keyText .@ toAttrVal bool
    PersistDay day -> Key keyText .@ toAttrVal (show day)
    PersistTimeOfDay timeOfDay -> Key keyText .@ toAttrVal (show timeOfDay)
    PersistUTCTime utcTime -> Key keyText .@ toAttrVal (show utcTime)
    PersistNull -> Key keyText .@ ("(null)" :: Text)
    PersistList innerPersistValues
      | null innerPersistValues -> Key keyText .@ ("(empty list)" :: Text)
      | otherwise ->
          go (fmap (\i -> keyText <> "." <> pack (show @Int i)) [0..]) innerPersistValues
    PersistMap assocList
      | null assocList -> Key keyText .@ ("(empty map)" :: Text)
      | otherwise -> go keys vals
          where
          (keys, vals) = first (fmap \k -> keyText <> "." <> k) $ unzip assocList
    PersistObjectId bytes -> Key keyText .@ Bytes bytes
    PersistArray innerPersistValues
      | null innerPersistValues -> Key keyText .@ ("(empty array)" :: Text)
      | otherwise ->
          go (fmap (\i -> keyText <> "." <> pack (show @Int i)) [0..]) innerPersistValues
    PersistLiteral_ Escaped bytes -> Key keyText .@ Bytes bytes
    PersistLiteral_ Unescaped bytes -> Key keyText .@ Bytes bytes
    PersistLiteral_ DbSpecific bytes -> Key keyText .@ Bytes bytes

newtype Bytes = Bytes
  { unBytes :: ByteString
  }

instance ToAttrVal Bytes Text where
  toAttrVal = decodeUtf8With lenientDecode . unBytes

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
