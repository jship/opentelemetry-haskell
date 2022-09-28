{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ) where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Text (Text)
import Database.Persist.Sql (IsolationLevel(..), SqlBackend, Statement)
import Database.Persist.SqlBackend (SqlBackendHooks, getConnHooks, getRDBMS, setConnHooks)
import Database.Persist.SqlBackend.Internal (hookGetStatement)
import Database.Persist.SqlBackend.Internal.SqlPoolHooks (runOnException)
import Database.Persist.SqlBackend.SqlPoolHooks
import OTel.API.Common (AttrsFor(..), (.@), AttrsBuilder)
import OTel.API.Trace (SpanName(..), TracingT(..), SpanSpec, TracerProvider, TracingBackend)
import Prelude
import qualified OTel.API.Common as OTel
import qualified OTel.API.Trace as OTel

buildSqlPoolHooks
  :: forall m
   . (MonadIO m, MonadMask m)
  => TracerProvider
  -> m (SqlPoolHooks m SqlBackend)
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
      flip runTracingT tracingBackend do
        let attrs = OTel.DB_STATEMENT .@ sqlText
        OTel.trace_ (spanSpec (SpanName $ getRDBMS sqlBackend) attrs sqlBackend) do
          liftIO $ oldGetStatement sqlBackend sqlText statement

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
      OTel.trace_ (spanSpec spanName mempty sqlBackend) do
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
      OTel.trace_ (spanSpec "COMMIT TRANSACTION" attrs sqlBackend) do
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
      OTel.trace (spanSpec "ROLLBACK TRANSACTION" attrs sqlBackend) \mutableSpan -> do
        OTel.updateSpan mutableSpan $ OTel.recordException someEx False OTel.Now mempty
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

spanSpec :: SpanName -> AttrsBuilder 'AttrsForSpan -> SqlBackend -> SpanSpec
spanSpec spanSpecName additionalAttrs sqlBackend =
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

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
