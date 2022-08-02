{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    TracingT(..)
  , runTracingT
  , mapTracingT

  , buildSpanSpec
  ) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, SomeException)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader (MonadReader(ask, local, reader))
import Control.Monad.Select (MonadSelect)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import OTel.API.Core
  ( NewSpanSpec(..), Span(spanContext, spanIsRecording), SpanEventSpecs(..), SpanLineage(..)
  , SpanLineageSource(..), SpanSpec(..), TimestampSource(..), Tracer(..)
  , UpdateSpanSpec(updateSpanSpecEvents), EndedSpan, Timestamp, buildSpanUpdater
  , defaultUpdateSpanSpec, recordException, toEndedSpan
  )
import OTel.API.Context
  ( ContextSnapshot(..), ContextT(..), ContextKey, attachContext, getAttachedContextKey, getContext
  , updateContext
  )
import OTel.API.Trace.Core (MonadTraceContext(..), MonadTracing(..))
import OTel.API.Trace.Core.Internal (MutableSpan(..))
import Prelude hiding (span)
import qualified Control.Exception.Safe as Safe

type TracingT :: (Type -> Type) -> Type -> Type

newtype TracingT m a = TracingT
  { unTracingT :: TracerOps -> ContextT Span m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
      , MonadAccum w, MonadCont, MonadError e, MonadSelect r, MonadState s, MonadWriter w -- @mtl@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      ) via (ReaderT TracerOps (ContextT Span m))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT TracerOps (ContextT Span m)) a)

instance (MonadReader r m) => MonadReader r (TracingT m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT m)

instance MonadTrans TracingT where
  lift action =
    TracingT \_tracerOps ->
      ContextT \_contextBackend ->
        action

instance MonadTransControl TracingT where
  type StT TracingT a = a
  liftWith action =
    TracingT \tracerOps ->
      ContextT \contextBackend ->
        action $ \t -> runContextT (unTracingT t tracerOps) contextBackend
  restoreT = lift

instance (MonadIO m, MonadMask m) => MonadTracing (TracingT m) where
  -- TODO: Record callstack in attributes
  traceCS _cs newSpanSpec action =
    TracingT \tracerOps -> do
      spanSpec <- buildSpanSpec (tracerOpsNow tracerOps) newSpanSpec
      span <- liftIO $ tracerOpsStartSpan tracerOps spanSpec
      attachContext span \spanKey -> do
        result <- unTracingT (action MutableSpan { spanKey }) tracerOps
        result <$ processSpan tracerOps spanKey
        `Safe.withException` handler tracerOps spanKey
    where
    processSpan :: TracerOps -> ContextKey Span -> ContextT Span m ()
    processSpan tracerOps spanKey = do
      span <- fmap contextSnapshotValue $ getContext spanKey
      timestamp <- liftIO $ tracerOpsNow tracerOps
      liftIO $ tracerOpsProcessSpan tracerOps $ toEndedSpan timestamp span
      () <$ updateContext spanKey \s -> s { spanIsRecording = False }

    handler :: TracerOps -> ContextKey Span -> SomeException -> ContextT Span m ()
    handler tracerOps spanKey someEx = do
      -- TODO: Set status to error? Spec docs make it sound like application
      -- authors set the status, and the API shouldn't set the status.
      -- https://opentelemetry.io/docs/reference/specification/trace/api/#set-status
      -- Probably would be convenient if this was a config option.
      _ <- updateContext spanKey
             =<< buildSpanUpdater (liftIO $ tracerOpsNow tracerOps) defaultUpdateSpanSpec
                   { updateSpanSpecEvents =
                       Just $ SpanEventSpecs
                         [ recordException someEx TimestampSourceNow mempty
                         ]
                   }
      -- N.B. It is important that we finish the span after recording the
      -- exception and not the other way around, because the span is no longer
      -- recording after it is ended.
      processSpan tracerOps spanKey

instance (MonadIO m) => MonadTraceContext (TracingT m) where
  getSpanContext mutableSpan =
    TracingT \_tracerOps ->
      fmap (fmap spanContext) $ getContext $ spanKey mutableSpan

  updateSpan mutableSpan updateSpanSpec =
    TracingT \tracerOps -> do
      updateContext (spanKey mutableSpan)
        =<< buildSpanUpdater (liftIO $ tracerOpsNow tracerOps) updateSpanSpec

runTracingT
  :: forall m a
   . TracingT m a
  -> Tracer
  -> m a
runTracingT action tracer =
  runContextT (unTracingT action tracerOps) contextBackend
  where
  tracerOps =
    TracerOps
      { tracerOpsNow = tracerGetCurrentTimestamp tracer
      , tracerOpsStartSpan = tracerStartSpan tracer
      , tracerOpsProcessSpan = tracerProcessSpan tracer
      }
  contextBackend = tracerContextBackend tracer

mapTracingT
  :: forall m n a b
   . (m a -> n b)
  -> TracingT m a
  -> TracingT n b
mapTracingT f action =
  TracingT \tracerOps ->
    ContextT \contextBackend ->
      f $ runContextT (unTracingT action tracerOps) contextBackend

buildSpanSpec
  :: (MonadIO m)
  => IO Timestamp
  -> NewSpanSpec
  -> ContextT Span m SpanSpec
buildSpanSpec getCurrentTimestamp newSpanSpec = do
  spanSpecLineage <- do
    case newSpanSpecLineageSource newSpanSpec of
      SpanLineageSourceExplicit spanLineage -> pure spanLineage
      SpanLineageSourceImplicit -> do
        getAttachedContextKey >>= \case
          Nothing -> pure SpanLineageRoot
          Just ctxKey -> do
            ContextSnapshot { contextSnapshotValue } <- getContext ctxKey
            pure $ SpanLineageChildOf $ spanContext contextSnapshotValue

  spanSpecStart <- do
    case newSpanSpecStart newSpanSpec of
      TimestampSourceAt timestamp -> pure timestamp
      TimestampSourceNow -> liftIO getCurrentTimestamp

  pure SpanSpec
    { spanSpecLineage
    , spanSpecStart
    , spanSpecKind = newSpanSpecKind newSpanSpec
    , spanSpecAttributes = newSpanSpecAttributes newSpanSpec
    , spanSpecLinks = newSpanSpecLinks newSpanSpec
    }

data TracerOps = TracerOps
  { tracerOpsNow :: IO Timestamp
  , tracerOpsStartSpan :: SpanSpec -> IO Span
  , tracerOpsProcessSpan :: EndedSpan -> IO ()
  }

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
