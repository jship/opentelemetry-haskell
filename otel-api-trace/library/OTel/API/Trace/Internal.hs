{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
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
import OTel.API.Common
  ( Span(spanContext, spanIsRecording), SpanEventSpecs(..), SpanUpdateSpec(spanUpdateSpecEvents)
  , TimestampSource(TimestampSourceNow), Tracer(..), EndedSpan, SpanName, SpanSpec, Timestamp
  , buildSpanUpdater, defaultSpanUpdateSpec, recordException, toEndedSpan
  )
import OTel.API.Context
  ( ContextSnapshot(..), ContextT(..), ContextKey, attachContext, getContext, updateContext
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
  traceCS _cs spanName spanSpec action =
    TracingT \tracerOps -> do
      span <- liftIO $ tracerOpsStartSpan tracerOps spanName spanSpec
      attachContext span \spanKey -> do
        result <- unTracingT (action MutableSpan { spanKey }) tracerOps
        result <$ processSpan tracerOps spanKey
        `Safe.withException` handler tracerOps spanKey
    where
    handler
      :: TracerOps
      -> ContextKey Span
      -> SomeException
      -> ContextT Span m ()
    handler tracerOps spanKey someEx = do
      updater <- do
        -- TODO: Set status to error? Spec docs make it sound like application
        -- authors set the status, and the API shouldn't set the status.
        buildSpanUpdater
          (liftIO $ tracerOpsGetCurrentTimestamp tracerOps)
          defaultSpanUpdateSpec
            { spanUpdateSpecEvents =
                Just $ SpanEventSpecs
                  [ recordException someEx TimestampSourceNow mempty
                  ]
            }
      () <$ updateContext spanKey updater
      processSpan tracerOps spanKey

    processSpan :: TracerOps -> ContextKey Span -> ContextT Span m ()
    processSpan tracerOps spanKey = do
      span <- fmap contextSnapshotValue $ getContext spanKey
      timestamp <- liftIO $ tracerOpsGetCurrentTimestamp tracerOps
      liftIO $ tracerOpsProcessSpan tracerOps $ toEndedSpan timestamp span
      () <$ updateContext spanKey \s -> s { spanIsRecording = False }

instance (MonadIO m) => MonadTraceContext (TracingT m) where
  getSpanContext mutableSpan =
    TracingT \_tracerOps ->
      fmap (fmap spanContext) $ getContext $ spanKey mutableSpan

  updateSpan mutableSpan spanUpdateSpec =
    TracingT \tracerOps -> do
      updater <- do
        buildSpanUpdater
          (liftIO $ tracerOpsGetCurrentTimestamp tracerOps)
          spanUpdateSpec
      updateContext (spanKey mutableSpan) updater

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
      { tracerOpsGetCurrentTimestamp = tracerGetCurrentTimestamp tracer
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

data TracerOps = TracerOps
  { tracerOpsGetCurrentTimestamp :: IO Timestamp
  , tracerOpsStartSpan :: SpanName -> SpanSpec -> IO Span
  , tracerOpsProcessSpan :: EndedSpan -> IO ()
  }

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
