{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    TracingT(..)
  , mapTracingT

  , TracingBackend(..)
  , toTracingBackend

  , getTracingBackend
  , getTracer
  , shutdownTracerProvider
  , forceFlushTracerProvider
  ) where

import Control.Applicative (Alternative)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, SomeException, withException)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger.Aeson (MonadLogger, withThreadContext)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader (MonadReader(ask, local, reader))
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import OTel.API.Baggage.Core (MonadBaggage)
import OTel.API.Common
  ( AttrsFor(AttrsForSpan), KV(..), TimestampSource(..), AttrsBuilder, InstrumentationScope
  )
import OTel.API.Context (ContextT(..), ContextBackend, attachContextValue, getAttachedContext)
import OTel.API.Trace.Core
  ( MonadTracing(..), MonadTracingIO(..), SpanSpec(..)
  , Span(spanContext, spanFrozenAt, spanIsRecording), MutableSpan, contextBackendSpan
  , recordException
  )
import OTel.API.Trace.Core.Internal
  ( Tracer(..), TracerProvider(..), buildSpanUpdater, freezeSpan, unsafeModifyMutableSpan
  , unsafeReadMutableSpan
  )
import Prelude hiding (span)
import qualified Control.Exception.Safe as Safe
import qualified GHC.Stack as Stack

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Select (MonadSelect)
#endif

type TracingT :: (Type -> Type) -> Type -> Type
newtype TracingT m a = TracingT
  { runTracingT :: TracingBackend -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadIO -- @base@
      , Alternative, MonadPlus -- @base@
      , MonadCont, MonadError e, MonadState s, MonadWriter w -- @mtl@
#if MIN_VERSION_mtl(2,3,0)
      , MonadAccum w, MonadSelect r -- @mtl@
#endif
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      , MonadBaggage -- @otel-api-baggage-core@
      ) via (ReaderT TracingBackend m)
    deriving
      ( MonadTransControl -- @monad-control@
      ) via (ReaderT TracingBackend)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT TracingBackend m) a)

-- On GHC 9.6.2/transformers-0.6.0.1, including this 'MonadTrans' instance
-- in the cleaner way above, e.g.:
--
--   deriving
--     ( MonadTrans -- @transformers@
--     , MonadTransControl -- @monad-control@
--     ) via (ReaderT TracingBackend)
--
-- produces a redundant constraint warning:
--
-- error: [GHC-30606] [-Wredundant-constraints, Werror=redundant-constraints]
--       • Redundant constraint: Monad m
--       • When deriving the instance for (MonadTrans TracingT)
--      |
--   75 |       ( MonadTrans -- @transformers@
--      |         ^^^^^^^^^^
--
-- Strangely, doing the same style of deriving but using @-XStandaloneDeriving@
-- does not produce this warning.
deriving via (ReaderT TracingBackend) instance MonadTrans TracingT

instance (MonadReader r m) => MonadReader r (TracingT m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT m)

instance (MonadIO m, MonadMask m) => MonadTracing (TracingT m) where
  traceCS cs spanSpec action =
    TracingT \tracingBackend -> do
      flip runContextT (tracingBackendContextBackend tracingBackend) do
        parentCtx <- getAttachedContext
        (mutableSpan, spanContextMeta) <- do
          liftIO $ tracerStartSpan (tracingBackendTracer tracingBackend) cs parentCtx spanSpec
        attachContextValue mutableSpan do
          withThreadContext spanContextMeta do
            result <- do
              lift (runTracingT (action mutableSpan) tracingBackend) `withException` \e -> do
                liftIO $ handler (tracingBackendTracer tracingBackend) mutableSpan e
            liftIO $ processSpan (tracingBackendTracer tracingBackend) mutableSpan
            pure result
    where
    processSpan :: Tracer -> MutableSpan -> IO ()
    processSpan tracer mutableSpan = do
      span <- unsafeReadMutableSpan mutableSpan
      timestamp <- now
      -- N.B. We set the span's 'spanFrozenAt' field both in the value copy we
      -- pass to the span processors' on-span-end method and in the mutable
      -- span. The former is important so that a span processor's on-span-start
      -- method has a reliable means of understanding if any spans it's tracking
      -- have ended or not. We could alternatively set the timestamp in the
      -- mutable span before passing the copy to the span processors, but the
      -- current flow requires that we only update the reference once, where we
      -- include both the timestamp and 'spanIsRecording'. Any recording spans
      -- must be seen as recording for span processors to receive them, so we
      -- don't set 'spanIsRecording' until after span processors have received
      -- the span.
      liftIO
        $ tracerProcessSpan
        $ freezeSpan timestamp spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits span
            { spanFrozenAt = Just timestamp
            }
      unsafeModifyMutableSpan mutableSpan \s ->
        (s { spanIsRecording = False, spanFrozenAt = Just timestamp }, ())
      where
      Tracer
        { tracerNow = now
        , tracerProcessSpan
        , tracerSpanAttrsLimits = spanAttrsLimits
        , tracerSpanEventAttrsLimits = spanEventAttrsLimits
        , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
        } = tracer

    handler :: Tracer -> MutableSpan -> SomeException -> IO ()
    handler tracer mutableSpan someEx = do
      spanUpdater <- do
        buildSpanUpdater now $ recordException someEx True TimestampSourceNow mempty
      unsafeModifyMutableSpan mutableSpan \s ->
        (spanUpdater s, ())
      -- N.B. It is important that we finish the span after recording the
      -- exception and not the other way around, because the span is no longer
      -- recording after it is ended.
      processSpan tracer mutableSpan
      where
      Tracer { tracerNow = now } = tracer

  getSpanContext mutableSpan = do
    liftIO $ fmap spanContext $ unsafeReadMutableSpan mutableSpan

  updateSpan mutableSpan updateSpanSpec =
    TracingT \tracingBackend -> do
      liftIO do
        spanUpdater <- do
          buildSpanUpdater (tracerNow $ tracingBackendTracer tracingBackend) updateSpanSpec
        unsafeModifyMutableSpan mutableSpan \s ->
          (spanUpdater s, ())

instance (MonadIO m, MonadMask m) => MonadTracingIO (TracingT m) where
  askTracerIO = TracingT $ pure . tracingBackendTracer

mapTracingT
  :: forall m n a b
   . (m a -> n b)
  -> TracingT m a
  -> TracingT n b
mapTracingT f action = TracingT $ f . runTracingT action
{-# INLINE mapTracingT #-}

data TracingBackend = TracingBackend
  { tracingBackendTracer :: Tracer
  , tracingBackendContextBackend :: ContextBackend MutableSpan
  }

toTracingBackend :: Tracer -> TracingBackend
toTracingBackend tracer =
  TracingBackend
    { tracingBackendTracer = tracer
    , tracingBackendContextBackend = contextBackendSpan
    }

getTracingBackend
  :: forall m
   . (MonadIO m)
  => TracerProvider
  -> InstrumentationScope
  -> m TracingBackend
getTracingBackend tracerProvider instScope =
  fmap toTracingBackend $ getTracer tracerProvider instScope

getTracer
  :: forall m
   . (MonadIO m)
  => TracerProvider
  -> InstrumentationScope
  -> m Tracer
getTracer tracerProvider = liftIO . tracerProviderGetTracer tracerProvider

shutdownTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
shutdownTracerProvider = liftIO . tracerProviderShutdown

forceFlushTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
forceFlushTracerProvider = liftIO . tracerProviderForceFlush


-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
