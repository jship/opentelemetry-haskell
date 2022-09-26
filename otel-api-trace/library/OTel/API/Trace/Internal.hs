{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, SomeException)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
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
import OTel.API.Common (AttrsFor(AttrsForSpan), KV(..), TimestampSource(..), AttrsBuilder)
import OTel.API.Context (ContextT(..), ContextBackend, attachContextValue, getAttachedContext)
import OTel.API.Trace.Core
  ( MonadTracing(..), MonadTracingIO(..), NewSpanSpec(..)
  , Span(spanContext, spanFrozenAt, spanIsRecording), MutableSpan, contextBackendSpan
  , recordException
  )
import OTel.API.Trace.Core.Internal
  ( Tracer(..), buildSpanUpdater, freezeSpan, unsafeModifyMutableSpan, unsafeReadMutableSpan
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
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
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
      ( MonadTrans -- @base@
      , MonadTransControl -- @monad-control@
      ) via (ReaderT TracingBackend)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT TracingBackend m) a)

instance (MonadReader r m) => MonadReader r (TracingT m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT m)

instance (MonadIO m, MonadMask m) => MonadTracing (TracingT m) where
  traceCS cs newSpanSpec action =
    TracingT \tracingBackend -> do
      flip runContextT (tracingBackendContextBackend tracingBackend) do
        parentCtx <- getAttachedContext
        (mutableSpan, spanContextMeta) <- do
          liftIO $ tracerStartSpan (tracingBackendTracer tracingBackend) cs parentCtx newSpanSpec
        attachContextValue mutableSpan do
          withThreadContext spanContextMeta do
            result <- lift $ runTracingT (action mutableSpan) tracingBackend
            liftIO do
              result <$ processSpan (tracingBackendTracer tracingBackend) mutableSpan
                `Safe.withException` handler (tracingBackendTracer tracingBackend) mutableSpan
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

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
