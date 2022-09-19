{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    TracingT(..)
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
import GHC.Stack (SrcLoc(..))
import OTel.API.Baggage.Core (MonadBaggage)
import OTel.API.Context (ContextT(..), attachContextValue, getAttachedContext)
import OTel.API.Core (AttrsFor(AttrsForSpan), KV(..), TimestampSource(..), AttrsBuilder)
import OTel.API.Trace.Core
  ( MonadTracing(..), MonadTracingContext(..), MonadTracingIO(..), NewSpanSpec(..)
  , Span(spanContext, spanFrozenAt, spanIsRecording), recordException, pattern CODE_FILEPATH
  , pattern CODE_FUNCTION, pattern CODE_LINENO, pattern CODE_NAMESPACE
  )
import OTel.API.Trace.Core.Internal
  ( MutableSpan(..), SpanBackend(..), Tracer(..), buildSpanUpdater, freezeSpan
  )
import Prelude hiding (span)
import qualified Control.Exception.Safe as Safe
import qualified Data.IORef as IORef
import qualified GHC.Stack as Stack

type TracingT :: (Type -> Type) -> Type -> Type
newtype TracingT m a = TracingT
  { runTracingT :: Tracer -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
      , MonadAccum w, MonadCont, MonadError e, MonadSelect r, MonadState s, MonadWriter w -- @mtl@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      , MonadBaggage -- @otel-api-baggage-core@
      ) via (ReaderT Tracer m)
    deriving
      ( MonadTrans -- @base@
      , MonadTransControl -- @monad-control@
      ) via (ReaderT Tracer)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Tracer m) a)

instance (MonadReader r m) => MonadReader r (TracingT m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT m)

instance (MonadIO m, MonadMask m) => MonadTracing (TracingT m) where
  traceCS cs newSpanSpec action =
    TracingT \tracer -> do
      flip runContextT (unSpanBackend $ tracerSpanBackend tracer) do
        parentCtx <- getAttachedContext
        mutableSpan <- do
          liftIO $ tracerStartSpan tracer parentCtx newSpanSpec
            { newSpanSpecAttrs = callStackAttrs <> newSpanSpecAttrs newSpanSpec
            }
        attachContextValue mutableSpan do
          result <- lift $ runTracingT (action mutableSpan) tracer
          liftIO do
            result <$ processSpan tracer mutableSpan
              `Safe.withException` handler tracer mutableSpan
    where
    processSpan :: Tracer -> MutableSpan -> IO ()
    processSpan tracer mutableSpan = do
      liftIO do
        span <- IORef.atomicModifyIORef' ref \s -> (s, s)
        timestamp <- now
        liftIO
          $ tracerProcessSpan
          $ freezeSpan
              timestamp
              spanLinkAttrsLimits
              spanEventAttrsLimits
              spanAttrsLimits
              span
        IORef.atomicModifyIORef' ref \s ->
          (s { spanIsRecording = False, spanFrozenAt = Just timestamp }, ())
      where
      Tracer
        { tracerNow = now
        , tracerProcessSpan
        , tracerSpanAttrsLimits = spanAttrsLimits
        , tracerSpanEventAttrsLimits = spanEventAttrsLimits
        , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
        } = tracer
      MutableSpan { unMutableSpan = ref } = mutableSpan

    handler :: Tracer -> MutableSpan -> SomeException -> IO ()
    handler tracer mutableSpan someEx = do
      -- TODO: Set status to error? Spec docs make it sound like application
      -- authors set the status, and the API shouldn't set the status.
      -- https://opentelemetry.io/docs/reference/specification/trace/api/#set-status
      -- Probably would be convenient if this was a config option.
      spanUpdater <- do
        buildSpanUpdater (liftIO now) $ recordException someEx True TimestampSourceNow mempty
      liftIO $ IORef.atomicModifyIORef' ref \span ->
        (spanUpdater span, ())
      -- N.B. It is important that we finish the span after recording the
      -- exception and not the other way around, because the span is no longer
      -- recording after it is ended.
      processSpan tracer mutableSpan
      where
      Tracer { tracerNow = now } = tracer
      MutableSpan { unMutableSpan = ref } = mutableSpan

    callStackAttrs :: AttrsBuilder 'AttrsForSpan
    callStackAttrs =
      case Stack.getCallStack cs of
        ((function, srcLoc) : _) ->
          CODE_FUNCTION .@ function
            <> CODE_NAMESPACE .@ srcLocModule srcLoc
            <> CODE_FILEPATH .@ srcLocFile srcLoc
            <> CODE_LINENO .@ srcLocStartLine srcLoc
        _ -> mempty

instance (MonadIO m, MonadMask m) => MonadTracingIO (TracingT m) where
  askTracer = TracingT pure

instance (MonadIO m, MonadMask m) => MonadTracingContext (TracingT m) where
  getSpanContext mutableSpan =
    TracingT \_tracer -> do
      liftIO $ IORef.atomicModifyIORef' ref \span -> (span, spanContext span)
    where
    MutableSpan { unMutableSpan = ref } = mutableSpan

  updateSpan mutableSpan updateSpanSpec =
    TracingT \tracer -> do
      liftIO do
        spanUpdater <- buildSpanUpdater (liftIO $ tracerNow tracer) updateSpanSpec
        IORef.atomicModifyIORef' ref \span ->
          (spanUpdater span, ())
    where
    MutableSpan { unMutableSpan = ref } = mutableSpan

mapTracingT
  :: forall m n a b
   . (m a -> n b)
  -> TracingT m a
  -> TracingT n b
mapTracingT f action = TracingT $ f . runTracingT action

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
