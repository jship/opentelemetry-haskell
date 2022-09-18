{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    TracingT(..)
  , mapTracingT
  ) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, SomeException)
import Control.Monad ((<=<), void)
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
import Data.Function ((&))
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import GHC.Stack (SrcLoc(..))
import Lens.Micro (over, set)
import Lens.Micro.Extras (view)
import OTel.API.Baggage.Core (MonadBaggage)
import OTel.API.Context (ContextT(..), ContextKey, getAttachedContextKey, getContext, updateContext)
import OTel.API.Context.Internal (unsafeAttachContext, unsafeNewContextBackend)
import OTel.API.Core
  ( AttrsFor(AttrsForSpan), HasSpan(..), KV(..), NewSpanSpec(..)
  , Span(spanContext, spanFrozenAt, spanIsRecording), TimestampSource(..), AttrsBuilder
  , recordException, pattern CODE_FILEPATH, pattern CODE_FUNCTION, pattern CODE_LINENO
  , pattern CODE_NAMESPACE
  )
import OTel.API.Core.Internal (MutableSpan(..), Tracer(..), buildSpanUpdater, freezeSpan)
import OTel.API.Trace.Core (MonadTracing(..), MonadTracingContext(..), MonadTracingIO(..))
import Prelude hiding (span)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception.Safe as Safe
import qualified GHC.Stack as Stack

type TracingT :: Type -> (Type -> Type) -> Type -> Type
newtype TracingT ctx m a = TracingT
  { runTracingT :: Tracer ctx -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
      , MonadAccum w, MonadCont, MonadError e, MonadSelect r, MonadState s, MonadWriter w -- @mtl@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      , MonadBaggage ctx -- @otel-api-baggage-core@
      ) via (ReaderT (Tracer ctx) m)
    deriving
      ( MonadTrans -- @base@
      , MonadTransControl -- @monad-control@
      ) via (ReaderT (Tracer ctx))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (Tracer ctx) m) a)

instance (MonadReader r m) => MonadReader r (TracingT ctx m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT ctx m)

instance (MonadIO m, MonadMask m, HasSpan ctx) => MonadTracing ctx (TracingT ctx m) where
  traceCS cs newSpanSpec action =
    TracingT \tracer -> do
      flip runContextT (tracerContextBackend tracer) do
        parentCtx <- getContext =<< getAttachedContextKey
        mutableSpan@MutableSpan { mutableSpanContextKey = ctxKey } <- do
          liftIO $ tracerStartSpan tracer parentCtx newSpanSpec
            { newSpanSpecAttrs = callStackAttrs <> newSpanSpecAttrs newSpanSpec
            }
        unsafeAttachContext ctxKey do
          result <- lift $ runTracingT (action mutableSpan) tracer
          result <$ processSpan tracer ctxKey
          `Safe.withException` handler tracer ctxKey
    where
    processSpan
      :: Tracer ctx
      -> ContextKey ctx
      -> ContextT ctx m ()
    processSpan tracer ctxKey = do
      span <- fmap (view _span) $ getContext ctxKey
      timestamp <- liftIO now
      liftIO
        $ tracerProcessSpan
        $ freezeSpan
            timestamp
            spanLinkAttrsLimits
            spanEventAttrsLimits
            spanAttrsLimits
            span
      void $ updateContext ctxKey \ctx ->
        ctx
          & set _spanIsRecording False
          & set _spanFrozenAt (Just timestamp)
      where
      Tracer
        { tracerNow = now
        , tracerProcessSpan
        , tracerSpanAttrsLimits = spanAttrsLimits
        , tracerSpanEventAttrsLimits = spanEventAttrsLimits
        , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
        } = tracer

    handler
      :: Tracer ctx
      -> ContextKey ctx
      -> SomeException
      -> ContextT ctx m ()
    handler tracer ctxKey someEx = do
      -- TODO: Set status to error? Spec docs make it sound like application
      -- authors set the status, and the API shouldn't set the status.
      -- https://opentelemetry.io/docs/reference/specification/trace/api/#set-status
      -- Probably would be convenient if this was a config option.
      spanUpdater <- do
        buildSpanUpdater (liftIO now) $ recordException someEx True TimestampSourceNow mempty
      _ <- updateContext ctxKey $ over _span spanUpdater
      -- N.B. It is important that we finish the span after recording the
      -- exception and not the other way around, because the span is no longer
      -- recording after it is ended.
      processSpan tracer ctxKey
      where
      Tracer { tracerNow = now } = tracer

    callStackAttrs :: AttrsBuilder 'AttrsForSpan
    callStackAttrs =
      case Stack.getCallStack cs of
        ((function, srcLoc) : _) ->
          CODE_FUNCTION .@ function
            <> CODE_NAMESPACE .@ srcLocModule srcLoc
            <> CODE_FILEPATH .@ srcLocFile srcLoc
            <> CODE_LINENO .@ srcLocStartLine srcLoc
        _ -> mempty

instance (MonadIO m, MonadMask m, HasSpan ctx) => MonadTracingIO ctx (TracingT ctx m) where
  askTracer = TracingT pure

instance (MonadIO m, MonadMask m, HasSpan ctx) => MonadTracingContext ctx (TracingT ctx m) where
  getSpanContext mutableSpan =
    TracingT \tracer -> do
      flip runContextT (tracerContextBackend tracer) do
        fmap (view _spanContext) $ getContext $ mutableSpanContextKey mutableSpan

  updateSpan mutableSpan updateSpanSpec =
    TracingT \tracer -> do
      flip runContextT (tracerContextBackend tracer) do
         _span <- do
           spanUpdater <- buildSpanUpdater (liftIO $ tracerNow tracer) updateSpanSpec
           updateContext ctxKey $ over _span spanUpdater
         pure ()
    where
    MutableSpan { mutableSpanContextKey = ctxKey } = mutableSpan

mapTracingT
  :: forall ctx m n a b
   . (m a -> n b)
  -> TracingT ctx m a
  -> TracingT ctx n b
mapTracingT f action = TracingT $ f . runTracingT action

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
