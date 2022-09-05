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
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import GHC.Stack (SrcLoc(..))
import OTel.API.Context (ContextT(..), ContextKey, getAttachedContextKey, getContext, updateContext)
import OTel.API.Context.Internal (unsafeAttachContext)
import OTel.API.Core
  ( AttrsFor(AttrsForSpan), KV(..), NewSpanSpec(..), Span(spanContext, spanIsRecording)
  , SpanParent(..), SpanParentSource(..), SpanSpec(..), TimestampSource(..), AttrsBuilder
  , buildSpanSpec, recordException, toEndedSpan, pattern CODE_FILEPATH, pattern CODE_FUNCTION
  , pattern CODE_LINENO, pattern CODE_NAMESPACE
  )
import OTel.API.Core.Internal (MutableSpan(..), Tracer(..), buildSpanUpdater)
import OTel.API.Trace.Core (MonadTraceContext(..), MonadTracing(..))
import Prelude hiding (span)
import qualified Control.Exception.Safe as Safe
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
      ) via (ReaderT Tracer m)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Tracer m) a)

instance (MonadReader r m) => MonadReader r (TracingT m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT m)

instance MonadTrans TracingT where
  lift action =
    TracingT \_tracer ->
      action

instance MonadTransControl TracingT where
  type StT TracingT a = a
  liftWith action =
    TracingT \tracer ->
      action $ \t -> runTracingT t tracer
  restoreT = lift

instance (MonadIO m, MonadMask m) => MonadTracing (TracingT m) where
  traceCS cs newSpanSpec action =
    TracingT \tracer -> do
      flip runContextT (tracerContextBackend tracer) do
        spanSpec <- toSpanSpec tracer newSpanSpec
        mutableSpan@MutableSpan { mutableSpanSpanKey = spanKey } <- do
          liftIO $ tracerStartSpan tracer spanSpec
        unsafeAttachContext spanKey do
          result <- lift $ runTracingT (action mutableSpan) tracer
          result <$ processSpan tracer spanKey
          `Safe.withException` handler tracer spanKey
    where
    processSpan :: Tracer -> ContextKey Span -> ContextT Span m ()
    processSpan tracer spanKey = do
      span <- getContext spanKey
      timestamp <- liftIO now
      liftIO
        $ tracerProcessSpan
        $ toEndedSpan
            timestamp
            spanLinkAttrsLimits
            spanEventAttrsLimits
            spanAttrsLimits
            span
      void $ updateContext spanKey \s -> s { spanIsRecording = False }
      where
      Tracer
        { tracerNow = now
        , tracerProcessSpan
        , tracerSpanAttrsLimits = spanAttrsLimits
        , tracerSpanEventAttrsLimits = spanEventAttrsLimits
        , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
        } = tracer

    handler :: Tracer -> ContextKey Span -> SomeException -> ContextT Span m ()
    handler tracer spanKey someEx = do
      -- TODO: Set status to error? Spec docs make it sound like application
      -- authors set the status, and the API shouldn't set the status.
      -- https://opentelemetry.io/docs/reference/specification/trace/api/#set-status
      -- Probably would be convenient if this was a config option.
      _ <- updateContext spanKey <=< buildSpanUpdater (liftIO now) $
        recordException someEx True TimestampSourceNow mempty
      -- N.B. It is important that we finish the span after recording the
      -- exception and not the other way around, because the span is no longer
      -- recording after it is ended.
      processSpan tracer spanKey
      where
      Tracer { tracerNow = now } = tracer

    toSpanSpec :: Tracer -> NewSpanSpec -> ContextT Span m SpanSpec
    toSpanSpec tracer spec =
      buildSpanSpec (liftIO now) spanParentFromSource spec { newSpanSpecAttrs }
      where
      spanParentFromSource
        :: SpanParentSource
        -> ContextT Span m SpanParent
      spanParentFromSource = \case
        SpanParentSourceExplicit spanParent -> pure spanParent
        SpanParentSourceImplicit -> do
          getAttachedContextKey >>= \case
            Nothing -> pure SpanParentRoot
            Just ctxKey -> do
              parentSpanContext <- fmap spanContext $ getContext ctxKey
              pure $ SpanParentChildOf parentSpanContext

      newSpanSpecAttrs :: AttrsBuilder 'AttrsForSpan
      newSpanSpecAttrs =
        case Stack.getCallStack cs of
          ((function, srcLoc) : _) ->
            CODE_FUNCTION .@ function
              <> CODE_NAMESPACE .@ srcLocModule srcLoc
              <> CODE_FILEPATH .@ srcLocFile srcLoc
              <> CODE_LINENO .@ srcLocStartLine srcLoc
          _ -> mempty

      Tracer { tracerNow = now } = tracer

instance (MonadIO m) => MonadTraceContext (TracingT m) where
  getSpanContext mutableSpan =
    TracingT \tracer -> do
      flip runContextT (tracerContextBackend tracer) do
        fmap spanContext $ getContext $ mutableSpanSpanKey mutableSpan

  updateSpan mutableSpan updateSpanSpec =
    TracingT \tracer -> do
      flip runContextT (tracerContextBackend tracer) do
        updateContext (mutableSpanSpanKey mutableSpan)
          =<< buildSpanUpdater (liftIO $ tracerNow tracer) updateSpanSpec

mapTracingT
  :: forall m n a b
   . (m a -> n b)
  -> TracingT m a
  -> TracingT n b
mapTracingT f action =
  TracingT \tracer ->
    f $ runTracingT action tracer

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
