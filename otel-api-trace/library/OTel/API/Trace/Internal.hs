{-# LANGUAGE BlockArguments #-}
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
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    TracingT(..)
  , mapTracingT

  , SpanBackend(..)
  , defaultSpanBackend
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
import OTel.API.Context (ContextT(..), ContextBackend, attachContextValue, getAttachedContext)
import OTel.API.Core (AttrsFor(AttrsForSpan), KV(..), TimestampSource(..), AttrsBuilder)
import OTel.API.Trace.Core
  ( MonadTracing(..), MonadTracingContext(..), MonadTracingIO(..), NewSpanSpec(..)
  , Span(spanContext, spanFrozenAt, spanIsRecording), MutableSpan, contextBackendSpan
  , recordException, pattern CODE_FILEPATH, pattern CODE_FUNCTION, pattern CODE_LINENO
  , pattern CODE_NAMESPACE
  )
import OTel.API.Trace.Core.Internal
  ( Tracer(..), buildSpanUpdater, freezeSpan, unsafeModifyMutableSpan, unsafeReadMutableSpan
  )
import Prelude hiding (span)
import qualified Control.Exception.Safe as Safe
import qualified GHC.Stack as Stack

type TracingT :: (Type -> Type) -> Type -> Type
newtype TracingT m a = TracingT
  { runTracingT :: Tracer -> SpanBackend -> m a
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
      ) via (ReaderT Tracer (ReaderT SpanBackend m))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Tracer (ReaderT SpanBackend m)) a)

instance MonadTrans TracingT where
  lift = TracingT . const . const
  {-# INLINE lift #-}

instance MonadTransControl TracingT where
    type StT TracingT a = a
    liftWith f =
      TracingT \tracer spanBackend ->
        f \action ->
          runTracingT action tracer spanBackend
    restoreT = lift
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadReader r m) => MonadReader r (TracingT m) where
  ask = lift ask
  reader = lift . reader
  local = mapTracingT . local

instance (MonadRWS r w s m) => MonadRWS r w s (TracingT m)

instance (MonadIO m, MonadMask m) => MonadTracing (TracingT m) where
  traceCS cs newSpanSpec action =
    TracingT \tracer spanBackend -> do
      flip runContextT (unSpanBackend spanBackend) do
        parentCtx <- getAttachedContext
        mutableSpan <- do
          liftIO $ tracerStartSpan tracer parentCtx newSpanSpec
            { newSpanSpecAttrs = callStackAttrs <> newSpanSpecAttrs newSpanSpec
            }
        attachContextValue mutableSpan do
          result <- lift $ runTracingT (action mutableSpan) tracer spanBackend
          liftIO do
            result <$ processSpan tracer mutableSpan
              `Safe.withException` handler tracer mutableSpan
    where
    processSpan :: Tracer -> MutableSpan -> IO ()
    processSpan tracer mutableSpan = do
      liftIO do
        span <- unsafeReadMutableSpan mutableSpan
        timestamp <- now
        liftIO
          $ tracerProcessSpan
          $ freezeSpan
              timestamp
              spanLinkAttrsLimits
              spanEventAttrsLimits
              spanAttrsLimits
              span
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
        buildSpanUpdater (liftIO now) $ recordException someEx True TimestampSourceNow mempty
      liftIO $ unsafeModifyMutableSpan mutableSpan \s ->
        (spanUpdater s, ())
      -- N.B. It is important that we finish the span after recording the
      -- exception and not the other way around, because the span is no longer
      -- recording after it is ended.
      processSpan tracer mutableSpan
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

instance (MonadIO m, MonadMask m) => MonadTracingIO (TracingT m) where
  askTracer = TracingT \tracer _spanBackend -> pure tracer

instance (MonadIO m, MonadMask m) => MonadTracingContext (TracingT m) where
  getSpanContext mutableSpan =
    TracingT \_tracer _spanBackend -> do
      liftIO $ fmap spanContext $ unsafeReadMutableSpan mutableSpan

  updateSpan mutableSpan updateSpanSpec =
    TracingT \tracer _spanBackend -> do
      liftIO do
        spanUpdater <- buildSpanUpdater (liftIO $ tracerNow tracer) updateSpanSpec
        unsafeModifyMutableSpan mutableSpan \s ->
          (spanUpdater s, ())

mapTracingT
  :: forall m n a b
   . (m a -> n b)
  -> TracingT m a
  -> TracingT n b
mapTracingT f action =
  TracingT \tracer spanBackend ->
    f $ runTracingT action tracer spanBackend
{-# INLINE mapTracingT #-}

newtype SpanBackend = SpanBackend
  { unSpanBackend :: ContextBackend MutableSpan
  }

defaultSpanBackend :: SpanBackend
defaultSpanBackend =
  SpanBackend
    { unSpanBackend = contextBackendSpan
    }

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
