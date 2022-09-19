{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Core
  ( -- * Synopsis
    -- $synopsis
    trace
  , trace_
  , MonadTracing(..)
  , MonadTracingContext(..)
  , MonadTracingIO(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl(liftWith, restoreT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
import GHC.Stack (CallStack, HasCallStack, callStack)
import OTel.API.Core (NewSpanSpec, SpanContext, Tracer, UpdateSpanSpec)
import OTel.API.Core.Internal (MutableSpan(..))
import Prelude
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict

trace
  :: (MonadTracing m, HasCallStack)
  => NewSpanSpec
  -> (MutableSpan -> m a)
  -> m a
trace = traceCS callStack

trace_
  :: (MonadTracing m, HasCallStack)
  => NewSpanSpec
  -> m a
  -> m a
trace_ newSpanSpec = traceCS callStack newSpanSpec . const

class (Monad m) => MonadTracing m where
  traceCS :: CallStack -> NewSpanSpec -> (MutableSpan -> m a) -> m a

  default traceCS
    :: (MonadTransControl t, MonadTracing n, m ~ t n)
    => CallStack
    -> NewSpanSpec
    -> (MutableSpan -> m a)
    -> m a
  traceCS cs newSpanSpec f = do
    restoreT . pure
      =<< liftWith \run -> traceCS cs newSpanSpec (run . f)

instance (MonadTracing m) => MonadTracing (ExceptT e m)
instance (MonadTracing m) => MonadTracing (IdentityT m)
instance (MonadTracing m) => MonadTracing (MaybeT m)
instance (MonadTracing m) => MonadTracing (ReaderT r m)
instance (MonadTracing m) => MonadTracing (State.Lazy.StateT r m)
instance (MonadTracing m) => MonadTracing (State.Strict.StateT r m)
instance (MonadTracing m, Monoid w) => MonadTracing (RWS.Lazy.RWST r w s m)
instance (MonadTracing m, Monoid w) => MonadTracing (RWS.Strict.RWST r w s m)
instance (MonadTracing m, Monoid w) => MonadTracing (Writer.Lazy.WriterT w m)
instance (MonadTracing m, Monoid w) => MonadTracing (Writer.Strict.WriterT w m)
instance (MonadTracing m) => MonadTracing (LoggingT m)
instance (MonadTracing m, MonadUnliftIO m) => MonadTracing (ResourceT m) where
  traceCS cs newSpanSpec f = do
    withRunInIO \runInIO -> do
      runInIO $ traceCS cs newSpanSpec f

class (MonadTracing m) => MonadTracingContext m where
  getSpanContext :: MutableSpan -> m SpanContext
  updateSpan :: MutableSpan -> UpdateSpanSpec -> m ()

  default getSpanContext
    :: (MonadTrans t, MonadTracingContext n, m ~ t n)
    => MutableSpan
    -> m SpanContext
  getSpanContext = lift . getSpanContext

  default updateSpan
    :: (MonadTrans t, MonadTracingContext n, m ~ t n)
    => MutableSpan
    -> UpdateSpanSpec
    -> m ()
  updateSpan mutableSpan = lift . updateSpan mutableSpan

instance (MonadTracingContext m) => MonadTracingContext (ExceptT e m)
instance (MonadTracingContext m) => MonadTracingContext (IdentityT m)
instance (MonadTracingContext m) => MonadTracingContext (MaybeT m)
instance (MonadTracingContext m) => MonadTracingContext (ReaderT r m)
instance (MonadTracingContext m) => MonadTracingContext (State.Lazy.StateT r m)
instance (MonadTracingContext m) => MonadTracingContext (State.Strict.StateT r m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (RWS.Lazy.RWST r w s m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (RWS.Strict.RWST r w s m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (Writer.Lazy.WriterT w m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (Writer.Strict.WriterT w m)
instance (MonadTracingContext m) => MonadTracingContext (LoggingT m)
instance (MonadTracingContext m, MonadUnliftIO m) => MonadTracingContext (ResourceT m) where
  getSpanContext mutableSpan = do
    withRunInIO \runInIO -> do
      runInIO $ getSpanContext mutableSpan

  updateSpan mutableSpan updateSpanSpec = do
    withRunInIO \runInIO -> do
      runInIO $ updateSpan mutableSpan updateSpanSpec

class (MonadTracing m, MonadIO m) => MonadTracingIO m where
  askTracer :: m (Tracer)

  default askTracer
    :: (MonadTrans t, MonadTracingIO n, m ~ t n)
    => m (Tracer)
  askTracer = lift askTracer

instance (MonadTracingIO m) => MonadTracingIO (ExceptT e m)
instance (MonadTracingIO m) => MonadTracingIO (IdentityT m)
instance (MonadTracingIO m) => MonadTracingIO (MaybeT m)
instance (MonadTracingIO m) => MonadTracingIO (ReaderT r m)
instance (MonadTracingIO m) => MonadTracingIO (State.Lazy.StateT r m)
instance (MonadTracingIO m) => MonadTracingIO (State.Strict.StateT r m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (RWS.Lazy.RWST r w s m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (RWS.Strict.RWST r w s m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (Writer.Lazy.WriterT w m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (Writer.Strict.WriterT w m)
instance (MonadTracingIO m) => MonadTracingIO (LoggingT m)
instance (MonadTracingIO m, MonadUnliftIO m) => MonadTracingIO (ResourceT m) where
  askTracer = do
    withRunInIO \runInIO -> do
      runInIO askTracer

-- $synopsis
--
-- @otel-api-trace-core@ STUB
--
