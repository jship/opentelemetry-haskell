{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
module OTel.API.Trace.Core
  ( -- * Synopsis
    -- $synopsis
    trace
  , trace_
  , MonadTracing(..)
  , MonadTracingContext(..)
  , MonadTracingEnv(..)
  ) where

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
import OTel.API.Core (AttrsBuilder, NewSpanSpec, Span, Tracer, UpdateSpanSpec)
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
  getSpan :: MutableSpan -> m (Span AttrsBuilder)
  updateSpan :: MutableSpan -> UpdateSpanSpec -> m (Span AttrsBuilder)

  default getSpan
    :: (MonadTrans t, MonadTracingContext n, m ~ t n)
    => MutableSpan
    -> m (Span AttrsBuilder)
  getSpan = lift . getSpan

  default updateSpan
    :: (MonadTrans t, MonadTracingContext n, m ~ t n)
    => MutableSpan
    -> UpdateSpanSpec
    -> m (Span AttrsBuilder)
  updateSpan ctxKey = lift . updateSpan ctxKey

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
  getSpan mutableSpan = do
    withRunInIO \runInIO -> do
      runInIO $ getSpan mutableSpan

  updateSpan mutableSpan updateSpanSpec = do
    withRunInIO \runInIO -> do
      runInIO $ updateSpan mutableSpan updateSpanSpec

class (MonadTracing m) => MonadTracingEnv m where
  askTracer :: m Tracer

  default askTracer
    :: (MonadTrans t, MonadTracingEnv n, m ~ t n)
    => m Tracer
  askTracer = lift askTracer

instance (MonadTracingEnv m) => MonadTracingEnv (ExceptT e m)
instance (MonadTracingEnv m) => MonadTracingEnv (IdentityT m)
instance (MonadTracingEnv m) => MonadTracingEnv (MaybeT m)
instance (MonadTracingEnv m) => MonadTracingEnv (ReaderT r m)
instance (MonadTracingEnv m) => MonadTracingEnv (State.Lazy.StateT r m)
instance (MonadTracingEnv m) => MonadTracingEnv (State.Strict.StateT r m)
instance (MonadTracingEnv m, Monoid w) => MonadTracingEnv (RWS.Lazy.RWST r w s m)
instance (MonadTracingEnv m, Monoid w) => MonadTracingEnv (RWS.Strict.RWST r w s m)
instance (MonadTracingEnv m, Monoid w) => MonadTracingEnv (Writer.Lazy.WriterT w m)
instance (MonadTracingEnv m, Monoid w) => MonadTracingEnv (Writer.Strict.WriterT w m)
instance (MonadTracingEnv m) => MonadTracingEnv (LoggingT m)
instance (MonadTracingEnv m, MonadUnliftIO m) => MonadTracingEnv (ResourceT m) where
  askTracer = do
    withRunInIO \runInIO -> do
      runInIO askTracer

-- $synopsis
--
-- @otel-api-trace-core@ STUB
--
