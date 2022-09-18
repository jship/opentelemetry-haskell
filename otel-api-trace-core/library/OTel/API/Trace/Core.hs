{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import OTel.API.Core (NewSpanSpec, SpanContext, Tracer, UpdateSpanSpec, HasSpan)
import OTel.API.Core.Internal (MutableSpan(..))
import Prelude
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict

trace
  :: (MonadTracing ctx m, HasCallStack)
  => NewSpanSpec ctx
  -> (MutableSpan ctx -> m a)
  -> m a
trace = traceCS callStack

trace_
  :: (MonadTracing ctx m, HasCallStack)
  => NewSpanSpec ctx
  -> m a
  -> m a
trace_ newSpanSpec = traceCS callStack newSpanSpec . const

class (Monad m, HasSpan ctx) => MonadTracing ctx m | m -> ctx where
  traceCS :: CallStack -> NewSpanSpec ctx -> (MutableSpan ctx -> m a) -> m a

  default traceCS
    :: (MonadTransControl t, MonadTracing ctx n, m ~ t n)
    => CallStack
    -> NewSpanSpec ctx
    -> (MutableSpan ctx -> m a)
    -> m a
  traceCS cs newSpanSpec f = do
    restoreT . pure
      =<< liftWith \run -> traceCS cs newSpanSpec (run . f)

instance (MonadTracing ctx m) => MonadTracing ctx (ExceptT e m)
instance (MonadTracing ctx m) => MonadTracing ctx (IdentityT m)
instance (MonadTracing ctx m) => MonadTracing ctx (MaybeT m)
instance (MonadTracing ctx m) => MonadTracing ctx (ReaderT r m)
instance (MonadTracing ctx m) => MonadTracing ctx (State.Lazy.StateT r m)
instance (MonadTracing ctx m) => MonadTracing ctx (State.Strict.StateT r m)
instance (MonadTracing ctx m, Monoid w) => MonadTracing ctx (RWS.Lazy.RWST r w s m)
instance (MonadTracing ctx m, Monoid w) => MonadTracing ctx (RWS.Strict.RWST r w s m)
instance (MonadTracing ctx m, Monoid w) => MonadTracing ctx (Writer.Lazy.WriterT w m)
instance (MonadTracing ctx m, Monoid w) => MonadTracing ctx (Writer.Strict.WriterT w m)
instance (MonadTracing ctx m) => MonadTracing ctx (LoggingT m)
instance (MonadTracing ctx m, MonadUnliftIO m) => MonadTracing ctx (ResourceT m) where
  traceCS cs newSpanSpec f = do
    withRunInIO \runInIO -> do
      runInIO $ traceCS cs newSpanSpec f

class (MonadTracing ctx m) => MonadTracingContext ctx m where
  getSpanContext :: MutableSpan ctx -> m SpanContext
  updateSpan :: MutableSpan ctx -> UpdateSpanSpec -> m ()

  default getSpanContext
    :: (MonadTrans t, MonadTracingContext ctx n, m ~ t n)
    => MutableSpan ctx
    -> m SpanContext
  getSpanContext = lift . getSpanContext

  default updateSpan
    :: (MonadTrans t, MonadTracingContext ctx n, m ~ t n)
    => MutableSpan ctx
    -> UpdateSpanSpec
    -> m ()
  updateSpan ctxKey = lift . updateSpan ctxKey

instance (MonadTracingContext ctx m) => MonadTracingContext ctx (ExceptT e m)
instance (MonadTracingContext ctx m) => MonadTracingContext ctx (IdentityT m)
instance (MonadTracingContext ctx m) => MonadTracingContext ctx (MaybeT m)
instance (MonadTracingContext ctx m) => MonadTracingContext ctx (ReaderT r m)
instance (MonadTracingContext ctx m) => MonadTracingContext ctx (State.Lazy.StateT r m)
instance (MonadTracingContext ctx m) => MonadTracingContext ctx (State.Strict.StateT r m)
instance (MonadTracingContext ctx m, Monoid w) => MonadTracingContext ctx (RWS.Lazy.RWST r w s m)
instance (MonadTracingContext ctx m, Monoid w) => MonadTracingContext ctx (RWS.Strict.RWST r w s m)
instance (MonadTracingContext ctx m, Monoid w) => MonadTracingContext ctx (Writer.Lazy.WriterT w m)
instance (MonadTracingContext ctx m, Monoid w) => MonadTracingContext ctx (Writer.Strict.WriterT w m)
instance (MonadTracingContext ctx m) => MonadTracingContext ctx (LoggingT m)
instance (MonadTracingContext ctx m, MonadUnliftIO m) => MonadTracingContext ctx (ResourceT m) where
  getSpanContext mutableSpan = do
    withRunInIO \runInIO -> do
      runInIO $ getSpanContext mutableSpan

  updateSpan mutableSpan updateSpanSpec = do
    withRunInIO \runInIO -> do
      runInIO $ updateSpan mutableSpan updateSpanSpec

class (MonadTracing ctx m, MonadIO m) => MonadTracingIO ctx m where
  askTracer :: m (Tracer ctx)

  default askTracer
    :: (MonadTrans t, MonadTracingIO ctx n, m ~ t n)
    => m (Tracer ctx)
  askTracer = lift askTracer

instance (MonadTracingIO ctx m) => MonadTracingIO ctx (ExceptT e m)
instance (MonadTracingIO ctx m) => MonadTracingIO ctx (IdentityT m)
instance (MonadTracingIO ctx m) => MonadTracingIO ctx (MaybeT m)
instance (MonadTracingIO ctx m) => MonadTracingIO ctx (ReaderT r m)
instance (MonadTracingIO ctx m) => MonadTracingIO ctx (State.Lazy.StateT r m)
instance (MonadTracingIO ctx m) => MonadTracingIO ctx (State.Strict.StateT r m)
instance (MonadTracingIO ctx m, Monoid w) => MonadTracingIO ctx (RWS.Lazy.RWST r w s m)
instance (MonadTracingIO ctx m, Monoid w) => MonadTracingIO ctx (RWS.Strict.RWST r w s m)
instance (MonadTracingIO ctx m, Monoid w) => MonadTracingIO ctx (Writer.Lazy.WriterT w m)
instance (MonadTracingIO ctx m, Monoid w) => MonadTracingIO ctx (Writer.Strict.WriterT w m)
instance (MonadTracingIO ctx m) => MonadTracingIO ctx (LoggingT m)
instance (MonadTracingIO ctx m, MonadUnliftIO m) => MonadTracingIO ctx (ResourceT m) where
  askTracer = do
    withRunInIO \runInIO -> do
      runInIO askTracer

-- $synopsis
--
-- @otel-api-trace-core@ STUB
--
