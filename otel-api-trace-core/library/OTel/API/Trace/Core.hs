{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
module OTel.API.Trace.Core
  ( -- * Synopsis
    -- $synopsis
    trace
  , trace_
  , MonadTracing(..)
  , MonadTraceContext(..)
  , MutableSpan
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Control (MonadTransControl(liftWith, restoreT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Select (SelectT)
import GHC.Stack (CallStack, HasCallStack, callStack)
import OTel.API.Context (ContextSnapshot)
import OTel.API.Core (Span, NewSpanSpec, SpanContext, UpdateSpanSpec)
import OTel.API.Core.Internal (MutableSpan(..))
import Prelude
import qualified Control.Monad.Trans.RWS.CPS as RWS.CPS
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.CPS as Writer.CPS
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

class (Monad m) => MonadTraceContext m where
  getSpanContext :: MutableSpan -> m (ContextSnapshot SpanContext)
  updateSpan :: MutableSpan -> UpdateSpanSpec -> m (ContextSnapshot Span)

  default getSpanContext
    :: (MonadTrans t, MonadTraceContext n, m ~ t n)
    => MutableSpan
    -> m (ContextSnapshot SpanContext)
  getSpanContext = lift . getSpanContext

  default updateSpan
    :: (MonadTrans t, MonadTraceContext n, m ~ t n)
    => MutableSpan
    -> UpdateSpanSpec
    -> m (ContextSnapshot Span)
  updateSpan ctxKey = lift . updateSpan ctxKey

instance (MonadTraceContext m, Monoid w) => MonadTraceContext (AccumT w m)
instance (MonadTraceContext m) => MonadTraceContext (ContT r m)
instance (MonadTraceContext m) => MonadTraceContext (ExceptT e m)
instance (MonadTraceContext m) => MonadTraceContext (IdentityT m)
instance (MonadTraceContext m) => MonadTraceContext (MaybeT m)
instance (MonadTraceContext m) => MonadTraceContext (ReaderT r m)
instance (MonadTraceContext m) => MonadTraceContext (SelectT r m)
instance (MonadTraceContext m) => MonadTraceContext (State.Lazy.StateT r m)
instance (MonadTraceContext m) => MonadTraceContext (State.Strict.StateT r m)
instance (MonadTraceContext m, Monoid w) => MonadTraceContext (RWS.CPS.RWST r w s m)
instance (MonadTraceContext m, Monoid w) => MonadTraceContext (RWS.Lazy.RWST r w s m)
instance (MonadTraceContext m, Monoid w) => MonadTraceContext (RWS.Strict.RWST r w s m)
instance (MonadTraceContext m, Monoid w) => MonadTraceContext (Writer.CPS.WriterT w m)
instance (MonadTraceContext m, Monoid w) => MonadTraceContext (Writer.Lazy.WriterT w m)
instance (MonadTraceContext m, Monoid w) => MonadTraceContext (Writer.Strict.WriterT w m)
instance (MonadTraceContext m) => MonadTraceContext (LoggingT m)
instance (MonadTraceContext m) => MonadTraceContext (ResourceT m)

-- $synopsis
--
-- @otel-api-trace-core@ STUB
--
