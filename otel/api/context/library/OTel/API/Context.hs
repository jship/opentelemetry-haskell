{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Context
  ( -- * Intro
    -- $intro

    -- * @MonadKeyedContext@ class
    MonadKeyedContext(..)

    -- * @MonadContext@ class
  , MonadContext(..)

    -- * @ContextT@ monad transformer
  , Core.ContextT
  , Core.runContextT
  , Core.mapContextT

    -- * Context backend
  , Core.ContextBackend
  , Core.withContextBackend

    -- * Supporting types
  , Core.ContextKey
  , Core.ContextSnapshot(..)
  , Core.ContextStatus(..)
  ) where

import Control.Exception.Safe (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.State (StateT)
import Prelude
import qualified Control.Monad.Trans.Control as Trans.Control
import qualified Control.Monad.Trans.RWS.Lazy as Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as Trans.RWS.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.Writer.Strict
import qualified OTel.API.Context.Core as Core

class (Monad m) => MonadKeyedContext ctx m | m -> ctx where
  getContext :: Core.ContextKey ctx -> m (Core.ContextSnapshot ctx)
  updateContext :: Core.ContextKey ctx -> (ctx -> ctx) -> m (Core.ContextSnapshot ctx)

  default getContext
    :: (MonadTrans t, MonadKeyedContext ctx n, m ~ t n)
    => Core.ContextKey ctx
    -> m (Core.ContextSnapshot ctx)
  getContext =
    lift . getContext

  default updateContext
    :: (MonadTrans t, MonadKeyedContext ctx n, m ~ t n)
    => Core.ContextKey ctx
    -> (ctx -> ctx)
    -> m (Core.ContextSnapshot ctx)
  updateContext ctxKey =
    lift . updateContext ctxKey

instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (ExceptT e m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (IdentityT m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (MaybeT m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (ReaderT r m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (StateT r m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.RWS.Lazy.RWST r w s m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.RWS.Strict.RWST r w s m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.Writer.Lazy.WriterT w m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.Writer.Strict.WriterT w m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (LoggingT m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (ResourceT m)
instance (MonadIO m) => MonadKeyedContext ctx (Core.ContextT ctx m) where
  updateContext = Core.updateContext
  getContext = Core.getContext

class (MonadKeyedContext ctx m) => MonadContext ctx m | m -> ctx where
  attachContext :: ctx -> (Core.ContextKey ctx -> m a) -> m a
  getAttachedContextKey :: m (Maybe (Core.ContextKey ctx))

  default attachContext
    :: (MonadTransControl t, MonadContext ctx n, m ~ t n)
    => ctx
    -> (Core.ContextKey ctx -> m a)
    -> m a
  attachContext ctx action = do
    Trans.Control.restoreT . pure
      =<< Trans.Control.liftWith \run -> attachContext ctx (run . action)

  default getAttachedContextKey
    :: (MonadTrans t, MonadContext ctx n, m ~ t n)
    => m (Maybe (Core.ContextKey ctx))
  getAttachedContextKey =
    lift getAttachedContextKey

instance (MonadContext ctx m) => MonadContext ctx (ExceptT e m)
instance (MonadContext ctx m) => MonadContext ctx (IdentityT m)
instance (MonadContext ctx m) => MonadContext ctx (MaybeT m)
instance (MonadContext ctx m) => MonadContext ctx (ReaderT r m)
instance (MonadContext ctx m) => MonadContext ctx (StateT r m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.Lazy.RWST r w s m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.Strict.RWST r w s m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.Lazy.WriterT w m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.Strict.WriterT w m)
instance (MonadContext ctx m) => MonadContext ctx (LoggingT m)
instance (MonadContext ctx m, MonadUnliftIO m) => MonadContext ctx (ResourceT m) where
  attachContext ctx action = do
    withRunInIO \runInIO -> do
      runInIO $ attachContext ctx action
  getAttachedContextKey = lift getAttachedContextKey
instance (MonadIO m, MonadMask m) => MonadContext ctx (Core.ContextT ctx m) where
  attachContext = Core.attachContext
  getAttachedContextKey = Core.getAttachedContextKey

-- $intro
--
-- @otel-api-context@ STUB
--
