{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OTel.API.Context.Core
  ( -- * Introduction
    -- $intro
    -- * @ContextT@ monad transformer
    ContextT
  , runContextT
  , mapContextT

    -- * Context operations
  , updateContext
  , getContext
  , attachContext
  , getAttachedContextKey

    -- * Context backend
  , ContextBackend
  , withContextBackend

    -- * Supporting types
  , ContextKey
  , ContextSnapshot(..)
  , ContextStatus(..)
  ) where

import Control.Exception.Safe (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import OTel.API.Context.Core.Internal
import Prelude
import qualified Context
import qualified Control.Exception.Safe as Exception

updateContext
  :: forall m ctx
   . (MonadIO m)
  => ContextKey ctx
  -> (ctx -> ctx)
  -> ContextT ctx m (ContextSnapshot ctx)
updateContext ctxKey updater =
  ContextT \_contextBackend ->
    liftIO $ updateContextRefValue (contextKeyRef ctxKey) updater

getContext
  :: forall m ctx
   . (MonadIO m)
  => ContextKey ctx
  -> ContextT ctx m (ContextSnapshot ctx)
getContext ctxKey = updateContext ctxKey id

attachContext
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => ctx
  -> (ContextKey ctx -> ContextT ctx m a)
  -> ContextT ctx m a
attachContext ctx f =
  ContextT \contextBackend -> do
    ctxRef <- liftIO $ newContextRef ContextStatusAttached ctx
    Context.use (contextBackendStore contextBackend) ctxRef do
      runContextT (f ContextKey { contextKeyRef = ctxRef }) contextBackend
      `Exception.finally` liftIO (markAsDetached ctxRef)

getAttachedContextKey
  :: forall m ctx
   . (MonadIO m)
  => ContextT ctx m (Maybe (ContextKey ctx))
getAttachedContextKey =
  ContextT \contextBackend -> do
    Context.minesMay (contextBackendStore contextBackend) ContextKey

-- $intro
--
-- @otel-api-context-core@ STUB
--
