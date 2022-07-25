module OTel.API.Context
  ( -- * Synopsis
    -- $synopsis
    attachContext
  , updateContext
  , getAttachedContext
  , getAttachedContextKey
  , getContextStatus

  , MonadContext(..)

  , ContextT
  , runContextT
  , mapContextT

  , ContextKey
  , ContextStatus
  , contextStatusAttached
  , contextStatusDetached

  , ContextBackend
  , withContextBackend
  ) where

import OTel.API.Context.Internal

-- $synopsis
--
-- @otel-api-context@ STUB
--
