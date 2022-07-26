module OTel.API.Context
  ( -- * Synopsis
    -- $synopsis
    MonadKeyedContext(..)
  , MonadContext(..)
  , ContextT
  , runContextT
  , mapContextT

  , ContextBackend
  , withContextBackend

  , ContextKey
  , ContextSnapshot(..)
  , ContextStatus(..)
  ) where

import OTel.API.Context.Internal

-- $synopsis
--
-- @otel-api-context@ STUB
--
