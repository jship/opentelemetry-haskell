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

import OTel.API.Context.Core.Internal

-- $intro
--
-- @otel-api-context-core@ STUB
--
