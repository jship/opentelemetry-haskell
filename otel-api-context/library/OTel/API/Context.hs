module OTel.API.Context
  ( -- * Introduction
    -- $intro
    -- * @ContextT@ monad transformer
    ContextT(..)
  , mapContextT

    -- * Context operations
  , updateContext
  , getContext
  , attachContext
  , getAttachedContextKey

    -- * Context backend
  , ContextBackend

    -- * Supporting types
  , ContextKey
  , contextKeyName
  ) where

import OTel.API.Context.Internal

-- $intro
--
-- @otel-api-context@ STUB
--
