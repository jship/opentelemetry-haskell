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
  , withContextBackend

    -- * Supporting types
  , ContextKey
  ) where

import OTel.API.Context.Internal

-- $intro
--
-- @otel-api-context@ STUB
--
