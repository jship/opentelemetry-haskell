module OTel.API.Context.Core
  ( -- * Introduction
    -- $intro
    Context
  , emptyContext
  , lookupContext
  , insertContext

  , ContextKey
  , contextKeyName
  , newContextKey

  , attachContextValueUsing
  , getAttachedContextValueUsing
  , getAttachedContextUsing

  , ContextBackend
  ) where

import OTel.API.Context.Core.Internal

-- $intro
--
-- @otel-api-context-core@ STUB
--
