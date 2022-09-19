module OTel.API.Context.Core
  ( -- * Introduction
    -- $intro
    Internal.Context
  , Internal.emptyContext
  , Internal.lookupContext
  , Internal.insertContext

  , Internal.ContextKey
  , Internal.contextKeyName

  , Internal.attachContextValueUsing
  , Internal.getAttachedContextValueUsing
  , Internal.getAttachedContextUsing

  , Internal.ContextBackend
  ) where

import qualified OTel.API.Context.Core.Internal as Internal

-- $intro
--
-- @otel-api-context-core@ STUB
--
