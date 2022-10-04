module OTel.API.Context
  ( -- * Introduction
    -- $intro
    -- * @ContextT@ monad transformer
    Internal.ContextT(..)
  , Internal.mapContextT

    -- * Context operations
  , Internal.attachContextValue
  , Internal.getAttachedContextValue
  , Internal.getAttachedContext

  , module OTel.API.Context.Core
  ) where

import OTel.API.Context.Core
import qualified OTel.API.Context.Internal as Internal

-- $intro
--
-- @otel-api-context@ STUB
--
