module OTel.SDK.Resource.Core.Builder
  ( Internal.ResourceBuilder
  , Internal.buildResource
  , Internal.buildResourcePure

  , Internal.ResourceMergeError
  , Internal.resourceMergeErrorSchemas

  , (.@)
  ) where

import OTel.API.Common (KV((.@)))
import qualified OTel.SDK.Resource.Core.Internal as Internal
