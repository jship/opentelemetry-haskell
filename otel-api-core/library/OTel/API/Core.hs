module OTel.API.Core
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Core.Attributes
  , module OTel.API.Core.Trace

  , KV(..)
  , Key(..)
  , InstrumentationScope(..)
  , InstrumentationScopeName(..)
  , Version(..)
  , SchemaURL
  , schemaURLFromText
  , schemaURLToText

  , Timestamp
  , timestampFromNanoseconds
  , timestampToNanoseconds
  , TimestampSource(..)
  ) where

import OTel.API.Core.Attributes
import OTel.API.Core.Internal
import OTel.API.Core.Trace

-- $synopsis
--
-- @hotel-api-common@ STUB
--
