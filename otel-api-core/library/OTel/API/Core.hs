module OTel.API.Core
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Core.Attributes

  , KV(..)
  , IsTextKV
  , Key(..)

  , Timestamp
  , timestampFromNanoseconds
  , timestampToNanoseconds
  , TimestampSource(..)

  , InstrumentationScope
  , defaultInstrumentationScope
  , instrumentationScopeName
  , instrumentationScopeVersion
  , instrumentationScopeSchemaURL
  , InstrumentationScopeName(..)

  , Version(..)

  , SchemaURL
  , schemaURLFromText
  , schemaURLToText
  ) where

import OTel.API.Core.Attributes
import OTel.API.Core.Internal

-- $synopsis
--
-- @otel-api-common@ STUB
--
