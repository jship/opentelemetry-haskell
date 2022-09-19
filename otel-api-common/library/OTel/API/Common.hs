module OTel.API.Common
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Common.Attributes

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

import OTel.API.Common.Attributes
import OTel.API.Common.Internal

-- $synopsis
--
-- @otel-api-common@ STUB
--
