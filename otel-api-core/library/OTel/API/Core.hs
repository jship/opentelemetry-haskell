module OTel.API.Core
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Core.Attributes
  , module OTel.API.Core.Trace

  , KV(..)
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
import OTel.API.Core.Trace

-- $synopsis
--
-- @otel-api-common@ STUB
--
