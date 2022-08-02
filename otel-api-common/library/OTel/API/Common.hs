module OTel.API.Common
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Common.Attributes
  , module OTel.API.Common.Trace

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

import OTel.API.Common.Attributes
import OTel.API.Common.Internal
import OTel.API.Common.Trace

-- $synopsis
--
-- @hotel-api-common@ STUB
--
