module OTel.API.Common.InstrumentationScope
  ( Internal.InstrumentationScope
  , Internal.defaultInstrumentationScope
  , Internal.instrumentationScopeName
  , Internal.instrumentationScopeVersion
  , Internal.instrumentationScopeSchemaURL

  , Internal.InstrumentationScopeName(..)

  , Internal.Version(..)

  , Internal.SchemaURL
  , Internal.schemaURLFromText
  , Internal.schemaURLToText
  ) where

import qualified OTel.API.Common.Internal as Internal
