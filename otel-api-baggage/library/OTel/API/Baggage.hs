module OTel.API.Baggage
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Baggage.Core

  , Internal.BaggageT
  , Internal.runBaggageT
  , Internal.mapBaggageT

  , Internal.BaggageBackend
  , Internal.defaultBaggageBackend
  ) where

import OTel.API.Baggage.Core
import qualified OTel.API.Baggage.Internal as Internal

-- $synopsis
--
-- @otel-api-trace@ STUB
--
