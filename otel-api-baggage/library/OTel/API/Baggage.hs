module OTel.API.Baggage
  ( -- * Synopsis
    -- $synopsis
    Internal.BaggageT
  , Internal.runBaggageT
  , Internal.mapBaggageT

  , Internal.BaggageBackend
  , Internal.defaultBaggageBackend

  , module OTel.API.Baggage.Core
  ) where

import OTel.API.Baggage.Core
import qualified OTel.API.Baggage.Internal as Internal

-- $synopsis
--
-- @otel-api-trace@ STUB
--
