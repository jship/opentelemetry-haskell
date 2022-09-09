module OTel.API.Baggage.Core
  ( -- * Synopsis
    -- $synopsis
    Internal.MonadBaggage(..)

  , Internal.Baggage
  , Internal.nullBaggage
  , Internal.sizeBaggage
  , Internal.memberBaggage
  , Internal.lookupBaggage
  , Internal.findWithDefaultBaggage
  , Internal.deleteBaggage
  , Internal.filterBaggage
  , Internal.filterWithKeyBaggage
  , Internal.foldMapWithKeyBaggage

  , Internal.BaggageBuilder
  , Internal.buildBaggage
  , Internal.buildBaggagePure
  , Internal.baggageKeyFromText
  , Internal.baggageValueFromText
  , Internal.BaggageBuildError(..)
  ) where

import qualified OTel.API.Baggage.Core.Internal as Internal

-- $synopsis
--
-- @otel-api-baggage-core@ STUB
--