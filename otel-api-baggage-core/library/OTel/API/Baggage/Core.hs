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
  , Internal.toListBaggage

  , Internal.BaggageBuilder
  , Internal.buildBaggage
  , Internal.buildBaggagePure
  , Internal.BaggageErrors(..)
  , Internal.BaggageError(..)
  , Internal.BaggageKeyIsEmptyError(..)
  , Internal.BaggageKeyContainsInvalidCharsError(..)
  , Internal.BaggageValueIsEmptyError(..)
  , Internal.BaggageValueContainsInvalidCharsError(..)

  , (.@)
  ) where

import OTel.API.Core (KV((.@)))
import qualified OTel.API.Baggage.Core.Internal as Internal

-- $synopsis
--
-- @otel-api-baggage-core@ STUB
--
