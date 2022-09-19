{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module OTel.API.Baggage.Core.Builder.Errors
  ( BaggageErrors(..)
  , BaggageError(..)
  , BaggageKeyIsEmptyError(..)
  , BaggageKeyContainsInvalidCharsError(..)
  , BaggageValueIsEmptyError(..)
  , BaggageValueContainsInvalidCharsError(..)
  ) where

import Control.Exception.Safe (Exception)
import Data.Text (Text)
import OTel.API.Core (Key)
import Prelude

newtype BaggageErrors = BaggageErrors
  { unBaggageErrors :: [BaggageError]
  } deriving stock (Eq, Show)
    deriving anyclass (Exception)

data BaggageError
  = BaggageKeyIsEmpty BaggageKeyIsEmptyError
  | BaggageKeyContainsInvalidChars BaggageKeyContainsInvalidCharsError
  | BaggageValueIsEmpty BaggageValueIsEmptyError
  | BaggageValueContainsInvalidChars BaggageValueContainsInvalidCharsError
  deriving stock (Eq, Show)

newtype BaggageKeyIsEmptyError = BaggageKeyIsEmptyError
  { rawValue :: Text
  } deriving stock (Eq, Show)

data BaggageKeyContainsInvalidCharsError = BaggageKeyContainsInvalidCharsError
  { rawKey :: Key Text
  , rawValue :: Text
  , invalidChars :: Text
  } deriving stock (Eq, Show)

newtype BaggageValueIsEmptyError = BaggageValueIsEmptyError
  { rawKey :: Key Text
  } deriving stock (Eq, Show)

data BaggageValueContainsInvalidCharsError = BaggageValueContainsInvalidCharsError
  { rawKey :: Key Text
  , rawValue :: Text
  , invalidChars :: Text
  } deriving stock (Eq, Show)
