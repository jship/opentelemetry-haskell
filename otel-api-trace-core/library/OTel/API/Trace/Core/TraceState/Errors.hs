{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
module OTel.API.Trace.Core.TraceState.Errors
  ( TraceStateErrors(..)
  , TraceStateError(..)
  , TraceStateSimpleKeyIsEmptyError(..)
  , TraceStateSimpleKeyContainsInvalidCharsError(..)
  , TraceStateTenantIdIsEmptyError(..)
  , TraceStateTenantIdContainsInvalidCharsError(..)
  , TraceStateSystemIdIsEmptyError(..)
  , TraceStateSystemIdContainsInvalidCharsError(..)
  , TraceStateSimpleKeyTooLongError(..)
  , TraceStateTenantIdTooLongError(..)
  , TraceStateSystemIdTooLongError(..)
  , TraceStateKeyTypeUnknownError(..)
  , TraceStateValueIsEmptyError(..)
  , TraceStateValueContainsInvalidCharsError(..)
  , TraceStateValueTooLongError(..)
  ) where

import Control.Monad.Catch (Exception)
import Data.Text (Text)
import OTel.API.Common (Key)
import Prelude

newtype TraceStateErrors = TraceStateErrors
  { unTraceStateErrors :: [TraceStateError]
  } deriving stock (Eq, Show)
    deriving anyclass (Exception)

data TraceStateError
  = TraceStateSimpleKeyIsEmpty TraceStateSimpleKeyIsEmptyError
  | TraceStateSimpleKeyContainsInvalidChars TraceStateSimpleKeyContainsInvalidCharsError
  | TraceStateTenantIdIsEmpty TraceStateTenantIdIsEmptyError
  | TraceStateTenantIdContainsInvalidChars TraceStateTenantIdContainsInvalidCharsError
  | TraceStateSystemIdIsEmpty TraceStateSystemIdIsEmptyError
  | TraceStateSystemIdContainsInvalidChars TraceStateSystemIdContainsInvalidCharsError
  | TraceStateSimpleKeyTooLong TraceStateSimpleKeyTooLongError
  | TraceStateTenantIdTooLong TraceStateTenantIdTooLongError
  | TraceStateSystemIdTooLong TraceStateSystemIdTooLongError
  | TraceStateKeyTypeUnknown TraceStateKeyTypeUnknownError
  | TraceStateValueIsEmpty TraceStateValueIsEmptyError
  | TraceStateValueContainsInvalidChars TraceStateValueContainsInvalidCharsError
  | TraceStateValueTooLong TraceStateValueTooLongError
  deriving stock (Eq, Show)

newtype TraceStateSimpleKeyIsEmptyError = TraceStateSimpleKeyIsEmptyError
  { rawValue :: Text
  } deriving stock (Eq, Show)

data TraceStateSimpleKeyContainsInvalidCharsError = TraceStateSimpleKeyContainsInvalidCharsError
  { rawKey :: Key Text
  , rawValue :: Text
  , invalidChars :: Text
  } deriving stock (Eq, Show)

data TraceStateTenantIdIsEmptyError = TraceStateTenantIdIsEmptyError
  { rawSystemId :: Text
  , rawValue :: Text
  } deriving stock (Eq, Show)

data TraceStateTenantIdContainsInvalidCharsError = TraceStateTenantIdContainsInvalidCharsError
  { rawTenantId :: Text
  , rawSystemId :: Text
  , rawValue :: Text
  , invalidChars :: Text
  } deriving stock (Eq, Show)

data TraceStateSystemIdIsEmptyError = TraceStateSystemIdIsEmptyError
  { rawSystemId :: Text
  , rawValue :: Text
  } deriving stock (Eq, Show)

data TraceStateSystemIdContainsInvalidCharsError = TraceStateSystemIdContainsInvalidCharsError
  { rawTenantId :: Text
  , rawSystemId :: Text
  , rawValue :: Text
  , invalidChars :: Text
  } deriving stock (Eq, Show)

data TraceStateSimpleKeyTooLongError = TraceStateSimpleKeyTooLongError
  { rawKey :: Key Text
  , rawValue :: Text
  } deriving stock (Eq, Show)

data TraceStateTenantIdTooLongError = TraceStateTenantIdTooLongError
  { rawTenantId :: Text
  , rawSystemId :: Text
  , rawValue :: Text
  } deriving stock (Eq, Show)

data TraceStateSystemIdTooLongError = TraceStateSystemIdTooLongError
  { rawTenantId :: Text
  , rawSystemId :: Text
  , rawValue :: Text
  } deriving stock (Eq, Show)

data TraceStateKeyTypeUnknownError = TraceStateKeyTypeUnknownError
  { rawKey :: Key Text
  , rawValue :: Text
  } deriving stock (Eq, Show)

newtype TraceStateValueIsEmptyError = TraceStateValueIsEmptyError
  { rawKey :: Key Text
  } deriving stock (Eq, Show)

data TraceStateValueContainsInvalidCharsError = TraceStateValueContainsInvalidCharsError
  { rawKey :: Key Text
  , rawValue :: Text
  , invalidChars :: Text
  } deriving stock (Eq, Show)

data TraceStateValueTooLongError = TraceStateValueTooLongError
  { rawKey :: Key Text
  , rawValue :: Text
  } deriving stock (Eq, Show)
