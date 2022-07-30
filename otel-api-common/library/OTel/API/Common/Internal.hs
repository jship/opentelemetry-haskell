{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
module OTel.API.Common.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- * General
    Attributes(..)
  , Timestamp(..)
  , timestampFromNanoseconds
  , TimestampSource(..)
  , InstrumentationScope(..)
  , InstrumentationScopeName(..)

    -- * Tracing
  , SpanContext(..)
  , defaultSpanContext
  , spanContextIsValid
  , TraceId(..)
  , nullTraceId
  , SpanId(..)
  , nullSpanId
  , TraceFlags(..)
  , TraceState(..)
  , SpanEvents(..)
  , SpanLinks(..)
  , SpanSpec(..)
  , defaultSpanSpec
  , SpanUpdateSpec(..)
  , defaultSpanUpdateSpec
  , SpanName(..)
  , Span(..)
  , EndedSpan(..)
  , toEndedSpan
  , SpanLineageSource(..)
  , SpanLineage(..)
  , SpanKind(..)
  , SpanStatus(..)

    -- * Miscellaneous
  , bug
  ) where

import Data.Int (Int64)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack, SrcLoc)
import Network.URI (URI)
import Prelude hiding (span)
import qualified Data.Text as Text

newtype Attributes = Attributes
  { unAttributes :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [(Text, Text)]

newtype Timestamp = Timestamp
  { unTimestamp :: Int64 -- ^ nanoseconds
  } deriving stock (Eq, Show)

timestampFromNanoseconds :: Int64 -> Timestamp
timestampFromNanoseconds = Timestamp

data TimestampSource
  = TimestampSourceNow
  | TimestampSourceAt Timestamp
  deriving stock (Eq, Show)

data InstrumentationScope = InstrumentationScope
  { instrumentationScopeName :: InstrumentationScopeName
  , instrumentationScopeVersion :: Maybe InstrumentationScopeVersion
  , instrumentationScopeSchemaURL :: Maybe URI
  }

buildInstrumentationScope :: InstrumentationScopeName -> InstrumentationScope
buildInstrumentationScope instrumentationScopeName =
  InstrumentationScope
    { instrumentationScopeName
    , instrumentationScopeVersion = Nothing
    , instrumentationScopeSchemaURL = Nothing
    }

newtype InstrumentationScopeName = InstrumentationScopeName
  { unInstrumentationScopeName :: Text
  } deriving stock (Eq, Show)

instance IsString InstrumentationScopeName where
  fromString = InstrumentationScopeName . Text.pack

newtype InstrumentationScopeVersion = InstrumentationScopeVersion
  { unInstrumentationScopeVersion :: Text
  } deriving stock (Eq, Show)

instance IsString InstrumentationScopeVersion where
  fromString = InstrumentationScopeVersion . Text.pack

data SpanContext = SpanContext
  { spanContextTraceId :: TraceId
  , spanContextSpanId :: SpanId
  , spanContextTraceFlags :: TraceFlags
  , spanContextTraceState :: TraceState
  , spanContextIsRemote :: Bool
  } deriving stock (Eq, Show)

defaultSpanContext :: SpanContext
defaultSpanContext =
  SpanContext
    { spanContextTraceId = nullTraceId
    , spanContextSpanId = nullSpanId
    , spanContextTraceFlags = TraceFlags { unTraceFlags = 0 }
    , spanContextTraceState = TraceState  { unTraceState = [] }
    , spanContextIsRemote = False
    }

spanContextIsValid :: SpanContext -> Bool
spanContextIsValid spanContext =
  spanContextTraceId /= nullTraceId && spanContextSpanId /= nullSpanId
  where
  SpanContext { spanContextTraceId, spanContextSpanId } = spanContext

-- TODO: Get hex string
-- TODO: Get byte array
data TraceId = TraceId
  { traceIdHi :: Word64
  , traceIdLo :: Word64
  } deriving stock (Eq, Show)

nullTraceId :: TraceId
nullTraceId = TraceId { traceIdHi = 0, traceIdLo = 0 }

-- TODO: Get hex string
-- TODO: Get byte array
data SpanId = SpanId
  { spanIdLo :: Word64
  } deriving stock (Eq, Show)

nullSpanId :: SpanId
nullSpanId = SpanId { spanIdLo = 0 }

newtype TraceFlags = TraceFlags
  { unTraceFlags :: Word8
  } deriving stock (Eq, Show)

newtype TraceState = TraceState
  { unTraceState :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)

newtype SpanEvents = SpanEvents
  { unSpanEvents :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [(Text, Text)]

newtype SpanLinks = SpanLinks
  { unSpanLinks :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [(Text, Text)]

data SpanSpec = SpanSpec
  { spanSpecLineageSource :: SpanLineageSource
  , spanSpecStart :: TimestampSource
  , spanSpecKind :: SpanKind
  , spanSpecAttributes :: Attributes
  , spanSpecLinks :: SpanLinks
  } deriving stock (Eq, Show)

defaultSpanSpec :: SpanSpec
defaultSpanSpec =
  SpanSpec
    { spanSpecLineageSource = SpanLineageSourceImplicit
    , spanSpecStart = TimestampSourceNow
    , spanSpecKind = SpanKindInternal
    , spanSpecAttributes = mempty
    , spanSpecLinks = mempty
    }

data SpanUpdateSpec = SpanUpdateSpec
  { spanUpdateSpecName :: Maybe Text
  , spanUpdateSpecStatus :: Maybe SpanStatus
  , spanUpdateSpecAttributes :: Maybe Attributes
  , spanUpdateSpecEvents :: Maybe SpanEvents
  }

defaultSpanUpdateSpec :: SpanUpdateSpec
defaultSpanUpdateSpec =
  SpanUpdateSpec
    { spanUpdateSpecName = Nothing
    , spanUpdateSpecStatus = Nothing
    , spanUpdateSpecAttributes = Nothing
    , spanUpdateSpecEvents = Nothing
    }

newtype SpanName = SpanName
  { unSpanName :: Text
  } deriving stock (Eq, Show)

instance IsString SpanName where
  fromString = SpanName . Text.pack

data Span = Span
  { spanLineage :: SpanLineage
  , spanContext :: SpanContext
  , spanName :: SpanName
  , spanStatus :: SpanStatus
  , spanStart :: Timestamp
  , spanKind :: SpanKind
  , spanAttributes :: Attributes
  , spanLinks :: SpanLinks
  , spanEvents :: SpanEvents
  , spanIsRecording :: Bool
  , spanSrcLoc :: SrcLoc
  } deriving stock (Eq, Show)

data EndedSpan = EndedSpan
  { endedSpanLineage :: SpanLineage
  , endedSpanContext :: SpanContext
  , endedSpanName :: SpanName
  , endedSpanStatus :: SpanStatus
  , endedSpanStart :: Timestamp
  , endedSpanEnd :: Timestamp
  , endedSpanKind :: SpanKind
  , endedSpanAttributes :: Attributes
  , endedSpanLinks :: SpanLinks
  , endedSpanEvents :: SpanEvents
  , endedSpanSrcLoc :: SrcLoc
  } deriving stock (Eq, Show)

toEndedSpan :: Timestamp -> Span -> EndedSpan
toEndedSpan endedSpanEnd span =
  EndedSpan
    { endedSpanLineage = spanLineage span
    , endedSpanContext = spanContext span
    , endedSpanName = spanName span
    , endedSpanStatus = spanStatus span
    , endedSpanStart = spanStart span
    , endedSpanEnd
    , endedSpanKind = spanKind span
    , endedSpanAttributes = spanAttributes span
    , endedSpanLinks = spanLinks span
    , endedSpanEvents = spanEvents span
    , endedSpanSrcLoc = spanSrcLoc span
    }

data SpanLineageSource
  = SpanLineageSourceImplicit
  | SpanLineageSourceExplicit SpanLineage
  deriving stock (Eq, Show)

data SpanLineage
  = SpanLineageRoot
  | SpanLineageChildOf SpanContext
  deriving stock (Eq, Show)

data SpanKind
  = SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  | SpanKindInternal
  deriving stock (Eq, Show)

data SpanStatus
  = SpanStatusUnset
  | SpanStatusOk
  | SpanStatusError Text
  deriving stock (Eq, Show)

bug :: HasCallStack => String -> a
bug prefix =
  error $
    "OTel.API.Trace.Internal." <> prefix <> ": Impossible! (if you see this "
      <> "message, please report it as a bug at "
      <> "https://github.com/jship/opentelemetry-haskell)"

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
