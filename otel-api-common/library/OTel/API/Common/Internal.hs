{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , Version(..)
  , SchemaURL(..)
  , schemaURLFromText
  , schemaURLToText

    -- * Tracing
  , SpanContext(..)
  , defaultSpanContext
  , buildSpanUpdater
  , recordException
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
  , SpanEventSpecs(..)
  , SpanEventSpec(..)
  , defaultSpanEventSpec
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

import Control.Exception (SomeException(..))
import Data.Int (Int64)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack, SrcLoc)
import Prelude hiding (span)
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable

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
  , instrumentationScopeVersion :: Maybe Version
  , instrumentationScopeSchemaURL :: Maybe SchemaURL
  }

instance IsString InstrumentationScope where
  fromString s =
    defaultInstrumentationScope
      { instrumentationScopeName = fromString s
      }

defaultInstrumentationScope :: InstrumentationScope
defaultInstrumentationScope =
  InstrumentationScope
    { instrumentationScopeName = ""
    , instrumentationScopeVersion = Nothing
    , instrumentationScopeSchemaURL = Nothing
    }

newtype InstrumentationScopeName = InstrumentationScopeName
  { unInstrumentationScopeName :: Text
  } deriving stock (Eq, Show)

instance IsString InstrumentationScopeName where
  fromString = InstrumentationScopeName . Text.pack

newtype Version = Version
  { unVersion :: Text
  } deriving stock (Eq, Show)

instance IsString Version where
  fromString = Version . Text.pack

newtype SchemaURL = SchemaURL
  { unSchemaURL :: Text
  } deriving stock (Eq, Show)

schemaURLFromText :: Text -> Either Text SchemaURL
schemaURLFromText = Right . SchemaURL

schemaURLToText :: SchemaURL -> Text
schemaURLToText = unSchemaURL

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
  { unSpanEvents :: [SpanEvent] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [SpanEvent]

data SpanEvent = SpanEvent
  { spanEventName :: SpanEventName
  , spanEventTimestamp :: Timestamp
  , spanEventAttributes :: Attributes
  } deriving stock (Eq, Show)

newtype SpanEventSpecs = SpanEventSpecs
  { unSpanEventSpecs :: [SpanEventSpec] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [SpanEventSpec]

data SpanEventSpec = SpanEventSpec
  { spanEventSpecName :: SpanEventName
  , spanEventSpecTimestamp :: TimestampSource
  , spanEventSpecAttributes :: Attributes
  } deriving stock (Eq, Show)

defaultSpanEventSpec :: SpanEventSpec
defaultSpanEventSpec =
  SpanEventSpec
    { spanEventSpecName = ""
    , spanEventSpecTimestamp = TimestampSourceNow
    , spanEventSpecAttributes = mempty
    }

instance IsString SpanEventSpec where
  fromString s =
    defaultSpanEventSpec
      { spanEventSpecName = fromString s
      }

newtype SpanEventName = SpanEventName
  { unSpanEventName :: Text
  } deriving stock (Eq, Show)

instance IsString SpanEventName where
  fromString = SpanEventName . Text.pack

--exceptionToSpanEvent

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
  { spanUpdateSpecName :: Maybe SpanName
  , spanUpdateSpecStatus :: Maybe SpanStatus
  , spanUpdateSpecAttributes :: Maybe Attributes
  , spanUpdateSpecEvents :: Maybe SpanEventSpecs
  }

defaultSpanUpdateSpec :: SpanUpdateSpec
defaultSpanUpdateSpec =
  SpanUpdateSpec
    { spanUpdateSpecName = Nothing
    , spanUpdateSpecStatus = Nothing
    , spanUpdateSpecAttributes = Nothing
    , spanUpdateSpecEvents = Nothing
    }

buildSpanUpdater
  :: (Monad m)
  => m Timestamp
  -> SpanUpdateSpec
  -> m (Span -> Span)
buildSpanUpdater getTimestamp spanUpdateSpec = do
  newSpanEvents <- do
    case spanUpdateSpecEvents of
      Nothing -> pure mempty
      Just spanEventSpecs -> do
        Traversable.for (unSpanEventSpecs spanEventSpecs) \spanEventSpec -> do
          spanEventTimestamp <- do
            case spanEventSpecTimestamp spanEventSpec of
              TimestampSourceAt timestamp -> pure timestamp
              TimestampSourceNow -> getTimestamp
          pure SpanEvent
            { spanEventName = spanEventSpecName spanEventSpec
            , spanEventTimestamp
            , spanEventAttributes = spanEventSpecAttributes spanEventSpec
            }
  pure \span -> span
    { spanName =
        Maybe.fromMaybe (spanName span) spanUpdateSpecName
    , spanStatus =
        Maybe.fromMaybe (spanStatus span) spanUpdateSpecStatus
    , spanAttributes =
        maybe id (\as -> (<> as)) spanUpdateSpecAttributes $ spanAttributes span
    , spanEvents =
        spanEvents span <> SpanEvents newSpanEvents
    }
  where
  SpanUpdateSpec
    { spanUpdateSpecName
    , spanUpdateSpecStatus
    , spanUpdateSpecAttributes
    , spanUpdateSpecEvents
    } = spanUpdateSpec

-- TODO: See https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/exceptions/
-- TODO: Should there be a convenience wrapper that produces a SpanUpdateSpec?
recordException
  :: SomeException
  -> TimestampSource
  -> Attributes
  -> SpanEventSpec
recordException (SomeException e) timestamp attributes =
  SpanEventSpec
    { spanEventSpecName = "exception"
    , spanEventSpecTimestamp = timestamp
    , spanEventSpecAttributes =
        Attributes [("exception.message", Text.pack $ Exception.displayException e)]
          <> attributes
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
