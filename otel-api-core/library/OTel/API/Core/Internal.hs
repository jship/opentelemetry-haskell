{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module OTel.API.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- * General
    KV(..)
  , Key(..)
  , Timestamp(..)
  , timestampFromNanoseconds
  , timestampToNanoseconds
  , TimestampSource(.., Now, At)
  , InstrumentationScope(..)
  , InstrumentationScopeName(..)
  , Version(..)
  , SchemaURL(..)
  , schemaURLFromText
  , schemaURLToText

    -- * Attributes
  , Attrs(..)
  , attrsToList
  , SomeAttr(..)
  , Attr(..)
  , AttrVals(..)
  , AttrType(..)
  , KnownAttrType(..)

    -- * Tracing
  , Tracer(..)
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
  , NewSpanSpec(..)
  , defaultNewSpanSpec
  , UpdateSpanSpec(..)
  , defaultUpdateSpanSpec
  , SpanEventSpecs(..)
  , SpanEventSpec(..)
  , defaultSpanEventSpec
  , SpanName(..)
  , Span(..)
  , EndedSpan(..)
  , toEndedSpan
  , SpanParentSource(.., Implicit, Explicit)
  , SpanParent(.., Root, ChildOf)
  , SpanKind(.., Server, Client, Producer, Consumer, Internal)
  , SpanStatus(.., Unset, OK, Error)
  ) where

import Control.Exception (SomeException(..))
import Data.DList (DList)
import Data.Int (Int64)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word64, Word8)
import OTel.API.Context (ContextBackend)
import Prelude hiding (span)
import qualified Control.Exception as Exception
import qualified Data.DList as DList
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable

class KV (kv :: Type) where
  type ValueConstraints kv :: Type -> Constraint
  (.@) :: ValueConstraints kv v => Key v -> v -> kv

instance KV Attrs where
  type ValueConstraints Attrs = KnownAttrType
  (.@) = go
    where
    go :: forall a. (KnownAttrType a) => Key a -> a -> Attrs
    go k v =
      Attrs $ DList.singleton $ SomeAttr Attr
        { attrType = attrTypeVal $ Proxy @a
        , attrKey = k
        , attrVal = v
        }

newtype Key a = Key
  { unKey :: Text
  } deriving stock (Eq, Show)

instance IsString (Key a) where
  fromString = Key . Text.pack

newtype Timestamp = Timestamp
  { unTimestamp :: Integer -- ^ nanoseconds
  } deriving stock (Eq, Show)

timestampFromNanoseconds :: Integer -> Timestamp
timestampFromNanoseconds = Timestamp

timestampToNanoseconds :: Timestamp -> Integer
timestampToNanoseconds = unTimestamp

data TimestampSource
  = TimestampSourceNow
  | TimestampSourceAt Timestamp
  deriving stock (Eq, Show)

pattern Now :: TimestampSource
pattern Now <- TimestampSourceNow where
  Now = TimestampSourceNow

pattern At :: Timestamp -> TimestampSource
pattern At timestamp <- TimestampSourceAt timestamp where
  At timestamp = TimestampSourceAt timestamp

{-# COMPLETE Now, At :: TimestampSource #-}

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

newtype Attrs = Attrs
  { unAttrs :: DList SomeAttr
  } deriving (Eq, Monoid, Semigroup, Show) via (DList SomeAttr)

attrsToList :: Attrs -> [SomeAttr]
attrsToList = DList.toList . unAttrs

data SomeAttr where
  SomeAttr :: Attr a -> SomeAttr

instance Eq SomeAttr where
  sa1 == sa2 =
    case (sa1, sa2) of
      (SomeAttr a1, SomeAttr a2) ->
        case (attrType a1, attrType a2) of
          (AttrTypeText, AttrTypeText) -> a1 == a2
          (AttrTypeBool, AttrTypeBool) -> a1 == a2
          (AttrTypeDouble, AttrTypeDouble) -> a1 == a2
          (AttrTypeInt, AttrTypeInt) -> a1 == a2
          (AttrTypeTextArray, AttrTypeTextArray) -> a1 == a2
          (AttrTypeBoolArray, AttrTypeBoolArray) -> a1 == a2
          (AttrTypeDoubleArray, AttrTypeDoubleArray) -> a1 == a2
          (AttrTypeIntArray, AttrTypeIntArray) -> a1 == a2
          (_, _) -> False

instance Show SomeAttr where
  show (SomeAttr attr) =
    case attrType attr of
      AttrTypeText -> show attr
      AttrTypeBool -> show attr
      AttrTypeDouble -> show attr
      AttrTypeInt -> show attr
      AttrTypeTextArray -> show attr
      AttrTypeBoolArray -> show attr
      AttrTypeDoubleArray -> show attr
      AttrTypeIntArray -> show attr

data Attr a = Attr
  { attrType :: AttrType a
  , attrKey :: Key a
  , attrVal :: a
  } deriving stock (Eq, Show)

newtype AttrVals a = AttrVals
  { unAttrVals :: DList a
  } deriving (Eq, Monoid, Semigroup, Show) via (DList a)
    deriving (Foldable, Functor, Applicative, Monad) via DList

data AttrType (a :: Type) where
  AttrTypeText        :: AttrType Text
  AttrTypeBool        :: AttrType Bool
  AttrTypeDouble      :: AttrType Double
  AttrTypeInt         :: AttrType Int64
  AttrTypeTextArray   :: AttrType (AttrVals Text)
  AttrTypeBoolArray   :: AttrType (AttrVals Bool)
  AttrTypeDoubleArray :: AttrType (AttrVals Double)
  AttrTypeIntArray    :: AttrType (AttrVals Int64)

deriving stock instance (Eq a) => Eq (AttrType a)
deriving stock instance (Show a) => Show (AttrType a)

class KnownAttrType a where
  attrTypeVal :: Proxy a -> AttrType a

instance KnownAttrType Text where
  attrTypeVal _ = AttrTypeText

instance KnownAttrType Bool where
  attrTypeVal _ = AttrTypeBool

instance KnownAttrType Double where
  attrTypeVal _ = AttrTypeDouble

instance KnownAttrType Int64 where
  attrTypeVal _ = AttrTypeInt

instance KnownAttrType (AttrVals Text) where
  attrTypeVal _ = AttrTypeTextArray

instance KnownAttrType (AttrVals Bool) where
  attrTypeVal _ = AttrTypeBoolArray

instance KnownAttrType (AttrVals Double) where
  attrTypeVal _ = AttrTypeDoubleArray

instance KnownAttrType (AttrVals Int64) where
  attrTypeVal _ = AttrTypeIntArray

-- TODO: Provide convenience conversions from other types to known attribute
-- types.

--data TracerProvider = TracerProvider
--  { tracerProviderGetTracer :: IO Tracer
--  , tracerProviderShutdown :: IO ()
--  }

data Tracer = Tracer
  { tracerGetCurrentTimestamp :: IO Timestamp
  , tracerStartSpan :: SpanSpec -> IO Span
  , tracerProcessSpan :: EndedSpan -> IO ()
  , tracerContextBackend :: ContextBackend Span
  }

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
  , spanEventAttributes :: Attrs
  } deriving stock (Eq, Show)

newtype SpanEventSpecs = SpanEventSpecs
  { unSpanEventSpecs :: [SpanEventSpec] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [SpanEventSpec]

data SpanEventSpec = SpanEventSpec
  { spanEventSpecName :: SpanEventName
  , spanEventSpecTimestamp :: TimestampSource
  , spanEventSpecAttributes :: Attrs
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

newtype SpanLinks = SpanLinks
  { unSpanLinks :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [(Text, Text)]

data SpanSpec = SpanSpec
  { spanSpecParent :: SpanParent
  , spanSpecStart :: Timestamp
  , spanSpecKind :: SpanKind
  , spanSpecAttributes :: Attrs
  , spanSpecLinks :: SpanLinks
  } deriving stock (Eq, Show)

data NewSpanSpec = NewSpanSpec
  { newSpanSpecName :: SpanName
  , newSpanSpecParentSource :: SpanParentSource
  , newSpanSpecStart :: TimestampSource
  , newSpanSpecKind :: SpanKind
  , newSpanSpecAttributes :: Attrs
  , newSpanSpecLinks :: SpanLinks
  } deriving stock (Eq, Show)

instance IsString NewSpanSpec where
  fromString s =
    defaultNewSpanSpec
      { newSpanSpecName = fromString s
      }

defaultNewSpanSpec :: NewSpanSpec
defaultNewSpanSpec =
  NewSpanSpec
    { newSpanSpecName = ""
    , newSpanSpecParentSource = SpanParentSourceImplicit
    , newSpanSpecStart = TimestampSourceNow
    , newSpanSpecKind = SpanKindInternal
    , newSpanSpecAttributes = mempty
    , newSpanSpecLinks = mempty
    }

data UpdateSpanSpec = UpdateSpanSpec
  { updateSpanSpecName :: Maybe SpanName
  , updateSpanSpecStatus :: Maybe SpanStatus
  , updateSpanSpecAttributes :: Maybe Attrs
  , updateSpanSpecEvents :: Maybe SpanEventSpecs
  }

defaultUpdateSpanSpec :: UpdateSpanSpec
defaultUpdateSpanSpec =
  UpdateSpanSpec
    { updateSpanSpecName = Nothing
    , updateSpanSpecStatus = Nothing
    , updateSpanSpecAttributes = Nothing
    , updateSpanSpecEvents = Nothing
    }

buildSpanUpdater
  :: (Monad m)
  => m Timestamp
  -> UpdateSpanSpec
  -> m (Span -> Span)
buildSpanUpdater getTimestamp updateSpanSpec = do
  newSpanEvents <- do
    case updateSpanSpecEvents of
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
  pure \span ->
    if not $ spanIsRecording span then
      span
    else
      span
        { spanName =
            Maybe.fromMaybe (spanName span) updateSpanSpecName
        , spanStatus =
            Maybe.fromMaybe (spanStatus span) updateSpanSpecStatus
        , spanAttributes =
            maybe id (\as -> (<> as)) updateSpanSpecAttributes $ spanAttributes span
        , spanEvents =
            spanEvents span <> SpanEvents newSpanEvents
        }
  where
  UpdateSpanSpec
    { updateSpanSpecName
    , updateSpanSpecStatus
    , updateSpanSpecAttributes
    , updateSpanSpecEvents
    } = updateSpanSpec

-- TODO: See https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/exceptions/
-- TODO: Should there be a convenience wrapper that produces a UpdateSpanSpec?
recordException
  :: SomeException
  -> TimestampSource
  -> Attrs
  -> SpanEventSpec
recordException (SomeException e) timestamp attributes =
  SpanEventSpec
    { spanEventSpecName = "exception"
    , spanEventSpecTimestamp = timestamp
    , spanEventSpecAttributes =
        "exception.message" .@ (Text.pack $ Exception.displayException e)
          <> attributes
    }

newtype SpanName = SpanName
  { unSpanName :: Text
  } deriving stock (Eq, Show)

instance IsString SpanName where
  fromString = SpanName . Text.pack

data Span = Span
  { spanParent :: SpanParent
  , spanContext :: SpanContext
  , spanName :: SpanName
  , spanStatus :: SpanStatus
  , spanStart :: Timestamp
  , spanKind :: SpanKind
  , spanAttributes :: Attrs
  , spanLinks :: SpanLinks
  , spanEvents :: SpanEvents
  , spanIsRecording :: Bool
  } deriving stock (Eq, Show)

data EndedSpan = EndedSpan
  { endedSpanParent :: SpanParent
  , endedSpanContext :: SpanContext
  , endedSpanName :: SpanName
  , endedSpanStatus :: SpanStatus
  , endedSpanStart :: Timestamp
  , endedSpanEnd :: Timestamp
  , endedSpanKind :: SpanKind
  , endedSpanAttributes :: Attrs
  , endedSpanLinks :: SpanLinks
  , endedSpanEvents :: SpanEvents
  } deriving stock (Eq, Show)

toEndedSpan :: Timestamp -> Span -> EndedSpan
toEndedSpan endedSpanEnd span =
  EndedSpan
    { endedSpanParent = spanParent span
    , endedSpanContext = spanContext span
    , endedSpanName = spanName span
    , endedSpanStatus = spanStatus span
    , endedSpanStart = spanStart span
    , endedSpanEnd
    , endedSpanKind = spanKind span
    , endedSpanAttributes = spanAttributes span
    , endedSpanLinks = spanLinks span
    , endedSpanEvents = spanEvents span
    }

data SpanParentSource
  = SpanParentSourceImplicit
  | SpanParentSourceExplicit SpanParent
  deriving stock (Eq, Show)

pattern Implicit :: SpanParentSource
pattern Implicit <- SpanParentSourceImplicit where
  Implicit = SpanParentSourceImplicit

pattern Explicit :: SpanParent -> SpanParentSource
pattern Explicit sp <- SpanParentSourceExplicit sp where
  Explicit sp = SpanParentSourceExplicit sp

{-# COMPLETE Implicit, Explicit :: SpanParentSource #-}

data SpanParent
  = SpanParentRoot
  | SpanParentChildOf SpanContext
  deriving stock (Eq, Show)

pattern Root :: SpanParent
pattern Root <- SpanParentRoot where
  Root = SpanParentRoot

pattern ChildOf :: SpanContext -> SpanParent
pattern ChildOf sc <- SpanParentChildOf sc where
  ChildOf sc = SpanParentChildOf sc

{-# COMPLETE Root, ChildOf :: SpanParent #-}

data SpanKind
  = SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  | SpanKindInternal
  deriving stock (Eq, Show)

pattern Server :: SpanKind
pattern Server <- SpanKindServer where
  Server = SpanKindServer

pattern Client :: SpanKind
pattern Client <- SpanKindClient where
  Client = SpanKindClient

pattern Producer :: SpanKind
pattern Producer <- SpanKindProducer where
  Producer = SpanKindProducer

pattern Consumer :: SpanKind
pattern Consumer <- SpanKindConsumer where
  Consumer = SpanKindConsumer

pattern Internal :: SpanKind
pattern Internal <- SpanKindInternal where
  Internal = SpanKindInternal

{-# COMPLETE Server, Client, Producer, Consumer, Internal :: SpanKind #-}

data SpanStatus
  = SpanStatusUnset
  | SpanStatusOk
  | SpanStatusError Text
  deriving stock (Eq, Show)

pattern Unset :: SpanStatus
pattern Unset <- SpanStatusUnset where
  Unset = SpanStatusUnset

pattern OK :: SpanStatus
pattern OK <- SpanStatusOk where
  OK = SpanStatusOk

pattern Error :: Text -> SpanStatus
pattern Error errText <- SpanStatusError errText where
  Error errText = SpanStatusError errText

{-# COMPLETE Unset, OK, Error :: SpanStatus #-}

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
