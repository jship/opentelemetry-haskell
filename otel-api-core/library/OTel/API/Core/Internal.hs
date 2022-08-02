{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , TimestampSource(..)
  , InstrumentationScope(..)
  , InstrumentationScopeName(..)
  , Version(..)
  , SchemaURL(..)
  , schemaURLFromText
  , schemaURLToText

    -- * Attributes
  , Attributes(..)
  , fromAttributes
  , Attribute(..)
  , AttrValue(..)
  , ToAttrValue(..)
  , KnownPrimAttrType(..)
  , primAttrVal
  , primAttrVal'
  , PrimAttrType(..)
  , PrimAttrValue(..)

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
  , SpanLineageSource(..)
  , SpanLineage(..)
  , SpanKind(..)
  , SpanStatus(..)

    -- * Miscellaneous
  , DList(..)
  , fromDList
  ) where

import Control.Exception (SomeException(..))
import Data.Int (Int64)
import Data.Kind (Constraint, Type)
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Typeable ((:~:)(..), Typeable, eqT)
import Data.Word (Word64, Word8)
import OTel.API.Context (ContextBackend)
import Prelude hiding (span)
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable

newtype Attributes = Attributes
  { unAttributes :: DList Attribute
  } deriving (Eq, Semigroup, Show, Monoid) via (DList Attribute)

fromAttributes :: Attributes -> [Attribute]
fromAttributes = fromDList . unAttributes

instance IsString Key where
  fromString = Key . Text.pack

data Attribute = Attribute
  { attributeKey :: Key
  , attributeValue :: AttrValue
  } deriving stock (Eq, Show)

data AttrValue where
  AttrValuePrimitive
    :: (KnownPrimAttrType a)
    => PrimAttrValue a
    -> AttrValue
  AttrValueArray
    :: (KnownPrimAttrType a)
    => [PrimAttrValue a]
    -> AttrValue

instance Eq AttrValue where
  ax == ay =
    case (ax, ay) of
      (AttrValuePrimitive x, AttrValuePrimitive y) ->
        case eqTPrim x y of
          Nothing -> False
          Just Refl -> x == y
      (AttrValueArray xs, AttrValueArray ys) ->
        case eqTPrimList xs ys of
          Nothing -> False
          Just Refl -> xs == ys
      (_, _) -> False
    where
    eqTPrim
      :: forall a b
       . (Typeable a, Typeable b)
      => PrimAttrValue a
      -> PrimAttrValue b
      -> Maybe (a :~: b)
    eqTPrim _ _ = eqT @a @b

    eqTPrimList
      :: forall a b
       . (Typeable a, Typeable b)
      => [PrimAttrValue a]
      -> [PrimAttrValue b]
      -> Maybe (a :~: b)
    eqTPrimList _ _ = eqT @a @b

instance Show AttrValue where
  show = \case
    AttrValuePrimitive x -> show x
    AttrValueArray xs -> show xs

class ToAttrValue a where
  toAttrValue :: a -> AttrValue

instance ToAttrValue Text where
  toAttrValue = AttrValuePrimitive . PrimAttrValueText

instance ToAttrValue Bool where
  toAttrValue = AttrValuePrimitive . PrimAttrValueBool

instance ToAttrValue Double where
  toAttrValue = AttrValuePrimitive . PrimAttrValueDouble

instance ToAttrValue Int64 where
  toAttrValue = AttrValuePrimitive . PrimAttrValueInt

instance (KnownPrimAttrType a) => ToAttrValue [a] where
  toAttrValue = AttrValueArray . fmap primAttrVal

class (Eq a, Show a, Typeable a) => KnownPrimAttrType a where
  primAttrTypeVal :: proxy a -> PrimAttrType a

instance KnownPrimAttrType Text where
  primAttrTypeVal _ = PrimAttrTypeText

instance KnownPrimAttrType Bool where
  primAttrTypeVal _ = PrimAttrTypeBool

instance KnownPrimAttrType Double where
  primAttrTypeVal _ = PrimAttrTypeDouble

instance KnownPrimAttrType Int64 where
  primAttrTypeVal _ = PrimAttrTypeInt

primAttrVal
  :: forall a
   . (KnownPrimAttrType a)
  => a
  -> PrimAttrValue a
primAttrVal x = primAttrVal' (primAttrTypeVal $ Proxy @a) x

primAttrVal' :: PrimAttrType a -> a -> PrimAttrValue a
primAttrVal' ty x =
  case ty of
    PrimAttrTypeText -> PrimAttrValueText x
    PrimAttrTypeBool -> PrimAttrValueBool x
    PrimAttrTypeDouble -> PrimAttrValueDouble x
    PrimAttrTypeInt -> PrimAttrValueInt x

data PrimAttrType a where
  PrimAttrTypeText   :: PrimAttrType Text
  PrimAttrTypeBool   :: PrimAttrType Bool
  PrimAttrTypeDouble :: PrimAttrType Double
  PrimAttrTypeInt    :: PrimAttrType Int64

deriving stock instance (Eq a) => Eq (PrimAttrType a)
deriving stock instance (Show a) => Show (PrimAttrType a)

data PrimAttrValue a where
  PrimAttrValueText   :: Text   -> PrimAttrValue Text
  PrimAttrValueBool   :: Bool   -> PrimAttrValue Bool
  PrimAttrValueDouble :: Double -> PrimAttrValue Double
  PrimAttrValueInt    :: Int64  -> PrimAttrValue Int64

deriving stock instance (Eq a) => Eq (PrimAttrValue a)
deriving stock instance (Show a) => Show (PrimAttrValue a)

class KV (kv :: Type) where
  type ToValue kv :: Type -> Constraint
  (.@) :: ToValue kv v => Key -> v -> kv

instance KV Attributes where
  type ToValue Attributes = ToAttrValue
  name .@ value =
    Attributes $ singletonDList Attribute
      { attributeKey = name
      , attributeValue = toAttrValue value
      }

newtype Key = Key
  { unKey :: Text
  } deriving stock (Eq, Show)

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

newtype SpanLinks = SpanLinks
  { unSpanLinks :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via [(Text, Text)]

data SpanSpec = SpanSpec
  { spanSpecLineage :: SpanLineage
  , spanSpecStart :: Timestamp
  , spanSpecKind :: SpanKind
  , spanSpecAttributes :: Attributes
  , spanSpecLinks :: SpanLinks
  } deriving stock (Eq, Show)

data NewSpanSpec = NewSpanSpec
  { newSpanSpecName :: SpanName
  , newSpanSpecLineageSource :: SpanLineageSource
  , newSpanSpecStart :: TimestampSource
  , newSpanSpecKind :: SpanKind
  , newSpanSpecAttributes :: Attributes
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
    , newSpanSpecLineageSource = SpanLineageSourceImplicit
    , newSpanSpecStart = TimestampSourceNow
    , newSpanSpecKind = SpanKindInternal
    , newSpanSpecAttributes = mempty
    , newSpanSpecLinks = mempty
    }

data UpdateSpanSpec = UpdateSpanSpec
  { updateSpanSpecName :: Maybe SpanName
  , updateSpanSpecStatus :: Maybe SpanStatus
  , updateSpanSpecAttributes :: Maybe Attributes
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
  -> Attributes
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

newtype DList a = DList
  { unDList :: [a] -> [a]
  } deriving (Semigroup, Monoid) via (Endo [a])

instance (Eq a) => Eq (DList a) where
  x == y = fromDList x == fromDList y

instance (Show a) => Show (DList a) where
  show = show . fromDList

fromDList :: DList a -> [a]
fromDList dlist = unDList dlist []

singletonDList :: a -> DList a
singletonDList = DList . (:)

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
