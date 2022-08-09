{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , defaultInstrumentationScope
  , InstrumentationScopeName(..)
  , Version(..)
  , SchemaURL(..)
  , schemaURLFromText
  , schemaURLToText

    -- * Attributes
  , Attrs(..)
  , nullAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs
  , AttrsBuilder(..)
  , AttrsFor(..)
  , SpanAttrsLimits
  , SpanEventAttrsLimits
  , SpanLinkAttrsLimits
  , AttrsLimits(..)
  , defaultAttrsLimits
  , SomeAttr(..)
  , Attr(..)
  , AttrVals(..)
  , AttrType(..)
  , KnownAttrType(..)
  , ToAttrVal(..)

    -- * Tracing
  , Tracer(..)
  , SpanContext(..)
  , emptySpanContext
  , spanContextIsValid
  , TraceId(..)
  , emptyTraceId
  , SpanId(..)
  , emptySpanId
  , TraceFlags(..)
  , TraceState(..)
  , SpanEvents(..)
  , spanEventsFromList
  , spanEventsToList
  , freezeAllSpanEventAttrs
  , SpanEvent(..)
  , freezeSpanEventAttrs
  , SpanEventSpecs(..)
  , singletonSpanEventSpecs
  , spanEventSpecsFromList
  , spanEventSpecsToList
  , SpanEventSpec(..)
  , defaultSpanEventSpec
  , SpanEventName(..)
  , SpanLinks(..)
  , spanLinksFromList
  , spanLinksToList
  , freezeAllSpanLinkAttrs
  , SpanLinkSpecs(..)
  , singletonSpanLinkSpecs
  , spanLinkSpecsFromList
  , spanLinkSpecsToList
  , SpanLink(..)
  , freezeSpanLinkAttrs
  , SpanLinkName(..)
  , SpanLinkSpec(..)
  , defaultSpanLinkSpec
  , SpanSpec(..)
  , buildSpanSpec
  , NewSpanSpec(..)
  , defaultNewSpanSpec
  , UpdateSpanSpec(..)
  , defaultUpdateSpanSpec
  , buildSpanUpdater
  , recordException
  , SpanName(..)
  , Span(..)
  , emptySpan
  , EndedSpan(..)
  , toEndedSpan
  , SpanParentSource(.., Implicit, Explicit)
  , SpanParent(.., Root, ChildOf)
  , SpanKind(.., Server, Client, Producer, Consumer, Internal)
  , SpanStatus(.., Unset, OK, Error)
  ) where

import Control.Exception (SomeException(..))
import Data.DList (DList)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (float2Double)
import OTel.API.Context (ContextBackend)
import Prelude hiding (span)
import qualified Control.Exception as Exception
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Traversable as Traversable
import qualified Data.Vector as Vector

class KV (kv :: Type) where
  type KVConstraints kv :: Type -> Type -> Constraint
  (.@) :: KVConstraints kv from to => Key to -> from -> kv

-- TODO: Enforce 'attrsLimitsValueLength' here!
instance KV (AttrsBuilder af) where
  type KVConstraints (AttrsBuilder af) = ToAttrVal
  (.@) = go
    where
    go :: forall to from. (ToAttrVal from to) => Key to -> from -> (AttrsBuilder af)
    go k v =
      AttrsBuilder \_attrsLimits -> Attrs $ HashMap.singleton (unKey k) $ SomeAttr Attr
        { attrType = attrTypeVal $ Proxy @to
        , attrVal = toAttrVal @from @to v
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

newtype Attrs (af :: AttrsFor) = Attrs
  { unAttrs :: HashMap Text SomeAttr
  } deriving stock (Eq, Show)

nullAttrs :: Attrs af -> Bool
nullAttrs = HashMap.null . unAttrs

sizeAttrs :: Attrs af -> Int
sizeAttrs = HashMap.size . unAttrs

memberAttrs :: Key a -> Attrs af -> Bool
memberAttrs key = HashMap.member (unKey key) . unAttrs

lookupAttrs
  :: forall a af
   . (KnownAttrType a)
  => Key a
  -> Attrs af
  -> Maybe (Attr a)
lookupAttrs key attrs =
  case HashMap.lookup (unKey key) $ unAttrs attrs of
    Nothing -> Nothing
    Just (SomeAttr attr) ->
      case (attrTypeVal $ Proxy @a, attrType attr) of
        (AttrTypeText, AttrTypeText) -> Just attr
        (AttrTypeBool, AttrTypeBool) -> Just attr
        (AttrTypeDouble, AttrTypeDouble) -> Just attr
        (AttrTypeInt, AttrTypeInt) -> Just attr
        (AttrTypeTextArray, AttrTypeTextArray) -> Just attr
        (AttrTypeBoolArray, AttrTypeBoolArray) -> Just attr
        (AttrTypeDoubleArray, AttrTypeDoubleArray) -> Just attr
        (AttrTypeIntArray, AttrTypeIntArray) -> Just attr
        (_, _) -> Nothing

foldMapWithKeyAttrs
  :: forall m af
   . (Monoid m)
  => (forall a. Key a -> Attr a -> m)
  -> Attrs af
  -> m
foldMapWithKeyAttrs f attrs =
  flip HashMap.foldMapWithKey (unAttrs attrs) \keyText someAttr ->
    case someAttr of
      SomeAttr attr -> f (Key keyText) attr

newtype AttrsBuilder (af :: AttrsFor) = AttrsBuilder
  { runAttrsBuilder :: AttrsLimits af -> Attrs af
  }

instance Semigroup (AttrsBuilder af) where
  bx <> by =
    AttrsBuilder \attrsLimits ->
      let x = unAttrs $ runAttrsBuilder bx attrsLimits
          y = unAttrs $ runAttrsBuilder by attrsLimits
       in Attrs $ snd $ HashMap.foldrWithKey' (build attrsLimits) (HashMap.size x, x) y
    where
    build
      :: AttrsLimits af
      -> Text
      -> SomeAttr
      -> (Int, HashMap Text SomeAttr)
      -> (Int, HashMap Text SomeAttr)
    build attrsLimits k v (!size, acc) =
      if size >= Maybe.fromMaybe (maxBound :: Int) attrsLimitsCount then
        (size, acc)
      else
        ( if k `HashMap.member` acc then
            size
          else
            1 + size
        , HashMap.insert k v acc
        )
      where
      AttrsLimits { attrsLimitsCount } = attrsLimits

instance Monoid (AttrsBuilder af) where
  mempty = AttrsBuilder \_attrsLimits -> Attrs $ HashMap.empty
  mappend = (<>)

data AttrsFor
  = AttrsForSpan
  | AttrsForSpanEvent
  | AttrsForSpanLink

type SpanAttrsLimits = AttrsLimits 'AttrsForSpan
type SpanEventAttrsLimits = AttrsLimits 'AttrsForSpanEvent
type SpanLinkAttrsLimits = AttrsLimits 'AttrsForSpanLink

data AttrsLimits (af :: AttrsFor) = AttrsLimits
  { attrsLimitsCount :: Maybe Int
  , attrsLimitsValueLength :: Maybe Int
  }

defaultAttrsLimits :: AttrsLimits af
defaultAttrsLimits =
  AttrsLimits
    { attrsLimitsCount = Just 128
    , attrsLimitsValueLength = Nothing
    }

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
  , attrVal :: a
  } deriving stock (Eq, Show)

newtype AttrVals a = AttrVals
  { unAttrVals :: Vector a
  } deriving (Eq, Monoid, Semigroup, Show) via (Vector a)
    deriving (Foldable, Functor, Applicative, Monad) via Vector

instance Traversable AttrVals where
  traverse f (AttrVals xs) = fmap AttrVals $ traverse f xs

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

class (KnownAttrType to) => ToAttrVal from to | from -> to where
  toAttrVal :: from -> to

instance ToAttrVal Text Text where
  toAttrVal = id

instance ToAttrVal Text.Lazy.Text Text where
  toAttrVal = Text.Lazy.toStrict

instance ToAttrVal String Text where
  toAttrVal = Text.pack

instance ToAttrVal Bool Bool where
  toAttrVal = id

instance ToAttrVal Double Double where
  toAttrVal = id

instance ToAttrVal Float Double where
  toAttrVal = float2Double

-- | Precision may be lost.
instance ToAttrVal Rational Double where
  toAttrVal = fromRational

instance ToAttrVal Int Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal Int8 Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal Int16 Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal Int32 Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal Int64 Int64 where
  toAttrVal = id

instance ToAttrVal Word8 Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal Word16 Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal Word32 Int64 where
  toAttrVal = fromIntegral

instance ToAttrVal (AttrVals Text) (AttrVals Text) where
  toAttrVal = id

instance ToAttrVal [Text] (AttrVals Text) where
  toAttrVal = AttrVals . Vector.fromList

instance ToAttrVal (Seq Text) (AttrVals Text) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList

instance ToAttrVal (Vector Text) (AttrVals Text) where
  toAttrVal = AttrVals

instance ToAttrVal (AttrVals Text.Lazy.Text) (AttrVals Text) where
  toAttrVal = fmap (toAttrVal @Text.Lazy.Text @Text)

instance ToAttrVal [Text.Lazy.Text] (AttrVals Text) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Text.Lazy.Text @Text)

instance ToAttrVal (Seq Text.Lazy.Text) (AttrVals Text) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Text.Lazy.Text @Text)

instance ToAttrVal (Vector Text.Lazy.Text) (AttrVals Text) where
  toAttrVal = AttrVals . fmap (toAttrVal @Text.Lazy.Text @Text)

instance ToAttrVal (AttrVals String) (AttrVals Text) where
  toAttrVal = fmap (toAttrVal @String @Text)

instance ToAttrVal [String] (AttrVals Text) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @String @Text)

instance ToAttrVal (Seq String) (AttrVals Text) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @String @Text)

instance ToAttrVal (Vector String) (AttrVals Text) where
  toAttrVal = AttrVals . fmap (toAttrVal @String @Text)

instance ToAttrVal (AttrVals Bool) (AttrVals Bool) where
  toAttrVal = id

instance ToAttrVal [Bool] (AttrVals Bool) where
  toAttrVal = AttrVals . Vector.fromList

instance ToAttrVal (Seq Bool) (AttrVals Bool) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList

instance ToAttrVal (Vector Bool) (AttrVals Bool) where
  toAttrVal = AttrVals

instance ToAttrVal (AttrVals Double) (AttrVals Double) where
  toAttrVal = id

instance ToAttrVal [Double] (AttrVals Double) where
  toAttrVal = AttrVals . Vector.fromList

instance ToAttrVal (Seq Double) (AttrVals Double) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList

instance ToAttrVal (Vector Double) (AttrVals Double) where
  toAttrVal = AttrVals

instance ToAttrVal (AttrVals Float) (AttrVals Double) where
  toAttrVal = fmap (toAttrVal @Float @Double)

instance ToAttrVal [Float] (AttrVals Double) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Float @Double)

instance ToAttrVal (Seq Float) (AttrVals Double) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Float @Double)

instance ToAttrVal (Vector Float) (AttrVals Double) where
  toAttrVal = AttrVals . fmap (toAttrVal @Float @Double)

-- | Precision may be lost.
instance ToAttrVal (AttrVals Rational) (AttrVals Double) where
  toAttrVal = fmap (toAttrVal @Rational @Double)

-- | Precision may be lost.
instance ToAttrVal [Rational] (AttrVals Double) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Rational @Double)

-- | Precision may be lost.
instance ToAttrVal (Seq Rational) (AttrVals Double) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Rational @Double)

-- | Precision may be lost.
instance ToAttrVal (Vector Rational) (AttrVals Double) where
  toAttrVal = AttrVals . fmap (toAttrVal @Rational @Double)

instance ToAttrVal (AttrVals Int) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Int @Int64)

instance ToAttrVal [Int] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Int @Int64)

instance ToAttrVal (Seq Int) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Int @Int64)

instance ToAttrVal (Vector Int) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Int @Int64)

instance ToAttrVal (AttrVals Int8) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Int8 @Int64)

instance ToAttrVal [Int8] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Int8 @Int64)

instance ToAttrVal (Seq Int8) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Int8 @Int64)

instance ToAttrVal (Vector Int8) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Int8 @Int64)

instance ToAttrVal (AttrVals Int16) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Int16 @Int64)

instance ToAttrVal [Int16] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Int16 @Int64)

instance ToAttrVal (Seq Int16) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Int16 @Int64)

instance ToAttrVal (Vector Int16) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Int16 @Int64)

instance ToAttrVal (AttrVals Int32) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Int32 @Int64)

instance ToAttrVal [Int32] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Int32 @Int64)

instance ToAttrVal (Seq Int32) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Int32 @Int64)

instance ToAttrVal (Vector Int32) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Int32 @Int64)

instance ToAttrVal (AttrVals Int64) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Int64 @Int64)

instance ToAttrVal [Int64] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList

instance ToAttrVal (Seq Int64) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList

instance ToAttrVal (Vector Int64) (AttrVals Int64) where
  toAttrVal = AttrVals

instance ToAttrVal (AttrVals Word8) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Word8 @Int64)

instance ToAttrVal [Word8] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Word8 @Int64)

instance ToAttrVal (Seq Word8) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Word8 @Int64)

instance ToAttrVal (Vector Word8) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Word8 @Int64)

instance ToAttrVal (AttrVals Word16) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Word16 @Int64)

instance ToAttrVal [Word16] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Word16 @Int64)

instance ToAttrVal (Seq Word16) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Word16 @Int64)

instance ToAttrVal (Vector Word16) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Word16 @Int64)

instance ToAttrVal (AttrVals Word32) (AttrVals Int64) where
  toAttrVal = fmap (toAttrVal @Word32 @Int64)

instance ToAttrVal [Word32] (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . fmap (toAttrVal @Word32 @Int64)

instance ToAttrVal (Seq Word32) (AttrVals Int64) where
  toAttrVal = AttrVals . Vector.fromList . Foldable.toList . fmap (toAttrVal @Word32 @Int64)

instance ToAttrVal (Vector Word32) (AttrVals Int64) where
  toAttrVal = AttrVals . fmap (toAttrVal @Word32 @Int64)

--data TracerProvider = TracerProvider
--  { tracerProviderGetTracer :: IO Tracer
--  , tracerProviderShutdown :: IO ()
--  }

data Tracer = Tracer
  { tracerNow :: IO Timestamp
  , tracerStartSpan :: SpanSpec -> IO Span
  , tracerProcessSpan :: EndedSpan -> IO ()
  , tracerContextBackend :: ContextBackend Span
  , tracerSpanAttrsLimits :: SpanAttrsLimits
  , tracerSpanEventAttrsLimits :: SpanEventAttrsLimits
  , tracerSpanLinkAttrsLimits :: SpanLinkAttrsLimits
  }

data SpanContext = SpanContext
  { spanContextTraceId :: TraceId
  , spanContextSpanId :: SpanId
  , spanContextTraceFlags :: TraceFlags
  , spanContextTraceState :: TraceState
  , spanContextIsRemote :: Bool
  } deriving stock (Eq, Show)

emptySpanContext :: SpanContext
emptySpanContext =
  SpanContext
    { spanContextTraceId = emptyTraceId
    , spanContextSpanId = emptySpanId
    , spanContextTraceFlags = TraceFlags { unTraceFlags = 0 }
    , spanContextTraceState = TraceState  { unTraceState = [] }
    , spanContextIsRemote = False
    }

spanContextIsValid :: SpanContext -> Bool
spanContextIsValid spanContext =
  spanContextTraceId /= emptyTraceId && spanContextSpanId /= emptySpanId
  where
  SpanContext { spanContextTraceId, spanContextSpanId } = spanContext

-- TODO: Get hex string
-- TODO: Get byte array
data TraceId = TraceId
  { traceIdHi :: Word64
  , traceIdLo :: Word64
  } deriving stock (Eq, Show)

emptyTraceId :: TraceId
emptyTraceId = TraceId { traceIdHi = 0, traceIdLo = 0 }

-- TODO: Get hex string
-- TODO: Get byte array
data SpanId = SpanId
  { spanIdLo :: Word64
  } deriving stock (Eq, Show)

emptySpanId :: SpanId
emptySpanId = SpanId { spanIdLo = 0 }

newtype TraceFlags = TraceFlags
  { unTraceFlags :: Word8
  } deriving stock (Eq, Show)

newtype TraceState = TraceState
  { unTraceState :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Show)

newtype SpanEvents (attrs :: AttrsFor -> Type) = SpanEvents
  { unSpanEvents :: DList (SpanEvent attrs)
  }

deriving stock instance (Eq (attrs 'AttrsForSpanEvent)) => Eq (SpanEvents attrs)
deriving stock instance (Show (attrs 'AttrsForSpanEvent)) => Show (SpanEvents attrs)
deriving via (DList (SpanEvent attrs)) instance Monoid (SpanEvents attrs)
deriving via (DList (SpanEvent attrs)) instance Semigroup (SpanEvents attrs)

spanEventsFromList :: [SpanEvent attrs] -> SpanEvents attrs
spanEventsFromList = SpanEvents . DList.fromList

spanEventsToList :: SpanEvents attrs -> [SpanEvent attrs]
spanEventsToList = Foldable.toList . unSpanEvents

freezeAllSpanEventAttrs
  :: SpanEventAttrsLimits
  -> SpanEvents AttrsBuilder
  -> SpanEvents Attrs
freezeAllSpanEventAttrs attrsLimits spanEvent =
  SpanEvents
    $ fmap (freezeSpanEventAttrs attrsLimits)
    $ unSpanEvents spanEvent

data SpanEvent (attrs :: AttrsFor -> Type) = SpanEvent
  { spanEventName :: SpanEventName
  , spanEventTimestamp :: Timestamp
  , spanEventAttrs :: attrs 'AttrsForSpanEvent
  }

deriving stock instance (Eq (attrs 'AttrsForSpanEvent)) => Eq (SpanEvent attrs)
deriving stock instance (Show (attrs 'AttrsForSpanEvent)) => Show (SpanEvent attrs)

freezeSpanEventAttrs
  :: SpanEventAttrsLimits
  -> SpanEvent AttrsBuilder
  -> SpanEvent Attrs
freezeSpanEventAttrs attrsLimits spanEvent =
  SpanEvent
    { spanEventName = spanEventName spanEvent
    , spanEventTimestamp = spanEventTimestamp spanEvent
    , spanEventAttrs = runAttrsBuilder (spanEventAttrs spanEvent) attrsLimits
    }

newtype SpanEventSpecs = SpanEventSpecs
  { unSpanEventSpecs :: DList SpanEventSpec
  } deriving (Monoid, Semigroup) via (DList SpanEventSpec)

singletonSpanEventSpecs :: SpanEventSpec -> SpanEventSpecs
singletonSpanEventSpecs = SpanEventSpecs . DList.singleton

spanEventSpecsFromList :: [SpanEventSpec] -> SpanEventSpecs
spanEventSpecsFromList = SpanEventSpecs . DList.fromList

spanEventSpecsToList :: SpanEventSpecs -> [SpanEventSpec]
spanEventSpecsToList = Foldable.toList . unSpanEventSpecs

data SpanEventSpec = SpanEventSpec
  { spanEventSpecName :: SpanEventName
  , spanEventSpecTimestamp :: TimestampSource
  , spanEventSpecAttrs :: AttrsBuilder 'AttrsForSpanEvent
  }

defaultSpanEventSpec :: SpanEventSpec
defaultSpanEventSpec =
  SpanEventSpec
    { spanEventSpecName = ""
    , spanEventSpecTimestamp = TimestampSourceNow
    , spanEventSpecAttrs = mempty
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

newtype SpanLinks (attrs :: AttrsFor -> Type) = SpanLinks
  { unSpanLinks :: DList (SpanLink attrs)
  }

deriving stock instance (Eq (attrs 'AttrsForSpanLink)) => Eq (SpanLinks attrs)
deriving stock instance (Show (attrs 'AttrsForSpanLink)) => Show (SpanLinks attrs)
deriving via (DList (SpanLink attrs)) instance Monoid (SpanLinks attrs)
deriving via (DList (SpanLink attrs)) instance Semigroup (SpanLinks attrs)

spanLinksFromList :: [SpanLink attrs] -> SpanLinks attrs
spanLinksFromList = SpanLinks . DList.fromList

spanLinksToList :: SpanLinks attrs -> [SpanLink attrs]
spanLinksToList = Foldable.toList . unSpanLinks

freezeAllSpanLinkAttrs
  :: SpanLinkAttrsLimits
  -> SpanLinks AttrsBuilder
  -> SpanLinks Attrs
freezeAllSpanLinkAttrs attrsLimits spanLink =
  SpanLinks
    $ fmap (freezeSpanLinkAttrs attrsLimits)
    $ unSpanLinks spanLink

newtype SpanLinkSpecs = SpanLinkSpecs
  { unSpanLinkSpecs :: DList SpanLinkSpec
  } deriving (Monoid, Semigroup) via (DList SpanLinkSpec)

singletonSpanLinkSpecs :: SpanLinkSpec -> SpanLinkSpecs
singletonSpanLinkSpecs = SpanLinkSpecs . DList.singleton

spanLinkSpecsFromList :: [SpanLinkSpec] -> SpanLinkSpecs
spanLinkSpecsFromList = SpanLinkSpecs . DList.fromList

spanLinkSpecsToList :: SpanLinkSpecs -> [SpanLinkSpec]
spanLinkSpecsToList = Foldable.toList . unSpanLinkSpecs

data SpanLink (attrs :: AttrsFor -> Type) = SpanLink
  { spanLinkSpanContext :: SpanContext
  , spanLinkAttrs :: attrs 'AttrsForSpanLink
  }

deriving stock instance (Eq (attrs 'AttrsForSpanLink)) => Eq (SpanLink attrs)
deriving stock instance (Show (attrs 'AttrsForSpanLink)) => Show (SpanLink attrs)

freezeSpanLinkAttrs
  :: SpanLinkAttrsLimits
  -> SpanLink AttrsBuilder
  -> SpanLink Attrs
freezeSpanLinkAttrs attrsLimits spanLink =
  SpanLink
    { spanLinkSpanContext = spanLinkSpanContext spanLink
    , spanLinkAttrs = runAttrsBuilder (spanLinkAttrs spanLink) attrsLimits
    }

newtype SpanLinkName = SpanLinkName
  { unSpanLinkName :: Text
  } deriving stock (Eq, Show)

instance IsString SpanLinkName where
  fromString = SpanLinkName . Text.pack

data SpanLinkSpec = SpanLinkSpec
  { spanLinkSpecSpanContext :: SpanContext
  , spanLinkSpecAttrs :: AttrsBuilder 'AttrsForSpanLink
  }

defaultSpanLinkSpec :: SpanLinkSpec
defaultSpanLinkSpec =
  SpanLinkSpec
    { spanLinkSpecSpanContext = emptySpanContext
    , spanLinkSpecAttrs = mempty
    }

data SpanSpec = SpanSpec
  { spanSpecParent :: SpanParent
  , spanSpecName :: SpanName
  , spanSpecStart :: Timestamp
  , spanSpecKind :: SpanKind
  , spanSpecAttrs :: AttrsBuilder 'AttrsForSpan
  , spanSpecLinks :: SpanLinks AttrsBuilder
  }

buildSpanSpec
  :: forall m
   . (Monad m)
  => m Timestamp
  -> (SpanParentSource -> m SpanParent)
  -> NewSpanSpec
  -> m SpanSpec
buildSpanSpec getTimestamp spanParentFromSource newSpanSpec = do
  spanSpecParent <- spanParentFromSource newSpanSpecParentSource
  spanSpecStart <- do
    case newSpanSpecStart of
      TimestampSourceAt timestamp -> pure timestamp
      TimestampSourceNow -> getTimestamp

  pure SpanSpec
    { spanSpecParent
    , spanSpecName = newSpanSpecName
    , spanSpecStart
    , spanSpecKind = newSpanSpecKind
    , spanSpecAttrs = newSpanSpecAttrs
    , spanSpecLinks =
        SpanLinks $ flip fmap rawLinkSpecs \spanLinkSpec ->
          SpanLink
            { spanLinkSpanContext =
                spanLinkSpecSpanContext spanLinkSpec
            , spanLinkAttrs =
                spanLinkSpecAttrs spanLinkSpec
            }
    }
  where
  rawLinkSpecs = unSpanLinkSpecs newSpanSpecLinks

  NewSpanSpec
    { newSpanSpecName
    , newSpanSpecParentSource
    , newSpanSpecStart
    , newSpanSpecKind
    , newSpanSpecAttrs
    , newSpanSpecLinks
    } = newSpanSpec

data NewSpanSpec = NewSpanSpec
  { newSpanSpecName :: SpanName
  , newSpanSpecParentSource :: SpanParentSource
  , newSpanSpecStart :: TimestampSource
  , newSpanSpecKind :: SpanKind
  , newSpanSpecAttrs :: AttrsBuilder 'AttrsForSpan
  , newSpanSpecLinks :: SpanLinkSpecs
  }

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
    , newSpanSpecAttrs = mempty
    , newSpanSpecLinks = mempty
    }

data UpdateSpanSpec = UpdateSpanSpec
  { updateSpanSpecName :: Maybe SpanName
  , updateSpanSpecStatus :: Maybe SpanStatus
  , updateSpanSpecAttrs :: Maybe (AttrsBuilder 'AttrsForSpan)
  , updateSpanSpecEvents :: Maybe SpanEventSpecs
  }

defaultUpdateSpanSpec :: UpdateSpanSpec
defaultUpdateSpanSpec =
  UpdateSpanSpec
    { updateSpanSpecName = Nothing
    , updateSpanSpecStatus = Nothing
    , updateSpanSpecAttrs = Nothing
    , updateSpanSpecEvents = Nothing
    }

buildSpanUpdater
  :: forall m
   . (Monad m)
  => m Timestamp
  -> UpdateSpanSpec
  -> m (Span -> Span)
buildSpanUpdater getTimestamp updateSpanSpec = do
  newSpanEvents <- do
    fmap SpanEvents do
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
              , spanEventAttrs = spanEventSpecAttrs spanEventSpec
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
        , spanAttrs =
            case updateSpanSpecAttrs of
              Nothing -> spanAttrs span
              Just attrsBuilder ->
                spanAttrs span <> attrsBuilder
        , spanEvents =
            spanEvents span <> newSpanEvents
        }
  where
  UpdateSpanSpec
    { updateSpanSpecName
    , updateSpanSpecStatus
    , updateSpanSpecAttrs
    , updateSpanSpecEvents
    } = updateSpanSpec

-- TODO: See https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/exceptions/
-- TODO: Should there be a convenience wrapper that produces a UpdateSpanSpec?
recordException
  :: SomeException
  -> TimestampSource
  -> AttrsBuilder 'AttrsForSpanEvent
  -> SpanEventSpec
recordException (SomeException e) timestamp attributes =
  SpanEventSpec
    { spanEventSpecName = "exception"
    , spanEventSpecTimestamp = timestamp
    , spanEventSpecAttrs =
        "exception.message" .@ Exception.displayException e <> attributes
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
  , spanAttrs :: AttrsBuilder 'AttrsForSpan
  , spanLinks :: SpanLinks AttrsBuilder
  , spanEvents :: SpanEvents AttrsBuilder
  , spanIsRecording :: Bool
  }

emptySpan :: SpanParent -> Timestamp -> Span
emptySpan spanParent spanStart =
  Span
    { spanParent
    , spanContext = emptySpanContext
    , spanName = ""
    , spanStatus = SpanStatusUnset
    , spanStart
    , spanKind = SpanKindInternal
    , spanAttrs = mempty
    , spanLinks = mempty
    , spanEvents = mempty
    , spanIsRecording = False
    }

data EndedSpan = EndedSpan
  { endedSpanParent :: SpanParent
  , endedSpanContext :: SpanContext
  , endedSpanName :: SpanName
  , endedSpanStatus :: SpanStatus
  , endedSpanStart :: Timestamp
  , endedSpanEnd :: Timestamp
  , endedSpanKind :: SpanKind
  , endedSpanAttrs :: Attrs 'AttrsForSpan
  , endedSpanLinks :: SpanLinks Attrs
  , endedSpanEvents :: SpanEvents Attrs
  } deriving stock (Eq, Show)

toEndedSpan
  :: Timestamp
  -> SpanLinkAttrsLimits
  -> SpanEventAttrsLimits
  -> SpanAttrsLimits
  -> Span
  -> EndedSpan
toEndedSpan endedSpanEnd spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits span =
  EndedSpan
    { endedSpanParent = spanParent span
    , endedSpanContext = spanContext span
    , endedSpanName = spanName span
    , endedSpanStatus = spanStatus span
    , endedSpanStart = spanStart span
    , endedSpanEnd
    , endedSpanKind = spanKind span
    , endedSpanAttrs =
        runAttrsBuilder (spanAttrs span) spanAttrsLimits
    , endedSpanLinks =
        freezeAllSpanLinkAttrs spanLinkAttrsLimits $ spanLinks span
    , endedSpanEvents =
        freezeAllSpanEventAttrs spanEventAttrsLimits $ spanEvents span
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
