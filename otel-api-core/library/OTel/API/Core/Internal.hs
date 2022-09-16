{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  , IsTextKV
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
  , runAttrsBuilder
  , AttrsAcc(..)
  , AttrsBuilderElem(..)
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
  , TracerProvider(..)
  , getTracer
  , Tracer(..)
  , SpanBackend(..)
  , SpanContext(..)
  , emptySpanContext
  , spanContextIsValid
  , spanContextIsSampled
  , TraceId(..)
  , traceIdToHexText
  , traceIdToBytesVector
  , traceIdToHexBuilder
  , traceIdToBytesBuilder
  , emptyTraceId
  , traceIdFromWords
  , SpanId(..)
  , spanIdToHexText
  , spanIdToBytesVector
  , spanIdToHexBuilder
  , spanIdToBytesBuilder
  , emptySpanId
  , spanIdFromWords
  , TraceFlags(..)
  , traceFlagsToHexText
  , traceFlagsToHexBuilder
  , emptyTraceFlags
  , setSampledFlag
  , isSampledFlagSet
  , TraceState(..)
  , emptyTraceState
  , nullTraceState
  , sizeTraceState
  , memberTraceState
  , lookupTraceState
  , findWithDefaultTraceState
  , deleteTraceState
  , filterTraceState
  , filterWithKeyTraceState
  , foldMapWithKeyTraceState
  , toListTraceState
  , TraceStateBuilder(..)
  , buildTraceState
  , buildTraceStatePure
  , TraceStateErrors(..)
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
  , SpanName(..)
  , MutableSpan(..)
  , Span(..)
  , SpanFrozenAt
  , freezeSpan
  , spanIsSampled
  , SpanParentSource(.., Implicit, Explicit)
  , SpanParent(.., Root, ChildOf)
  , spanParentContext
  , SpanKind(.., Server, Client, Producer, Consumer, Internal)
  , SpanStatus(.., Unset, OK, Error)
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (KeyValue((.=)), ToJSON(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bits (Bits((.|.), testBit), Ior(..))
import Data.ByteString.Builder (Builder)
import Data.DList (DList)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Monoid (Ap(..))
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (float2Double)
import OTel.API.Context (ContextBackend, ContextKey)
import Prelude hiding (span)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Traversable as Traversable
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed

class KV (kv :: Type) where
  type KVConstraints kv :: Type -> Type -> Constraint
  (.@) :: KVConstraints kv from to => Key to -> from -> kv

infixr 8 .@

instance KV (AttrsBuilder af) where
  type KVConstraints (AttrsBuilder af) = ToAttrVal
  (.@) = go
    where
    go :: forall to from. (ToAttrVal from to) => Key to -> from -> AttrsBuilder af
    go k v =
      AttrsBuilder \textLengthLimit ->
        pure $
          AttrsBuilderElem
            { attrsBuilderElemKey = unKey k
            , attrsBuilderElemVal =
                SomeAttr Attr
                  { attrType
                  , attrVal =
                      case attrType of
                        AttrTypeText -> Text.take textLengthLimit val
                        AttrTypeTextArray -> fmap (Text.take textLengthLimit) val
                        _ -> val
                  }
            }
      where
      attrType = attrTypeVal $ Proxy @to
      val = toAttrVal @from @to v

class (k ~ Text, v ~ Text) => IsTextKV k v
instance IsTextKV Text Text

newtype Key a = Key
  { unKey :: Text
  } deriving stock (Eq, Ord, Show)

instance IsString (Key a) where
  fromString = Key . Text.pack

newtype Timestamp = Timestamp
  { unTimestamp :: Integer -- ^ nanoseconds
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (Integer)

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

instance ToJSON InstrumentationScope where
  toJSON instrumentationScope =
    Aeson.object
      [ "name" .= instrumentationScopeName
      , "version" .= instrumentationScopeVersion
      , "schemaURL" .= instrumentationScopeSchemaURL
      ]
    where
    InstrumentationScope
      { instrumentationScopeName
      , instrumentationScopeVersion
      , instrumentationScopeSchemaURL
      } = instrumentationScope

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
    deriving (ToJSON) via (Text)

instance IsString InstrumentationScopeName where
  fromString = InstrumentationScopeName . Text.pack

newtype Version = Version
  { unVersion :: Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (Text)

instance IsString Version where
  fromString = Version . Text.pack

newtype SchemaURL = SchemaURL
  { unSchemaURL :: Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (Text)

schemaURLFromText :: Text -> Either Text SchemaURL
schemaURLFromText = Right . SchemaURL

schemaURLToText :: SchemaURL -> Text
schemaURLToText = unSchemaURL

data Attrs (af :: AttrsFor) = Attrs
  { attrsMap :: HashMap Text SomeAttr
  , attrsDropped :: Int
  } deriving stock (Eq, Show)

instance ToJSON (Attrs af) where
  toJSON attrs =
    Aeson.object
      [ "attributes" .= toJSON attrsMap
      , "attributesDropped" .= toJSON attrsDropped
      ]
    where
    Attrs { attrsMap, attrsDropped } = attrs

nullAttrs :: Attrs af -> Bool
nullAttrs = HashMap.null . attrsMap

sizeAttrs :: Attrs af -> Int
sizeAttrs = HashMap.size . attrsMap

memberAttrs :: Key a -> Attrs af -> Bool
memberAttrs key = HashMap.member (unKey key) . attrsMap

lookupAttrs
  :: forall a af
   . (KnownAttrType a)
  => Key a
  -> Attrs af
  -> Maybe (Attr a)
lookupAttrs key attrs =
  case HashMap.lookup (unKey key) $ attrsMap attrs of
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
  flip HashMap.foldMapWithKey (attrsMap attrs) \keyText someAttr ->
    case someAttr of
      SomeAttr attr -> f (Key keyText) attr

newtype AttrsBuilder (af :: AttrsFor) = AttrsBuilder
  { unAttrsBuilder :: Int -> DList AttrsBuilderElem
  } deriving (Semigroup, Monoid) via (Int -> DList AttrsBuilderElem)

data AttrsBuilderElem = AttrsBuilderElem
  { attrsBuilderElemKey :: Text
  , attrsBuilderElemVal :: SomeAttr
  }

runAttrsBuilder :: AttrsBuilder af -> AttrsLimits af -> Attrs af
runAttrsBuilder attrsBuilder attrsLimits =
  Attrs
    { attrsMap = attrsAccMap finalAcc
    , attrsDropped = attrsAccDropped finalAcc
    }
  where
  finalAcc :: AttrsAcc
  finalAcc = Foldable.foldl' buildAcc initAcc attrsDList

  buildAcc :: AttrsAcc -> AttrsBuilderElem -> AttrsAcc
  buildAcc attrsAcc attrsBuilderElem
    | attrsBuilderElemKey `HashMap.member` attrsAccMap =
        attrsAcc
    | attrsAccMapSize >= countLimit =
        attrsAcc
          { attrsAccDropped = 1 + attrsAccDropped
          }
    | otherwise =
        attrsAcc
          { attrsAccMap =
              HashMap.insert attrsBuilderElemKey attrsBuilderElemVal attrsAccMap
          , attrsAccMapSize = 1 + attrsAccMapSize
          }
    where
    AttrsAcc
      { attrsAccMap
      , attrsAccMapSize
      , attrsAccDropped
      } = attrsAcc
    AttrsBuilderElem
      { attrsBuilderElemKey
      , attrsBuilderElemVal
      } = attrsBuilderElem

  initAcc :: AttrsAcc
  initAcc =
    AttrsAcc
      { attrsAccMap = mempty
      , attrsAccMapSize = 0
      , attrsAccDropped = 0
      }

  attrsDList :: DList AttrsBuilderElem
  attrsDList = unAttrsBuilder attrsBuilder textLengthLimit
    where
    textLengthLimit :: Int
    textLengthLimit = Maybe.fromMaybe (maxBound @Int) attrsLimitsValueLength

  countLimit :: Int
  countLimit = Maybe.fromMaybe (maxBound @Int) attrsLimitsCount

  AttrsLimits { attrsLimitsCount, attrsLimitsValueLength } = attrsLimits

-- N.B. Little ad-hoc type for use in 'runAttrsBuilder'.
data AttrsAcc = AttrsAcc
  { attrsAccMap :: HashMap Text SomeAttr
  , attrsAccMapSize :: Int
  , attrsAccDropped :: Int
  }

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

instance ToJSON SomeAttr where
  toJSON = \case
    SomeAttr Attr { attrType, attrVal } ->
      case attrType of
        AttrTypeText ->
          Aeson.object
            [ "tag" .= ("text" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeBool ->
          Aeson.object
            [ "tag" .= ("bool" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeDouble ->
          Aeson.object
            [ "tag" .= ("double" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeInt ->
          Aeson.object
            [ "tag" .= ("int" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeTextArray ->
          Aeson.object
            [ "tag" .= ("textArray" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeBoolArray ->
          Aeson.object
            [ "tag" .= ("boolArray" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeDoubleArray ->
          Aeson.object
            [ "tag" .= ("doubleArray" :: Text)
            , "content" .= toJSON attrVal
            ]
        AttrTypeIntArray ->
          Aeson.object
            [ "tag" .= ("intArray" :: Text)
            , "content" .= toJSON attrVal
            ]

data Attr a = Attr
  { attrType :: AttrType a
  , attrVal :: a
  } deriving stock (Eq, Show)

newtype AttrVals a = AttrVals
  { unAttrVals :: Vector a
  } deriving (Eq, Monoid, Semigroup, Show, ToJSON) via (Vector a)
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

data TracerProvider = TracerProvider
  { tracerProviderGetTracer :: InstrumentationScope -> IO Tracer
  , tracerProviderShutdown :: IO ()
  , tracerProviderForceFlush :: IO ()
  }

getTracer
  :: forall m
   . (MonadIO m)
  => TracerProvider
  -> InstrumentationScope
  -> m Tracer
getTracer tracerProvider = liftIO . tracerProviderGetTracer tracerProvider

data Tracer = Tracer
  { tracerInstrumentationScope :: InstrumentationScope
  , tracerNow :: IO Timestamp
  , tracerStartSpan :: SpanSpec -> IO MutableSpan
  , tracerProcessSpan :: Span Attrs -> IO ()
  , tracerSpanBackend :: SpanBackend
  , tracerSpanAttrsLimits :: SpanAttrsLimits
  , tracerSpanEventAttrsLimits :: SpanEventAttrsLimits
  , tracerSpanLinkAttrsLimits :: SpanLinkAttrsLimits
  }

newtype SpanBackend = SpanBackend
  { unSpanBackend :: ContextBackend (Span AttrsBuilder)
  }

data SpanContext = SpanContext
  { spanContextTraceId :: TraceId
  , spanContextSpanId :: SpanId
  , spanContextTraceFlags :: TraceFlags
  , spanContextTraceState :: TraceState
  , spanContextIsRemote :: Bool
  } deriving stock (Eq, Show)

instance ToJSON SpanContext where
  toJSON spanContext =
    Aeson.object
      [ "traceId" .= spanContextTraceId
      , "spanId" .= spanContextSpanId
      , "traceFlags" .= spanContextTraceFlags
      , "traceState" .= spanContextTraceState
      , "isRemote" .= spanContextIsRemote
      ]
    where
    SpanContext
      { spanContextTraceId
      , spanContextSpanId
      , spanContextTraceFlags
      , spanContextTraceState
      , spanContextIsRemote
      } = spanContext

emptySpanContext :: SpanContext
emptySpanContext =
  SpanContext
    { spanContextTraceId = emptyTraceId
    , spanContextSpanId = emptySpanId
    , spanContextTraceFlags = mempty
    , spanContextTraceState = emptyTraceState
    , spanContextIsRemote = False
    }

spanContextIsValid :: SpanContext -> Bool
spanContextIsValid spanContext =
  spanContextTraceId /= emptyTraceId && spanContextSpanId /= emptySpanId
  where
  SpanContext { spanContextTraceId, spanContextSpanId } = spanContext

spanContextIsSampled :: SpanContext -> Bool
spanContextIsSampled spanContext = isSampledFlagSet spanContextTraceFlags
  where
  SpanContext { spanContextTraceFlags } = spanContext

data TraceId = TraceId
  { traceIdHi :: Word64
  , traceIdLo :: Word64
  } deriving stock (Eq, Show)

instance ToJSON TraceId where
  toJSON = toJSON . traceIdToHexText

traceIdToHexText :: TraceId -> Text
traceIdToHexText traceId =
  Text.Encoding.decodeUtf8
    $ ByteString.toStrict
    $ Builder.toLazyByteString
    $ traceIdToHexBuilder traceId

traceIdToBytesVector :: TraceId -> Unboxed.Vector Word8
traceIdToBytesVector traceId =
  Unboxed.fromList
    $ ByteString.Lazy.unpack
    $ Builder.toLazyByteString
    $ traceIdToBytesBuilder traceId

traceIdToHexBuilder :: TraceId -> Builder
traceIdToHexBuilder traceId =
  Builder.word64HexFixed traceIdHi <> Builder.word64HexFixed traceIdLo
  where
  TraceId { traceIdHi, traceIdLo } = traceId

traceIdToBytesBuilder :: TraceId -> Builder
traceIdToBytesBuilder traceId =
  Builder.word64LE traceIdHi <> Builder.word64LE traceIdLo
  where
  TraceId { traceIdHi, traceIdLo } = traceId

emptyTraceId :: TraceId
emptyTraceId = TraceId { traceIdHi = 0, traceIdLo = 0 }

traceIdFromWords :: Word64 -> Word64 -> TraceId
traceIdFromWords = TraceId

newtype SpanId = SpanId
  { spanIdLo :: Word64
  } deriving stock (Eq, Show)

instance ToJSON SpanId where
  toJSON = toJSON . spanIdToHexText

spanIdToHexText :: SpanId -> Text
spanIdToHexText spanId =
  Text.Encoding.decodeUtf8
    $ ByteString.toStrict
    $ Builder.toLazyByteString
    $ spanIdToHexBuilder spanId

spanIdToBytesVector :: SpanId -> Unboxed.Vector Word8
spanIdToBytesVector spanId =
  Unboxed.fromList
    $ ByteString.Lazy.unpack
    $ Builder.toLazyByteString
    $ spanIdToBytesBuilder spanId

spanIdToHexBuilder :: SpanId -> Builder
spanIdToHexBuilder spanId =
  Builder.word64HexFixed spanIdLo
  where
  SpanId { spanIdLo } = spanId

spanIdToBytesBuilder :: SpanId -> Builder
spanIdToBytesBuilder spanId =
  Builder.word64LE spanIdLo
  where
  SpanId { spanIdLo } = spanId

emptySpanId :: SpanId
emptySpanId = SpanId { spanIdLo = 0 }

spanIdFromWords :: Word64 -> SpanId
spanIdFromWords = SpanId

newtype TraceFlags = TraceFlags
  { unTraceFlags :: Word8
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via (Ior Word8)

instance ToJSON TraceFlags where
  toJSON = toJSON . traceFlagsToHexText

traceFlagsToHexText :: TraceFlags -> Text
traceFlagsToHexText traceFlags =
  Text.Encoding.decodeUtf8
    $ ByteString.toStrict
    $ Builder.toLazyByteString
    $ traceFlagsToHexBuilder traceFlags

traceFlagsToHexBuilder :: TraceFlags -> Builder
traceFlagsToHexBuilder traceFlags =
  Builder.word8HexFixed $ unTraceFlags traceFlags

emptyTraceFlags :: TraceFlags
emptyTraceFlags = TraceFlags { unTraceFlags = 0 }

setSampledFlag :: TraceFlags -> TraceFlags
setSampledFlag traceFlags =
  traceFlags
    { unTraceFlags = 1 .|. unTraceFlags traceFlags
    }

isSampledFlagSet :: TraceFlags -> Bool
isSampledFlagSet traceFlags =
  unTraceFlags traceFlags `testBit` 0

newtype TraceState = TraceState
  { unTraceState :: HashMap Text Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (HashMap Text Text)

emptyTraceState :: TraceState
emptyTraceState = TraceState { unTraceState = mempty }

nullTraceState :: TraceState -> Bool
nullTraceState = HashMap.null . unTraceState

sizeTraceState :: TraceState -> Int
sizeTraceState = HashMap.size . unTraceState

memberTraceState :: Key Text -> TraceState -> Bool
memberTraceState key = HashMap.member (unKey key) . unTraceState

lookupTraceState :: Key Text -> TraceState -> Maybe Text
lookupTraceState key = HashMap.lookup (unKey key) . unTraceState

findWithDefaultTraceState :: Text -> Key Text -> TraceState -> Text
findWithDefaultTraceState defVal key =
  HashMap.findWithDefault defVal (unKey key) . unTraceState

deleteTraceState :: Key Text -> TraceState -> TraceState
deleteTraceState key = TraceState . HashMap.delete (unKey key) . unTraceState

filterTraceState :: (Text -> Bool) -> TraceState -> TraceState
filterTraceState f = TraceState . HashMap.filter f . unTraceState

filterWithKeyTraceState :: (Key Text -> Text -> Bool) -> TraceState -> TraceState
filterWithKeyTraceState f = TraceState . HashMap.filterWithKey f' . unTraceState
  where
  f' keyText val = f (Key keyText) val

foldMapWithKeyTraceState
  :: forall m
   . (Monoid m)
  => (Key Text -> Text -> m)
  -> TraceState
  -> m
foldMapWithKeyTraceState f traceState =
  flip HashMap.foldMapWithKey (unTraceState traceState) \keyText val ->
    f (Key keyText) val

toListTraceState :: TraceState -> [(Key Text, Text)]
toListTraceState traceState = foldMapWithKeyTraceState (\k v -> ((k, v) :)) traceState []

newtype TraceStateBuilder a = TraceStateBuilder
  { unTraceStateBuilder :: Either (DList TraceStateError) a
  } deriving
      ( Functor -- @base@
      ) via (Either (DList TraceStateError))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap TraceStateBuilder a)

instance Applicative TraceStateBuilder where
  pure = TraceStateBuilder . Right
  liftA2 f (TraceStateBuilder mx) (TraceStateBuilder my) =
    TraceStateBuilder $ case (mx, my) of
      (Left ex, Left ey) -> Left $ ex <> ey
      (Left ex, Right {}) -> Left ex
      (Right {}, Left ey) -> Left ey
      (Right x, Right y) -> Right $ f x y

instance KV (TraceStateBuilder TraceState) where
  type KVConstraints (TraceStateBuilder TraceState) = IsTextKV
  (.@) = go
    where
    go :: Key Text -> Text -> TraceStateBuilder TraceState
    go (Key keyText) valText = do
      traceStateKey <- fmap unKey parseKey
      traceStateVal <- parseValue
      pure $ TraceState $ HashMap.singleton traceStateKey traceStateVal
      where
      parseKey :: TraceStateBuilder (Key Text)
      parseKey =
        TraceStateBuilder do
          case Text.splitOn "@" keyText of
            [] -> error "TraceStateBuilder: parseKey - impossible!"
            [simpleKeyText] -> do
              if Text.null simpleKeyText then do
                Left $ DList.singleton $ TraceStateSimpleKeyIsEmpty TraceStateSimpleKeyIsEmptyError
                  { rawValue = valText
                  }
              else if Text.length simpleKeyText > 256 then do
                Left $ DList.singleton $ TraceStateSimpleKeyTooLong TraceStateSimpleKeyTooLongError
                  { rawKey = Key simpleKeyText
                  , rawValue = valText
                  }
              else if not (isFirstSimpleKeyCharValid $ Text.head simpleKeyText) then do
                Left $ DList.singleton $ TraceStateSimpleKeyContainsInvalidChars TraceStateSimpleKeyContainsInvalidCharsError
                  { rawKey = Key simpleKeyText
                  , rawValue = valText
                  , invalidChars = Text.singleton $ Text.head simpleKeyText
                  }
              else if not (Text.null invalidChars) then do
                Left $ DList.singleton $ TraceStateSimpleKeyContainsInvalidChars TraceStateSimpleKeyContainsInvalidCharsError
                  { rawKey = Key simpleKeyText
                  , rawValue = valText
                  , invalidChars
                  }
              else do
                Right $ Key keyText
              where
              invalidChars = Text.filter (not . isValidKeyChar) simpleKeyText
            [tenantIdText, systemIdText] -> do
              if Text.null tenantIdText then do
                Left $ DList.singleton $ TraceStateTenantIdIsEmpty TraceStateTenantIdIsEmptyError
                  { rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if Text.length tenantIdText > 241 then do
                Left $ DList.singleton $ TraceStateTenantIdTooLong TraceStateTenantIdTooLongError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if not (isFirstTenantIdCharValid $ Text.head tenantIdText) then do
                Left $ DList.singleton $ TraceStateTenantIdContainsInvalidChars TraceStateTenantIdContainsInvalidCharsError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = Text.singleton $ Text.head tenantIdText
                  }
              else if not (Text.null invalidTenantIdChars) then do
                Left $ DList.singleton $ TraceStateTenantIdContainsInvalidChars TraceStateTenantIdContainsInvalidCharsError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = invalidTenantIdChars
                  }
              else if Text.null systemIdText then do
                Left $ DList.singleton $ TraceStateSystemIdIsEmpty TraceStateSystemIdIsEmptyError
                  { rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if Text.length systemIdText > 14 then do
                Left $ DList.singleton $ TraceStateSystemIdTooLong TraceStateSystemIdTooLongError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if not (isFirstSystemIdCharValid $ Text.head systemIdText) then do
                Left $ DList.singleton $ TraceStateSystemIdContainsInvalidChars TraceStateSystemIdContainsInvalidCharsError
                  { rawTenantId = systemIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = Text.singleton $ Text.head systemIdText
                  }
              else if not (Text.null invalidSystemIdChars) then do
                Left $ DList.singleton $ TraceStateSystemIdContainsInvalidChars TraceStateSystemIdContainsInvalidCharsError
                  { rawTenantId = systemIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = invalidSystemIdChars
                  }
              else do
                Right $ Key keyText
              where
              invalidTenantIdChars = Text.filter (not . isValidKeyChar) tenantIdText
              invalidSystemIdChars = Text.filter (not . isValidKeyChar) systemIdText
            _texts -> do
              Left $ DList.singleton $ TraceStateKeyTypeUnknown TraceStateKeyTypeUnknownError
                { rawKey = Key keyText
                , rawValue = valText
                }

      parseValue :: TraceStateBuilder Text
      parseValue =
        TraceStateBuilder do
          if Text.null valText then do
            Left $ DList.singleton $ TraceStateValueIsEmpty TraceStateValueIsEmptyError
              { rawKey = Key keyText
              }
          else if Text.length valText > 256 then do
            Left $ DList.singleton $ TraceStateValueTooLong TraceStateValueTooLongError
              { rawKey = Key keyText
              , rawValue = valText
              }
          else if not (isLastValueCharValid $ Text.last valText) then do
            Left $ DList.singleton $ TraceStateValueContainsInvalidChars TraceStateValueContainsInvalidCharsError
              { rawKey = Key keyText
              , rawValue = valText
              , invalidChars = Text.singleton $ Text.last valText
              }
          else if not (Text.null invalidChars) then do
            Left $ DList.singleton $ TraceStateValueContainsInvalidChars TraceStateValueContainsInvalidCharsError
              { rawKey = Key keyText
              , rawValue = valText
              , invalidChars
              }
          else do
            pure valText
        where
        invalidChars = Text.filter (not . isValidValueChar) valText

      isValidKeyChar :: Char -> Bool
      isValidKeyChar c
        | c == '_' || c == '-' || c == '*' || c == '/' = True
        | otherwise = n >= 0x61 && n <= 0x7a
        where
        n = Char.ord c

      isFirstSimpleKeyCharValid :: Char -> Bool
      isFirstSimpleKeyCharValid c = n >= 0x61 && n <= 0x7a
        where
        n = Char.ord c

      isFirstTenantIdCharValid :: Char -> Bool
      isFirstTenantIdCharValid c = (n >= 0x61 && n <= 0x7a) || Char.isDigit c
        where
        n = Char.ord c

      isFirstSystemIdCharValid :: Char -> Bool
      isFirstSystemIdCharValid c = n >= 0x61 && n <= 0x7a
        where
        n = Char.ord c

      isValidValueChar :: Char -> Bool
      isValidValueChar c
        | n >= 0x20 && n <= 0x2b = True
        | n >= 0x2d && n <= 0x3c = True
        | n >= 0x3e && n <= 0x7e = True
        | otherwise = False
        where
        n = Char.ord c

      isLastValueCharValid :: Char -> Bool
      isLastValueCharValid c = isValidValueChar c && n /= 0x20
        where
        n = Char.ord c

buildTraceState
  :: forall m
   . (MonadThrow m)
  => TraceStateBuilder TraceState
  -> m TraceState
buildTraceState builder =
  case buildTraceStatePure builder of
    Left err -> throwM err
    Right x -> pure x

buildTraceStatePure :: TraceStateBuilder TraceState -> Either TraceStateErrors TraceState
buildTraceStatePure = first (TraceStateErrors . DList.toList) . unTraceStateBuilder

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

newtype SpanEvents (attrs :: AttrsFor -> Type) = SpanEvents
  { unSpanEvents :: DList (SpanEvent attrs)
  }

instance ToJSON (SpanEvents Attrs) where
  toJSON = toJSON . unSpanEvents

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

instance ToJSON (SpanEvent Attrs) where
  toJSON spanEvent =
    Aeson.object
      [ "name" .= spanEventName
      , "timestamp" .= spanEventTimestamp
      , "attrs" .= spanEventAttrs
      ]
    where
    SpanEvent
      { spanEventName
      , spanEventTimestamp
      , spanEventAttrs
      } = spanEvent

deriving stock instance (Eq (attrs 'AttrsForSpanEvent)) => Eq (SpanEvent attrs)
deriving stock instance (Show (attrs 'AttrsForSpanEvent)) => Show (SpanEvent attrs)

freezeSpanEventAttrs
  :: SpanEventAttrsLimits
  -> SpanEvent AttrsBuilder
  -> SpanEvent Attrs
freezeSpanEventAttrs attrsLimits spanEvent =
  spanEvent
    { spanEventAttrs = runAttrsBuilder (spanEventAttrs spanEvent) attrsLimits
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
    deriving (ToJSON) via (Text)

instance IsString SpanEventName where
  fromString = SpanEventName . Text.pack

newtype SpanLinks (attrs :: AttrsFor -> Type) = SpanLinks
  { unSpanLinks :: DList (SpanLink attrs)
  }

instance ToJSON (SpanLinks Attrs) where
  toJSON = toJSON . unSpanLinks

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

instance ToJSON (SpanLink Attrs) where
  toJSON spanLink =
    Aeson.object
      [ "spanContext" .= spanLinkSpanContext
      , "attrs" .= spanLinkAttrs
      ]
    where
    SpanLink
      { spanLinkSpanContext
      , spanLinkAttrs
      } = spanLink

deriving stock instance (Eq (attrs 'AttrsForSpanLink)) => Eq (SpanLink attrs)
deriving stock instance (Show (attrs 'AttrsForSpanLink)) => Show (SpanLink attrs)

freezeSpanLinkAttrs
  :: SpanLinkAttrsLimits
  -> SpanLink AttrsBuilder
  -> SpanLink Attrs
freezeSpanLinkAttrs attrsLimits spanLink =
  spanLink
    { spanLinkAttrs = runAttrsBuilder (spanLinkAttrs spanLink) attrsLimits
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
  -> m (Span AttrsBuilder -> Span AttrsBuilder)
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
                attrsBuilder <> spanAttrs span
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

newtype SpanName = SpanName
  { unSpanName :: Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (Text)

instance IsString SpanName where
  fromString = SpanName . Text.pack

newtype MutableSpan = MutableSpan
  { mutableSpanSpanKey :: ContextKey (Span AttrsBuilder)
  }

-- TODO: Add reference to Resource?
-- See https://opentelemetry.io/docs/reference/specification/trace/sdk/#additional-span-interfaces
data Span (attrs :: AttrsFor -> Type) = Span
  { spanParent :: SpanParent
  , spanContext :: SpanContext
  , spanName :: SpanName
  , spanStatus :: SpanStatus
  , spanStart :: Timestamp
  , spanFrozenAt :: SpanFrozenAt attrs
  , spanKind :: SpanKind
  , spanAttrs :: attrs 'AttrsForSpan
  , spanLinks :: SpanLinks attrs
  , spanEvents :: SpanEvents attrs
  , spanIsRecording :: Bool
  , spanInstrumentationScope :: InstrumentationScope
  }

instance ToJSON (Span Attrs) where
  toJSON span =
    Aeson.object
      [ "parent" .= spanParent
      , "spanContext" .= spanContext
      , "name" .= spanName
      , "status" .= spanStatus
      , "start" .= spanStart
      , "frozenAt" .= spanFrozenAt
      , "kind" .= spanKind
      , "attrs" .= spanAttrs
      , "links" .= spanLinks
      , "events" .= spanEvents
      , "isRecording" .= spanIsRecording
      , "instrumentationScope" .= spanInstrumentationScope
      ]
    where
    Span
      { spanParent
      , spanContext
      , spanName
      , spanStatus
      , spanStart
      , spanFrozenAt
      , spanKind
      , spanAttrs
      , spanLinks
      , spanEvents
      , spanIsRecording
      , spanInstrumentationScope
      } = span

type family SpanFrozenAt (attrs :: AttrsFor -> Type) :: Type where
  SpanFrozenAt AttrsBuilder = Maybe Timestamp
  SpanFrozenAt Attrs = Timestamp

freezeSpan
  :: Timestamp
  -> SpanLinkAttrsLimits
  -> SpanEventAttrsLimits
  -> SpanAttrsLimits
  -> Span AttrsBuilder
  -> Span Attrs
freezeSpan defaultSpanFrozenAt spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits span =
  span
    { spanFrozenAt = Maybe.fromMaybe defaultSpanFrozenAt $ spanFrozenAt span
    , spanAttrs =
        runAttrsBuilder (spanAttrs span) spanAttrsLimits
    , spanLinks =
        freezeAllSpanLinkAttrs spanLinkAttrsLimits $ spanLinks span
    , spanEvents =
        freezeAllSpanEventAttrs spanEventAttrsLimits $ spanEvents span
    }

spanIsSampled :: Span attrs -> Bool
spanIsSampled span = spanContextIsSampled spanContext
  where
  Span { spanContext } = span

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

-- TODO: Rename back to SpanLineage?
data SpanParent
  = SpanParentRoot
  | SpanParentChildOf SpanContext
  deriving stock (Eq, Show)

instance ToJSON SpanParent where
  toJSON = \case
    SpanParentRoot ->
      Aeson.object
        [ "tag" .= ("root" :: Text)
        ]
    SpanParentChildOf spanContext ->
      Aeson.object
        [ "tag" .= ("childOf" :: Text)
        , "content" .= toJSON spanContext
        ]

pattern Root :: SpanParent
pattern Root <- SpanParentRoot where
  Root = SpanParentRoot

pattern ChildOf :: SpanContext -> SpanParent
pattern ChildOf sc <- SpanParentChildOf sc where
  ChildOf sc = SpanParentChildOf sc

spanParentContext :: SpanParent -> Maybe SpanContext
spanParentContext = \case
  SpanParentRoot -> Nothing
  SpanParentChildOf sc -> Just sc

{-# COMPLETE Root, ChildOf :: SpanParent #-}

data SpanKind
  = SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  | SpanKindInternal
  deriving stock (Eq, Show)

instance ToJSON SpanKind where
  toJSON = \case
    SpanKindServer -> Aeson.object ["tag" .= ("server" :: Text)]
    SpanKindClient -> Aeson.object ["tag" .= ("client" :: Text)]
    SpanKindProducer -> Aeson.object ["tag" .= ("producer" :: Text)]
    SpanKindConsumer -> Aeson.object ["tag" .= ("consumer" :: Text)]
    SpanKindInternal -> Aeson.object ["tag" .= ("internal" :: Text)]

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

instance ToJSON SpanStatus where
  toJSON = \case
    SpanStatusUnset ->
      Aeson.object
        [ "tag" .= ("unset" :: Text)
        ]
    SpanStatusOk ->
      Aeson.object
        [ "tag" .= ("ok" :: Text)
        ]
    SpanStatusError errText ->
      Aeson.object
        [ "tag" .= ("error" :: Text)
        , "content" .= toJSON errText
        ]

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
