{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
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
module OTel.API.Common.Internal
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
  , emptyAttrs
  , nullAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs
  , droppedAttrsCount
  , AttrsBuilder(..)
  , runAttrsBuilder
  , AttrsAcc(..)
  , AttrsBuilderElem(..)
  , AttrsFor(..)
  , AttrsLimits(..)
  , defaultAttrsLimits
  , SomeAttr(..)
  , Attr(..)
  , AttrVals(..)
  , AttrType(..)
  , KnownAttrType(..)
  , ToAttrVal(..)

  , with
  ) where

import Data.Aeson (KeyValue((.=)), ToJSON(..))
import Data.DList (DList)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word8)
import GHC.Float (float2Double)
import Prelude hiding (span)
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector

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
  } deriving stock (Eq, Ord, Show)

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
  } deriving stock (Eq, Ord, Show)
    deriving (ToJSON) via (Text)

instance IsString InstrumentationScopeName where
  fromString = InstrumentationScopeName . Text.pack

newtype Version = Version
  { unVersion :: Text
  } deriving stock (Eq, Ord, Show)
    deriving (ToJSON) via (Text)

instance IsString Version where
  fromString = Version . Text.pack

newtype SchemaURL = SchemaURL
  { unSchemaURL :: Text
  } deriving stock (Eq, Ord, Show)
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

emptyAttrs :: Attrs af
emptyAttrs =
  Attrs
    { attrsMap = mempty
    , attrsDropped = 0
    }

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

droppedAttrsCount :: Attrs af -> Int
droppedAttrsCount = attrsDropped

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

with :: a -> (a -> b) -> b
with = (&)

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
