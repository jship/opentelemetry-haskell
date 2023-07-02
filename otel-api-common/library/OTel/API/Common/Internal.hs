{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , TimestampSource(..)

  , InstrumentationScope(..)
  , defaultInstrumentationScope
  , InstrumentationScopeName(..)
  , Version(..)
  , SchemaURL(..)
  , schemaURLFromText
  , schemaURLToText

    -- * Attributes
  , WithAttrs(..)
  , Attrs(..)
  , emptyAttrs
  , nullAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs
  , filterWithKeyAttrs
  , mapWithKeyAttrs
  , convertWithKeyAttrs
  , droppedAttrsCount
  , AttrsBuilder(..)
  , runAttrsBuilder
  , jsonAttrs
  , AttrsAcc(..)
  , AttrsBuilderElem(..)
  , AttrsFor(..)
  , AttrsLimits(..)
  , defaultAttrsLimits
  , SomeAttr(..)
  , Attr(..)
  , asTextAttr
  , AttrVals(..)
  , AttrType(..)
  , KnownAttrType(..)
  , ToAttrVal(..)

  , with

  , OnException(..)
  , askException
  , askExceptionMetadata

  , OnTimeout(..)
  , askTimeoutMicros
  , askTimeoutMetadata

  , BufferedLoggerSpec(..)
  , defaultBufferedLoggerSpec
  , includeLogAggregateViaAeson
  , withBufferedLogger
  , withBufferedLoggerIO
  , BufferedLogs
  , insertBufferedLog
  , insertBufferedLogWithAgg
  , BufferedLog(..)
  , toBufferedLog
  , BufferedLogAgg(..)
  , Logger
  ) where

import Control.Exception.Safe
  ( SomeException(..), MonadCatch, MonadMask, MonadThrow, catchAny, displayException, finally
  )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( Loc(..), LogLevel(..), LoggingT(..), Message(..), ToLogStr(..), LogSource, LogStr, MonadLogger
  , MonadLoggerIO, SeriesElem, fromLogStr, logError
  )
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Aeson (KeyValue((.=)), ToJSON(..), Value(..), (.:), (.:?), object)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.DList (DList)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable(..))
import Data.IORef (IORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Monoid (Ap(..))
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word8)
import GHC.Float (float2Double)
import Prelude hiding (span)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Parser as Aeson.Parser
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified System.Timeout as Timeout
import qualified Data.Text.Encoding as Text.Encoding

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
    deriving (Hashable, ToJSON) via (Text)

schemaURLFromText :: Text -> Either Text SchemaURL
schemaURLFromText = Right . SchemaURL

schemaURLToText :: SchemaURL -> Text
schemaURLToText = unSchemaURL

class WithAttrs (a :: Type) where
  type WithAttrsAttrType a :: AttrsFor
  (.:@) :: a -> AttrsBuilder (WithAttrsAttrType a) -> a

infixr 6 .:@

data Attrs (af :: AttrsFor) = Attrs
  { attrsMap :: HashMap Text SomeAttr
  , attrsDropped :: Int
  } deriving stock (Eq, Show)

instance ToJSON (Attrs af) where
  toJSON attrs =
    Aeson.object
      [ "attributePairs" .= toJSON attrsMap
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

filterWithKeyAttrs
  :: forall af
   . (forall a. Key a -> Attr a -> Bool)
  -> Attrs af
  -> Attrs af
filterWithKeyAttrs f attrs =
  attrs
    { attrsMap =
        flip HashMap.filterWithKey (attrsMap attrs) \keyText someAttr ->
          case someAttr of
            SomeAttr attr -> f (Key keyText) attr
    }

mapWithKeyAttrs
  :: forall af
   . (forall a. Key a -> Attr a -> Attr a)
  -> Attrs af
  -> Attrs af
mapWithKeyAttrs f = convertWithKeyAttrs go
  where
  go :: Key a -> Attr a -> SomeAttr
  go k v = SomeAttr $ f k v

-- | Equivalent to 'mapWithKeyAttrs' but allows for changing both the type and
-- value of each attribute rather than just the value of the attribute.
convertWithKeyAttrs
  :: forall af
   . (forall a. Key a -> Attr a -> SomeAttr)
  -> Attrs af
  -> Attrs af
convertWithKeyAttrs f attrs =
  attrs
    { attrsMap =
        flip HashMap.mapWithKey (attrsMap attrs) \keyText someAttr ->
          case someAttr of
            SomeAttr attr -> f (Key keyText) attr
    }

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

jsonAttrs :: forall a af. (ToJSON a) => Text -> a -> AttrsBuilder af
jsonAttrs initKeyText = go initKeyText . toJSON
  where
  go :: Text -> Value -> AttrsBuilder af
  go keyText = \case
    Null -> Key keyText .@ ("(null)" :: Text)
    Bool x -> Key keyText .@ x
    String x -> Key keyText .@ x
    Number x
      | Just i <- Scientific.toBoundedInteger x ->
          Key keyText .@ (i :: Int64)
      | Right d <- Scientific.toBoundedRealFloat x ->
          Key keyText .@ (d :: Double)
      | otherwise ->
          Key keyText .@ show x
    Array xs
      | null xs -> Key keyText .@ ("(empty array)" :: Text)
      | otherwise ->
          Vector.ifoldl'
            (\acc i x -> acc <> go (keyText <> "." <> Text.pack (show @Int i)) x)
            mempty
            xs
    Object kvs
      | null kvs -> Key keyText .@ ("(empty object)" :: Text)
      | otherwise ->
          Aeson.KeyMap.foldMapWithKey
            (\k v -> go (keyText <> "." <> Aeson.Key.toText k) v)
            kvs

-- N.B. Little ad-hoc type for use in 'runAttrsBuilder'.
data AttrsAcc = AttrsAcc
  { attrsAccMap :: HashMap Text SomeAttr
  , attrsAccMapSize :: Int
  , attrsAccDropped :: Int
  }

data AttrsFor
  = AttrsForResource
  | AttrsForSpan
  | AttrsForSpanEvent
  | AttrsForSpanLink

data AttrsLimits (af :: AttrsFor) = AttrsLimits
  { attrsLimitsCount :: Maybe Int
  , attrsLimitsValueLength :: Maybe Int
  }

instance ToJSON (AttrsLimits af) where
  toJSON attrsLimits =
    object
      [ "count" .= toJSON attrsLimitsCount
      , "valueLength" .= toJSON attrsLimitsValueLength
      ]
    where
    AttrsLimits { attrsLimitsCount, attrsLimitsValueLength } = attrsLimits

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

-- | Convert an attribute to a text attribute.
--
-- This function is identity if the attribute already is a text attribute.
-- Otherwise, the attribute value is passed to 'show'.
asTextAttr :: Attr a -> Attr Text
asTextAttr attr =
  case attrType attr of
    AttrTypeText -> attr
    AttrTypeBool ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
    AttrTypeDouble ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
    AttrTypeInt ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
    AttrTypeTextArray ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
    AttrTypeBoolArray ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
    AttrTypeDoubleArray ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
    AttrTypeIntArray ->
      Attr { attrType = AttrTypeText, attrVal = packShow $ attrVal attr }
  where
  packShow :: (Show v) => v -> Text
  packShow = Text.pack . show


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

type OnException :: Type -> Type
newtype OnException a = OnException
  { runOnException :: SomeException -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT SomeException (ReaderT [SeriesElem] (LoggingT IO)))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT SomeException (ReaderT [SeriesElem] (LoggingT IO))) a)

askException :: OnException SomeException
askException = OnException \someEx _pairs -> pure someEx

askExceptionMetadata :: OnException [SeriesElem]
askExceptionMetadata = OnException \_someEx pairs -> pure pairs

type OnTimeout :: Type -> Type
newtype OnTimeout a = OnTimeout
  { runOnTimeout :: Int -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT Int (ReaderT [SeriesElem] (LoggingT IO)))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Int (ReaderT [SeriesElem] (LoggingT IO))) a)

askTimeoutMicros :: OnTimeout Int
askTimeoutMicros = OnTimeout \timeoutMicros _pairs -> pure timeoutMicros

askTimeoutMetadata :: OnTimeout [SeriesElem]
askTimeoutMetadata = OnTimeout \_timeoutMicros pairs -> pure pairs

data BufferedLoggerSpec = BufferedLoggerSpec
  { -- | Predicate that determines if a log message should be buffered. The
    -- default is no buffering, so all log messages are logged immediately.
    bufferedLoggerSpecShouldBuffer
      :: Loc -> LogSource -> LogLevel -> LogStr -> Bool
    -- | The logger to wrap. For all unbuffered log messages, these
    -- are passed to 'bufferedLoggerSpecLogger' immediately. For
    -- buffered log messages, aggregates will eventually be passed
    -- to 'bufferedLoggerSpecLogger' on the configured period (see
    -- 'bufferedLoggerSpecFlushPeriod').
  , bufferedLoggerSpecLogger :: Logger
    -- | Buffered logs are regularly flushed from internal storage on this
    -- period (in microseconds). The default is 5 minutes.
  , bufferedLoggerSpecFlushPeriod :: Int
    -- | Max amount of time allowed for flushing buffered logs (in
    -- microseconds). The default is 10 seconds.
  , bufferedLoggerSpecFlushTimeout :: Int
    -- | Handler that is run if flushing buffered logs takes longer than
    -- 'bufferedLoggerSpecFlushTimeout'. The default is to log the timeout using
    -- 'bufferedLoggerSpecOnFlushExceptionLogger'. This may be overridden if the
    -- timeout needs to be reported or dealt with via some means other than
    -- logging. Note that if the handler throws a synchronous exception, that
    -- exception will be caught and ignored.
    --
    -- The input argument is all remaining buffered logs that were unable to
    -- be flushed within the timeout.
  , bufferedLoggerSpecOnFlushTimeout
      :: BufferedLogs -> OnTimeout ()
    -- | Handler that is run if a synchronous exception is encountered during
    -- log flushing. The default is to log the exception using
    -- 'bufferedLoggerSpecOnFlushExceptionLogger'. This may be overridden if the
    -- exception needs to be reported or dealt with via some means other than
    -- logging. Note that if the handler rethrows the exception, the exception
    -- will be caught and ignored.
    --
    -- The input arguments are the specific buffered log and aggregate
    -- that encountered an exception during flushing.
  , bufferedLoggerSpecOnFlushException
      :: BufferedLog -> BufferedLogAgg -> OnException ()
    -- | The 'bufferedLoggerSpecOnFlushTimeout' and
    -- 'bufferedLoggerSpecOnFlushException' handlers are run using this logger.
    -- This logger is intentionally different from 'bufferedLoggerSpecLogger',
    -- as 'bufferedLoggerSpecLogger' may have ultimately been what triggered
    -- running either handler in the first place.
  , bufferedLoggerSpecOnFlushExceptionLogger :: Logger
    -- | The internal storage tracks a count of each buffered log message's
    -- occurrences (rather than tracking each occurrence) and each occurrence's
    -- metadata (convenience for @monad-logger-aeson@ users). This function
    -- enables including this aggregate info in the log message when it is
    -- eventually logged. The default is to not include the aggregate info.
    --
    -- @monad-logger-aeson@ users may find it convenient to set this function
    -- to @includeLogAggregateViaAeson Just@. This will include the aggregate in
    -- full. If there is a risk that a buffered log's aggregated metadata may
    -- exceed a logging system's max payload per message, 'Just' can be replaced
    -- with a function that more carefully summarizes the metadata. For example,
    -- to ignore the metadata altogether and instead only log the count of
    -- aggregated messages, the following could be used:
    --
    -- @
    -- 'includeLogAggregateViaAeson' \\bufferedLogAgg ->
    --   'Just' $ 'object' ["count" '.=' 'bufferedLogAggCount' bufferedLogAgg]
    -- @
  , bufferedLoggerSpecIncludeLogAggregate
      :: BufferedLog -> BufferedLogAgg -> [SeriesElem] -> LogStr
  }

defaultBufferedLoggerSpec :: BufferedLoggerSpec
defaultBufferedLoggerSpec =
  BufferedLoggerSpec
    { bufferedLoggerSpecShouldBuffer = \_loc _logSource _logLevel _logStr ->
        False
    , bufferedLoggerSpecLogger = mempty
    , bufferedLoggerSpecFlushPeriod = 300_000_000 -- 5 minutes
    , bufferedLoggerSpecFlushTimeout = 10_000_000 -- 10 seconds
    , bufferedLoggerSpecOnFlushTimeout = \unflushedLogs -> do
        timeoutMicros <- askTimeoutMicros
        pairs <- askTimeoutMetadata
        logError $ "Flushing buffered logs took too long" :#
          "timeoutMicros" .= timeoutMicros
            : "unflushedLogs" .=
                fmap
                  ( \(bufferedLog, bufferedLogAgg) ->
                      object
                        [ "bufferedLog" .= bufferedLog
                        , "bufferedLogAgg" .= bufferedLogAgg
                        ]
                  )
                  (HashMap.toList unflushedLogs)
            : pairs
    , bufferedLoggerSpecOnFlushException = \bufferedLog bufferedLogAgg -> do
        SomeException ex <- askException
        pairs <- askExceptionMetadata
        logError $ "Ignoring exception from flushing buffered log" :#
          "exception" .= displayException ex
            : "bufferedLog" .= bufferedLog
            : "bufferedLogAgg" .= bufferedLogAgg
            : pairs
    , bufferedLoggerSpecOnFlushExceptionLogger = mempty
    , bufferedLoggerSpecIncludeLogAggregate =
        includeLogAggregateViaAeson $ const $ Nothing @BufferedLogAgg
    }

includeLogAggregateViaAeson
  :: forall a
   . (ToJSON a)
  => (BufferedLogAgg -> Maybe a) -- ^ Summarizes the aggregated info
  -> BufferedLog
  -> BufferedLogAgg
  -> [SeriesElem]
  -> LogStr
includeLogAggregateViaAeson summarizeAgg bufferedLog bufferedLogAgg pairs =
  case logStrBytesOrMsgText of
    Left logStrBytes -> toLogStr logStrBytes
    Right msgText ->
      toLogStr $ msgText :#
        "bufferedLogAgg" .= summarizeAgg bufferedLogAgg : pairs
  where
  BufferedLog
    { bufferedLogLogStr = logStrBytesOrMsgText
    } = bufferedLog

withBufferedLogger
  :: forall m a
   . (MonadUnliftIO m)
  => BufferedLoggerSpec
  -> (Logger -> m a)
  -> m a
withBufferedLogger bufferedLoggerSpec action =
  withRunInIO \runInIO ->
    withBufferedLoggerIO bufferedLoggerSpec (runInIO . action)

withBufferedLoggerIO
  :: forall a
   . BufferedLoggerSpec
  -> (Logger -> IO a)
  -> IO a
withBufferedLoggerIO bufferedLoggerSpec action = do
  bufferedLogsRef <- IORef.newIORef mempty
  Async.withAsync (mkWorker bufferedLogsRef) $ const do
    action (logger' bufferedLogsRef) `finally` flush bufferedLogsRef
  where
  logger'
    :: IORef BufferedLogs
    -> Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> IO ()
  logger' bufferedLogsRef loc logSource logLevel logStr = do
    if shouldBuffer loc logSource logLevel logStr then do
      uncurry (buffer bufferedLogsRef) $ toBufferedLog loc logSource logLevel logStr
    else do
      logger loc logSource logLevel logStr

  mkWorker :: IORef BufferedLogs -> IO ()
  mkWorker bufferedLogsRef = do
    Monad.forever do
      Concurrent.threadDelay period
      flush bufferedLogsRef

  buffer :: IORef BufferedLogs -> BufferedLog -> KeyMap Value -> IO ()
  buffer bufferedLogsRef bufferedLog meta = do
    IORef.atomicModifyIORef' bufferedLogsRef \bufferedLogs ->
      (insertBufferedLog bufferedLog meta bufferedLogs, ())

  flush :: IORef BufferedLogs -> IO ()
  flush bufferedLogsRef = do
    flushedLogsRef <- IORef.newIORef mempty
    bufferedLogs <- IORef.atomicModifyIORef' bufferedLogsRef (mempty,)
    flip runLoggingT onFlushExLogger do
      mResult <- withRunInIO \runInIO -> do
        Timeout.timeout timeoutMicros $ runInIO do
          Foldable.for_ (HashMap.toList bufferedLogs) \(bufferedLog, bufferedLogAgg) -> do
            liftIO (flushElem flushedLogsRef bufferedLog bufferedLogAgg) `catchAny` \someEx -> do
              runOnException (onFlushEx bufferedLog bufferedLogAgg) someEx loggingMeta `catchAny` \_ ->
                -- If the custom handler throws a synchronous exception, we
                -- ignore it, as we don't want to kill the async worker.
                pure ()
      flushedLogElems <- liftIO $ IORef.readIORef flushedLogsRef
      let unflushedLogs = bufferedLogs `HashMap.difference` flushedLogElems
      case mResult of
        Just () -> pure ()
        Nothing ->
          runOnTimeout (onFlushTimeout unflushedLogs) timeoutMicros loggingMeta `catchAny` \_ ->
            -- If the custom handler throws a synchronous exception, we ignore
            -- it, as we don't want to kill the async worker.
            pure ()

  flushElem :: IORef BufferedLogs -> BufferedLog -> BufferedLogAgg -> IO ()
  flushElem flushedLogsRef bufferedLog bufferedLogAgg = do
    logger loc logSource logLevel $ includeLogAgg bufferedLog bufferedLogAgg loggingMeta
    IORef.atomicModifyIORef' flushedLogsRef \flushedLogs ->
      (insertBufferedLogWithAgg bufferedLog bufferedLogAgg flushedLogs, ())
    where
    BufferedLog
      { bufferedLogLoc = loc
      , bufferedLogLogSource = logSource
      , bufferedLogLogLevel = logLevel
      } = bufferedLog

  loggingMeta :: [SeriesElem]
  loggingMeta =
    [ "bufferedLogger" .= object
        [ "flushPeriod" .= period
        , "flushTimeout" .= timeoutMicros
        ]
    ]

  BufferedLoggerSpec
    { bufferedLoggerSpecShouldBuffer = shouldBuffer
    , bufferedLoggerSpecLogger = logger
    , bufferedLoggerSpecFlushPeriod = period
    , bufferedLoggerSpecFlushTimeout = timeoutMicros
    , bufferedLoggerSpecOnFlushTimeout = onFlushTimeout
    , bufferedLoggerSpecOnFlushException = onFlushEx
    , bufferedLoggerSpecOnFlushExceptionLogger = onFlushExLogger
    , bufferedLoggerSpecIncludeLogAggregate = includeLogAgg
    } = bufferedLoggerSpec

type BufferedLogs = HashMap BufferedLog BufferedLogAgg

insertBufferedLog
  :: BufferedLog
  -> KeyMap Value
  -> BufferedLogs
  -> BufferedLogs
insertBufferedLog bufferedLog meta =
  insertBufferedLogWithAgg bufferedLog
    BufferedLogAgg
      { bufferedLogAggCount = 1
      , bufferedLogAggMetas =
          if Aeson.KeyMap.null meta then
            mempty
          else
            DList.singleton meta
      }

insertBufferedLogWithAgg
  :: BufferedLog
  -> BufferedLogAgg
  -> BufferedLogs
  -> BufferedLogs
insertBufferedLogWithAgg = HashMap.insertWith (<>)

data BufferedLog = BufferedLog
  { bufferedLogLoc :: Loc
  , bufferedLogLogSource :: LogSource
  , bufferedLogLogLevel :: LogLevel
    -- | 'Right' when the log message was parsed as a 'Message', 'Left'
    -- otherwise.
  , bufferedLogLogStr :: Either ByteString Text
  } deriving stock (Eq)

instance Hashable BufferedLog where
  hashWithSalt salt bufferedLog =
    salt
      `hashWithSalt` loc_filename
      `hashWithSalt` loc_package
      `hashWithSalt` loc_module
      `hashWithSalt` loc_start
      `hashWithSalt` logSource
      `hashWithSalt` show logLevel
      `hashWithSalt` logStrBytesOrMsgText
    where
    BufferedLog
      { bufferedLogLoc =
          Loc { loc_filename, loc_package, loc_module, loc_start }
      , bufferedLogLogSource = logSource
      , bufferedLogLogLevel = logLevel
      , bufferedLogLogStr = logStrBytesOrMsgText
      } = bufferedLog

instance ToJSON BufferedLog where
  toJSON bufferedLog =
    object
      [ "loc" .=
          object
            [ "package" .= loc_package
            , "module" .= loc_module
            , "file" .= loc_filename
            , "line" .= fst loc_start
            , "char" .= snd loc_start
            ]
      , "source" .= logSource
      , "level" .=
          case logLevel of
            LevelDebug -> "debug"
            LevelInfo -> "info"
            LevelWarn -> "warn"
            LevelError -> "error"
            LevelOther otherLevel -> otherLevel
      , "text" .=
          case logStrBytesOrMsgText of
            Left logStrBytes ->
              case Text.Encoding.decodeUtf8' logStrBytes of
                Left _unicodeEx -> "Log message could not be decoded via UTF-8"
                Right text -> text
            Right msgText -> msgText
      ]
    where
    BufferedLog
      { bufferedLogLoc =
          Loc { loc_filename, loc_package, loc_module, loc_start }
      , bufferedLogLogSource = logSource
      , bufferedLogLogLevel = logLevel
      , bufferedLogLogStr = logStrBytesOrMsgText
      } = bufferedLog

toBufferedLog
  :: Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> (BufferedLog, KeyMap Value)
toBufferedLog loc logSource logLevel logStr =
  ( BufferedLog
      { bufferedLogLoc = loc
      , bufferedLogLogSource = logSource
      , bufferedLogLogLevel = logLevel
      , bufferedLogLogStr
      }
  , meta
  )
  where
  (bufferedLogLogStr, meta) =
    case runAesonParser parseMessage logStrBytes of
      Nothing -> (Left logStrBytes, mempty)
      Just (text, keyMap) -> (Right text, keyMap)

  logStrBytes = fromLogStr logStr

  parseMessage :: Value -> Parser (Text, KeyMap Value)
  parseMessage = Aeson.withObject "Message" \obj ->
    (,) <$> obj .: "text" <*> (parsePairs =<< obj .:? "meta")

  parsePairs :: Maybe Value -> Parser (KeyMap Value)
  parsePairs = \case
    Nothing -> pure mempty
    Just value -> flip (Aeson.withObject "[Pair]") value \obj -> do
      pure obj

  runAesonParser :: (Value -> Parser a) -> ByteString -> Maybe a
  runAesonParser parser =
    Aeson.Parser.decodeStrictWith Aeson.Parser.json' (Aeson.Types.parse parser)

data BufferedLogAgg = BufferedLogAgg
  { bufferedLogAggCount :: Int
  , bufferedLogAggMetas :: DList (KeyMap Value)
  } deriving stock (Eq)

instance Semigroup BufferedLogAgg where
  (<>) x y =
    BufferedLogAgg
      { bufferedLogAggCount = bufferedLogAggCount x + bufferedLogAggCount y
      , bufferedLogAggMetas = bufferedLogAggMetas x <> bufferedLogAggMetas y
      }

instance ToJSON BufferedLogAgg where
  toJSON bufferedLogAgg =
    object
      $ "count" .= count
      : ["metas" .= DList.toList metas | not $ null metas]
    where
    BufferedLogAgg
      { bufferedLogAggCount = count
      , bufferedLogAggMetas = metas
      } = bufferedLogAgg

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
