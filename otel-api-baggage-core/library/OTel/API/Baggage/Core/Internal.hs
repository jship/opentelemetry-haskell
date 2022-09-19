{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Baggage.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer
    MonadBaggage(..)
  , Baggage(..)
  , nullBaggage
  , sizeBaggage
  , memberBaggage
  , lookupBaggage
  , findWithDefaultBaggage
  , deleteBaggage
  , filterBaggage
  , filterWithKeyBaggage
  , foldMapWithKeyBaggage
  , toListBaggage
  , BaggageBuilder(..)
  , buildBaggage
  , buildBaggagePure
  , BaggageErrors(..)
  , BaggageError(..)
  , BaggageKeyIsEmptyError(..)
  , BaggageKeyContainsInvalidCharsError(..)
  , BaggageValueIsEmptyError(..)
  , BaggageValueContainsInvalidCharsError(..)
  , isRFC7230TokenChar
  , isRFC7230VCHARChar
  ) where

import Control.Applicative (Applicative(..))
import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl(liftWith, restoreT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Bifunctor (Bifunctor(..))
import Data.DList (DList)
import Data.HashMap.Strict (HashMap)
import Data.Monoid (Ap(..))
import Data.Text (Text)
import OTel.API.Core (KV(..), Key(..), IsTextKV)
import Prelude
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

class (Monad m) => MonadBaggage m where
  getBaggage :: m Baggage
  setBaggage :: Baggage -> m a -> m a

  default getBaggage
    :: (MonadTrans t, MonadBaggage n, m ~ t n)
    => m Baggage
  getBaggage = lift getBaggage

  default setBaggage
    :: (MonadTransControl t, MonadBaggage n, m ~ t n)
    => Baggage
    -> m a
    -> m a
  setBaggage baggage action =
    restoreT . pure
      =<< liftWith \run -> setBaggage baggage (run action)

instance (MonadBaggage m) => MonadBaggage (ExceptT e m)
instance (MonadBaggage m) => MonadBaggage (IdentityT m)
instance (MonadBaggage m) => MonadBaggage (MaybeT m)
instance (MonadBaggage m) => MonadBaggage (ReaderT r m)
instance (MonadBaggage m) => MonadBaggage (State.Lazy.StateT r m)
instance (MonadBaggage m) => MonadBaggage (State.Strict.StateT r m)
instance (MonadBaggage m, Monoid w) => MonadBaggage (RWS.Lazy.RWST r w s m)
instance (MonadBaggage m, Monoid w) => MonadBaggage (RWS.Strict.RWST r w s m)
instance (MonadBaggage m, Monoid w) => MonadBaggage (Writer.Lazy.WriterT w m)
instance (MonadBaggage m, Monoid w) => MonadBaggage (Writer.Strict.WriterT w m)
instance (MonadBaggage m) => MonadBaggage (LoggingT m)
instance (MonadBaggage m, MonadUnliftIO m) => MonadBaggage (ResourceT m) where
  setBaggage baggage action = do
    withRunInIO \runInIO -> do
      runInIO $ setBaggage baggage action

newtype Baggage = Baggage
  { unBaggage :: HashMap Text Text
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via (HashMap Text Text)

nullBaggage :: Baggage -> Bool
nullBaggage = HashMap.null . unBaggage

sizeBaggage :: Baggage -> Int
sizeBaggage = HashMap.size . unBaggage

memberBaggage :: Key Text -> Baggage -> Bool
memberBaggage key = HashMap.member (unKey key) . unBaggage

lookupBaggage :: Key Text -> Baggage -> Maybe Text
lookupBaggage key = HashMap.lookup (unKey key) . unBaggage

findWithDefaultBaggage :: Text -> Key Text -> Baggage -> Text
findWithDefaultBaggage defVal key =
  HashMap.findWithDefault defVal (unKey key) . unBaggage

deleteBaggage :: Key Text -> Baggage -> Baggage
deleteBaggage key = Baggage . HashMap.delete (unKey key) . unBaggage

filterBaggage :: (Text -> Bool) -> Baggage -> Baggage
filterBaggage f = Baggage . HashMap.filter f . unBaggage

filterWithKeyBaggage :: (Key Text -> Text -> Bool) -> Baggage -> Baggage
filterWithKeyBaggage f = Baggage . HashMap.filterWithKey f' . unBaggage
  where
  f' keyText val = f (Key keyText) val

foldMapWithKeyBaggage
  :: forall m
   . (Monoid m)
  => (Key Text -> Text -> m)
  -> Baggage
  -> m
foldMapWithKeyBaggage f baggage =
  flip HashMap.foldMapWithKey (unBaggage baggage) \keyText val ->
    f (Key keyText) val

toListBaggage :: Baggage -> [(Key Text, Text)]
toListBaggage baggage = foldMapWithKeyBaggage (\k v -> ((k, v) :)) baggage []

newtype BaggageBuilder a = BaggageBuilder
  { unBaggageBuilder :: Either (DList BaggageError) a
  } deriving
      ( Functor -- @base@
      ) via (Either (DList BaggageError))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap BaggageBuilder a)

instance Applicative BaggageBuilder where
  pure = BaggageBuilder . Right
  liftA2 f (BaggageBuilder mx) (BaggageBuilder my) =
    BaggageBuilder $ case (mx, my) of
      (Left ex, Left ey) -> Left $ ex <> ey
      (Left ex, Right {}) -> Left ex
      (Right {}, Left ey) -> Left ey
      (Right x, Right y) -> Right $ f x y

instance KV (BaggageBuilder Baggage) where
  type KVConstraints (BaggageBuilder Baggage) = IsTextKV
  (.@) = go
    where
    go :: Key Text -> Text -> BaggageBuilder Baggage
    go (Key keyText) valText = do
      baggageKey <- fmap unKey parseKey
      baggageVal <- parseValue
      pure $ Baggage $ HashMap.singleton baggageKey baggageVal
      where
      parseKey :: BaggageBuilder (Key Text)
      parseKey =
        BaggageBuilder do
          if Text.null keyText then do
            Left $ DList.singleton $ BaggageKeyIsEmpty BaggageKeyIsEmptyError
              { rawValue = valText
              }
          else if not (Text.null invalidChars) then do
            Left $ DList.singleton $ BaggageKeyContainsInvalidChars BaggageKeyContainsInvalidCharsError
              { rawKey = Key keyText
              , rawValue = valText
              , invalidChars
              }
          else do
            Right $ Key keyText
        where
        invalidChars = Text.filter (not . isRFC7230TokenChar) keyText

      parseValue :: BaggageBuilder Text
      parseValue =
        BaggageBuilder do
          if Text.null valText then do
            Left $ DList.singleton $ BaggageValueIsEmpty BaggageValueIsEmptyError
              { rawKey = Key keyText
              }
          else if not (Text.null invalidChars) then do
            Left $ DList.singleton $ BaggageValueContainsInvalidChars BaggageValueContainsInvalidCharsError
              { rawKey = Key keyText
              , rawValue = valText
              , invalidChars
              }
          else do
            pure valText
        where
        invalidChars = Text.filter (not . isRFC7230VCHARChar) valText

buildBaggage
  :: forall m
   . (MonadThrow m)
  => BaggageBuilder Baggage
  -> m Baggage
buildBaggage builder =
  case buildBaggagePure builder of
    Left err -> throwM err
    Right x -> pure x

buildBaggagePure :: BaggageBuilder Baggage -> Either BaggageErrors Baggage
buildBaggagePure = first (BaggageErrors . DList.toList) . unBaggageBuilder

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

isRFC7230TokenChar :: Char -> Bool
isRFC7230TokenChar = \case
  '!'  -> True
  '#'  -> True
  '$'  -> True
  '%'  -> True
  '&'  -> True
  '\'' -> True
  '*'  -> True
  '+'  -> True
  '-'  -> True
  '.'  -> True
  '^'  -> True
  '_'  -> True
  '`'  -> True
  '|'  -> True
  '~'  -> True
  c    -> Char.isAscii c && Char.isAlphaNum c

isRFC7230VCHARChar :: Char -> Bool
isRFC7230VCHARChar c = Char.isAscii c && Char.isPrint c

_isRFC7230DelimiterChar :: Char -> Bool
_isRFC7230DelimiterChar = \case
  '"'  -> True
  '('  -> True
  ')'  -> True
  ','  -> True
  '/'  -> True
  ':'  -> True
  ';'  -> True
  '<'  -> True
  '='  -> True
  '>'  -> True
  '?'  -> True
  '@'  -> True
  '['  -> True
  '\\' -> True
  ']'  -> True
  '{'  -> True
  '}'  -> True
  _    -> False

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
