{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
  , baggageKeyFromText
  , baggageValueFromText
  , BaggageBuildError(..)
  , isRFC7230TokenChar
  , isRFC7230VCHARChar
  ) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl(liftWith, restoreT))
import Control.Monad.Trans.Except (Except, ExceptT, runExcept, throwE)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
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
  { unBaggageBuilder :: Except BaggageBuildError a
  } deriving
      ( Applicative, Functor, Monad -- @base@
      ) via (Except BaggageBuildError)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (Except BaggageBuildError) a)

instance KV (BaggageBuilder Baggage) where
  type KVConstraints (BaggageBuilder Baggage) = IsTextKV
  (.@) = go
    where
    go :: Key Text -> Text -> BaggageBuilder Baggage
    go k v = do
      baggageKey <- fmap unKey $ baggageKeyFromText $ unKey k
      baggageVal <- baggageValueFromText v
      pure $ Baggage $ HashMap.singleton baggageKey baggageVal

buildBaggage
  :: forall m
   . (MonadThrow m)
  => BaggageBuilder Baggage
  -> m Baggage
buildBaggage builder =
  case buildBaggagePure builder of
    Left err -> throwM err
    Right x -> pure x

buildBaggagePure :: BaggageBuilder Baggage -> Either BaggageBuildError Baggage
buildBaggagePure = runExcept . unBaggageBuilder

baggageKeyFromText :: Text -> BaggageBuilder (Key Text)
baggageKeyFromText keyText =
  BaggageBuilder do
    if Text.null keyText then do
      throwE BaggageKeyIsEmpty
    else if not (Text.null invalidChars) then do
      throwE $ BaggageKeyContainsInvalidChars (Key keyText) invalidChars
    else do
      pure $ Key keyText
  where
  invalidChars = Text.filter (not . isRFC7230TokenChar) keyText

baggageValueFromText :: Text -> BaggageBuilder Text
baggageValueFromText valText =
  BaggageBuilder do
    if Text.null valText then do
      throwE BaggageValueIsEmpty
    else if not (Text.null invalidChars) then do
      throwE $ BaggageValueContainsInvalidChars valText invalidChars
    else do
      pure valText
  where
  invalidChars = Text.filter (not . isRFC7230VCHARChar) valText

data BaggageBuildError
  = BaggageKeyIsEmpty
  | BaggageKeyContainsInvalidChars (Key Text) Text
  | BaggageValueIsEmpty
  | BaggageValueContainsInvalidChars Text Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

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
