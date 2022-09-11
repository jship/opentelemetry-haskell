{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.OTel.API.Baggage.CoreSpec
  ( spec
  ) where

import Control.Exception (Exception(..))
import Data.Bifunctor (Bifunctor(..))
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Exts (IsList)
import OTel.API.Baggage.Core
  ( BaggageBuildError(..), (.@), BaggageBuilder, buildBaggage, buildBaggagePure, deleteBaggage
  , filterBaggage, filterWithKeyBaggage, findWithDefaultBaggage, foldMapWithKeyBaggage
  , lookupBaggage, memberBaggage, nullBaggage, sizeBaggage, toListBaggage
  )
import OTel.API.Baggage.Core.Internal (Baggage(..))
import OTel.API.Core (Key(..))
import Prelude
import Test.Hspec (HasCallStack, Spec, describe, it, shouldBe)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text

spec :: Spec
spec = do
  describe "buildBaggage" do
    it "empty" do
      runTest BuildBaggageTestCase
        { expectedValue = Right []
        , baggageBuilder = mempty
        }
    it "single pair" do
      runTest BuildBaggageTestCase
        { expectedValue = Right [("a", "1")]
        , baggageBuilder = "a" .@ "1"
        }
    it "multiple pairs" do
      runTest BuildBaggageTestCase
        { expectedValue = Right [("a", "1"), ("b", "2"), ("c", "3")]
        , baggageBuilder = "a" .@ "1" <> "b" .@ "2" <> "c" .@ "3"
        }
    it "left-biased" do
      runTest BuildBaggageTestCase
        { expectedValue = Right [("a", "1"), ("b", "2")]
        , baggageBuilder = "a" .@ "1" <> "b" .@ "2" <> "a" .@ "42"
        }
    it "specific error when parsing key" do
      runTest BuildBaggageTestCase
        { expectedValue = Left BaggageKeyIsEmpty
        , baggageBuilder = "" .@ "1"
        }
      runTest BuildBaggageTestCase
        { expectedValue = Left $ BaggageKeyContainsInvalidChars (Key "(a)") "()"
        , baggageBuilder = "(a)" .@ "1"
        }
    it "specific error when parsing value" do
      runTest BuildBaggageTestCase
        { expectedValue = Left BaggageValueIsEmpty
        , baggageBuilder = "a" .@ ""
        }
      let nullByte = Text.singleton $ Char.chr 0
      runTest BuildBaggageTestCase
        { expectedValue = Left $ BaggageValueContainsInvalidChars nullByte nullByte
        , baggageBuilder = "a" .@ nullByte
        }

  describe "nullBaggage" do
    it "returns true when empty" do
      runTest NullTestCase
        { expectedValue = True
        , baggage = []
        }
    it "returns false when not empty" do
      runTest NullTestCase
        { expectedValue = False
        , baggage = [("a", "1")]
        }

  describe "sizeBaggage" do
    it "returns 0 when empty" do
      runTest SizeTestCase
        { expectedValue = 0
        , baggage = []
        }
    it "returns positive when not empty" do
      runTest SizeTestCase
        { expectedValue = 1
        , baggage = [("a", "1")]
        }
      runTest SizeTestCase
        { expectedValue = 3
        , baggage = [("a", "1"), ("b", "2"), ("c", "3")]
        }

  describe "memberBaggage" do
    it "returns false when empty" do
      runTest MemberTestCase
        { expectedValue = False
        , baggage = []
        , needle = "a"
        }
    it "returns false when key not present" do
      runTest MemberTestCase
        { expectedValue = False
        , baggage = [("b", "2")]
        , needle = "a"
        }
    it "returns true when key present" do
      runTest MemberTestCase
        { expectedValue = True
        , baggage = [("a", "1")]
        , needle = "a"
        }

  describe "lookupBaggage" do
    it "returns nothing when empty" do
      runTest LookupTestCase
        { expectedValue = Nothing
        , baggage = []
        , needle = "a"
        }
    it "returns nothing when key not present" do
      runTest LookupTestCase
        { expectedValue = Nothing
        , baggage = [("b", "2")]
        , needle = "a"
        }
    it "returns value when key present" do
      runTest LookupTestCase
        { expectedValue = Just "1"
        , baggage = [("a", "1")]
        , needle = "a"
        }

  describe "findWithDefaultBaggage" do
    it "returns default when empty" do
      runTest FindWithDefaultTestCase
        { expectedValue = "42"
        , baggage = []
        , needle = "a"
        , def = "42"
        }
    it "returns default when key not present" do
      runTest FindWithDefaultTestCase
        { expectedValue = "42"
        , baggage = [("b", "2")]
        , needle = "a"
        , def = "42"
        }
    it "returns value when key present" do
      runTest FindWithDefaultTestCase
        { expectedValue = "1"
        , baggage = [("a", "1")]
        , needle = "a"
        , def = "42"
        }

  describe "deleteBaggage" do
    it "returns empty when empty" do
      runTest DeleteTestCase
        { expectedValue = []
        , baggage = []
        , needle = "a"
        }
    it "returns input when key not present" do
      runTest DeleteTestCase
        { expectedValue = [("b", "2")]
        , baggage = [("b", "2")]
        , needle = "a"
        }
    it "returns input without key when key present" do
      runTest DeleteTestCase
        { expectedValue = [("b", "2")]
        , baggage = [("a", "1"), ("b", "2")]
        , needle = "a"
        }

  describe "filterBaggage" do
    it "returns empty when empty" do
      runTest FilterTestCase
        { expectedValue = []
        , baggage = []
        , predicate = \v -> v == "1"
        }
    it "returns empty when filter finds no matches" do
      runTest FilterTestCase
        { expectedValue = []
        , baggage = [("b", "2")]
        , predicate = \v -> v == "1"
        }
    it "returns filtered input" do
      runTest FilterTestCase
        { expectedValue = [("a", "1")]
        , baggage = [("a", "1"), ("b", "2")]
        , predicate = \v -> v == "1"
        }

  describe "filterWithKeyBaggage" do
    it "returns empty when empty" do
      runTest FilterWithKeyTestCase
        { expectedValue = []
        , baggage = []
        , predicate = \_k v -> v == "1"
        }
    it "returns empty when filter finds no matches" do
      runTest FilterWithKeyTestCase
        { expectedValue = []
        , baggage = [("b", "2")]
        , predicate = \_k v -> v == "1"
        }
    it "returns filtered input" do
      runTest FilterWithKeyTestCase
        { expectedValue = [("a", "1")]
        , baggage = [("a", "1"), ("b", "2")]
        , predicate = \_k v -> v == "1"
        }
      runTest FilterWithKeyTestCase
        { expectedValue = [("a", "1")]
        , baggage = [("a", "1"), ("b", "2")]
        , predicate = \k _v -> k == "a"
        }

  describe "foldMapWithKeyBaggage" do
    it "returns empty when empty" do
      runTest FoldMapWithKeyTestCase
        { expectedValue = []
        , baggage = []
        , foldFunc = \k v -> HashMap.singleton (unKey k) v
        }
    it "returns mapped-and-folded input" do
      runTest FoldMapWithKeyTestCase
        { expectedValue = [("a", "1")]
        , baggage = [("a", "1")]
        , foldFunc = \k v -> HashMap.singleton (unKey k) v
        }
      runTest FoldMapWithKeyTestCase
        { expectedValue = [("a", "1"), ("b", "2")]
        , baggage = [("a", "1"), ("b", "2")]
        , foldFunc = \k v -> HashMap.singleton (unKey k) v
        }

  describe "toListBaggage" do
    it "returns empty list when empty" do
      runTest ToListTestCase
        { expectedValue = []
        , baggage = []
        }
    it "returns non-empty list when non-empty" do
      runTest ToListTestCase
        { expectedValue = [("a", "1")]
        , baggage = [("a", "1")]
        }
      runTest ToListTestCase
        { expectedValue = [("a", "1"), ("b", "2")]
        , baggage = [("a", "1"), ("b", "2")]
        }

data BuildBaggageTestCase = BuildBaggageTestCase
  { expectedValue :: Either BaggageBuildError Baggage
  , baggageBuilder :: BaggageBuilder Baggage
  }

instance IsTest BuildBaggageTestCase where
  runTest testCase = do
    buildBaggagePure baggageBuilder
      `shouldBe` expectedValue
    first (fromJust . fromException) (buildBaggage baggageBuilder)
      `shouldBe` expectedValue
    where
    BuildBaggageTestCase { expectedValue, baggageBuilder } = testCase

data NullTestCase = NullTestCase
  { expectedValue :: Bool
  , baggage :: Baggage
  }

instance IsTest NullTestCase where
  runTest testCase = do
    nullBaggage baggage `shouldBe` expectedValue
    where
    NullTestCase { expectedValue, baggage } = testCase

data SizeTestCase = SizeTestCase
  { expectedValue :: Int
  , baggage :: Baggage
  }

instance IsTest SizeTestCase where
  runTest testCase = do
    sizeBaggage baggage `shouldBe` expectedValue
    where
    SizeTestCase { expectedValue, baggage } = testCase

data MemberTestCase = MemberTestCase
  { expectedValue :: Bool
  , baggage :: Baggage
  , needle :: Key Text
  }

instance IsTest MemberTestCase where
  runTest testCase = do
    memberBaggage needle baggage `shouldBe` expectedValue
    where
    MemberTestCase { expectedValue, baggage, needle } = testCase

data LookupTestCase = LookupTestCase
  { expectedValue :: Maybe Text
  , baggage :: Baggage
  , needle :: Key Text
  }

instance IsTest LookupTestCase where
  runTest testCase = do
    lookupBaggage needle baggage `shouldBe` expectedValue
    where
    LookupTestCase { expectedValue, baggage, needle } = testCase

data FindWithDefaultTestCase = FindWithDefaultTestCase
  { expectedValue :: Text
  , baggage :: Baggage
  , needle :: Key Text
  , def :: Text
  }

instance IsTest FindWithDefaultTestCase where
  runTest testCase = do
    findWithDefaultBaggage def needle baggage `shouldBe` expectedValue
    where
    FindWithDefaultTestCase { expectedValue, baggage, needle, def } = testCase

data DeleteTestCase = DeleteTestCase
  { expectedValue :: Baggage
  , baggage :: Baggage
  , needle :: Key Text
  }

instance IsTest DeleteTestCase where
  runTest testCase = do
    deleteBaggage needle baggage `shouldBe` expectedValue
    where
    DeleteTestCase { expectedValue, baggage, needle } = testCase

data FilterTestCase = FilterTestCase
  { expectedValue :: Baggage
  , baggage :: Baggage
  , predicate :: Text -> Bool
  }

instance IsTest FilterTestCase where
  runTest testCase = do
    filterBaggage predicate baggage `shouldBe` expectedValue
    where
    FilterTestCase { expectedValue, baggage, predicate } = testCase

data FilterWithKeyTestCase = FilterWithKeyTestCase
  { expectedValue :: Baggage
  , baggage :: Baggage
  , predicate :: Key Text -> Text -> Bool
  }

instance IsTest FilterWithKeyTestCase where
  runTest testCase = do
    filterWithKeyBaggage predicate baggage `shouldBe` expectedValue
    where
    FilterWithKeyTestCase { expectedValue, baggage, predicate } = testCase

data FoldMapWithKeyTestCase m = FoldMapWithKeyTestCase
  { expectedValue :: m
  , baggage :: Baggage
  , foldFunc :: Key Text -> Text -> m
  }

instance (Eq m, Monoid m, Show m) => IsTest (FoldMapWithKeyTestCase m) where
  runTest testCase = do
    foldMapWithKeyBaggage foldFunc baggage `shouldBe` expectedValue
    where
    FoldMapWithKeyTestCase { expectedValue, baggage, foldFunc } = testCase

data ToListTestCase = ToListTestCase
  { expectedValue :: [(Key Text, Text)]
  , baggage :: Baggage
  }

instance IsTest ToListTestCase where
  runTest testCase = do
    List.sort (toListBaggage baggage) `shouldBe` List.sort expectedValue
    where
    ToListTestCase { expectedValue, baggage } = testCase

class IsTest a where
  runTest :: (HasCallStack) => a -> IO ()

deriving via (HashMap Text Text) instance IsList Baggage
