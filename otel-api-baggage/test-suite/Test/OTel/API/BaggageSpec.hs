{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.OTel.API.BaggageSpec
  ( spec
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Exts (IsList)
import OTel.API.Baggage
import OTel.API.Baggage.Core.Internal (Baggage(..))
import Prelude
import Test.Hspec (HasCallStack, Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "MonadBaggage/BaggageT" do
    it "empty" do
      runTest TestCase
        { initBaggage = mempty
        , initBaggageBuilder = mempty
        , nestedBaggage = mempty
        , nestedBaggageBuilder = \baggage -> pure baggage
        }
    it "empty to non-empty" do
      runTest TestCase
        { initBaggage = mempty
        , initBaggageBuilder = mempty
        , nestedBaggage = [("a", "1")]
        , nestedBaggageBuilder = \baggage -> "a" .@ "1" <> pure baggage
        }
    it "non-empty to non-empty" do
      runTest TestCase
        { initBaggage = [("a", "1")]
        , initBaggageBuilder = "a" .@ "1"
        , nestedBaggage = [("a", "1"), ("b", "2")]
        , nestedBaggageBuilder = \baggage -> "b" .@ "2" <> pure baggage
        }
    it "left-biased" do
      runTest TestCase
        { initBaggage = [("a", "1")]
        , initBaggageBuilder = "a" .@ "1"
        , nestedBaggage = [("a", "42")]
        , nestedBaggageBuilder = \baggage -> "a" .@ "42" <> pure baggage
        }
    it "full replace" do
      runTest TestCase
        { initBaggage = [("a", "1")]
        , initBaggageBuilder = "a" .@ "1"
        , nestedBaggage = [("b", "2")]
        , nestedBaggageBuilder = \_baggage -> "b" .@ "2"
        }

data TestCase = TestCase
  { initBaggage :: Baggage
  , initBaggageBuilder :: BaggageBuilder Baggage
  , nestedBaggage :: Baggage
  , nestedBaggageBuilder :: Baggage -> BaggageBuilder Baggage
  }

instance IsTest TestCase where
  runTest testCase = do
    flip runBaggageT defaultBaggageBackend do
      getBaggage `shouldReturnLifted` mempty
      buildBaggage initBaggageBuilder `shouldReturnLifted` initBaggage
      setBaggage initBaggage do
        getBaggage `shouldReturnLifted` initBaggage
        buildBaggage (nestedBaggageBuilder initBaggage)
          `shouldReturnLifted` nestedBaggage
        setBaggage nestedBaggage do
          getBaggage `shouldReturnLifted` nestedBaggage
        getBaggage `shouldReturnLifted` initBaggage
      getBaggage `shouldReturnLifted` mempty
    where
    TestCase
      { initBaggage
      , initBaggageBuilder
      , nestedBaggage
      , nestedBaggageBuilder
      } = testCase

class IsTest a where
  runTest :: (HasCallStack) => a -> IO ()

deriving via (HashMap Text Text) instance IsList Baggage

shouldReturnLifted :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturnLifted action expected = action >>= \x -> liftIO $ x `shouldBe` expected
