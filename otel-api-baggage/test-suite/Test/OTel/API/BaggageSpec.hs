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
import OTel.API.Context (ContextBackend)
import OTel.API.Context.Internal (unsafeNewContextBackend)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (HasCallStack, Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "MonadBaggage/BaggageT" do
    it "empty" do
      runTest TestCase
        { initBaggage = mempty
        , initBaggageBuilder = mempty
        , nextBaggage = mempty
        , nextBaggageBuilder = \baggage -> pure baggage
        }
    it "empty to non-empty" do
      runTest TestCase
        { initBaggage = mempty
        , initBaggageBuilder = mempty
        , nextBaggage = [("a", "1")]
        , nextBaggageBuilder = \baggage -> "a" .@ "1" <> pure baggage
        }
    it "non-empty to non-empty" do
      runTest TestCase
        { initBaggage = [("a", "1")]
        , initBaggageBuilder = "a" .@ "1"
        , nextBaggage = [("a", "1"), ("b", "2")]
        , nextBaggageBuilder = \baggage -> "b" .@ "2" <> pure baggage
        }
    it "left-biased" do
      runTest TestCase
        { initBaggage = [("a", "1")]
        , initBaggageBuilder = "a" .@ "1"
        , nextBaggage = [("a", "42")]
        , nextBaggageBuilder = \baggage -> "a" .@ "42" <> pure baggage
        }
    it "full replace" do
      runTest TestCase
        { initBaggage = [("a", "1")]
        , initBaggageBuilder = "a" .@ "1"
        , nextBaggage = [("b", "2")]
        , nextBaggageBuilder = \_baggage -> "b" .@ "2"
        }

data TestCase = TestCase
  { initBaggage :: Baggage
  , initBaggageBuilder :: BaggageBuilder Baggage
  , nextBaggage :: Baggage
  , nextBaggageBuilder :: Baggage -> BaggageBuilder Baggage
  }

instance IsTest TestCase where
  runTest testCase = do
    flip runBaggageT testContextBackend do
      --getBaggage `shouldReturnLifted` mempty
      buildBaggage initBaggageBuilder `shouldReturnLifted` initBaggage
      setBaggage initBaggage
      getBaggage `shouldReturnLifted` initBaggage
      buildBaggage (nextBaggageBuilder initBaggage) `shouldReturnLifted` nextBaggage
      setBaggage nextBaggage
      getBaggage `shouldReturnLifted` nextBaggage
    where
    TestCase
      { initBaggage
      , initBaggageBuilder
      , nextBaggage
      , nextBaggageBuilder
      } = testCase

class IsTest a where
  runTest :: (HasCallStack) => a -> IO ()

deriving via (HashMap Text Text) instance IsList Baggage

testContextBackend :: ContextBackend Baggage
testContextBackend = unsafePerformIO $ liftIO $ unsafeNewContextBackend mempty
{-# NOINLINE testContextBackend #-}

shouldReturnLifted :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturnLifted action expected = action >>= \x -> liftIO $ x `shouldBe` expected
