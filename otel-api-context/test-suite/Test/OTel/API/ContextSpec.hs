{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.OTel.API.ContextSpec
  ( spec
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Text (Text)
import OTel.API.Context
import OTel.API.Context.Core.Internal (unsafeNewContextBackend)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (HasCallStack, Spec, describe, it)
import qualified Test.Hspec as Hspec

spec :: Spec
spec = do
  describe "ContextT" do
    it "it works" do
      contextKeyName intContextKey `shouldBe` "int"
      contextKeyName textContextKey `shouldBe` "text"
      flip (runContextT @Int @IO) intContextBackend do
        do
          getAttachedContextValue `shouldReturn` Nothing
          context <- getAttachedContext
          lookupContext intContextKey context `shouldBe` Nothing
          lookupContext textContextKey context `shouldBe` Nothing
        attachContextValue 1 do
          do
            getAttachedContextValue `shouldReturn` Just 1
            context <- getAttachedContext
            lookupContext intContextKey context `shouldBe` Just 1
            lookupContext textContextKey context `shouldBe` Nothing
          attachContextValue 2 do
            do
              getAttachedContextValue `shouldReturn` Just 2
              context <- getAttachedContext
              lookupContext intContextKey context `shouldBe` Just 2
              lookupContext textContextKey context `shouldBe` Nothing
          do
            getAttachedContextValue `shouldReturn` Just 1
            context <- getAttachedContext
            lookupContext intContextKey context `shouldBe` Just 1
            lookupContext textContextKey context `shouldBe` Nothing
          flip runContextT textContextBackend do
            attachContextValue "foo" do
              do
                lift getAttachedContextValue `shouldReturn` Just 1
                context <- lift getAttachedContext
                lookupContext intContextKey context `shouldBe` Just 1
                lookupContext textContextKey context `shouldBe` Just "foo"
              do
                getAttachedContextValue `shouldReturn` Just "foo"
                context <- getAttachedContext
                lookupContext intContextKey context `shouldBe` Just 1
                lookupContext textContextKey context `shouldBe` Just "foo"
        do
          getAttachedContextValue `shouldReturn` Nothing
          context <- getAttachedContext
          lookupContext intContextKey context `shouldBe` Nothing
          lookupContext textContextKey context `shouldBe` Nothing

intContextBackend :: ContextBackend Int
intContextBackend = unsafePerformIO $ liftIO $ unsafeNewContextBackend intContextKey
{-# NOINLINE intContextBackend #-}

intContextKey :: ContextKey Int
intContextKey = unsafePerformIO $ liftIO $ newContextKey "int"
{-# NOINLINE intContextKey #-}

textContextBackend :: ContextBackend Text
textContextBackend = unsafePerformIO $ liftIO $ unsafeNewContextBackend textContextKey
{-# NOINLINE textContextBackend #-}

textContextKey :: ContextKey Text
textContextKey = unsafePerformIO $ liftIO $ newContextKey "text"
{-# NOINLINE textContextKey #-}

shouldReturn :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn action expected = action >>= \x -> x `shouldBe` expected

shouldBe :: (HasCallStack, MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x expected = liftIO $ x `Hspec.shouldBe` expected
