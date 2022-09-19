{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.OTel.API.Context.CoreSpec
  ( spec
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text)
import OTel.API.Context.Core
import OTel.API.Context.Core.Internal
  ( ContextBackend(contextBackendValueKey), unsafeNewContextBackend
  )
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (HasCallStack, Spec, describe, it)
import qualified Test.Hspec as Hspec

spec :: Spec
spec = do
  describe "Spec" do
    it "it works" do
      contextKeyName intContextKey `shouldBe` "Int"
      contextKeyName textContextKey `shouldBe` "Text"
      do
        getAttachedContextValueUsing @IO intContextBackend `shouldReturn` Nothing
        context <- getAttachedContextUsing intContextBackend
        lookupContext intContextKey context `shouldBe` Nothing
        lookupContext textContextKey context `shouldBe` Nothing
      attachContextValueUsing intContextBackend 1 do
        do
          getAttachedContextValueUsing intContextBackend `shouldReturn` Just 1
          context <- getAttachedContextUsing intContextBackend
          lookupContext intContextKey context `shouldBe` Just 1
          lookupContext textContextKey context `shouldBe` Nothing
        attachContextValueUsing intContextBackend 2 do
          do
            getAttachedContextValueUsing intContextBackend `shouldReturn` Just 2
            context <- getAttachedContextUsing intContextBackend
            lookupContext intContextKey context `shouldBe` Just 2
            lookupContext textContextKey context `shouldBe` Nothing
        do
          getAttachedContextValueUsing intContextBackend `shouldReturn` Just 1
          context <- getAttachedContextUsing intContextBackend
          lookupContext intContextKey context `shouldBe` Just 1
          lookupContext textContextKey context `shouldBe` Nothing
        attachContextValueUsing textContextBackend "foo" do
          do
            getAttachedContextValueUsing intContextBackend `shouldReturn` Just 1
            context <- getAttachedContextUsing intContextBackend
            lookupContext intContextKey context `shouldBe` Just 1
            lookupContext textContextKey context `shouldBe` Just "foo"
          do
            getAttachedContextValueUsing textContextBackend `shouldReturn` Just "foo"
            context <- getAttachedContextUsing textContextBackend
            lookupContext intContextKey context `shouldBe` Just 1
            lookupContext textContextKey context `shouldBe` Just "foo"
      do
        getAttachedContextValueUsing intContextBackend `shouldReturn` Nothing
        context <- getAttachedContextUsing intContextBackend
        lookupContext intContextKey context `shouldBe` Nothing
        lookupContext textContextKey context `shouldBe` Nothing

intContextBackend :: ContextBackend Int
intContextBackend = unsafePerformIO $ liftIO unsafeNewContextBackend
{-# NOINLINE intContextBackend #-}

intContextKey :: ContextKey Int
intContextKey = contextBackendValueKey intContextBackend

textContextBackend :: ContextBackend Text
textContextBackend = unsafePerformIO $ liftIO unsafeNewContextBackend
{-# NOINLINE textContextBackend #-}

textContextKey :: ContextKey Text
textContextKey = contextBackendValueKey textContextBackend

shouldReturn :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn action expected = action >>= \x -> x `shouldBe` expected

shouldBe :: (HasCallStack, MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x expected = liftIO $ x `Hspec.shouldBe` expected
