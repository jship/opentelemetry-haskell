{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.OTel.API.ContextSpec
  ( spec
  ) where

import Control.Exception (PatternMatchFail(..), evaluate)
import Control.Exception.Safe (catch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import OTel.API.Context
import OTel.API.Context.Internal
  ( unsafeAttachContext, unsafeEqContextKeys, unsafeNewContextBackend, unsafeNewContextKey
  )
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (assertFailure)
import Test.Hspec (HasCallStack, Spec, describe, it)
import qualified Test.Hspec as Hspec

spec :: Spec
spec = do
  describe "ContextT" do
    it "it works" do
      flip runContextTIO testContextBackend do
        shouldMatchNothingM getAttachedContextKey

        key1 <- do
          attachContext "name-1" 1 \key1 -> do
            key1' <- shouldMatchJustM getAttachedContextKey
            unsafeEqContextKeys key1 key1' `shouldBe` True
            contextKeyName key1 `shouldBe` "name-1"
            getContext key1 `shouldReturn` 1

            updateContext key1 (* 10) `shouldReturn` 10
            contextKeyName key1 `shouldBe` "name-1"
            getContext key1 `shouldReturn` 10

            key2 <- do
              attachContext "name-2" 2 \key2 -> do
                unsafeEqContextKeys key1 key2 `shouldBe` False
                key2' <- shouldMatchJustM getAttachedContextKey
                unsafeEqContextKeys key2 key2' `shouldBe` True
                contextKeyName key1 `shouldBe` "name-1"
                getContext key1 `shouldReturn` 10

                contextKeyName key2 `shouldBe` "name-2"
                getContext key2 `shouldReturn` 2

                updateContext key2 (* 10) `shouldReturn` 20
                contextKeyName key2 `shouldBe` "name-2"
                getContext key2 `shouldReturn` 20

                contextKeyName key1 `shouldBe` "name-1"
                getContext key1 `shouldReturn` 10

                -- This chunk exercises the unsafe, internal API:
                --   * 'unsafeNewContextKey'
                --   * 'unsafeAttachContext'
                key3 <- liftIO $ unsafeNewContextKey "name-3" 3
                unsafeEqContextKeys key1 key3 `shouldBe` False
                unsafeEqContextKeys key2 key3 `shouldBe` False
                unsafeAttachContext key3 do
                  key3' <- shouldMatchJustM getAttachedContextKey
                  unsafeEqContextKeys key3 key3' `shouldBe` True
                  contextKeyName key3 `shouldBe` "name-3"
                  getContext key3 `shouldReturn` 3

                  updateContext key3 (* 10) `shouldReturn` 30
                  contextKeyName key3 `shouldBe` "name-3"
                  getContext key3 `shouldReturn` 30
                contextKeyName key3 `shouldBe` "name-3"
                getContext key3 `shouldReturn` 30

                pure key2


            contextKeyName key1 `shouldBe` "name-1"
            getContext key1 `shouldReturn` 10

            contextKeyName key2 `shouldBe` "name-2"
            getContext key2 `shouldReturn` 20

            key1'' <- shouldMatchJustM getAttachedContextKey
            unsafeEqContextKeys key1' key1'' `shouldBe` True

            pure key1

        shouldMatchNothingM getAttachedContextKey

        contextKeyName key1 `shouldBe` "name-1"
        getContext key1 `shouldReturn` 10

-- Helper to keep Hspec happy in regards to the underlying monad.
runContextTIO :: ContextT ctx IO a -> ContextBackend ctx -> IO a
runContextTIO = runContextT

testContextBackend :: ContextBackend Int
testContextBackend = unsafePerformIO $ liftIO unsafeNewContextBackend
{-# NOINLINE testContextBackend #-}

shouldReturn :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn action expected = action >>= \x -> x `shouldBe` expected

shouldBe :: (HasCallStack, MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x expected = liftIO $ x `Hspec.shouldBe` expected

shouldMatchJustM
  :: forall m a
   . (HasCallStack, MonadIO m)
  => m (Maybe a)
  -> m a
shouldMatchJustM mx = shouldMatchPatternM mx \case Just y -> y

shouldMatchNothingM
  :: forall m a
   . (HasCallStack, MonadIO m)
  => m (Maybe a)
  -> m ()
shouldMatchNothingM mx = shouldMatchPatternM mx \case Nothing -> ()

shouldMatchPatternM
  :: forall m a b
   . (HasCallStack, MonadIO m)
  => m a
  -> (a -> b)
  -> m b
shouldMatchPatternM mx matcher = do
  x <- mx
  liftIO do
    evaluate (matcher x) `catch` \case
      PatternMatchFail err -> do
        assertFailure $ "Pattern did not match: " <> err
