{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Test.OTel.SDK.Trace.ConcurrentWorkersSpec
  ( spec
  ) where

import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception.Safe (Exception, throwIO)
import Control.Monad
import Data.Foldable (traverse_)
import Data.List (sort)
import OTel.SDK.Trace.Internal
  ( ConcurrentWorkers(..), ConcurrentWorkersSpec(..), defaultConcurrentWorkersSpec
  , withConcurrentWorkers
  )
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.OTel.SDK.Common (IsTest(..))

spec :: Spec
spec = do
  describe "ConcurrentWorkers" do
    it "basic test" do
      runTest TestCase
        { processItem = mempty
        , itemsToEnqueue = [1]
        , expectedItems = [1]
        }

    it "handful of items" do
      runTest TestCase
        { processItem = mempty
        , itemsToEnqueue = [1..1000]
        , expectedItems = [1..1000]
        }

    it "exceptions" do
      runTest TestCase
        { processItem = \item ->
            when (item <= 5) do
              throwIO $ ItemException item
        , itemsToEnqueue = [1..10]
        , expectedItems = [6..10]
        }

data TestCase = TestCase
  { processItem :: Int -> IO ()
  , itemsToEnqueue :: [Int]
  , expectedItems :: [Int]
  }

instance IsTest TestCase where
  runTest testCase = do
    queue <- newTQueueIO
    withConcurrentWorkers (concurrentWorkersSpec queue) \workers -> do
      traverse_ (concurrentWorkersEnqueueItem workers) itemsToEnqueue
      concurrentWorkersStopWorkers workers
    items <- drainQueue queue
    sort items `shouldBe` sort expectedItems
    where
    concurrentWorkersSpec queue =
      defaultConcurrentWorkersSpec
        { concurrentWorkersSpecProcessItem = \item -> do
            processItem item
            atomically $ writeTQueue queue item
        }

    drainQueue queue = go []
      where
      go items
        | length items == length expectedItems = pure items
        | otherwise = do
            item <- atomically $ readTQueue queue
            go $ item : items

    TestCase
      { processItem
      , itemsToEnqueue
      , expectedItems
      } = testCase

newtype ItemException = ItemException
  { unItemException :: Int
  } deriving stock (Eq, Show)
    deriving anyclass (Exception)
