--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.OTel.SDK.TraceSpec
  ( spec
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad.IO.Class (MonadIO(liftIO))
import OTel.API.Common
import OTel.API.Trace
import OTel.API.Trace.Core.Internal
import OTel.SDK.Trace.Internal
import Prelude hiding (span)
import Test.Hspec (Spec, describe, it)
import Test.Hspec (HasCallStack) -- @hspec@
import qualified Test.Hspec as Hspec -- @hspec@

--import Control.Exception (PatternMatchFail(..), evaluate) -- @base@
--import Control.Exception.Safe (catch) -- @safe-exceptions@
--import Test.HUnit (assertFailure) -- @HUnit@

spec :: Spec
spec = do
  describe "Spec" do
    it "it works" do
      spanQueue <- newTMQueueIO
      withTracerProvider (testTracerProviderSpec spanQueue) \tracerProvider -> do
        tracer <- getTracer tracerProvider "testing"
        traced tracer defaultSpanBackend do
          trace_ "abc" do
            pure ()
      atomically (readTMQueue spanQueue) `shouldReturn` Just Span
        { spanParent = SpanParentRoot
        , spanContext = emptySpanContext
        , spanName = "empty"
        , spanStatus = SpanStatusUnset
        , spanStart = timestampFromNanoseconds 0
        , spanFrozenAt = timestampFromNanoseconds 0
        , spanKind = SpanKindInternal
        , spanAttrs = emptyAttrs
        , spanLinks = mempty
        , spanEvents = mempty
        , spanIsRecording = False
        , spanInstrumentationScope = "testing"
        }

testTracerProviderSpec :: TMQueue (Span Attrs) -> TracerProviderSpec
testTracerProviderSpec spanQueue =
  defaultTracerProviderSpec
    { tracerProviderSpecNow = fmap timestampFromNanoseconds $ pure 0
    , tracerProviderSpecSpanProcessors =
        [ simpleSpanProcessor defaultSimpleSpanProcessorSpec
            { simpleSpanProcessorSpecExporter = stmSpanExporter spanQueue
            , simpleSpanProcessorSpecOnSpansExported = do
                askSpansExportedResult >>= \case
                  SpanExportResultSuccess -> pure ()
                  SpanExportResultFailure -> do
                    spans <- askSpansExported
                    liftIO $ Hspec.expectationFailure $ "Export of spans failed: " <> show spans
            }
        ]
    , tracerProviderSpecSampler = alwaysOnSampler
    }

shouldReturn :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn action expected = action >>= \x -> x `shouldBe` expected

shouldBe :: (HasCallStack, MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x expected = liftIO $ x `Hspec.shouldBe` expected

--shouldMatchJustM
--  :: forall m a
--   . (HasCallStack, MonadIO m)
--  => m (Maybe a)
--  -> m a
--shouldMatchJustM mx = shouldMatchPatternM mx \case Just y -> y
--
--shouldMatchNothingM
--  :: forall m a
--   . (HasCallStack, MonadIO m)
--  => m (Maybe a)
--  -> m ()
--shouldMatchNothingM mx = shouldMatchPatternM mx \case Nothing -> ()
--
--shouldMatchPatternM
--  :: forall m a b
--   . (HasCallStack, MonadIO m)
--  => m a
--  -> (a -> b)
--  -> m b
--shouldMatchPatternM mx matcher = do
--  x <- mx
--  liftIO do
--    evaluate (matcher x) `catch` \case
--      PatternMatchFail err -> do
--        assertFailure $ "Pattern did not match: " <> err
