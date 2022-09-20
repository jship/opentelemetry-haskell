--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}
module Test.OTel.SDK.TraceSpec
  ( spec
  ) where

import Control.Monad.Logger.Aeson
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
import System.IO (stdout)
import Data.Word (Word64)
import Data.Foldable (for_)

--import Control.Exception (PatternMatchFail(..), evaluate) -- @base@
--import Control.Exception.Safe (catch) -- @safe-exceptions@
--import Test.HUnit (assertFailure) -- @HUnit@

spec :: Spec
spec = do
  describe "Spec" do
    it "single span" do
      runTest TestCase
        { action = \tracerProvider -> do
            tracer <- getTracer tracerProvider "testTracer"
            traced tracer defaultSpanBackend do
              trace_ "1" do
                pure ()
        , expectedSpans =
            [ Span
                { spanParent = SpanParentRoot
                , spanContext =
                    emptySpanContext
                      { spanContextTraceId = traceIdFromWords 0 0
                      , spanContextSpanId = spanIdFromWords 0
                      , spanContextTraceFlags = traceFlagsSampled
                      }
                , spanName = "1"
                , spanStatus = SpanStatusUnset
                , spanStart = timestampFromNanoseconds 0
                , spanFrozenAt = timestampFromNanoseconds 1
                , spanKind = SpanKindInternal
                , spanAttrs = emptyAttrs
                , spanLinks = mempty
                , spanEvents = mempty
                , spanIsRecording = True
                , spanInstrumentationScope = "testTracer"
                }
            ]
        }

    it "multiple spans" do
      runTest TestCase
        { action = \tracerProvider -> do
            tracer <- getTracer tracerProvider "testTracer"
            traced tracer defaultSpanBackend do
              trace_ "1" do
                trace_ "1.1" do
                  trace_ "1.1.1" do
                    pure ()
        , expectedSpans =
            [ Span
                { spanParent = SpanParentRoot
                , spanContext =
                    emptySpanContext
                      { spanContextTraceId = traceIdFromWords 0 0
                      , spanContextSpanId = spanIdFromWords 0
                      , spanContextTraceFlags = traceFlagsSampled
                      }
                , spanName = "1"
                , spanStatus = SpanStatusUnset
                , spanStart = timestampFromNanoseconds 0
                , spanFrozenAt = timestampFromNanoseconds 5
                , spanKind = SpanKindInternal
                , spanAttrs = emptyAttrs
                , spanLinks = mempty
                , spanEvents = mempty
                , spanIsRecording = True
                , spanInstrumentationScope = "testTracer"
                }
            , Span
                { spanParent =
                    SpanParentChildOf emptySpanContext
                      { spanContextTraceId = traceIdFromWords 0 0
                      , spanContextSpanId = spanIdFromWords 0
                      , spanContextTraceFlags = traceFlagsSampled
                      }
                , spanContext =
                    emptySpanContext
                      { spanContextTraceId = traceIdFromWords 0 0
                      , spanContextSpanId = spanIdFromWords 1
                      , spanContextTraceFlags = traceFlagsSampled
                      }
                , spanName = "1.1"
                , spanStatus = SpanStatusUnset
                , spanStart = timestampFromNanoseconds 1
                , spanFrozenAt = timestampFromNanoseconds 4
                , spanKind = SpanKindInternal
                , spanAttrs = emptyAttrs
                , spanLinks = mempty
                , spanEvents = mempty
                , spanIsRecording = True
                , spanInstrumentationScope = "testTracer"
                }
            , Span
                { spanParent =
                    SpanParentChildOf emptySpanContext
                      { spanContextTraceId = traceIdFromWords 0 0
                      , spanContextSpanId = spanIdFromWords 1
                      , spanContextTraceFlags = traceFlagsSampled
                      }
                , spanContext =
                    emptySpanContext
                      { spanContextTraceId = traceIdFromWords 0 0
                      , spanContextSpanId = spanIdFromWords 2
                      , spanContextTraceFlags = traceFlagsSampled
                      }
                , spanName = "1.1.1"
                , spanStatus = SpanStatusUnset
                , spanStart = timestampFromNanoseconds 2
                , spanFrozenAt = timestampFromNanoseconds 3
                , spanKind = SpanKindInternal
                , spanAttrs = emptyAttrs
                , spanLinks = mempty
                , spanEvents = mempty
                , spanIsRecording = True
                , spanInstrumentationScope = "testTracer"
                }
            ]
        }

data TestCase = TestCase
  { action :: TracerProvider -> IO ()
  , expectedSpans :: [Span Attrs]
  }

instance IsTest TestCase where
  runTest testCase = do
    nanosRef <- newTVarIO 0
    traceIdRef <- newTVarIO 0
    spanIdRef <- newTVarIO 0
    spanQueue <- newTMQueueIO
    withTracerProviderIO (testTracerProviderSpec nanosRef traceIdRef spanIdRef spanQueue) action
    for_ (reverse expectedSpans) \expectedSpan -> do
      atomically (readTMQueue spanQueue) `shouldReturn` Just expectedSpan
    where
    TestCase { action, expectedSpans } = testCase

testTracerProviderSpec
  :: TVar Integer
  -> TVar Word64
  -> TVar Word64
  -> TMQueue (Span Attrs)
  -> TracerProviderSpec
testTracerProviderSpec nanosRef traceIdRef spanIdRef spanQueue =
  defaultTracerProviderSpec
    { tracerProviderSpecNow = do
        atomically do
          x <- readTVar nanosRef
          modifyTVar' nanosRef succ
          pure $ timestampFromNanoseconds x
    , tracerProviderSpecLogger = defaultOutput stdout
    , tracerProviderSpecIdGenerator =
        IdGeneratorSpec
          { idGeneratorSpecGenTraceId =
              liftIO $ atomically do
                x <- readTVar traceIdRef
                modifyTVar' traceIdRef succ
                pure $ traceIdFromWords 0 x
          , idGeneratorSpecGenSpanId = do
              liftIO $ atomically do
                x <- readTVar spanIdRef
                modifyTVar' spanIdRef succ
                pure $ spanIdFromWords x
          }
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
    , tracerProviderSpecCallStackAttrs = mempty
    }

class IsTest a where
  runTest :: (HasCallStack) => a -> IO ()

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
