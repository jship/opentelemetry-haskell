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

import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.Aeson
import Control.Monad.Trans.State (State)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable (for)
import Data.Tree
import Data.Word (Word64)
import OTel.API.Common
import OTel.API.Trace
import OTel.API.Trace.Core.Internal
import OTel.SDK.Trace.Internal
import Prelude hiding (span)
import System.IO (stdout)
import Test.HUnit (assertFailure)
import Test.Hspec (HasCallStack, Spec, describe, it)
import qualified Control.Monad.Trans.State as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Test.Hspec as Hspec

--import Control.Exception (PatternMatchFail(..), evaluate) -- @base@
--import Control.Exception.Safe (catch) -- @safe-exceptions@

spec :: Spec
spec = do
  describe "Spec" do
    it "single span" do
      runTest TestCase
        { action = \tracerProvider -> do
            tracer <- getTracer tracerProvider "testTracer"
            withTracing tracer defaultSpanBackend do
              trace_ "1" do
                pure ()
        , expectedSpans =
            [ pure Span
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

    it "couple spans" do
      runTest TestCase
        { action = \tracerProvider -> do
            tracer <- getTracer tracerProvider "testTracer"
            withTracing tracer defaultSpanBackend do
              trace_ "1" do
                trace_ "1.1" do
                  pure ()
        , expectedSpans =
            [ Node
                { rootLabel =
                    Span
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
                      , spanFrozenAt = timestampFromNanoseconds 3
                      , spanKind = SpanKindInternal
                      , spanAttrs = emptyAttrs
                      , spanLinks = mempty
                      , spanEvents = mempty
                      , spanIsRecording = True
                      , spanInstrumentationScope = "testTracer"
                      }
                , subForest =
                    [ pure Span
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
                        , spanFrozenAt = timestampFromNanoseconds 2
                        , spanKind = SpanKindInternal
                        , spanAttrs = emptyAttrs
                        , spanLinks = mempty
                        , spanEvents = mempty
                        , spanIsRecording = True
                        , spanInstrumentationScope = "testTracer"
                        }
                    ]
                }
            ]
        }

data TestCase = TestCase
  { action :: TracerProvider -> IO ()
  , expectedSpans :: Forest (Span Attrs)
  }

instance IsTest TestCase where
  runTest testCase = do
    nanosRef <- newTVarIO 0
    traceIdRef <- newTVarIO 0
    spanIdRef <- newTVarIO 0
    spanQueue <- newTMQueueIO
    withTracerProviderIO (testTracerProviderSpec nanosRef traceIdRef spanIdRef spanQueue) action
    spans <- drainQueue spanQueue
    buildSpanForest spans `shouldReturn` expectedSpans
    where
    drainQueue spanQueue = go []
      where
      go spans = do
        atomically (readTMQueue spanQueue) >>= \case
          Nothing -> pure spans
          Just span -> go $ span : spans

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

buildSpanForest
  :: [Span Attrs]
  -> IO (Forest (Span Attrs))
buildSpanForest = forestFromLabels spanIsRoot spanIsChildOf

forestFromLabels
  :: forall a
   . (Show a)
  => (a -> Bool)
  -> (a -> a -> Bool)
  -> [a]
  -> IO (Forest a)
forestFromLabels isRoot isChildOf labels =
  case List.partition isRoot labels of
    ([], []) -> pure []
    ([], _children) -> assertFailure $ "No roots found amongst labels: " <> show labels
    (roots, children) ->
      case forestWithOrphans of
        (forest, (orphan : orphans)) ->
          assertFailure $
            "Found orphans: forest=" <> show forest
              <> ", orphans=" <> show (orphan :| orphans)
        (forest, []) ->
          pure $ NonEmpty.toList forest
      where
      forestWithOrphans :: (NonEmpty (Tree a), [a])
      forestWithOrphans = do
        flip State.runState children do
          for (NonEmpty.fromList roots) \rootLabel -> do
            unfoldTreeM parentChildrenPair rootLabel

      parentChildrenPair :: a -> State [a] (a, [a])
      parentChildrenPair parent = do
        (cs, rest) <- fmap partitionChildren State.get
        State.put rest
        pure (parent, cs)
        where
        partitionChildren :: [a] -> ([a], [a])
        partitionChildren =
          List.partition \possibleChild ->
            possibleChild `isChildOf` parent
