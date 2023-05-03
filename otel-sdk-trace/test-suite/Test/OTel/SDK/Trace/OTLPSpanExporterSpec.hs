{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.OTel.SDK.Trace.OTLPSpanExporterSpec
  ( spec
  ) where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Exception.Safe (finally)
import Control.Monad (guard, unless)
import Control.Monad.Logger.Aeson
  ( LogLevel(..), Message(..), ToLogStr(toLogStr), (.=), Loc, LogSource, LogStr, fromLogStr
  )
import Control.Monad.Logger.Aeson.Internal (SeriesElem(..))
import Data.Aeson ((.:), (.:?), Value, json', withObject)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Parser, parse)
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Foldable (Foldable(..), find, for_, traverse_)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (Header, mkStatus)
import Network.HTTP.Types.Header (HeaderName, hRetryAfter)
import Network.URI (parseURI)
import Network.Wai (Request(..), Application, responseLBS)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import OTel.API.Common (Attrs, emptyAttrs, timestampFromNanoseconds)
import OTel.API.Trace.Core
  ( Span(..), SpanFrozenTimestamp(SpanFrozenTimestampEnded), SpanKind(SpanKindServer)
  , SpanLineage(SpanLineageRoot), SpanStatus(SpanStatusUnset), emptySpanContext
  )
import OTel.API.Trace.Core.Internal (Span(..))
import OTel.SDK.Resource.Core (buildResource, defaultResourceBuilder)
import OTel.SDK.Trace
  ( OTLPSpanExporterSpec(..), SpanExportResult(..), SpanExporter(..), Batch
  , defaultOTLPSpanExporterSpec, otlpSpanExporter
  )
import OTel.SDK.Trace.Internal (buildSpanExporter)
import Prelude
import System.Timeout (timeout)
import Test.Hspec
  ( HasCallStack, Spec, aroundAll, describe, expectationFailure, it, parallel, shouldBe
  )
import Test.OTel.SDK.Common (IsTest(..))
import Text.Read (readMaybe)
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Parser as Aeson.Parser
import qualified Data.ByteString as ByteString
import qualified Network.HTTP.Client as HTTP.Client

spec :: Spec
spec = do
  aroundAll withDummyServer do
    parallel do
      describe "OTLPSpanExporter" do
        -- These tests are a bit subtle in a few ways. We want to test how the
        -- OTLP span exporter reacts to various HTTP status codes, but we don't
        -- want to actually hit an observability backend. To that end, a test
        -- server is spun up. It is both error-prone at a system-level and
        -- prohibitively expensive time-wise to spin the test server up for each
        -- test case, so we instead spin it up once around the entire test suite
        -- (see @aroundAll withDummyServer@ above). While not prohibitively
        -- expensive, it is annoyingly expensive to run each test case
        -- sequentially, as tests that retry can take a few seconds each to
        -- complete. To address that, the tests are run in parallel (see
        -- @parallel@ above).
        --
        -- We need a means for each test case to be able to specify the exact
        -- HTTP status codes the server should respond with when a span export
        -- is attempted (or retried), but the nuance described previosuly around
        -- using a global server for all tests and running the tests in parallel
        -- suggest that using some shared state where each test case could write
        -- its desired responses isn't going to fly. Instead, the test runner
        -- (see the @IsTest RetriesTestCase@ instance) writes the test case's
        -- desired responses to a local queue and uses a modified @http-client@
        -- @Manager@ that updates each request before it goes out the door, by
        -- injecting the desired response into the request itself's headers. The
        -- test server then checks each request it receives for these headers,
        -- and responds with exactly what it is told via those headers.
        --
        -- This gives us complete control over how the server should respond for
        -- each test case, and no individual test case wires are crossed
        -- throughout the whole test suite's execution.
        describe "Retry behavior" do
          it "429 Too Many Requests is retried (w/o Retry-After header)" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  replicate 10 (429, Nothing) <> [(200, Nothing)]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs = replicate 10 $ msgRetryingWithPolicyDelay "429"
              , updateMeta = filterMeta
              }

          -- N.B. Would be nice to also test Retry-After in the full HTTP Date
          -- format. The internal parser supports this (see
          -- @parseRetryAfterHeader@), but to test it, would need to DI
          -- @getCurrentTime@ which adds weight to @OTLPSpanExporterSpec@ that may
          -- not be worth it.
          it "429 Too Many Requests is retried (w/ Retry-After header)" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  [(429, Just 1), (429, Just 2), (200, Nothing)]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs =
                  [ msgRetryingWithOverriddenDelay "429" 1_000_000
                  , msgRetryingWithOverriddenDelay "429" 2_000_000
                  ]
              , updateMeta = filterMeta
              }

          it "502 Bad Gateway is retried" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  replicate 10 (502, ignoredRetryAfter) <> [(200, Nothing)]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs = replicate 10 $ msgRetryingWithPolicyDelay "502"
              , updateMeta = filterMeta
              }

          it "503 Service Unavailable is retried (w/o Retry-After header)" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  replicate 10 (503, Nothing) <> [(200, Nothing)]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs = replicate 10 $ msgRetryingWithPolicyDelay "503"
              , updateMeta = filterMeta
              }

          -- N.B. Would be nice to also test Retry-After in the full HTTP Date
          -- format. The internal header parser supports this (see
          -- @parseRetryAfterHeader@), but to test it, would need to DI
          -- @getCurrentTime@ which adds weight to @OTLPSpanExporterSpec@ that may
          -- not be worth it.
          it "503 Service Unavailable is retried (w/ Retry-After header)" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  [(503, Just 1), (503, Just 2), (200, Nothing)]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs =
                  [ msgRetryingWithOverriddenDelay "503" 1_000_000
                  , msgRetryingWithOverriddenDelay "503" 2_000_000
                  ]
              , updateMeta = filterMeta
              }

          it "504 Gateway Timeout is retried" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  replicate 10 (504, ignoredRetryAfter)
                    <> [(200, Nothing)]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs = replicate 10 $ msgRetryingWithPolicyDelay "504"
              , updateMeta = filterMeta
              }

          it "Mixed status codes are retried" \serverPort -> do
            runTest RetriesTestCase
              { serverPort
              , serverResponses =
                  [ (429, Just 1)
                  , (504, ignoredRetryAfter)
                  , (503, Just 2)
                  , (502, ignoredRetryAfter)
                  , (200, Nothing)
                  ]
              , expectedSpanExportResult = SpanExportResultSuccess
              , expectedLogs =
                  [ msgRetryingWithOverriddenDelay "429" 1_000_000
                  , msgRetryingWithPolicyDelay "504"
                  , msgRetryingWithOverriddenDelay "503" 2_000_000
                  , msgRetryingWithPolicyDelay "502"
                  ]
              , updateMeta = filterMeta
              }

          for_ [200..299] \code -> do
            it (show code <> " succeeds without retrying") \serverPort -> do
              runTest RetriesTestCase
                { serverPort
                , serverResponses = [(code, ignoredRetryAfter)]
                , expectedSpanExportResult = SpanExportResultSuccess
                , expectedLogs = []
                , updateMeta = filterMeta
                }

          for_ [400..599] \code ->
            unless (code `elem` [429, 502, 503, 504]) do
              it (show code <> " is not retried") \serverPort -> do
                runTest RetriesTestCase
                  { serverPort
                  , serverResponses = [(code, ignoredRetryAfter)]
                  , expectedSpanExportResult = SpanExportResultFailure
                  , expectedLogs = [msgConcurrentWorkerIgnoringEx]
                  , updateMeta = const mempty
                  }

data RetriesTestCase = RetriesTestCase
  { serverPort :: Int
  , serverResponses :: [(Int, Maybe Int)]
  , expectedSpanExportResult :: SpanExportResult
  , expectedLogs :: [(LogLevel, Message)]
  , updateMeta :: KeyMap Value -> KeyMap Value
  }

instance IsTest RetriesTestCase where
  runTest testCase = do
    respQueue <- newTQueueIO
    atomically $ traverse_ (writeTQueue respQueue) serverResponses
    manager <- HTTP.Client.newManager HTTP.Client.defaultManagerSettings
      { HTTP.Client.managerModifyRequest = \req -> do
          if clientReqAlreadyModified req then do
            pure req
          else do
            atomically (tryReadTQueue respQueue) >>= \case
              Nothing -> do
                expectationFailure "Too many requests made to server"
                error "IsTest RetriesTestCase: impossible to reach this point"
              Just (code, mRetryAfter) ->
                pure req
                  { HTTP.Client.requestHeaders =
                      foldr ($) (HTTP.Client.requestHeaders req)
                        [ (("TEST-RESPONSE-CODE", pack $ show code) :)
                        , fold do
                            val <- fmap (pack . show) mRetryAfter
                            pure (("TEST-RESPONSE-RETRY-AFTER", val) :)
                        ]
                  }
      }

    resource <- buildResource $ defaultResourceBuilder "test"

    logQueue <- newTQueueIO
    let logger = stmLogger logQueue

    otlpSpanExporter (otlpSpanExporterSpec manager) logger \spanExporterSpec -> do
      spanExporter <- buildSpanExporter resource logger spanExporterSpec
      go (spanExporterExport spanExporter) `finally` spanExporterShutdown spanExporter
      checkLogs updateMeta logQueue expectedLogs
    where
    go :: (Batch (Span Attrs) -> (SpanExportResult -> IO ()) -> IO ()) -> IO ()
    go export = do
      export (pure dummySpan) \spanExportResult -> do
        spanExportResult `shouldBe` expectedSpanExportResult

    dummySpan :: Span Attrs
    dummySpan =
      Span
        { spanLineage = SpanLineageRoot
        , spanContext = emptySpanContext
        , spanName = "Test Span"
        , spanStatus = SpanStatusUnset
        , spanStart = timestampFromNanoseconds 0
        , spanFrozenAt = SpanFrozenTimestampEnded $ timestampFromNanoseconds 0
        , spanKind = SpanKindServer
        , spanAttrs = emptyAttrs
        , spanLinks = mempty
        , spanEvents = mempty
        , spanIsRecording = True
        , spanInstrumentationScope = "foo"
        }

    otlpSpanExporterSpec :: Manager -> OTLPSpanExporterSpec
    otlpSpanExporterSpec manager =
      defaultOTLPSpanExporterSpec
        { otlpSpanExporterSpecEndpoint =
            fromJust $ parseURI $ "http://localhost:" <> show serverPort
        , otlpSpanExporterSpecManager = manager
        }

    clientReqAlreadyModified :: HTTP.Client.Request -> Bool
    clientReqAlreadyModified req = "TEST-RESPONSE-CODE" `elem` headerNames
      where
      headerNames = fst <$> HTTP.Client.requestHeaders req

    RetriesTestCase
      { serverPort
      , serverResponses
      , expectedSpanExportResult
      , expectedLogs
      , updateMeta
      } = testCase

stmLogger
  :: TQueue (LogLevel, LogStr)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
stmLogger logQueue _loc _logSource logLevel logStr = do
  atomically $ writeTQueue logQueue (logLevel, logStr)

checkLogs
  :: HasCallStack
  => (KeyMap Value -> KeyMap Value)
  -> TQueue (LogLevel, LogStr)
  -> [(LogLevel, Message)]
  -> IO ()
checkLogs updateMeta logQueue expectedLogs = do
  mResult <- timeout 3_000_000 $ atomically do
    actualLogs <- getActualLogsSTM
    guard $ actualLogs == expectedLogs'
  case mResult of
    Just () -> pure ()
    Nothing -> do
      actualLogs <- atomically getActualLogsSTM
      -- If a test is failing and the output is hard to decipher, it can be
      -- convenient to temporarily change the @expectationFailure@ to the
      -- following:
      --
      --actualLogs `shouldBe` expectedLogs'
      expectationFailure $
        "checkLogs timed out: "
          <> "expected=" <> show expectedLogs' <> ", "
          <> "actual=" <> show actualLogs
  where
  getActualLogsSTM :: STM [(LogLevel, Either LogStr (Text, KeyMap Value))]
  getActualLogsSTM = do
    logs <- fmap (second $ applyMetaUpdate . messagePiecesFromRawLog) <$> flushTQueue logQueue
    pure $ flip filter logs \(logLevel, logMsg) ->
      logLevel /= LevelDebug || case logMsg of
        (Right (text, _pairs)) ->
          case text of
            "Starting concurrent workers" -> False
            "Concurrent workers started" -> False
            "Stopping concurrent workers" -> False
            _ -> True
        _ -> True

  expectedLogs' :: [(LogLevel, Either LogStr (Text, KeyMap Value))]
  expectedLogs' = fmap (second $ applyMetaUpdate . messageToPieces) expectedLogs

  applyMetaUpdate
    :: Either LogStr (Text, KeyMap Value)
    -> Either LogStr (Text, KeyMap Value)
  applyMetaUpdate = \case
    Right (text, meta) -> Right (text, updateMeta meta)
    x -> x

msgRetryingWithPolicyDelay :: Text -> (LogLevel, Message)
msgRetryingWithPolicyDelay hint =
  ( LevelDebug
  , "Retrying span export with policy delay" :#
      [ "hint" .= hint
      , "exception" .= ("SOME-IGNORED-EXCEPTION" :: Text)
      ]
  )

msgRetryingWithOverriddenDelay :: Text -> Int -> (LogLevel, Message)
msgRetryingWithOverriddenDelay hint delayMicros =
  ( LevelDebug
  , "Retrying span export with overridden delay" :#
      [ "hint" .= hint
      , "delayMicros" .= delayMicros
      , "exception" .= ("SOME-IGNORED-EXCEPTION" :: Text)
      ]
  )

msgConcurrentWorkerIgnoringEx :: (LogLevel, Message)
msgConcurrentWorkerIgnoringEx =
  (LevelError, "Concurrent worker ignoring exception from exporting batch")

-- N.B. While we don't ever explicitly check for exceptions in the tests, if the
-- tests or library code are modified and cause a new failure, it is useful for
-- the test output to report the exception. That's why the exception key is
-- included here.
filterMeta :: KeyMap Value -> KeyMap Value
filterMeta = mapped . filtered
  where
  mapped =
    Aeson.KeyMap.foldMapWithKey \k v ->
      Aeson.KeyMap.singleton k $
        -- The tests don't check that the key map contains a specific exception,
        -- just that it contains an exception. Here we override the shown
        -- exception text with a dummy string we check for in
        -- @msgRetryingWithPolicyDelay@ and @msgRetryingWithOverriddenDelay@
        if k == "exception" then
          "SOME-IGNORED-EXCEPTION"
        else
          v

  filtered =
    Aeson.KeyMap.filterWithKey \k _v ->
      k `elem` ["hint", "exception", "delayMicros"]

ignoredRetryAfter :: Maybe Int
ignoredRetryAfter = Just 42

messageToPieces :: Message -> Either LogStr (Text, KeyMap Value)
messageToPieces message@(text :# pairs) =
  case runAesonParser (parsePairs . Just) pairsBytes of
    Nothing -> Left $ toLogStr message
    Just keyMap -> Right (text, keyMap)
  where
  pairsBytes :: ByteString
  pairsBytes =
    ByteString.toStrict
      $ Aeson.Encoding.encodingToLazyByteString
      $ Aeson.Encoding.pairs
      $ foldMap unSeriesElem pairs

messagePiecesFromRawLog :: LogStr -> Either LogStr (Text, KeyMap Value)
messagePiecesFromRawLog logStr =
  case runAesonParser parseMessage $ fromLogStr logStr of
    Nothing -> Left logStr
    Just (text, keyMap) -> Right (text, keyMap)

parseMessage :: Value -> Parser (Text, KeyMap Value)
parseMessage = withObject "Message" \obj ->
  (,) <$> obj .: "text" <*> (parsePairs =<< obj .:? "meta")

parsePairs :: Maybe Value -> Parser (KeyMap Value)
parsePairs = \case
  Nothing -> pure mempty
  Just value -> withObject "[Pair]" pure value

runAesonParser :: (Value -> Parser a) -> ByteString -> Maybe a
runAesonParser parser =
  Aeson.Parser.decodeStrictWith json' (parse parser)

withDummyServer :: forall a . (Port -> IO a) -> IO a
withDummyServer = testWithApplication $ pure dummyServer

dummyServer :: Application
dummyServer request sendResponse
  | Just code <- findCode = do
      sendResponse $ responseLBS (mkStatus code mempty) respHeaders mempty
  | otherwise = do
      expectationFailure "No test status code was found in request"
      error "dummyServer: impossible to reach this point"
  where
  findCode :: Maybe Int
  findCode = do
    val <- findHeaderVal "TEST-RESPONSE-CODE"
    readMaybe $ unpack val

  respHeaders :: [Header]
  respHeaders = fromMaybe [] do
    retryAfterVal <- findRetryAfter
    pure [(hRetryAfter, pack $ show retryAfterVal)]

  findRetryAfter :: Maybe Int
  findRetryAfter = do
    val <- findHeaderVal "TEST-RESPONSE-RETRY-AFTER"
    readMaybe $ unpack val

  findHeaderVal :: HeaderName -> Maybe ByteString
  findHeaderVal name = fmap snd $ find ((== name) . fst) headers

  headers :: [Header]
  headers = requestHeaders request
