{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.Aeson
import Data.Text (Text)
import OTel.API.Common
import OTel.API.Trace
import OTel.SDK.Resource.Core
import OTel.SDK.Trace.Internal
import Prelude
import System.IO
import System.Random.MWC

main :: IO ()
main = do
  gen <- createSystemRandom
  tracerProviderSpec <- buildTracerProviderSpec
  withTracerProvider tracerProviderSpec \tracerProvider -> do
    tracingBackend <- getTracingBackend tracerProvider "otlp-tracing-example"
    flip runTracingT tracingBackend do
      trace "1" \mutableSpan -> do
        parentSpanContext <- getSpanContext mutableSpan
        randomDelay gen
        trace_ "1.1" do
          randomDelay gen
          trace_ "1.1.1" do
            randomDelay gen
          trace_ "1.1.2" do
            randomDelay gen
        let attrs =
              mconcat
                [ DB_SYSTEM .@ ("postgres" :: Text)
                , DB_OPERATION .@ ("INSERT" :: Text)
                , DB_STATEMENT .@ ("INSERT INTO foo VALUES blah blah" :: Text)
                ]
        let links =
              singletonSpanLinkSpecs defaultSpanLinkSpec
                { spanLinkSpecSpanContext = parentSpanContext
                , spanLinkSpecAttrs =
                    mconcat
                      [ CLOUDEVENTS_EVENT_ID .@ ("dummy-event-id" :: Text)
                      , CLOUDEVENTS_EVENT_SOURCE .@ ("dummy-event-source" :: Text)
                      ]
                }
        trace_ "1.2" { spanSpecAttrs = attrs, spanSpecLinks = links, spanSpecKind = SpanKindServer } do
          randomDelay gen
        trace_ "1.3" do
          randomDelay gen
          updateSpan mutableSpan defaultUpdateSpanSpec
            { updateSpanSpecName = Just "1 (modified)"
            , updateSpanSpecStatus = Just SpanStatusOk
            , updateSpanSpecAttrs =
                Just $ HTTP_METHOD .@ ("POSTEROONI" :: Text)
            , updateSpanSpecEvents =
                Just $ singletonSpanEventSpecs defaultSpanEventSpec
                  { spanEventSpecName = "span event THE ULTIMATE"
                  , spanEventSpecAttrs = AWS_DYNAMODB_COUNT .@ (42 :: Int)
                  }
            }
          trace_ "1.3.1" do
            randomDelay gen
            trace_ "1.3.1.1" do
              randomDelay gen
            trace_ "1.3.1.2" do
              randomDelay gen
              trace_ "1.3.1.2.1" do
                randomDelay gen
            trace_ "1.3.1.3" do
              randomDelay gen
          updateSpan mutableSpan defaultUpdateSpanSpec
            { updateSpanSpecAttrs =
                Just $ HTTP_METHOD .@ ("POSTEROONI 2" :: Text) <> HTTP_SCHEME .@ ("http" :: Text)
            }

buildTracerProviderSpec :: IO TracerProviderSpec
buildTracerProviderSpec = do
  resource <- buildResource $ defaultResourceBuilder "otlp-tracing-example"
  pure defaultTracerProviderSpec
      { tracerProviderSpecSpanProcessors =
          [ simpleSpanProcessor defaultSimpleSpanProcessorSpec
              { simpleSpanProcessorSpecExporter =
                  otlpSpanExporter defaultOTLPSpanExporterSpec
              }
          ]
      , tracerProviderSpecSampler = alwaysOnSampler
      , tracerProviderSpecLogger = defaultOutput stdout
      , tracerProviderSpecResource = resource
      }

randomDelay :: (MonadIO m) => GenIO -> m ()
randomDelay gen =
  liftIO do
    delay <- uniformR (10_000, 400_000) gen
    threadDelay delay
