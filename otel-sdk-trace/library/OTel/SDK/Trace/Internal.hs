{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module OTel.SDK.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    module OTel.SDK.Trace.Internal -- TODO: Explicit exports
  ) where

import Control.Applicative (Applicative(..))
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.Async (Async, waitCatch, withAsync)
import Control.Concurrent.STM (STM, atomically, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBMQueue
  ( TBMQueue, closeTBMQueue, isClosedTBMQueue, newTBMQueueIO, readTBMQueue, tryWriteTBMQueue
  )
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, writeTMQueue)
import Control.Exception (evaluate)
import Control.Exception.Safe
  ( Exception(..), SomeException(..), MonadCatch, MonadMask, MonadThrow, catchAny, finally, throwM
  )
import Control.Monad (join, when)
import Control.Monad.Cont (cont, runCont)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( LoggingT(..), Message((:#)), MonadLoggerIO(..), (.=), Loc, LogLevel, LogSource, LogStr
  , MonadLogger, SeriesElem, logDebug, logError
  )
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (ToJSON, object)
import Data.DList (DList)
import Data.Foldable (Foldable(foldMap), for_, traverse_)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (SrcLoc(..), CallStack)
import OTel.API.Common
  ( AttrsFor(..), KV(..), TimestampSource(..), Attrs, AttrsBuilder, AttrsLimits
  , InstrumentationScope, Timestamp, defaultAttrsLimits, timestampFromNanoseconds
  )
import OTel.API.Context.Core (Context, lookupContext)
import OTel.API.Trace
  ( NewSpanSpec(..), Span(spanContext, spanFrozenAt, spanIsRecording), SpanParent(..)
  , SpanStatus(..), MutableSpan, SpanId, SpanKind, SpanName, TraceId, TraceState, UpdateSpanSpec
  , contextKeySpan, emptySpanContext, emptyTraceState, shutdownTracerProvider, spanContextIsSampled
  , spanContextIsValid, spanIdFromWords, spanIsSampled, traceFlagsSampled, traceIdFromWords
  , pattern CODE_FILEPATH, pattern CODE_FUNCTION, pattern CODE_LINENO, pattern CODE_NAMESPACE
  )
import OTel.API.Trace.Core.Internal
  ( NewSpanSpec(..), Span(..), SpanContext(..), SpanLink(..), SpanLinkSpec(..), SpanLinkSpecs(..)
  , SpanLinks(..), Tracer(..), TracerProvider(..), buildSpanUpdater, freezeSpan
  , unsafeModifyMutableSpan, unsafeNewMutableSpan, unsafeReadMutableSpan
  )
import Prelude hiding (span)
import System.Clock (Clock(Realtime), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (Variate(..), GenIO, Seed, createSystemSeed, fromSeed, initialize, uniform)
import System.Timeout (timeout)
import qualified Control.Exception.Safe as Exception
import qualified Data.DList as DList
import qualified GHC.Stack as Stack

data TracerProviderSpec = TracerProviderSpec
  { tracerProviderSpecNow :: IO Timestamp
  , tracerProviderSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , tracerProviderSpecSeed :: Seed
  , tracerProviderSpecIdGenerator :: IdGeneratorSpec
  , tracerProviderSpecSpanProcessors :: [SpanProcessorSpec]
  , tracerProviderSpecSampler :: SamplerSpec
  , tracerProviderSpecSpanAttrsLimits :: AttrsLimits 'AttrsForSpan
  , tracerProviderSpecSpanEventAttrsLimits :: AttrsLimits 'AttrsForSpanEvent
  , tracerProviderSpecSpanLinkAttrsLimits :: AttrsLimits 'AttrsForSpanLink
  , tracerProviderSpecCallStackAttrs :: CallStack -> AttrsBuilder 'AttrsForSpan
  }

defaultTracerProviderSpec :: TracerProviderSpec
defaultTracerProviderSpec =
  TracerProviderSpec
    { tracerProviderSpecNow =
        fmap (timestampFromNanoseconds . toNanoSecs) $ getTime Realtime
    , tracerProviderSpecLogger = mempty
    , tracerProviderSpecSeed = defaultSystemSeed
    , tracerProviderSpecIdGenerator = defaultIdGeneratorSpec
    , tracerProviderSpecSpanProcessors = mempty
    , tracerProviderSpecSampler = defaultSamplerSpec
    , tracerProviderSpecSpanAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecCallStackAttrs = \cs ->
        case Stack.getCallStack cs of
          ((function, srcLoc) : _) ->
            CODE_FUNCTION .@ function
              <> CODE_NAMESPACE .@ srcLocModule srcLoc
              <> CODE_FILEPATH .@ srcLocFile srcLoc
              <> CODE_LINENO .@ srcLocStartLine srcLoc
          _ -> mempty
    }

withTracerProvider
  :: forall m a
   . (MonadUnliftIO m)
  => TracerProviderSpec
  -> (TracerProvider -> m a)
  -> m a
withTracerProvider spec action =
  withRunInIO \runInIO -> withTracerProviderIO spec (runInIO . action)

withTracerProviderIO
  :: forall a
   . TracerProviderSpec
  -> (TracerProvider -> IO a)
  -> IO a
withTracerProviderIO spec f = do
  Exception.bracket
    (newTracerProviderIO spec)
    shutdownTracerProvider
    f

newTracerProvider
  :: forall m
   . (MonadIO m)
  => TracerProviderSpec
  -> m TracerProvider
newTracerProvider = liftIO . newTracerProvider

newTracerProviderIO :: TracerProviderSpec -> IO TracerProvider
newTracerProviderIO tracerProviderSpec = do
  shutdownRef <- newTVarIO False
  prngRef <- newPRNGRef seed
  spanProcessor <- foldMap (buildSpanProcessor logger) spanProcessorSpecs
  sampler <- buildSampler logger samplerSpec
  pure TracerProvider
    { tracerProviderGetTracer =
        pure . getTracerWith prngRef sampler spanProcessor
    , tracerProviderShutdown = do
        unlessSTM (readTVar shutdownRef) do
          writeTVar shutdownRef True
          pure $ spanProcessorShutdown spanProcessor
    , tracerProviderForceFlush = do
        unlessSTM (readTVar shutdownRef) do
          pure $ spanProcessorForceFlush spanProcessor
    }
  where
  getTracerWith
    :: MVar PRNG
    -> Sampler
    -> SpanProcessor
    -> InstrumentationScope
    -> Tracer
  getTracerWith prngRef sampler spanProcessor scope =
    Tracer
      { tracerInstrumentationScope = scope
      , tracerNow = now
      , tracerStartSpan = startSpan prngRef sampler scope spanProcessor
      , tracerProcessSpan = endSpan spanProcessor
      , tracerSpanAttrsLimits = spanAttrsLimits
      , tracerSpanEventAttrsLimits = spanEventAttrsLimits
      , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
      }

  endSpan :: SpanProcessor -> Span Attrs -> IO ()
  endSpan spanProcessor endedSpan = do
    when (spanIsRecording endedSpan) do
      spanProcessorOnSpanEnd spanProcessor endedSpan

  startSpan
    :: MVar PRNG
    -> Sampler
    -> InstrumentationScope
    -> SpanProcessor
    -> CallStack
    -> Context
    -> NewSpanSpec
    -> IO MutableSpan
  startSpan prngRef sampler scope spanProcessor cs implicitParentContext newSpanSpec = do
    span <- buildSpan
    mutableSpan <- unsafeNewMutableSpan span

    when (spanIsRecording span) do
      spanProcessorOnSpanStart spanProcessor parentContext \updateSpanSpec -> do
        spanUpdater mutableSpan updateSpanSpec

    pure mutableSpan
    where
    buildSpan :: IO (Span AttrsBuilder)
    buildSpan = do
      spanStart <- do
        case newSpanSpecStart of
          TimestampSourceAt timestamp -> pure timestamp
          TimestampSourceNow -> now

      spanParent <- spanParentFromParentContext parentContext
      initSpanContext <- newSpanContext spanParent

      samplingResult <- samplerShouldSample sampler SamplerInput
        { samplerInputContext = parentContext
        , samplerInputTraceId = spanContextTraceId initSpanContext
        , samplerInputSpanName = newSpanSpecName
        , samplerInputSpanKind = newSpanSpecKind
        , samplerInputSpanAttrs = newSpanSpecAttrs
        , samplerInputSpanLinks = spanLinks
        }

      let (spanIsRecording, spanContextPostSampling) =
            case samplingResultDecision samplingResult of
              SamplingDecisionDrop -> (False, initSpanContext)
              SamplingDecisionRecordOnly -> (True, initSpanContext)
              SamplingDecisionRecordAndSample ->
                ( True
                , initSpanContext
                    { spanContextTraceFlags =
                        traceFlagsSampled <>  spanContextTraceFlags initSpanContext
                    }
                )

      pure Span
        { spanParent
        , spanContext =
            spanContextPostSampling
              { spanContextTraceState = samplingResultTraceState samplingResult
              }
        , spanName = newSpanSpecName
        , spanStatus = SpanStatusUnset
        , spanStart
        , spanFrozenAt = Nothing
        , spanKind = newSpanSpecKind
        , spanAttrs =
            samplingResultSpanAttrs samplingResult
              <> callStackAttrs cs
              <> newSpanSpecAttrs
        , spanLinks
        , spanEvents = mempty
        , spanIsRecording
        , spanInstrumentationScope = scope
        }
      where
      spanParentFromParentContext :: Context -> IO SpanParent
      spanParentFromParentContext context =
        case lookupContext contextKeySpan context of
          Nothing -> pure SpanParentRoot
          Just mutableSpan -> do
            fmap (SpanParentChildOf . spanContext) $ unsafeReadMutableSpan mutableSpan

      newSpanContext :: SpanParent -> IO SpanContext
      newSpanContext spanParent = do
        (spanContextTraceId, spanContextSpanId) <- do
          runIdGeneratorM prngRef logger
            case spanParent of
              SpanParentRoot ->
                liftA2 (,) genTraceId genSpanId
                  `catchAny` \(SomeException ex) -> do
                    traceId <- idGeneratorSpecGenTraceId defaultIdGeneratorSpec
                    spanId <- idGeneratorSpecGenSpanId defaultIdGeneratorSpec
                    logError $ "Fell back to default trace/span ID gen due to exception" :#
                      [ "exception" .= displayException ex
                      , "traceId" .= traceId
                      , "spanId" .= spanId
                      ]
                    pure (traceId, spanId)
              SpanParentChildOf scParent ->
                fmap (spanContextTraceId scParent,) genSpanId
                  `catchAny` \(SomeException ex) -> do
                    let traceId = spanContextTraceId scParent
                    spanId <- idGeneratorSpecGenSpanId defaultIdGeneratorSpec
                    logError $ "Fell back to default trace/span ID gen due to exception" :#
                      [ "exception" .= displayException ex
                      , "traceId" .= traceId
                      , "spanId" .= spanId
                      ]
                    pure (traceId, spanId)

        pure emptySpanContext
          { spanContextTraceId
          , spanContextSpanId
          , spanContextTraceFlags = mempty
          , spanContextTraceState = emptyTraceState
          , spanContextIsRemote = False -- TODO: Populate correctly
          }

    spanUpdater :: MutableSpan -> UpdateSpanSpec -> IO (Span Attrs)
    spanUpdater mutableSpan updateSpanSpec = do
      updater <- buildSpanUpdater (liftIO now) updateSpanSpec
      frozenAt <- liftIO now
      unsafeModifyMutableSpan mutableSpan \span ->
        let span' = updater span
         in ( span'
            , freezeSpan frozenAt spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits span'
            )

    spanLinks =
      SpanLinks $ flip fmap (unSpanLinkSpecs newSpanSpecLinks) \spanLinkSpec ->
        SpanLink
          { spanLinkSpanContext =
              spanLinkSpecSpanContext spanLinkSpec
          , spanLinkAttrs =
              spanLinkSpecAttrs spanLinkSpec
          }

    parentContext = fromMaybe implicitParentContext newSpanSpecParentContext

    NewSpanSpec
      { newSpanSpecName
      , newSpanSpecParentContext
      , newSpanSpecStart
      , newSpanSpecKind
      , newSpanSpecAttrs
      , newSpanSpecLinks
      } = newSpanSpec

  TracerProviderSpec
    { tracerProviderSpecNow = now
    , tracerProviderSpecLogger = logger
    , tracerProviderSpecSeed = seed
    , tracerProviderSpecIdGenerator =
        IdGeneratorSpec
          { idGeneratorSpecGenTraceId = genTraceId
          , idGeneratorSpecGenSpanId = genSpanId
          }
    , tracerProviderSpecSpanProcessors = spanProcessorSpecs
    , tracerProviderSpecSampler = samplerSpec
    , tracerProviderSpecSpanAttrsLimits = spanAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = spanEventAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = spanLinkAttrsLimits
    , tracerProviderSpecCallStackAttrs = callStackAttrs
    } = tracerProviderSpec

data SpanProcessor = SpanProcessor
  { spanProcessorOnSpanStart
      :: Context -- Parent context
      -> (UpdateSpanSpec -> IO (Span Attrs))
      -> IO ()
  , spanProcessorOnSpanEnd :: Span Attrs -> IO ()
  , spanProcessorShutdown :: IO ()
  , spanProcessorForceFlush :: IO ()
  }

instance Semigroup SpanProcessor where
  sp1 <> sp2 =
    SpanProcessor
      { spanProcessorOnSpanStart =
          spanProcessorOnSpanStart sp1 <> spanProcessorOnSpanStart sp2
      , spanProcessorOnSpanEnd =
          spanProcessorOnSpanEnd sp1 <> spanProcessorOnSpanEnd sp2
      , spanProcessorShutdown =
          spanProcessorShutdown sp1 <> spanProcessorShutdown sp2
      , spanProcessorForceFlush =
          spanProcessorForceFlush sp1 <> spanProcessorForceFlush sp2
      }

instance Monoid SpanProcessor where
  mempty =
    SpanProcessor
      { spanProcessorOnSpanStart = mempty
      , spanProcessorOnSpanEnd = mempty
      , spanProcessorShutdown = mempty
      , spanProcessorForceFlush = mempty
      }

buildSpanProcessor
  :: forall m
   . (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanProcessorSpec
  -> m SpanProcessor
buildSpanProcessor logger spanProcessorSpec = do
  spanExporter <- buildSpanExporter logger spanProcessorSpecExporter
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanProcessor shutdownRef spanExporter
  where
  spanProcessor shutdownRef spanExporter =
    SpanProcessor
      { spanProcessorOnSpanStart = \mParentSpanContext spanUpdater -> do
          unlessSTM (readTVar shutdownRef) do
            pure do
              run defaultTimeout metaOnSpanStart do
                spanProcessorSpecOnSpanStart mParentSpanContext \updateSpanSpec -> do
                  liftIO $ spanUpdater updateSpanSpec
      , spanProcessorOnSpanEnd = \endedSpan -> do
          unlessSTM (readTVar shutdownRef) do
            pure do
              run defaultTimeout metaOnSpanEnd do
                spanProcessorSpecOnSpanEnd endedSpan
      , spanProcessorShutdown = do
          unlessSTM (readTVar shutdownRef) do
            writeTVar shutdownRef True
            pure do
              run shutdownTimeout metaShutdown do
                spanProcessorSpecShutdown
      , spanProcessorForceFlush = do
          unlessSTM (readTVar shutdownRef) do
            pure do
              run forceFlushTimeout metaForceFlush do
                spanProcessorSpecForceFlush
      }
    where
    run :: Int -> [SeriesElem] -> SpanProcessorM () -> IO ()
    run = runSpanProcessorM spanExporter logger onTimeout onEx

  defaultTimeout :: Int
  defaultTimeout = 5_000_000

  metaOnSpanStart = mkLoggingMeta "onSpanStart"
  metaOnSpanEnd = mkLoggingMeta "onSpanEnd"
  metaShutdown = mkLoggingMeta "shutdown"
  metaForceFlush = mkLoggingMeta "forceFlush"

  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "spanProcessor" .= object
        [ "name" .= spanProcessorSpecName
        , "method" .= method
        ]
    ]

  SpanProcessorSpec
    { spanProcessorSpecName
    , spanProcessorSpecExporter
    , spanProcessorSpecOnSpanStart
    , spanProcessorSpecOnSpanEnd
    , spanProcessorSpecShutdown
    , spanProcessorSpecShutdownTimeout = shutdownTimeout
    , spanProcessorSpecForceFlush
    , spanProcessorSpecForceFlushTimeout = forceFlushTimeout
    , spanProcessorSpecOnTimeout = onTimeout
    , spanProcessorSpecOnException = onEx
    } = spanProcessorSpec

data SimpleSpanProcessorSpec = SimpleSpanProcessorSpec
  { simpleSpanProcessorSpecName :: Text
  , simpleSpanProcessorSpecExporter :: SpanExporterSpec
  , simpleSpanProcessorSpecOnSpansExported :: OnSpansExported ()
  }

defaultSimpleSpanProcessorSpec :: SimpleSpanProcessorSpec
defaultSimpleSpanProcessorSpec =
  SimpleSpanProcessorSpec
    { simpleSpanProcessorSpecExporter = defaultSpanExporterSpec
    , simpleSpanProcessorSpecName = "simple"
    , simpleSpanProcessorSpecOnSpansExported = do
        askSpansExportedResult >>= \case
          SpanExportResultSuccess -> pure ()
          SpanExportResultFailure -> do
            spans <- askSpansExported
            pairs <- askSpansExportedMetadata
            logError $ "Exporter failed to export spans" :#
              "spans" .= spans : pairs
    }

simpleSpanProcessor :: SimpleSpanProcessorSpec -> SpanProcessorSpec
simpleSpanProcessor simpleSpanProcessorSpec = spanProcessorSpec
  where
  spanProcessorSpec =
    defaultSpanProcessorSpec
      { spanProcessorSpecName = name
      , spanProcessorSpecExporter = spanExporterSpec
      , spanProcessorSpecOnSpanEnd = \span -> do
          when (spanIsSampled span) do
            let batch = singletonBatch span
            logger <- askLoggerIO
            spanExporter <- askSpanExporter
            liftIO $ spanExporterExport spanExporter batch \spanExportResult -> do
              flip runLoggingT logger do
                runOnSpansExported onSpansExported batch spanExportResult metaOnSpanEnd
      }

  metaOnSpanEnd :: [SeriesElem]
  metaOnSpanEnd = mkLoggingMeta "onSpanEnd"

  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "spanProcessorSpec" .= object
        [ "name" .= name
        , "method" .= method
        ]
    ]

  SimpleSpanProcessorSpec
    { simpleSpanProcessorSpecName = name
    , simpleSpanProcessorSpecExporter = spanExporterSpec
    , simpleSpanProcessorSpecOnSpansExported = onSpansExported
    } = simpleSpanProcessorSpec

data SpanProcessorSpec = SpanProcessorSpec
  { spanProcessorSpecName :: Text
  , spanProcessorSpecExporter :: SpanExporterSpec
  , spanProcessorSpecOnSpanStart
      :: Context -- Parent context
      -> (UpdateSpanSpec -> SpanProcessorM (Span Attrs))
      -> SpanProcessorM ()
  , spanProcessorSpecOnSpanEnd :: Span Attrs -> SpanProcessorM ()
  , spanProcessorSpecShutdown :: SpanProcessorM ()
  , spanProcessorSpecShutdownTimeout :: Int
  , spanProcessorSpecForceFlush :: SpanProcessorM ()
  , spanProcessorSpecForceFlushTimeout :: Int
  , spanProcessorSpecOnTimeout :: OnTimeout ()
  , spanProcessorSpecOnException :: OnException ()
  }

defaultSpanProcessorSpec :: SpanProcessorSpec
defaultSpanProcessorSpec =
  SpanProcessorSpec
    { spanProcessorSpecName = "default"
    , spanProcessorSpecExporter = defaultSpanExporterSpec
    , spanProcessorSpecOnSpanStart = mempty
    , spanProcessorSpecOnSpanEnd = mempty
    , spanProcessorSpecShutdown = do
        spanExporter <- askSpanExporter
        liftIO $ spanExporterShutdown spanExporter
    , spanProcessorSpecShutdownTimeout = 30_000_000
    , spanProcessorSpecForceFlush = do
        spanExporter <- askSpanExporter
        liftIO $ spanExporterForceFlush spanExporter
    , spanProcessorSpecForceFlushTimeout = 30_000_000
    , spanProcessorSpecOnTimeout = do
        timeoutMicros <- askTimeoutMicros
        pairs <- askTimeoutMetadata
        logError $ "Action did not complete within timeout" :#
          "timeoutMicros" .= timeoutMicros : pairs
    , spanProcessorSpecOnException = do
        SomeException ex <- askException
        pairs <- askExceptionMetadata
        logError $ "Ignoring exception" :#
          "exception" .= displayException ex : pairs
    }

data SpanExportResult
  = SpanExportResultSuccess
  | SpanExportResultFailure

data SpanExporter = SpanExporter
  { spanExporterExport
      :: Batch (Span Attrs)
      -> (SpanExportResult -> IO ())
      -> IO ()
  , spanExporterShutdown :: IO ()
  , spanExporterForceFlush :: IO ()
  }

buildSpanExporter
  :: forall m
   . (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanExporterSpec
  -> m SpanExporter
buildSpanExporter logger spanExporterSpec = do
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanExporter shutdownRef
  where
  spanExporter shutdownRef =
    SpanExporter
      { spanExporterExport = \spans onSpansExported -> do
          unlessSTM (readTVar shutdownRef) do
            pure do
              runSpanExporterM logger onTimeout onEx defaultTimeout metaExport do
                spanExporterSpecExport spans \spanExportResult -> do
                  liftIO $ onSpansExported spanExportResult
      , spanExporterShutdown = do
          unlessSTM (readTVar shutdownRef) do
            writeTVar shutdownRef True
            pure do
              runSpanExporterM logger onTimeout onEx shutdownTimeout metaShutdown do
                spanExporterSpecShutdown
      , spanExporterForceFlush = do
          unlessSTM (readTVar shutdownRef) do
            pure do
              runSpanExporterM logger onTimeout onEx forceFlushTimeout metaForceFlush do
                spanExporterSpecForceFlush
      }

  defaultTimeout :: Int
  defaultTimeout = 10_000_000

  metaExport = mkLoggingMeta "export"
  metaShutdown = mkLoggingMeta "shutdown"
  metaForceFlush = mkLoggingMeta "forceFlush"

  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "spanExporter" .= object
        [ "name" .= spanExporterSpecName
        , "method" .= method
        ]
    ]

  SpanExporterSpec
    { spanExporterSpecName
    , spanExporterSpecExport
    , spanExporterSpecShutdown
    , spanExporterSpecShutdownTimeout = shutdownTimeout
    , spanExporterSpecForceFlush
    , spanExporterSpecForceFlushTimeout = forceFlushTimeout
    , spanExporterSpecOnTimeout = onTimeout
    , spanExporterSpecOnException = onEx
    } = spanExporterSpec

stmSpanExporter :: TMQueue (Span Attrs) -> SpanExporterSpec
stmSpanExporter queue = spanExporterSpec
  where
  spanExporterSpec =
    defaultSpanExporterSpec
      { spanExporterSpecName = "stm"
      , spanExporterSpecExport = \spans onSpansExported -> do
          join $ liftIO $ atomically do
            traverse_ (writeTMQueue queue) spans
            pure $ onSpansExported SpanExportResultSuccess
      , spanExporterSpecShutdown = do
          liftIO $ atomically $ closeTMQueue queue
      }

data SpanExporterSpec = SpanExporterSpec
  { spanExporterSpecName :: Text
  , spanExporterSpecExport
      :: Batch (Span Attrs)
      -> (SpanExportResult -> SpanExporterM ())
      -> SpanExporterM ()
  , spanExporterSpecShutdown :: SpanExporterM ()
  , spanExporterSpecShutdownTimeout :: Int
  , spanExporterSpecForceFlush :: SpanExporterM ()
  , spanExporterSpecForceFlushTimeout :: Int
  , spanExporterSpecOnTimeout :: OnTimeout ()
  , spanExporterSpecOnException :: OnException ()
  }

defaultSpanExporterSpec :: SpanExporterSpec
defaultSpanExporterSpec =
  SpanExporterSpec
    { spanExporterSpecName = "default"
    , spanExporterSpecExport = mempty
    , spanExporterSpecShutdown = mempty
    , spanExporterSpecShutdownTimeout = 30_000_000
    , spanExporterSpecForceFlush = mempty
    , spanExporterSpecForceFlushTimeout = 30_000_000
    , spanExporterSpecOnTimeout = do
        timeoutMicros <- askTimeoutMicros
        pairs <- askTimeoutMetadata
        logError $ "Action did not complete within timeout" :#
          "timeoutMicros" .= timeoutMicros : pairs
    , spanExporterSpecOnException = do
        SomeException ex <- askException
        pairs <- askExceptionMetadata
        logError $ "Ignoring exception" :#
          "exception" .= displayException ex : pairs
    }

data Sampler = Sampler
  { samplerName :: Text
  , samplerDescription :: Text
  , samplerShouldSample :: SamplerInput -> IO SamplingResult
  }

buildSampler
  :: forall m
   . (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SamplerSpec
  -> m Sampler
buildSampler logger samplerSpec = do
  pure sampler
  where
  sampler =
    Sampler
      { samplerName = samplerSpecName
      , samplerDescription = samplerSpecDescription
      , samplerShouldSample = \samplerInput -> do
          runSamplerM logger onEx metaShouldSample do
            samplerSpecShouldSample samplerInput
      }

  metaShouldSample = mkLoggingMeta "shouldSample"

  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "sampler" .= object
        [ "name" .= samplerSpecName
        , "description" .= samplerSpecDescription
        , "method" .= method
        ]
    ]

  SamplerSpec
    { samplerSpecName
    , samplerSpecDescription
    , samplerSpecShouldSample
    , samplerSpecOnException = onEx
    } = samplerSpec

data SamplerSpec = SamplerSpec
  { samplerSpecName :: Text
  , samplerSpecDescription :: Text
  , samplerSpecShouldSample :: SamplerInput -> SamplerM SamplingResult
  , samplerSpecOnException :: OnException SamplingResult
  }

defaultSamplerSpec :: SamplerSpec
defaultSamplerSpec = alwaysOffSampler

alwaysOnSampler :: SamplerSpec
alwaysOnSampler =
  (constDecisionSampler SamplingDecisionRecordAndSample)
    { samplerSpecName = "AlwaysOn"
    , samplerSpecDescription = "AlwaysOnSampler"
    }

alwaysOffSampler :: SamplerSpec
alwaysOffSampler =
  (constDecisionSampler SamplingDecisionDrop)
    { samplerSpecName = "AlwaysOff"
    , samplerSpecDescription = "AlwaysOffSampler"
    }

data ParentBasedSamplerSpec = ParentBasedSamplerSpec
  { parentBasedSamplerSpecOnRoot :: SamplerSpec
  , parentBasedSamplerSpecOnRemoteParentSampled :: SamplerSpec
  , parentBasedSamplerSpecOnRemoteParentNotSampled :: SamplerSpec
  , parentBasedSamplerSpecOnLocalParentSampled :: SamplerSpec
  , parentBasedSamplerSpecOnLocalParentNotSampled :: SamplerSpec
  }

defaultParentBasedSamplerSpec :: ParentBasedSamplerSpec
defaultParentBasedSamplerSpec =
  ParentBasedSamplerSpec
    { parentBasedSamplerSpecOnRoot = defaultSamplerSpec
    , parentBasedSamplerSpecOnRemoteParentSampled = alwaysOnSampler
    , parentBasedSamplerSpecOnRemoteParentNotSampled = alwaysOffSampler
    , parentBasedSamplerSpecOnLocalParentSampled = alwaysOnSampler
    , parentBasedSamplerSpecOnLocalParentNotSampled = alwaysOffSampler
    }

parentBasedSampler :: ParentBasedSamplerSpec -> SamplerSpec
parentBasedSampler parentBasedSamplerSpec =
  defaultSamplerSpec
    { samplerSpecName = "ParentBased"
    , samplerSpecDescription = "ParentBased"
    , samplerSpecShouldSample = \samplerInput -> do
        parentSpanContext <- do
          case lookupContext contextKeySpan $ samplerInputContext samplerInput of
            Nothing -> pure emptySpanContext
            Just mutableSpan -> do
              liftIO $ fmap spanContext $ unsafeReadMutableSpan mutableSpan
        shouldSample parentSpanContext samplerInput
    }
  where
  shouldSample parentSpanContext samplerInput
    | hasParent && parentIsRemote && parentIsSampled =
        samplerSpecShouldSample onRemoteParentSampled samplerInput
    | hasParent && parentIsRemote && not parentIsSampled =
        samplerSpecShouldSample onRemoteParentNotSampled samplerInput
    | hasParent && not parentIsRemote && parentIsSampled =
        samplerSpecShouldSample onLocalParentSampled samplerInput
    | hasParent && not parentIsRemote && not parentIsSampled =
        samplerSpecShouldSample onLocalParentNotSampled samplerInput
    | otherwise =
        samplerSpecShouldSample onRoot samplerInput
    where
    hasParent = spanContextIsValid parentSpanContext
    parentIsRemote = spanContextIsRemote parentSpanContext
    parentIsSampled = spanContextIsSampled parentSpanContext

  ParentBasedSamplerSpec
    { parentBasedSamplerSpecOnRoot = onRoot
    , parentBasedSamplerSpecOnRemoteParentSampled = onRemoteParentSampled
    , parentBasedSamplerSpecOnRemoteParentNotSampled = onRemoteParentNotSampled
    , parentBasedSamplerSpecOnLocalParentSampled = onLocalParentSampled
    , parentBasedSamplerSpecOnLocalParentNotSampled = onLocalParentNotSampled
    } = parentBasedSamplerSpec

constDecisionSampler :: SamplingDecision -> SamplerSpec
constDecisionSampler samplingDecision =
  SamplerSpec
    { samplerSpecName = "ConstDecision"
    , samplerSpecDescription = "ConstDecision{" <> samplingDecisionText <> "}"
    , samplerSpecShouldSample = shouldSample
    , samplerSpecOnException = do
        SomeException ex <- askException
        pairs <- askExceptionMetadata
        logError $ "Rethrowing unhandled exception from sampler" :#
          "exception" .= displayException ex : pairs
        throwM ex
    }
  where
  shouldSample samplerInput = do
    samplingResultTraceState <- do
      case lookupContext contextKeySpan samplerInputContext of
        Nothing -> pure emptyTraceState
        Just mutableSpan -> do
          liftIO
            $ fmap (spanContextTraceState . spanContext)
            $ unsafeReadMutableSpan mutableSpan
    pure defaultSamplingResult
      { samplingResultDecision = samplingDecision
      , samplingResultSpanAttrs = mempty
      , samplingResultTraceState
      }
    where
    SamplerInput { samplerInputContext } = samplerInput

  samplingDecisionText =
    case samplingDecision of
      SamplingDecisionDrop -> "DROP"
      SamplingDecisionRecordOnly -> "RECORD_ONLY"
      SamplingDecisionRecordAndSample -> "RECORD_AND_SAMPLE"

data SamplerInput = SamplerInput
  { samplerInputContext :: Context
  , samplerInputTraceId :: TraceId
  , samplerInputSpanName :: SpanName
  , samplerInputSpanKind :: SpanKind
  , samplerInputSpanAttrs :: AttrsBuilder 'AttrsForSpan
  , samplerInputSpanLinks :: SpanLinks AttrsBuilder
  }

data SamplingResult = SamplingResult
  { samplingResultDecision :: SamplingDecision
  , samplingResultSpanAttrs :: AttrsBuilder 'AttrsForSpan
  , samplingResultTraceState :: TraceState
  }

defaultSamplingResult :: SamplingResult
defaultSamplingResult =
  SamplingResult
    { samplingResultDecision = SamplingDecisionDrop
    , samplingResultSpanAttrs = mempty
    , samplingResultTraceState = emptyTraceState
    }

data SamplingDecision
  = SamplingDecisionDrop
  | SamplingDecisionRecordOnly
  | SamplingDecisionRecordAndSample

pattern Drop :: SamplingDecision
pattern Drop <- SamplingDecisionDrop where
  Drop = SamplingDecisionDrop

pattern RecordOnly :: SamplingDecision
pattern RecordOnly <- SamplingDecisionRecordOnly where
  RecordOnly = SamplingDecisionRecordOnly

pattern RecordAndSample :: SamplingDecision
pattern RecordAndSample <- SamplingDecisionRecordAndSample where
  RecordAndSample = SamplingDecisionRecordAndSample

{-# COMPLETE Drop, RecordOnly, RecordAndSample :: SamplingDecision #-}

type SpanProcessorM :: Type -> Type
newtype SpanProcessorM a = SpanProcessorM
  { unSpanProcessorM :: SpanExporter -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT SpanExporter (LoggingT IO))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT SpanExporter (LoggingT IO)) a)

askSpanExporter :: SpanProcessorM SpanExporter
askSpanExporter = SpanProcessorM pure

runSpanProcessorM
  :: SpanExporter
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnTimeout a
  -> OnException a
  -> Int
  -> [SeriesElem]
  -> SpanProcessorM a
  -> IO a
runSpanProcessorM spanExporter logger onTimeout onEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanProcessorM action spanExporter `catchAny` \someEx -> do
          runOnException onEx someEx pairs
    case mResult of
      Just x -> pure x
      Nothing -> runOnTimeout onTimeout timeoutMicros pairs

type SpanExporterM :: Type -> Type
newtype SpanExporterM a = SpanExporterM
  { unSpanExporterM :: LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (LoggingT IO)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (LoggingT IO) a)

runSpanExporterM
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnTimeout a
  -> OnException a
  -> Int
  -> [SeriesElem]
  -> SpanExporterM a
  -> IO a
runSpanExporterM logger onTimeout onEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanExporterM action `catchAny` \someEx -> do
          runOnException onEx someEx pairs
    case mResult of
      Just x -> pure x
      Nothing -> runOnTimeout onTimeout timeoutMicros pairs

type SamplerM :: Type -> Type
newtype SamplerM a = SamplerM
  { unSamplerM :: LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (LoggingT IO)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (LoggingT IO) a)

runSamplerM
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnException a
  -> [SeriesElem]
  -> SamplerM a
  -> IO a
runSamplerM logger onEx pairs action = do
  flip runLoggingT logger do
    unSamplerM action `catchAny` \someEx -> do
      runOnException onEx someEx pairs

type IdGeneratorM :: Type -> Type
newtype IdGeneratorM a = IdGeneratorM
  { unIdGeneratorM :: PRNG -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT PRNG (LoggingT IO))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT PRNG (LoggingT IO)) a)

runIdGeneratorM
  :: MVar PRNG
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IdGeneratorM a
  -> IO a
runIdGeneratorM prngRef logger action = do
  withMVar prngRef \prng -> do
    flip runLoggingT logger do
      unIdGeneratorM action prng

data IdGeneratorSpec = IdGeneratorSpec
  { idGeneratorSpecGenTraceId :: IdGeneratorM TraceId
  , idGeneratorSpecGenSpanId :: IdGeneratorM SpanId
  }

defaultIdGeneratorSpec :: IdGeneratorSpec
defaultIdGeneratorSpec =
  IdGeneratorSpec
    { idGeneratorSpecGenTraceId = liftA2 traceIdFromWords genUniform genUniform
    , idGeneratorSpecGenSpanId = fmap spanIdFromWords genUniform
    }

newtype PRNG = PRNG
  { unPRNG :: GenIO
  }

genUniform :: forall a. (Variate a) => IdGeneratorM a
genUniform =
  IdGeneratorM \PRNG { unPRNG = gen } -> liftIO $ uniform gen

newPRNGRef :: Seed -> IO (MVar PRNG)
newPRNGRef seed = do
  prng <- fmap PRNG $ initialize $ fromSeed seed
  newMVar prng

newtype OnException a = OnException
  { runOnException :: SomeException -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT SomeException (ReaderT [SeriesElem] (LoggingT IO)))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT SomeException (ReaderT [SeriesElem] (LoggingT IO))) a)

askException :: OnException SomeException
askException = OnException \someEx _pairs -> pure someEx

askExceptionMetadata :: OnException [SeriesElem]
askExceptionMetadata = OnException \_someEx pairs -> pure pairs

newtype OnTimeout a = OnTimeout
  { runOnTimeout :: Int -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT Int (ReaderT [SeriesElem] (LoggingT IO)))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Int (ReaderT [SeriesElem] (LoggingT IO))) a)

askTimeoutMicros :: OnTimeout Int
askTimeoutMicros = OnTimeout \timeoutMicros _pairs -> pure timeoutMicros

askTimeoutMetadata :: OnTimeout [SeriesElem]
askTimeoutMetadata = OnTimeout \_timeoutMicros pairs -> pure pairs

newtype OnSpansExported a = OnSpansExported
  { runOnSpansExported :: Batch (Span Attrs) -> SpanExportResult -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT (Batch (Span Attrs)) (ReaderT SpanExportResult (ReaderT [SeriesElem] (LoggingT IO))))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (Batch (Span Attrs)) (ReaderT SpanExportResult (ReaderT [SeriesElem] (LoggingT IO)))) a)

askSpansExported :: OnSpansExported (Batch (Span Attrs))
askSpansExported = OnSpansExported \spans _spanExportResult _pairs -> pure spans

askSpansExportedResult :: OnSpansExported SpanExportResult
askSpansExportedResult = OnSpansExported \_spans spanExportResult _pairs -> pure spanExportResult

askSpansExportedMetadata :: OnSpansExported [SeriesElem]
askSpansExportedMetadata = OnSpansExported \_spans _spanExportResult pairs -> pure pairs

newtype Batch a = Batch
  { unBatch :: DList a
  } deriving stock (Eq, Show)
    deriving (Monoid, Semigroup, ToJSON) via (DList a)
    deriving (Foldable, Functor, Applicative, Monad) via DList

instance Traversable Batch where
  traverse f (Batch xs) = fmap Batch $ traverse f xs

singletonBatch :: a -> Batch a
singletonBatch = Batch . DList.singleton

fromListBatch :: [a] -> Batch a
fromListBatch = Batch . DList.fromList

data ConcurrentWorkersSpec item = ConcurrentWorkersSpec
  { concurrentWorkersSpecQueueSize :: Int
  , concurrentWorkersSpecWorkerCount :: Int
  , concurrentWorkersSpecProcessItem :: item -> IO ()
  , concurrentWorkersSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , concurrentWorkersSpecLoggingMeta :: [SeriesElem]
  , concurrentWorkersSpecOnException :: OnException ()
  }

defaultConcurrentWorkersSpec :: ConcurrentWorkersSpec item
defaultConcurrentWorkersSpec =
  ConcurrentWorkersSpec
    { concurrentWorkersSpecQueueSize = 2048
    , concurrentWorkersSpecWorkerCount = 5
    , concurrentWorkersSpecProcessItem = mempty
    , concurrentWorkersSpecLogger = mempty
    , concurrentWorkersSpecLoggingMeta = mempty
    , concurrentWorkersSpecOnException = do
        SomeException ex <- askException
        pairs <- askExceptionMetadata
        logError $ "Concurrent worker ignoring exception from processing item" :#
          "exception" .= displayException ex : pairs
    }

data ConcurrentWorkers item = ConcurrentWorkers
  { concurrentWorkersEnqueueItem :: item -> IO ()
  , concurrentWorkersStopWorkers :: IO ()
  }

withConcurrentWorkers
  :: forall m item a
   . (MonadUnliftIO m, ToJSON item, Typeable item)
  => ConcurrentWorkersSpec item
  -> (ConcurrentWorkers item -> m a)
  -> m a
withConcurrentWorkers spec action =
  withRunInIO \runInIO -> withConcurrentWorkersIO spec (runInIO . action)

withConcurrentWorkersIO
  :: forall item a
   . (ToJSON item, Typeable item)
  => ConcurrentWorkersSpec item
  -> (ConcurrentWorkers item -> IO a)
  -> IO a
withConcurrentWorkersIO concurrentWorkersSpec action = do
  flip runLoggingT logger do
    logDebug $ "Starting concurrent workers" :# loggingMeta
  queue <- newTBMQueueIO queueSize
  withWorkers queue \workers -> do
    flip runLoggingT logger do
      logDebug $ "Concurrent workers started" :# loggingMeta
    action ConcurrentWorkers
      { concurrentWorkersEnqueueItem = enqueueItem queue
      , concurrentWorkersStopWorkers = stopWorkers queue workers
      }
  where
  enqueueItem :: TBMQueue item -> item -> IO ()
  enqueueItem queue item = do
    atomically (tryWriteTBMQueue queue item) >>= \case
      Just True -> pure ()
      Just False -> do
        flip runLoggingT logger do
          logError $ "Dropped item as queue was full" :#
            "item" .= item : loggingMeta
      Nothing -> do
        flip runLoggingT logger do
          logError $ "Dropped item as queue was closed" :#
            "item" .= item : loggingMeta

  withWorkers :: TBMQueue item -> ([Async ()] -> IO a) -> IO a
  withWorkers queue f =
    withAll (replicate workerCount $ withAsync $ mkWorker queue) \workers -> do
      f workers `finally` stopWorkers queue workers

  stopWorkers :: TBMQueue item -> [Async ()] -> IO ()
  stopWorkers queue workers = do
    unlessSTM (isClosedTBMQueue queue) do
      closeTBMQueue queue
      pure do
        flip runLoggingT logger do
          logDebug $ "Stopping concurrent workers" :# loggingMeta
          for_ workers \worker -> do
            liftIO (waitCatch worker) >>= \case
              Right () -> pure ()
              Left (SomeException ex) -> do
                logError $ "Concurrent worker previously died due to unhandled exception" :#
                  "exception" .= displayException ex : loggingMeta

  mkWorker :: TBMQueue item -> IO ()
  mkWorker queue = go
    where
    go = do
      mItem <- atomically $ readTBMQueue queue
      for_ mItem \item -> do
        flip runLoggingT logger do
          logDebug $ "Concurrent worker is processing item" :#
            "item" .= item : loggingMeta
          liftIO (evaluate =<< processItem item) `catchAny` \someEx -> do
              runOnException onEx someEx pairs
        go

  loggingMeta :: [SeriesElem]
  loggingMeta =
    "itemType" .= show (typeRep $ Proxy @item)
      : "workerCount" .= workerCount
      : "queueSize" .= queueSize
      : pairs

  ConcurrentWorkersSpec
    { concurrentWorkersSpecQueueSize = queueSize
    , concurrentWorkersSpecWorkerCount = workerCount
    , concurrentWorkersSpecProcessItem = processItem
    , concurrentWorkersSpecLogger = logger
    , concurrentWorkersSpecLoggingMeta = pairs
    , concurrentWorkersSpecOnException = onEx
    } = concurrentWorkersSpec

unlessSTM :: (Monoid a) => STM Bool -> STM (IO a) -> IO a
unlessSTM isShutdownSTM actionSTM =
  join $ atomically $ isShutdownSTM >>= \case
    True -> pure mempty
    False -> actionSTM

defaultSystemSeed :: Seed
defaultSystemSeed = unsafePerformIO createSystemSeed
{-# NOINLINE defaultSystemSeed #-}

withAll
  :: forall a b
   . [(a -> b) -> b]
  -> ([a] -> b)
  -> b
withAll = runCont . mapM cont

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
