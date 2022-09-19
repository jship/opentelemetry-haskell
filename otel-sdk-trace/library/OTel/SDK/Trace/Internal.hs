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
{-# LANGUAGE UndecidableInstances #-}
module OTel.SDK.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    module OTel.SDK.Trace.Internal -- TODO: Explicit exports
  ) where

import Control.Applicative (Applicative(..))
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.STM (STM, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception.Safe
  ( Exception(..), SomeException(..), MonadCatch, MonadMask, MonadThrow, catchAny
  )
import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( LoggingT(..), Message(..), MonadLoggerIO(askLoggerIO), (.=), Loc, LogLevel, LogSource, LogStr
  , MonadLogger, SeriesElem, logError
  )
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (object)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(..))
import Data.Text (Text)
import Data.Vector (Vector)
import OTel.API.Context.Core (Context, lookupContext)
import OTel.API.Core
  ( AttrsFor(..), TimestampSource(..), Attrs, AttrsBuilder, AttrsLimits, InstrumentationScope
  , Timestamp, defaultAttrsLimits, timestampFromNanoseconds
  )
import OTel.API.Trace.Core
  ( SpanParent(..), SpanStatus(..), SpanId, SpanKind, SpanName, TraceId, TraceState, UpdateSpanSpec
  , contextKeySpan, emptySpanContext, emptyTraceState, setSampledFlag, spanContextIsSampled
  , spanContextIsValid, spanIdFromWords, spanIsSampled, traceIdFromWords
  )
import OTel.API.Trace.Core.Internal
  ( MutableSpan(..), NewSpanSpec(..), Span(..), SpanContext(..), SpanLink(..), SpanLinkSpec(..)
  , SpanLinkSpecs(..), SpanLinks(..), Tracer(..), TracerProvider(..), buildSpanUpdater, freezeSpan
  )
import Prelude hiding (span)
import System.Clock (Clock(Realtime), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (Variate(..), GenIO, Seed, createSystemSeed, fromSeed, initialize, uniform)
import System.Timeout (timeout)
import qualified Control.Exception.Safe as Exception
import qualified Data.IORef as IORef
import qualified Data.Vector as Vector

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
withTracerProviderIO tracerProviderSpec f = do
  Exception.bracket
    (newTracerProviderIO tracerProviderSpec)
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
    -> Context
    -> NewSpanSpec
    -> IO MutableSpan
  startSpan prngRef sampler scope spanProcessor implicitParentContext newSpanSpec = do
    span <- buildSpan
    mutableSpan <- do
      fmap MutableSpan $ IORef.newIORef span

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
                        setSampledFlag $ spanContextTraceFlags initSpanContext
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
        , spanAttrs = samplingResultSpanAttrs samplingResult <> newSpanSpecAttrs
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
          Just MutableSpan { unMutableSpan = ref } -> do
            IORef.atomicModifyIORef' ref \span ->
              (span, SpanParentChildOf $ spanContext span)

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
      IORef.atomicModifyIORef' ref \span ->
        let span' = updater span
         in ( span'
            , freezeSpan frozenAt spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits span'
            )
      where
      MutableSpan { unMutableSpan = ref } = mutableSpan

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
    } = tracerProviderSpec

shutdownTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
shutdownTracerProvider = liftIO . tracerProviderShutdown

forceFlushTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
forceFlushTracerProvider = liftIO . tracerProviderForceFlush

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
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanProcessor shutdownRef
  where
  spanProcessor shutdownRef =
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

  run :: Int -> [SeriesElem] -> SpanProcessorM () -> IO ()
  run = runSpanProcessorM logger onTimeout onEx

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
  , simpleSpanProcessorSpecExporter :: SpanExporter Identity
  , simpleSpanProcessorSpecOnSpansExported :: OnSpansExported Identity ()
  }

defaultSimpleSpanProcessorSpec :: SimpleSpanProcessorSpec
defaultSimpleSpanProcessorSpec =
  SimpleSpanProcessorSpec
    { simpleSpanProcessorSpecExporter = noopSpanExporter
    , simpleSpanProcessorSpecName = "simple"
    , simpleSpanProcessorSpecOnSpansExported = do
        askSpansExportedResult >>= \case
          SpanExportResultSuccess -> pure ()
          SpanExportResultFailure -> do
            span <- askSpansExported
            pairs <- askSpansExportedMetadata
            logError $ "Exporter failed to export span" :#
              "span" .= span : pairs
    }

simpleSpanProcessor :: SimpleSpanProcessorSpec -> SpanProcessorSpec
simpleSpanProcessor simpleSpanProcessorSpec = spanProcessorSpec
  where
  spanProcessorSpec =
    defaultSpanProcessorSpec
      { spanProcessorSpecName = name
      , spanProcessorSpecOnSpanEnd = \span -> do
          when (spanIsSampled span) do
            let idSpan = Identity { runIdentity = span }
            logger <- askLoggerIO
            liftIO $ spanExporterExport spanExporter idSpan \spanExportResult -> do
              flip runLoggingT logger do
                runOnSpansExported onSpansExported idSpan spanExportResult metaOnSpanEnd
      , spanProcessorSpecShutdown = liftIO $ spanExporterShutdown spanExporter
      , spanProcessorSpecForceFlush = liftIO $ spanExporterForceFlush spanExporter
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
    , simpleSpanProcessorSpecExporter = spanExporter
    , simpleSpanProcessorSpecOnSpansExported = onSpansExported
    } = simpleSpanProcessorSpec

data SpanProcessorSpec = SpanProcessorSpec
  { spanProcessorSpecName :: Text
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
    , spanProcessorSpecOnSpanStart = mempty
    , spanProcessorSpecOnSpanEnd = mempty
    , spanProcessorSpecShutdown = mempty
    , spanProcessorSpecShutdownTimeout = 30_000_000
    , spanProcessorSpecForceFlush = mempty
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

data SpanExporter (f :: Type -> Type) = SpanExporter
  { spanExporterExport
      :: f (Span Attrs)
      -> (SpanExportResult -> IO ())
      -> IO ()
  , spanExporterShutdown :: IO ()
  , spanExporterForceFlush :: IO ()
  }

noopSpanExporter :: SpanExporter f
noopSpanExporter =
  SpanExporter
    { spanExporterExport = mempty
    , spanExporterShutdown = mempty
    , spanExporterForceFlush = mempty
    }

toSingletonSpanExporter :: SpanExporter Batch -> SpanExporter Identity
toSingletonSpanExporter spanExporter =
  spanExporter
    { spanExporterExport = \Identity { runIdentity = span } onSpansExported -> do
        spanExporterExport spanExporter (singletonBatch span) onSpansExported
    }

buildSpanExporter
  :: forall m f
   . (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanExporterSpec f
  -> m (SpanExporter f)
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

data SpanExporterSpec (f :: Type -> Type) = SpanExporterSpec
  { spanExporterSpecName :: Text
  , spanExporterSpecExport
      :: f (Span Attrs)
      -> (SpanExportResult -> SpanExporterM ())
      -> SpanExporterM ()
  , spanExporterSpecShutdown :: SpanExporterM ()
  , spanExporterSpecShutdownTimeout :: Int
  , spanExporterSpecForceFlush :: SpanExporterM ()
  , spanExporterSpecForceFlushTimeout :: Int
  , spanExporterSpecOnTimeout :: OnTimeout ()
  , spanExporterSpecOnException :: OnException ()
  }

defaultSpanExporterSpec :: SpanExporterSpec f
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
defaultSamplerSpec =
  alwaysOffSampler
    { samplerSpecName = "default"
    , samplerSpecDescription = "default"
    }

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
            Just MutableSpan { unMutableSpan = ref } -> do
              liftIO $ IORef.atomicModifyIORef' ref \span ->
                (span, spanContext span)
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
  defaultSamplerSpec
    { samplerSpecName = "ConstDecision"
    , samplerSpecDescription = "ConstDecision{" <> samplingDecisionText <> "}"
    , samplerSpecShouldSample = shouldSample
    }
  where
  shouldSample samplerInput = do
    samplingResultTraceState <- do
      case lookupContext contextKeySpan samplerInputContext of
        Nothing -> pure emptyTraceState
        Just MutableSpan { unMutableSpan = ref } -> do
          liftIO $ IORef.atomicModifyIORef' ref \span ->
            (span, spanContextTraceState $ spanContext span)
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

-- Context with parent Span. The Span’s SpanContext may be invalid to indicate a root span.
-- TraceId of the Span to be created. If the parent SpanContext contains a valid TraceId, they MUST always match.
-- Name of the Span to be created.
-- SpanKind of the Span to be created.
-- Initial set of Attributes of the Span to be created.
-- Collection of links that will be associated with the Span to be created. Typically useful for batch operations, see Links Between Spans.
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
  { unSpanProcessorM :: LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (LoggingT IO)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (LoggingT IO) a)

runSpanProcessorM
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnTimeout a
  -> OnException a
  -> Int
  -> [SeriesElem]
  -> SpanProcessorM a
  -> IO a
runSpanProcessorM logger onTimeout onEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanProcessorM action `catchAny` \someEx -> do
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

newtype OnSpansExported f a = OnSpansExported
  { runOnSpansExported :: f (Span Attrs) -> SpanExportResult -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT (f (Span Attrs)) (ReaderT SpanExportResult (ReaderT [SeriesElem] (LoggingT IO))))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (f (Span Attrs)) (ReaderT SpanExportResult (ReaderT [SeriesElem] (LoggingT IO)))) a)

askSpansExported :: OnSpansExported f (f (Span Attrs))
askSpansExported = OnSpansExported \spans _spanExportResult _pairs -> pure spans

askSpansExportedResult :: OnSpansExported f SpanExportResult
askSpansExportedResult = OnSpansExported \_spans spanExportResult _pairs -> pure spanExportResult

askSpansExportedMetadata :: OnSpansExported f [SeriesElem]
askSpansExportedMetadata = OnSpansExported \_spans _spanExportResult pairs -> pure pairs

newtype Batch a = Batch
  { unBatch :: Vector a
  } deriving (Eq, Monoid, Semigroup, Show) via (Vector a)
    deriving (Foldable, Functor, Applicative, Monad) via Vector

instance Traversable Batch where
  traverse f (Batch xs) = fmap Batch $ traverse f xs

singletonBatch :: a -> Batch a
singletonBatch = Batch . Vector.singleton

fromListBatch :: [a] -> Batch a
fromListBatch = Batch . Vector.fromList

unlessSTM :: (Monoid a) => STM Bool -> STM (IO a) -> IO a
unlessSTM isShutdownSTM actionSTM =
  join $ liftIO $ atomically $ isShutdownSTM >>= \case
    True -> pure mempty
    False -> actionSTM

defaultSystemSeed :: Seed
defaultSystemSeed = unsafePerformIO createSystemSeed
{-# NOINLINE defaultSystemSeed #-}

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
