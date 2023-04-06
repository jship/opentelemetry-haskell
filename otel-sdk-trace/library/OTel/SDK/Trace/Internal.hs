{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.SDK.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    TracerProviderSpec(..)
  , defaultTracerProviderSpec
  , withTracerProvider
  , withTracerProviderIO

  , SpanProcessor(..)
  , buildSpanProcessor
  , SimpleSpanProcessorSpec(..)
  , defaultSimpleSpanProcessorSpec
  , simpleSpanProcessor
  , SpanProcessorSpec(..)
  , defaultSpanProcessorSpec

  , SpanExportResult(..)
  , SpanExporter(..)
  , buildSpanExporter
  , OTLPSpanExporterSpec(..)
  , defaultOTLPSpanExporterSpec
  , otlpSpanExporter
  , OTLPProtocol(..)
  , httpProtobufProtocol
  , OTLPSpanExporterItem(..)
  , stmSpanExporter
  , SpanExporterSpec(..)
  , defaultSpanExporterSpec

  , Sampler(..)
  , buildSampler
  , SamplerSpec(..)
  , defaultSamplerSpec
  , alwaysOnSampler
  , alwaysOffSampler
  , ParentBasedSamplerSpec(..)
  , defaultParentBasedSamplerSpec
  , parentBasedSampler
  , constDecisionSampler
  , SamplerInput(..)
  , SamplingResult(..)
  , defaultSamplingResult
  , SamplingDecision(..)
  , samplingDecisionDrop
  , samplingDecisionRecordOnly
  , samplingDecisionRecordAndSample

  , SpanProcessorM(..)
  , askSpanExporter
  , runSpanProcessorM

  , SpanExporterM(..)
  , askResource
  , runSpanExporterM

  , SamplerM(..)
  , runSamplerM

  , IdGeneratorM(..)
  , runIdGeneratorM
  , IdGenerator(..)
  , buildIdGenerator
  , IdGeneratorSpec(..)
  , defaultIdGeneratorSpec
  , PRNG(..)
  , genUniform
  , newPRNGRef

  , OnException(..)
  , askException
  , askExceptionMetadata

  , OnTimeout(..)
  , askTimeoutMicros
  , askTimeoutMetadata

  , OnSpansExported(..)
  , askSpansExported
  , askSpansExportedResult
  , askSpansExportedMetadata

  , Batch(..)
  , singletonBatch
  , fromListBatch

  , ConcurrentWorkersSpec(..)
  , defaultConcurrentWorkersSpec
  , ConcurrentWorkers(..)
  , withConcurrentWorkers

  , unlessSTM

  , with
  , withAll

  , defaultSystemSeed
  , defaultManager

  , spanSummary

  , redactHttpExceptionHeaders
  ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.Async (Async, waitCatch, withAsync)
import Control.Concurrent.STM (STM, atomically, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBMQueue
  ( TBMQueue, closeTBMQueue, isClosedTBMQueue, newTBMQueueIO, readTBMQueue, tryWriteTBMQueue
  )
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, writeTMQueue)
import Control.Exception (AsyncException, SomeAsyncException, evaluate)
import Control.Exception.Safe
  ( Exception(..), Handler(..), SomeException(..), MonadCatch, MonadMask, MonadThrow, catchAny
  , finally, throwM
  )
import Control.Monad (join, when)
import Control.Monad.Cont (ContT(..), cont, runCont)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( LoggingT(..), Message((:#)), MonadLoggerIO(..), (.=), Loc, LogLevel, LogSource, LogStr
  , MonadLogger, SeriesElem, logDebug, logError
  )
import Control.Monad.Reader (ReaderT(..))
import Control.Retry
  ( RetryAction(..), RetryPolicyM, RetryStatus, applyPolicy, fullJitterBackoff, limitRetries
  , recoveringDynamic
  )
import Data.Aeson (ToJSON(..), Value, object)
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy (ByteString)
import Data.Either (fromRight)
import Data.Foldable (fold, for_, traverse_)
import Data.Function (on)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Monoid (Ap(..))
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time
  ( NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, getCurrentTime, parseTimeM
  )
import Data.Typeable (Typeable, typeRep)
import Data.Vector (Vector)
import GHC.Stack (SrcLoc(..), CallStack)
import Lens.Micro ((&), (.~))
import Network.HTTP.Client
  ( HttpException(..), HttpExceptionContent(..), Request(method, requestBody, requestHeaders)
  , RequestBody(RequestBodyBS), Response(responseHeaders, responseStatus), Manager, httpLbs
  , requestFromURI, setRequestCheckStatus
  )
import Network.HTTP.Client.Internal (responseOriginalRequest)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status(statusCode), Header, serviceUnavailable503, tooManyRequests429)
import Network.HTTP.Types.Header (HeaderName, hContentType, hRetryAfter)
import Network.URI (URI(..), parseURI)
import OTel.API.Common
  ( Attr(attrType, attrVal)
  , AttrType
    ( AttrTypeBool, AttrTypeBoolArray, AttrTypeDouble, AttrTypeDoubleArray, AttrTypeInt
    , AttrTypeIntArray, AttrTypeText, AttrTypeTextArray
    )
  , AttrsFor(AttrsForSpan, AttrsForSpanEvent, AttrsForSpanLink), InstrumentationScope(..)
  , InstrumentationScopeName(unInstrumentationScopeName), KV((.@)), Key(unKey)
  , TimestampSource(TimestampSourceAt, TimestampSourceNow), Version(unVersion), AttrVals, Attrs
  , AttrsBuilder, AttrsLimits, Timestamp, askException, askExceptionMetadata, askTimeoutMetadata
  , askTimeoutMicros, defaultAttrsLimits, droppedAttrsCount, foldMapWithKeyAttrs, schemaURLToText
  , timestampFromNanoseconds, timestampToNanoseconds, with
  )
import OTel.API.Common.Internal
  ( AttrVals(..), InstrumentationScope(..), OnException(..), OnTimeout(..)
  )
import OTel.API.Context.Core (Context, lookupContext)
import OTel.API.Trace
  ( Span(..), SpanContext(..), SpanEvent(..), SpanEventName(unSpanEventName)
  , SpanKind(SpanKindClient, SpanKindConsumer, SpanKindInternal, SpanKindProducer, SpanKindServer)
  , SpanLineage(SpanLineageChildOf, SpanLineageRoot), SpanLink(..), SpanName(unSpanName)
  , SpanSpec(..), SpanStatus(SpanStatusError, SpanStatusOk, SpanStatusUnset), MutableSpan, SpanId
  , SpanLinks, TraceId, TraceState, Tracer, TracerProvider, UpdateSpanSpec, contextKeySpan
  , emptySpanContext, emptyTraceState, frozenTimestamp, spanContextIsSampled, spanContextIsValid
  , spanEventsToList, spanIdFromWords, spanIdToBytesBuilder, spanIsSampled, spanLinksToList
  , traceFlagsSampled, traceIdFromWords, traceIdToBytesBuilder, pattern CODE_FILEPATH
  , pattern CODE_FUNCTION, pattern CODE_LINENO, pattern CODE_NAMESPACE
  )
import OTel.API.Trace.Core.Internal
  ( Span(..), SpanContext(..), SpanLinkSpec(..), SpanLinkSpecs(..), SpanLinks(..), SpanSpec(..)
  , Tracer(..), TracerProvider(..), buildSpanUpdater, freezeSpan, unsafeModifyMutableSpan
  , unsafeNewMutableSpan, unsafeReadMutableSpan
  )
import OTel.SDK.Resource.Core (buildResourcePure, defaultResourceBuilder)
import OTel.SDK.Resource.Core.Internal (Resource(..))
import Prelude hiding (span)
import System.Clock (Clock(Realtime), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (Variate(..), GenIO, Seed, createSystemSeed, fromSeed, initialize, uniform)
import System.Timeout (timeout)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.ProtoLens as ProtoLens
import qualified Data.Set as Set
import qualified GHC.Stack as Stack
import qualified OTel.SDK.OTLP.Bindings.Collector.Trace.V1.TraceService as OTLP.Collector
import qualified OTel.SDK.OTLP.Bindings.Collector.Trace.V1.TraceService_Fields as OTLP.Collector
import qualified OTel.SDK.OTLP.Bindings.Common.V1.Common as OTLP.Common
import qualified OTel.SDK.OTLP.Bindings.Common.V1.Common_Fields as OTLP.Common
import qualified OTel.SDK.OTLP.Bindings.Resource.V1.Resource as OTLP.Resource
import qualified OTel.SDK.OTLP.Bindings.Resource.V1.Resource_Fields as OTLP.Resource
import qualified OTel.SDK.OTLP.Bindings.Trace.V1.Trace as OTLP.Trace
import qualified OTel.SDK.OTLP.Bindings.Trace.V1.Trace_Fields as OTLP.Trace

data TracerProviderSpec = TracerProviderSpec
  { tracerProviderSpecNow :: IO Timestamp
  , tracerProviderSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , tracerProviderSpecSeed :: Seed
  , tracerProviderSpecIdGenerator :: forall a. (IdGeneratorSpec -> IO a) -> IO a
  , tracerProviderSpecSpanProcessors :: forall a. [(SpanProcessorSpec -> IO a) -> IO a]
  , tracerProviderSpecSampler :: forall a. (SamplerSpec -> IO a) -> IO a
  , tracerProviderSpecResource :: Resource Attrs
  , tracerProviderSpecSpanAttrsLimits :: AttrsLimits 'AttrsForSpan
  , tracerProviderSpecSpanEventAttrsLimits :: AttrsLimits 'AttrsForSpanEvent
  , tracerProviderSpecSpanLinkAttrsLimits :: AttrsLimits 'AttrsForSpanLink
  , tracerProviderSpecCallStackAttrs :: CallStack -> AttrsBuilder 'AttrsForSpan
  , tracerProviderSpecSpanContextMeta :: SpanContext -> [Pair]
  }

defaultTracerProviderSpec :: TracerProviderSpec
defaultTracerProviderSpec =
  TracerProviderSpec
    { tracerProviderSpecNow =
        fmap (timestampFromNanoseconds . toNanoSecs) $ getTime Realtime
    , tracerProviderSpecLogger = mempty
    , tracerProviderSpecSeed = defaultSystemSeed
    , tracerProviderSpecIdGenerator = with defaultIdGeneratorSpec
    , tracerProviderSpecSpanProcessors = mempty
    , tracerProviderSpecSampler = with defaultSamplerSpec
    , tracerProviderSpecResource =
        fromRight (error "defaultTracerProviderSpec: defaultResource is never a Left") $
          buildResourcePure $ defaultResourceBuilder "unknown_service"
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
    , tracerProviderSpecSpanContextMeta = \spanContext ->
        [ "spanContext" .= spanContext ]
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
withTracerProviderIO tracerProviderSpec action = do
  flip runLoggingT logger do
    logDebug "Acquiring tracer provider"
  shutdownRef <- newTVarIO False
  prngRef <- newPRNGRef seed
  defIdGenerator <- buildIdGenerator mempty defaultIdGeneratorSpec
  withIdGenerator \idGenerator -> do
    withSampler \sampler -> do
      withCompositeSpanProcessor \spanProcessor -> do
        let tracerProvider =
              TracerProvider
                { tracerProviderGetTracer =
                    getTracerWith prngRef defIdGenerator idGenerator sampler spanProcessor
                , tracerProviderShutdown = do
                    unlessSTM (readTVar shutdownRef) do
                      writeTVar shutdownRef True
                      pure $ spanProcessorShutdown spanProcessor
                , tracerProviderForceFlush = do
                    unlessSTM (readTVar shutdownRef) do
                      pure $ spanProcessorForceFlush spanProcessor
                }
        flip runLoggingT logger do
          logDebug $ "Acquired tracer provider" :#
            [ "resource" .= res
            , "limits" .= object
                [ "attributes" .= object
                    [ "span" .= spanAttrsLimits
                    , "spanEvent" .= spanEventAttrsLimits
                    , "spanLink" .= spanLinkAttrsLimits
                    ]
                ]
            ]
        action tracerProvider `finally` tracerProviderShutdown tracerProvider
  where
  withIdGenerator :: (IdGenerator -> IO r) -> IO r
  withIdGenerator =
    runContT do
      acquiredIdGenerator <- ContT idGeneratorSpec
      liftIO $ buildIdGenerator logger acquiredIdGenerator

  withSampler :: (Sampler -> IO r) -> IO r
  withSampler =
    runContT do
      acquiredSamplerSpec <- ContT samplerSpec
      liftIO $ buildSampler logger acquiredSamplerSpec

  withCompositeSpanProcessor :: (SpanProcessor -> IO r) -> IO r
  withCompositeSpanProcessor =
    runContT do
      acquiredSpanProcessorSpecs <- traverse ContT spanProcessorSpecs
      acquiredSpanProcessors <- do
        traverse (ContT . buildSpanProcessor res logger) acquiredSpanProcessorSpecs
      pure $ fold acquiredSpanProcessors

  getTracerWith
    :: MVar PRNG
    -> IdGenerator
    -> IdGenerator
    -> Sampler
    -> SpanProcessor
    -> InstrumentationScope
    -> IO Tracer
  getTracerWith prngRef defIdGenerator idGenerator sampler spanProcessor scope =
    flip runLoggingT logger do
      logDebug $ "Providing tracer" :# [ "instrumentationScope" .= scope ]
      pure Tracer
        { tracerInstrumentationScope = scope
        , tracerNow = now
        , tracerStartSpan = startSpan prngRef defIdGenerator idGenerator sampler scope spanProcessor
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
    -> IdGenerator
    -> IdGenerator
    -> Sampler
    -> InstrumentationScope
    -> SpanProcessor
    -> CallStack
    -> Context
    -> SpanSpec
    -> IO (MutableSpan, [Pair])
  startSpan prngRef defIdGenerator idGenerator sampler scope spanProcessor cs implicitParentContext spanSpec = do
    span <- buildSpan
    mutableSpan <- unsafeNewMutableSpan span

    when (spanIsRecording span) do
      spanProcessorOnSpanStart spanProcessor parentContext \updateSpanSpec -> do
        spanUpdater mutableSpan updateSpanSpec

    pure (mutableSpan, spanContextMeta $ spanContext span)
    where
    buildSpan :: IO (Span AttrsBuilder)
    buildSpan = do
      spanLineage <- spanLineageFromParentContext parentContext
      initSpanContext <- newSpanContext spanLineage

      samplingResult <- samplerShouldSample sampler SamplerInput
        { samplerInputContext = parentContext
        , samplerInputTraceId = spanContextTraceId initSpanContext
        , samplerInputSpanName = spanSpecName
        , samplerInputSpanKind = spanSpecKind
        , samplerInputSpanAttrs = spanSpecAttrs
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

      spanStart <- do
        case spanSpecStart of
          TimestampSourceAt timestamp -> pure timestamp
          TimestampSourceNow -> now

      pure Span
        { spanLineage
        , spanContext =
            spanContextPostSampling
              { spanContextTraceState = samplingResultTraceState samplingResult
              }
        , spanName = spanSpecName
        , spanStatus = SpanStatusUnset
        , spanStart
        , spanFrozenAt = Nothing
        , spanKind = spanSpecKind
        , spanAttrs =
            samplingResultSpanAttrs samplingResult
              <> callStackAttrs cs
              <> spanSpecAttrs
        , spanLinks
        , spanEvents = mempty
        , spanIsRecording
        , spanInstrumentationScope = scope
        }
      where
      spanLineageFromParentContext :: Context -> IO SpanLineage
      spanLineageFromParentContext context =
        case lookupContext contextKeySpan context of
          Nothing -> pure SpanLineageRoot
          Just mutableSpan -> do
            fmap (SpanLineageChildOf . spanContext) $ unsafeReadMutableSpan mutableSpan

      newSpanContext :: SpanLineage -> IO SpanContext
      newSpanContext spanLineage = do
        (spanContextTraceId, spanContextSpanId) <- do
          withMVar prngRef \prng -> do
            case spanLineage of
              SpanLineageRoot ->
                liftA2 (,) (genTraceId prng) (genSpanId prng)
                  `catchAny` \(SomeException ex) -> do
                    traceId <- idGeneratorGenTraceId defIdGenerator prng
                    spanId <- idGeneratorGenSpanId defIdGenerator prng
                    flip runLoggingT logger do
                      logError $ "Fell back to default trace/span ID gen due to exception" :#
                        [ "exception" .= displayException ex
                        , "traceId" .= traceId
                        , "spanId" .= spanId
                        ]
                    pure (traceId, spanId)
              SpanLineageChildOf scParent ->
                fmap (spanContextTraceId scParent,) (genSpanId prng)
                  `catchAny` \(SomeException ex) -> do
                    let traceId = spanContextTraceId scParent
                    spanId <- idGeneratorGenSpanId defIdGenerator prng
                    flip runLoggingT logger do
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
      SpanLinks $ flip fmap (unSpanLinkSpecs spanSpecLinks) \spanLinkSpec ->
        SpanLink
          { spanLinkSpanContext =
              spanLinkSpecSpanContext spanLinkSpec
          , spanLinkAttrs =
              spanLinkSpecAttrs spanLinkSpec
          }

    parentContext = fromMaybe implicitParentContext spanSpecParentContext

    IdGenerator
      { idGeneratorGenTraceId = genTraceId
      , idGeneratorGenSpanId = genSpanId
      } = idGenerator

    SpanSpec
      { spanSpecName
      , spanSpecParentContext
      , spanSpecStart
      , spanSpecKind
      , spanSpecAttrs
      , spanSpecLinks
      } = spanSpec

  TracerProviderSpec
    { tracerProviderSpecNow = now
    , tracerProviderSpecLogger = logger
    , tracerProviderSpecSeed = seed
    , tracerProviderSpecIdGenerator = idGeneratorSpec
    , tracerProviderSpecSpanProcessors = spanProcessorSpecs
    , tracerProviderSpecSampler = samplerSpec
    , tracerProviderSpecResource = res
    , tracerProviderSpecSpanAttrsLimits = spanAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = spanEventAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = spanLinkAttrsLimits
    , tracerProviderSpecCallStackAttrs = callStackAttrs
    , tracerProviderSpecSpanContextMeta = spanContextMeta
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
  :: forall a
   . Resource Attrs
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanProcessorSpec
  -> (SpanProcessor -> IO a)
  -> IO a
buildSpanProcessor res logger spanProcessorSpec = do
  runContT do
    spanExporterSpec <- ContT spanProcessorSpecExporter
    shutdownRef <- liftIO $ newTVarIO False
    flip runLoggingT logger do
      logDebug $ "Building span processor" :#
        [ "name" .= spanProcessorSpecName
        , "shutdownTimeout" .= shutdownTimeout
        , "forceFlushTimeout" .= forceFlushTimeout
        , "spanExporter" .= object
            [ "name" .= spanExporterSpecName spanExporterSpec
            , "shutdownTimeout" .= spanExporterSpecShutdownTimeout spanExporterSpec
            , "forceFlushTimeout" .= spanExporterSpecForceFlushTimeout spanExporterSpec
            ]
        ]
    spanExporter <- liftIO $ buildSpanExporter res logger spanExporterSpec
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
  , simpleSpanProcessorSpecExporter :: forall a. (SpanExporterSpec -> IO a) -> IO a
  , simpleSpanProcessorSpecOnSpansExported :: OnSpansExported ()
  }

defaultSimpleSpanProcessorSpec :: SimpleSpanProcessorSpec
defaultSimpleSpanProcessorSpec =
  SimpleSpanProcessorSpec
    { simpleSpanProcessorSpecExporter = with defaultSpanExporterSpec
    , simpleSpanProcessorSpecName = "simple"
    , simpleSpanProcessorSpecOnSpansExported = do
        askSpansExportedResult >>= \case
          SpanExportResultSuccess -> pure ()
          SpanExportResultFailure -> do
            spans <- askSpansExported
            pairs <- askSpansExportedMetadata
            logError $ "Exporter failed to export spans" :#
              "spans" .= fmap spanSummary spans : pairs
    }

simpleSpanProcessor
  :: forall a
   . SimpleSpanProcessorSpec
  -> (SpanProcessorSpec -> IO a)
  -> IO a
simpleSpanProcessor simpleSpanProcessorSpec =
  with defaultSpanProcessorSpec
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
  where

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
  , spanProcessorSpecExporter :: forall a. (SpanExporterSpec -> IO a) -> IO a
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
    , spanProcessorSpecExporter = with defaultSpanExporterSpec
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
  :: Resource Attrs
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanExporterSpec
  -> IO SpanExporter
buildSpanExporter res logger spanExporterSpec = do
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanExporter shutdownRef
  where
  spanExporter shutdownRef =
    SpanExporter
      { spanExporterExport = \spans onSpansExported -> do
          unlessSTM (readTVar shutdownRef) do
            pure do
              runSpanExporterM res logger onTimeout onEx defaultTimeout metaExport do
                spanExporterSpecExport spans \spanExportResult -> do
                  liftIO $ onSpansExported spanExportResult
      , spanExporterShutdown = do
          unlessSTM (readTVar shutdownRef) do
            writeTVar shutdownRef True
            pure do
              runSpanExporterM res logger onTimeout onEx shutdownTimeout metaShutdown do
                spanExporterSpecShutdown
      , spanExporterForceFlush = do
          unlessSTM (readTVar shutdownRef) do
            pure do
              runSpanExporterM res logger onTimeout onEx forceFlushTimeout metaForceFlush do
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

data OTLPSpanExporterSpec = OTLPSpanExporterSpec
  { otlpSpanExporterSpecManager :: Manager
  , otlpSpanExporterSpecEndpoint :: URI
  , otlpSpanExporterSpecTimeout :: Int
  , otlpSpanExporterSpecProtocol :: OTLPProtocol
    -- | A list of headers to include when communicating with the observability
    -- backend (e.g. Honeycomb) over HTTP.
    --
    -- Use this list to include the necessary secrets for talking with your
    -- observability backend(s).
  , otlpSpanExporterSpecHeaders :: [Header]
    -- | A list of sensitive header names that will be redacted before a
    -- 'Request' is displayed. Note that the only time a 'Request' is displayed
    -- is when the span exporter encounters an 'HttpException' when
    -- communicating with the observability backend (e.g. Honeycomb). The
    -- default is 'mempty'.
    --
    -- Use this list to avoid leaking sensitive data like API keys into your
    -- logs:
    --
    -- @
    -- 'defaultOTLPSpanExporterSpec'
    --   { 'otlpSpanExporterSpecRedactedRequestHeaders' = ["x-honeycomb-team"]
    --   }
    -- @
  , otlpSpanExporterSpecRedactedRequestHeaders :: [HeaderName]
    -- | A list of sensitive header names that will be redacted before a
    -- 'Response' is displayed. Note that the only time a 'Response' is
    -- displayed is when the span exporter encounters an 'HttpException' when
    -- communicating with the observability backend (e.g. Honeycomb). The
    -- default is 'mempty'.
    --
    -- Use this list to avoid leaking sensitive data like API keys into your
    -- logs.
    --
    -- @
    -- 'defaultOTLPSpanExporterSpec'
    --   { 'otlpSpanExporterSpecRedactedResponseHeaders' = ["x-honeycomb-team"]
    --   }
    -- @
  , otlpSpanExporterSpecRedactedResponseHeaders :: [HeaderName]
  , otlpSpanExporterSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , otlpSpanExporterSpecWorkerQueueSize :: Int
  , otlpSpanExporterSpecWorkerCount :: Int
  }

-- TODO: Env vars
defaultOTLPSpanExporterSpec :: OTLPSpanExporterSpec
defaultOTLPSpanExporterSpec =
  OTLPSpanExporterSpec
    { otlpSpanExporterSpecManager = defaultManager
    , otlpSpanExporterSpecEndpoint = fromJust $ parseURI "http://localhost:4318/v1/traces"
    , otlpSpanExporterSpecTimeout = 10_000_000
    , otlpSpanExporterSpecProtocol = httpProtobufProtocol
    , otlpSpanExporterSpecHeaders = mempty
    , otlpSpanExporterSpecRedactedRequestHeaders = mempty
    , otlpSpanExporterSpecRedactedResponseHeaders = mempty
    , otlpSpanExporterSpecLogger = mempty
    , otlpSpanExporterSpecWorkerQueueSize =
        concurrentWorkersSpecQueueSize defaultConcurrentWorkersSpec
    , otlpSpanExporterSpecWorkerCount =
        concurrentWorkersSpecWorkerCount defaultConcurrentWorkersSpec
    }

otlpSpanExporter
  :: forall a
   . OTLPSpanExporterSpec
  -> (SpanExporterSpec -> IO a)
  -> IO a
otlpSpanExporter otlpSpanExporterSpec f = do
  req <- do
    flip fmap (requestFromURI endpoint) \baseReq ->
      setRequestCheckStatus baseReq
        { method = "POST"
        , requestHeaders =
            DList.toList $ mconcat
              [ DList.singleton (hContentType, "application/x-protobuf")
              , DList.fromList headers
              , DList.fromList $ requestHeaders baseReq
              ]
        }
  withConcurrentWorkers (concurrentWorkersSpec req) \workers -> do
    f $ spanExporterSpec workers
  where
  spanExporterSpec workers =
    defaultSpanExporterSpec
      { spanExporterSpecName = "otlp"
      , spanExporterSpecExport = \spans onSpansExported -> do
          res <- askResource
          liftIO $ concurrentWorkersEnqueueItem workers OTLPSpanExporterItem
            { otlpSpanExporterItemBatch = spans
            , otlpSpanExporterItemCallback = onSpansExported
            , otlpSpanExporterResource = res
            }
      , spanExporterSpecShutdown = do
          liftIO $ concurrentWorkersStopWorkers workers
      }

  concurrentWorkersSpec req =
    defaultConcurrentWorkersSpec
      { concurrentWorkersSpecQueueSize = queueSize
      , concurrentWorkersSpecWorkerCount = workerCount
      , concurrentWorkersSpecProcessItem = \item -> do
          mResp <- do
            timeout exportTimeout do
              send req
                { requestBody =
                    RequestBodyBS
                      $ ProtoLens.encodeMessage
                      $ exportTraceServiceRequest (otlpSpanExporterResource item)
                      $ otlpSpanExporterItemBatch item
                }
          flip runLoggingT logger do
            case mResp of
              Just _resp -> pure ()
              Nothing -> do
                logError $ "Exporting spans timed out" :#
                  "spans" .= fmap spanSummary (otlpSpanExporterItemBatch item) : loggingMeta
          otlpSpanExporterItemCallback item SpanExportResultSuccess
      , concurrentWorkersSpecOnException = \item -> do
          SomeException ex <- do
            redactHttpExceptionHeaders redactedReqHeaders redactedRespHeaders <$> askException
          pairs <- askExceptionMetadata
          logError $ "Concurrent worker ignoring exception from exporting batch" :#
            "exception" .= displayException ex
              : "batch" .= fmap spanSummary (otlpSpanExporterItemBatch item)
              : pairs
          liftIO $ otlpSpanExporterItemCallback item SpanExportResultFailure
      , concurrentWorkersSpecLogger = logger
      , concurrentWorkersSpecLoggingMeta = loggingMeta
      }

  send :: Request -> IO (Response ByteString)
  send req = do
    recoveringDynamic retryPolicy handlers \_retryStatus -> do
      httpLbs req manager
    where
    retryPolicy :: RetryPolicyM IO
    retryPolicy = fullJitterBackoff 5_000 <> limitRetries 10

    handlers :: [RetryStatus -> Handler IO RetryAction]
    handlers =
      [ const $ Handler \(_ :: AsyncException) -> pure DontRetry
      , const $ Handler \(_ :: SomeAsyncException) -> pure DontRetry
      , \retryStatus -> Handler \case
          InvalidUrlException {} -> pure DontRetry
          HttpExceptionRequest _req httpExceptionContent -> do
            case httpExceptionContent of
              ConnectionClosed {} -> consult retryStatus "ConnectionClosed" Nothing Nothing
              ConnectionFailure someEx -> consult retryStatus "ConnectionFailure" (Just someEx) Nothing
              ConnectionTimeout {} -> consult retryStatus "ConnectionTimeout" Nothing Nothing
              InternalException someEx -> consult retryStatus "InternalException" (Just someEx) Nothing
              ResponseTimeout {} -> consult retryStatus "ResponseTimeout" Nothing Nothing
              StatusCodeException resp _bs
                | responseStatus resp `elem` [tooManyRequests429, serviceUnavailable503] ->
                    checkRetryAfterHeader retryStatus resp
                | otherwise -> pure DontRetry
              _ -> pure DontRetry
      ]
      where
      checkRetryAfterHeader :: RetryStatus -> Response () -> IO RetryAction
      checkRetryAfterHeader retryStatus resp = do
        case lookup hRetryAfter $ responseHeaders resp of
          Nothing -> consult retryStatus "StatusCodeException" Nothing Nothing
          Just headerVal -> do
            case parseRetryAfterHeader $ ByteString.Char8.unpack headerVal of
              Nothing -> consult retryStatus (pack $ show code) Nothing Nothing
              Just (Left delay) -> do
                consult retryStatus (pack $ show code) Nothing $ Just $ ceiling $ 1_000_000 * delay
              Just (Right httpDate) -> do
                delay <- fmap (diffUTCTime httpDate) getCurrentTime
                consult retryStatus (pack $ show code) Nothing $ Just $ ceiling $ 1_000_000 * delay
        where
        parseRetryAfterHeader :: String -> Maybe (Either NominalDiffTime UTCTime)
        parseRetryAfterHeader headerVal =
          fmap Left parseNominalDiffTime <|> fmap Right parseHttpDate <|> Nothing
          where
          parseNominalDiffTime = parseTimeM True defaultTimeLocale "%s" headerVal
          parseHttpDate = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" headerVal

        code :: Int
        code = statusCode $ responseStatus resp

      consult :: RetryStatus -> Text -> Maybe SomeException -> Maybe Int -> IO RetryAction
      consult retryStatus hint mSomeEx mOverrideDelay =
        flip runLoggingT logger do
          liftIO (applyPolicy retryPolicy retryStatus) >>= \case
            Nothing -> do
              logError $ "Span export exceeded maximum retries due to exceptions" :# meta
              pure DontRetry
            Just {} -> do
              logDebug $ "Retrying span export due to HTTP client exception" :# meta
              pure $ maybe ConsultPolicy ConsultPolicyOverrideDelay mOverrideDelay
        where
        meta :: [SeriesElem]
        meta =
          foldr ($) loggingMeta
            [ ("hint" .= hint :)
            , maybe id (\e -> ("exception" .= displayException e :)) mSomeEx
            ]

  exportTraceServiceRequest
    :: Resource Attrs
    -> Batch (Span Attrs)
    -> OTLP.Collector.ExportTraceServiceRequest
  exportTraceServiceRequest res batch =
    ProtoLens.defMessage
      & OTLP.Collector.resourceSpans .~
          [ ProtoLens.defMessage
              & maybe id (\x -> OTLP.Trace.schemaUrl .~ schemaURLToText x) (resourceSchemaURL res)
              & OTLP.Trace.resource .~ convertResourceAttrs
              & OTLP.Trace.scopeSpans .~ mapMaybe convertSpanGroup groupedSpans
          ]
    where
    convertResourceAttrs :: OTLP.Resource.Resource
    convertResourceAttrs =
      ProtoLens.defMessage
        & OTLP.Resource.attributes .~ convertAttrKVs (resourceAttrs res)

    groupedSpans :: [[Span Attrs]]
    groupedSpans =
      unBatch batch
        & List.sortOn spanInstrumentationScope
        & List.groupBy ((==) `on` spanInstrumentationScope)

    convertSpanGroup :: [Span Attrs] -> Maybe OTLP.Trace.ScopeSpans
    convertSpanGroup spans =
      case spans of
        [] -> Nothing
        span : _ ->
          ProtoLens.defMessage
            & OTLP.Trace.scope .~ convertInstScope instScope
            & OTLP.Trace.spans .~ fmap convertSpan spans
            & maybe id (\x -> OTLP.Trace.schemaUrl .~ schemaURLToText x) schemaURL
            & Just
          where
          InstrumentationScope { instrumentationScopeSchemaURL = schemaURL } = instScope
          instScope = spanInstrumentationScope span

    convertSpan :: Span Attrs -> OTLP.Trace.Span
    convertSpan span =
      ProtoLens.defMessage
        & OTLP.Trace.traceId .~ bytesBuilderToBS8 (traceIdToBytesBuilder traceId)
        & OTLP.Trace.spanId .~ bytesBuilderToBS8 (spanIdToBytesBuilder spanId)
        -- TODO: & OTLP.Trace.traceState .~ undefined
        & maybe id (\x -> OTLP.Trace.parentSpanId .~ bytesBuilderToBS8 (spanIdToBytesBuilder x)) mParentSpanId
        & OTLP.Trace.name .~ unSpanName spanName
        & OTLP.Trace.kind .~ convertSpanKind spanKind
        & OTLP.Trace.startTimeUnixNano .~
            fromIntegral (timestampToNanoseconds spanStart)
        & OTLP.Trace.endTimeUnixNano .~
            fromIntegral (timestampToNanoseconds $ frozenTimestamp spanFrozenAt)
        & OTLP.Trace.attributes .~ convertAttrKVs spanAttrs
        & OTLP.Trace.droppedAttributesCount .~ fromIntegral (droppedAttrsCount spanAttrs)
        & OTLP.Trace.events .~ fmap convertSpanEvent (spanEventsToList spanEvents)
        & OTLP.Trace.links .~ fmap convertSpanLink (spanLinksToList spanLinks)
        & OTLP.Trace.status .~ convertSpanStatus spanStatus
      where
      mParentSpanId =
        case spanLineage of
          SpanLineageRoot -> Nothing
          SpanLineageChildOf parentSpanContext ->
            Just $ spanContextSpanId parentSpanContext


      Span
        { spanLineage
        , spanContext =
            SpanContext
              { spanContextTraceId = traceId
              , spanContextSpanId = spanId
              --, spanContextTraceState = traceState
              }
        , spanName
        , spanStatus
        , spanStart
        , spanFrozenAt
        , spanKind
        , spanAttrs
        , spanLinks
        , spanEvents
        } = span

    convertSpanEvent :: SpanEvent Attrs -> OTLP.Trace.Span'Event
    convertSpanEvent spanEvent =
      ProtoLens.defMessage
        & OTLP.Trace.timeUnixNano .~ fromIntegral (timestampToNanoseconds timestamp)
        & OTLP.Trace.name .~ unSpanEventName name
        & OTLP.Trace.attributes .~ convertAttrKVs attrs
        & OTLP.Trace.droppedAttributesCount .~ fromIntegral (droppedAttrsCount attrs)
      where
      SpanEvent
        { spanEventName = name
        , spanEventTimestamp = timestamp
        , spanEventAttrs = attrs
        } = spanEvent

    convertSpanLink :: SpanLink Attrs -> OTLP.Trace.Span'Link
    convertSpanLink spanLink =
      ProtoLens.defMessage
        & OTLP.Trace.traceId .~ bytesBuilderToBS8 (traceIdToBytesBuilder traceId)
        & OTLP.Trace.spanId .~ bytesBuilderToBS8 (spanIdToBytesBuilder spanId)
        -- TODO: & OTLP.Trace.traceState .~ undefined
        & OTLP.Trace.attributes .~ convertAttrKVs attrs
        & OTLP.Trace.droppedAttributesCount .~ fromIntegral (droppedAttrsCount attrs)
      where
      SpanLink
        { spanLinkSpanContext =
            SpanContext
              { spanContextTraceId = traceId
              , spanContextSpanId = spanId
              --, spanContextTraceState = traceState
              }
        , spanLinkAttrs = attrs
        } = spanLink

    convertAttrKVs :: Attrs af -> [OTLP.Common.KeyValue]
    convertAttrKVs =
      DList.toList . foldMapWithKeyAttrs \k v -> pure $ convertAttrKV k v

    convertAttrKV :: Key typ -> Attr typ -> OTLP.Common.KeyValue
    convertAttrKV k v =
      ProtoLens.defMessage
        & OTLP.Common.key .~ unKey k
        & OTLP.Common.value .~ convertAttrValue v

    convertAttrValue :: Attr typ -> OTLP.Common.AnyValue
    convertAttrValue attr =
      ProtoLens.defMessage @OTLP.Common.AnyValue
        & case attrType attr of
            AttrTypeText -> OTLP.Common.stringValue .~ attrVal attr
            AttrTypeBool -> OTLP.Common.boolValue .~ attrVal attr
            AttrTypeInt -> OTLP.Common.intValue .~ attrVal attr
            AttrTypeDouble -> OTLP.Common.doubleValue .~ attrVal attr
            AttrTypeTextArray ->
              OTLP.Common.arrayValue .~
                ( ProtoLens.defMessage
                    & OTLP.Common.vec'values .~ convertTextArrayAttrVals (attrVal attr)
                )
            AttrTypeBoolArray ->
              OTLP.Common.arrayValue .~
                ( ProtoLens.defMessage
                    & OTLP.Common.vec'values .~ convertBoolArrayAttrVals (attrVal attr)
                )
            AttrTypeIntArray ->
              OTLP.Common.arrayValue .~
                ( ProtoLens.defMessage
                    & OTLP.Common.vec'values .~ convertIntArrayAttrVals (attrVal attr)
                )
            AttrTypeDoubleArray ->
              OTLP.Common.arrayValue .~
                ( ProtoLens.defMessage
                    & OTLP.Common.vec'values .~ convertDoubleArrayAttrVals (attrVal attr)
                )

    convertTextArrayAttrVals :: AttrVals Text -> Vector OTLP.Common.AnyValue
    convertTextArrayAttrVals =
      fmap (\x -> ProtoLens.defMessage & OTLP.Common.stringValue .~ x) . unAttrVals

    convertBoolArrayAttrVals :: AttrVals Bool -> Vector OTLP.Common.AnyValue
    convertBoolArrayAttrVals =
      fmap (\x -> ProtoLens.defMessage & OTLP.Common.boolValue .~ x) . unAttrVals

    convertIntArrayAttrVals :: AttrVals Int64 -> Vector OTLP.Common.AnyValue
    convertIntArrayAttrVals =
      fmap (\x -> ProtoLens.defMessage & OTLP.Common.intValue .~ x) . unAttrVals

    convertDoubleArrayAttrVals :: AttrVals Double -> Vector OTLP.Common.AnyValue
    convertDoubleArrayAttrVals =
      fmap (\x -> ProtoLens.defMessage & OTLP.Common.doubleValue .~ x) . unAttrVals

    convertSpanStatus :: SpanStatus -> OTLP.Trace.Status
    convertSpanStatus = \case
      SpanStatusUnset ->
        ProtoLens.defMessage
          & OTLP.Trace.code .~ OTLP.Trace.Status'STATUS_CODE_UNSET
      SpanStatusOk ->
        ProtoLens.defMessage
          & OTLP.Trace.code .~ OTLP.Trace.Status'STATUS_CODE_OK
      SpanStatusError errText ->
        ProtoLens.defMessage
          & OTLP.Trace.code .~ OTLP.Trace.Status'STATUS_CODE_ERROR
          & OTLP.Trace.message .~ errText

    convertSpanKind :: SpanKind -> OTLP.Trace.Span'SpanKind
    convertSpanKind = \case
      SpanKindServer -> OTLP.Trace.Span'SPAN_KIND_SERVER
      SpanKindClient -> OTLP.Trace.Span'SPAN_KIND_CLIENT
      SpanKindProducer -> OTLP.Trace.Span'SPAN_KIND_PRODUCER
      SpanKindConsumer -> OTLP.Trace.Span'SPAN_KIND_CONSUMER
      SpanKindInternal -> OTLP.Trace.Span'SPAN_KIND_INTERNAL

    convertInstScope :: InstrumentationScope -> OTLP.Common.InstrumentationScope
    convertInstScope instScope =
      -- N.B. There are 'attributes' and 'droppedAttributesCount' fields available too.
      ProtoLens.defMessage
        & OTLP.Common.name .~ unInstrumentationScopeName name
        & maybe id (\x -> OTLP.Common.version .~ unVersion x) version
      where
      InstrumentationScope
        { instrumentationScopeName = name
        , instrumentationScopeVersion = version
        } = instScope

    bytesBuilderToBS8 = ByteString.Char8.toStrict . Builder.toLazyByteString

  loggingMeta =
    [ "spanExporter" .= object
        [ "name" .= ("otlp" :: Text)
        ]
    ]

  redactedReqHeaders = Set.fromList redactedReqHeadersList
  redactedRespHeaders = Set.fromList redactedRespHeadersList

  OTLPSpanExporterSpec
    { otlpSpanExporterSpecManager = manager
    , otlpSpanExporterSpecEndpoint = endpoint
    , otlpSpanExporterSpecTimeout = exportTimeout
    , otlpSpanExporterSpecProtocol = _protocol -- N.B. Only http/protobuf is supported
    , otlpSpanExporterSpecHeaders = headers
    , otlpSpanExporterSpecRedactedRequestHeaders = redactedReqHeadersList
    , otlpSpanExporterSpecRedactedResponseHeaders = redactedRespHeadersList
    , otlpSpanExporterSpecLogger = logger
    , otlpSpanExporterSpecWorkerQueueSize = queueSize
    , otlpSpanExporterSpecWorkerCount = workerCount
    } = otlpSpanExporterSpec

data OTLPProtocol
  = OTLPProtocolHTTPProtobuf

httpProtobufProtocol :: OTLPProtocol
httpProtobufProtocol = OTLPProtocolHTTPProtobuf

-- | Little ad-hoc helper type for use in 'otlpSpanExporterIO'.
data OTLPSpanExporterItem = OTLPSpanExporterItem
  { otlpSpanExporterItemBatch :: Batch (Span Attrs)
  , otlpSpanExporterItemCallback :: SpanExportResult -> IO ()
  , otlpSpanExporterResource :: Resource Attrs
  }

instance ToJSON OTLPSpanExporterItem where
  toJSON = toJSON . otlpSpanExporterItemBatch

stmSpanExporter
  :: forall a
   . TMQueue (Span Attrs)
  -> (SpanExporterSpec -> IO a)
  -> IO a
stmSpanExporter queue =
  with defaultSpanExporterSpec
    { spanExporterSpecName = "stm"
    , spanExporterSpecExport = \spans onSpansExported -> do
        join $ liftIO $ atomically do
          traverse_ (writeTMQueue queue) spans
          pure $ liftIO $ onSpansExported SpanExportResultSuccess
    , spanExporterSpecShutdown = do
        liftIO $ atomically $ closeTMQueue queue
    }

data SpanExporterSpec = SpanExporterSpec
  { spanExporterSpecName :: Text
  , spanExporterSpecExport
      :: Batch (Span Attrs)
      -> (SpanExportResult -> IO ())
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
  flip runLoggingT logger do
    logDebug $ "Building sampler" :#
      [ "name" .= samplerSpecName
      , "description" .= samplerSpecDescription
      ]
  pure Sampler
    { samplerName = samplerSpecName
    , samplerDescription = samplerSpecDescription
    , samplerShouldSample = \samplerInput -> do
        runSamplerM logger onEx metaShouldSample do
          samplerSpecShouldSample samplerInput
    }
  where
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
defaultSamplerSpec = alwaysOffSampler'

alwaysOnSampler
  :: forall a
   . (SamplerSpec -> IO a)
  -> IO a
alwaysOnSampler = with alwaysOnSampler'

alwaysOnSampler' :: SamplerSpec
alwaysOnSampler' =
  (constDecisionSampler SamplingDecisionRecordAndSample)
    { samplerSpecName = "AlwaysOn"
    , samplerSpecDescription = "AlwaysOnSampler"
    }

alwaysOffSampler
  :: forall a
   . (SamplerSpec -> IO a)
  -> IO a
alwaysOffSampler = with alwaysOffSampler'

alwaysOffSampler' :: SamplerSpec
alwaysOffSampler' =
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
    , parentBasedSamplerSpecOnRemoteParentSampled = alwaysOnSampler'
    , parentBasedSamplerSpecOnRemoteParentNotSampled = alwaysOffSampler'
    , parentBasedSamplerSpecOnLocalParentSampled = alwaysOnSampler'
    , parentBasedSamplerSpecOnLocalParentNotSampled = alwaysOffSampler'
    }

parentBasedSampler
  :: forall a
   . ParentBasedSamplerSpec
  -> (SamplerSpec -> IO a)
  -> IO a
parentBasedSampler parentBasedSamplerSpec =
  with defaultSamplerSpec
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

samplingDecisionDrop :: SamplingDecision
samplingDecisionDrop = SamplingDecisionDrop

samplingDecisionRecordOnly :: SamplingDecision
samplingDecisionRecordOnly = SamplingDecisionRecordOnly

samplingDecisionRecordAndSample :: SamplingDecision
samplingDecisionRecordAndSample = SamplingDecisionRecordAndSample

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
  { unSpanExporterM :: Resource Attrs -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger, MonadLoggerIO -- @monad-logger@
      ) via (ReaderT (Resource Attrs) (LoggingT IO))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (Resource Attrs) (LoggingT IO)) a)

runSpanExporterM
  :: Resource Attrs
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnTimeout a
  -> OnException a
  -> Int
  -> [SeriesElem]
  -> SpanExporterM a
  -> IO a
runSpanExporterM res logger onTimeout onEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanExporterM action res `catchAny` \someEx -> do
          runOnException onEx someEx pairs
    case mResult of
      Just x -> pure x
      Nothing -> runOnTimeout onTimeout timeoutMicros pairs

askResource :: SpanExporterM (Resource Attrs)
askResource = SpanExporterM pure

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
  :: PRNG
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IdGeneratorM a
  -> IO a
runIdGeneratorM prng logger action = do
  flip runLoggingT logger do
    unIdGeneratorM action prng

data IdGenerator = IdGenerator
  { idGeneratorGenTraceId :: PRNG -> IO TraceId
  , idGeneratorGenSpanId :: PRNG -> IO SpanId
  }

buildIdGenerator
  :: forall m
   . (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IdGeneratorSpec
  -> m IdGenerator
buildIdGenerator logger idGeneratorSpec = do
  flip runLoggingT logger do
    logDebug $ "Building ID generator" :#
      [ "name" .= name
      ]
  pure IdGenerator
    { idGeneratorGenTraceId = \prng -> do
        runIdGeneratorM prng logger genTraceId
    , idGeneratorGenSpanId = \prng -> do
        runIdGeneratorM prng logger genSpanId
    }
  where
  IdGeneratorSpec
    { idGeneratorSpecName = name
    , idGeneratorSpecGenTraceId = genTraceId
    , idGeneratorSpecGenSpanId = genSpanId
    } = idGeneratorSpec

data IdGeneratorSpec = IdGeneratorSpec
  { idGeneratorSpecName :: Text
  , idGeneratorSpecGenTraceId :: IdGeneratorM TraceId
  , idGeneratorSpecGenSpanId :: IdGeneratorM SpanId
  }

defaultIdGeneratorSpec :: IdGeneratorSpec
defaultIdGeneratorSpec =
  IdGeneratorSpec
    { idGeneratorSpecName = "default"
    , idGeneratorSpecGenTraceId = liftA2 traceIdFromWords genUniform genUniform
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
  { unBatch :: [a]
  } deriving stock (Eq, Show)
    deriving (Monoid, Semigroup, ToJSON) via [a]
    deriving (Foldable, Functor, Applicative, Monad) via []

instance Traversable Batch where
  traverse f (Batch xs) = fmap Batch $ traverse f xs

singletonBatch :: a -> Batch a
singletonBatch = Batch . pure

fromListBatch :: [a] -> Batch a
fromListBatch = Batch

data ConcurrentWorkersSpec item = ConcurrentWorkersSpec
  { concurrentWorkersSpecQueueSize :: Int
  , concurrentWorkersSpecWorkerCount :: Int
  , concurrentWorkersSpecProcessItem :: item -> IO ()
  , concurrentWorkersSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , concurrentWorkersSpecLoggingMeta :: [SeriesElem]
  , concurrentWorkersSpecOnException :: item -> OnException ()
  }

defaultConcurrentWorkersSpec :: ConcurrentWorkersSpec item
defaultConcurrentWorkersSpec =
  ConcurrentWorkersSpec
    { concurrentWorkersSpecQueueSize = 2048
    , concurrentWorkersSpecWorkerCount = 5
    , concurrentWorkersSpecProcessItem = mempty
    , concurrentWorkersSpecLogger = mempty
    , concurrentWorkersSpecLoggingMeta = mempty
    , concurrentWorkersSpecOnException = \_item -> do
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
  :: forall item a
   . (ToJSON item, Typeable item)
  => ConcurrentWorkersSpec item
  -> (ConcurrentWorkers item -> IO a)
  -> IO a
withConcurrentWorkers concurrentWorkersSpec action = do
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
          logError $ "Dropped item as queue was full" :# loggingMeta
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
          liftIO (evaluate =<< processItem item) `catchAny` \someEx -> do
              runOnException (onEx item) someEx pairs
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

withAll
  :: forall a b
   . [(a -> b) -> b]
  -> ([a] -> b)
  -> b
withAll = runCont . traverse cont

defaultSystemSeed :: Seed
defaultSystemSeed = unsafePerformIO createSystemSeed
{-# NOINLINE defaultSystemSeed #-}

defaultManager :: Manager
defaultManager = unsafePerformIO newTlsManager
{-# NOINLINE defaultManager #-}

spanSummary :: Span attrs -> Value
spanSummary s =
  object
    [ "lineage" .= spanLineage s
    , "spanContext" .= spanContext s
    , "name" .= spanName s
    ]

redactHttpExceptionHeaders
  :: Set HeaderName
  -> Set HeaderName
  -> SomeException
  -> SomeException
redactHttpExceptionHeaders redactsForReq redactsForResp someEx =
  fromMaybe someEx do
    HttpExceptionRequest req content <- fromException someEx
    pure $ toException $ go req content
  where
  go
    :: Request
    -> HttpExceptionContent
    -> HttpException
  go req content =
    HttpExceptionRequest (redactReqHeaders req)
      case content of
        StatusCodeException resp bs ->
          StatusCodeException (redactRespHeaders resp) bs
        TooManyRedirects resps ->
          TooManyRedirects $ fmap redactRespHeaders resps
        x -> x

  redactReqHeaders :: Request -> Request
  redactReqHeaders req =
    req
      { requestHeaders =
          redactSensitiveHeader redactsForReq <$> requestHeaders req
      }

  redactRespHeaders :: Response a -> Response a
  redactRespHeaders resp =
    resp
      { responseHeaders =
          redactSensitiveHeader redactsForResp <$> responseHeaders resp
      , responseOriginalRequest =
          (responseOriginalRequest resp)
            { requestHeaders =
                redactSensitiveHeader redactsForReq <$> requestHeaders (responseOriginalRequest resp)
            }
      }

  -- This function exists in @Network.HTTP.Client.Types@ but is not exported. We
  -- inline it here and use it rather than relying on the user having
  -- @http-client.0.7.13@ which would let us use @redactHeaders@ on the request
  -- directly. There also isn't a @Response@ analogue of @redactHeaders@, so
  -- we'd need a more general-purpose function like @redactSensitiveHeader@
  -- anyways.
  redactSensitiveHeader :: Set HeaderName -> Header -> Header
  redactSensitiveHeader toRedact h@(name, _) =
    if name `Set.member` toRedact then
      (name, "<REDACTED>")
    else
      h

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
