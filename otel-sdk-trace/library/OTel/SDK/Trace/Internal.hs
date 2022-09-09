{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Concurrent (MVar, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async
  ( AsyncCancelled(..), Async, uninterruptibleCancel, waitCatch, withAsync
  )
import Control.Concurrent.STM (STM, atomically, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBMQueue
import Control.Exception.Safe
  ( Exception(..), SomeException(..), MonadCatch, MonadMask, MonadThrow, catchAny, finally
  )
import Control.Monad (join, unless, void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( LoggingT(..), Message(..), (.=), Loc, LogLevel, LogSource, LogStr, MonadLogger, SeriesElem
  , logDebug, logError
  )
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (object)
import Data.DList (DList)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import Data.Text (Text)
import Data.Vector (Vector)
import OTel.API.Context (ContextT(runContextT), ContextKey, updateContext)
import OTel.API.Context.Internal (newContextKey)
import OTel.API.Core
  ( SpanParent(..), SpanSpec(..), SpanStatus(..), Attrs, AttrsBuilder, InstrumentationScope
  , SpanAttrsLimits, SpanContext, SpanEventAttrsLimits, SpanId, SpanLinkAttrsLimits, Timestamp
  , TraceId, UpdateSpanSpec, defaultAttrsLimits, emptySpanContext, spanContextIsRemote
  , spanContextSpanId, spanContextTraceFlags, spanContextTraceId, spanContextTraceState
  , spanIdFromWords, spanParentContext, timestampFromNanoseconds, traceIdFromWords
  )
import OTel.API.Core.Internal
  ( MutableSpan(..), Span(..), SpanBackend(..), TraceFlags(..), TraceState(..), Tracer(..)
  , TracerProvider(..), buildSpanUpdater, freezeSpan
  )
import OTel.API.Trace (defaultSpanBackend)
import Prelude hiding (span)
import System.Clock (Clock(Realtime), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (Variate(..), GenIO, Seed, createSystemSeed, fromSeed, initialize, uniform)
import System.Timeout (timeout)
import qualified Control.Exception.Safe as Exception
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Vector as Vector

data TracerProviderSpec = TracerProviderSpec
  { tracerProviderSpecNow :: IO Timestamp
  , tracerProviderSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , tracerProviderSpecSeed :: Seed
  , tracerProviderSpecIdGenerator :: IdGeneratorSpec
  , tracerProviderSpecSpanProcessors :: [SpanProcessorSpec]
  , tracerProviderSpecSpanAttrsLimits :: SpanAttrsLimits
  , tracerProviderSpecSpanEventAttrsLimits :: SpanEventAttrsLimits
  , tracerProviderSpecSpanLinkAttrsLimits :: SpanLinkAttrsLimits
  , tracerProviderSpecSpanBackend :: SpanBackend
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
    , tracerProviderSpecSpanAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanBackend = defaultSpanBackend
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
  shutdownRef <- liftIO $ newTVarIO False
  prngRef <- newPRNGRef seed
  spanProcessor <- liftIO $ foldMap (buildSpanProcessor logger) spanProcessorSpecs
  pure TracerProvider
    { tracerProviderGetTracer =
        pure . getTracerWith prngRef spanProcessor
    , tracerProviderShutdown = do
        unlessShutdown (readTVar shutdownRef) do
          writeTVar shutdownRef True
          pure $ spanProcessorShutdown spanProcessor
    , tracerProviderForceFlush = do
        unlessShutdown (readTVar shutdownRef) do
          pure $ spanProcessorForceFlush spanProcessor
    }
  where
  getTracerWith
    :: MVar PRNG
    -> SpanProcessor
    -> InstrumentationScope
    -> Tracer
  getTracerWith prngRef spanProcessor scope =
    Tracer
      { tracerInstrumentationScope = scope
      , tracerNow = now
      , tracerStartSpan = startSpan prngRef scope spanProcessor
      , tracerProcessSpan = endSpan spanProcessor
      , tracerSpanBackend = ctxBackendSpan
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
    -> InstrumentationScope
    -> SpanProcessor
    -> SpanSpec
    -> IO MutableSpan
  startSpan prngRef scope spanProcessor spanSpec = do
    let spanParent = spanSpecParent spanSpec
    let mParentSpanContext = spanParentContext spanParent

    spanContext <- newSpanContext prngRef spanParent
    let span = Span
          { spanParent
          , spanContext
          , spanName = spanSpecName spanSpec
          , spanStatus = SpanStatusUnset
          , spanStart = spanSpecStart spanSpec
          , spanFrozenAt = Nothing
          , spanKind = spanSpecKind spanSpec
          , spanAttrs = spanSpecAttrs spanSpec
          , spanLinks = spanSpecLinks spanSpec
          , spanEvents = mempty
          , spanIsRecording = True -- TODO: Implement!
          , spanInstrumentationScope = scope
          }

    mutableSpan@MutableSpan { mutableSpanSpanKey = spanKey } <- do
      fmap MutableSpan $ newContextKey "spanKey" span

    when (spanIsRecording span) do
      -- TODO: Fetch baggage from context and pass along too
      spanProcessorOnSpanStart spanProcessor mParentSpanContext $ spanUpdater spanKey

    pure mutableSpan

  newSpanContext
    :: MVar PRNG
    -> SpanParent
    -> IO SpanContext
  newSpanContext prngRef spanParent = do
    (spanContextTraceId, spanContextSpanId) <- do
      runIdGeneratorM prngRef logger
        case spanParent of
          SpanParentRoot ->
            liftA2 (,) genTraceId genSpanId
              `catchAny` \(SomeException ex) -> do
                logError $ "Falling back to default trace/span ID gen due to exception" :#
                  [ "exception" .= displayException ex ]
                traceId <- idGeneratorSpecGenTraceId defaultIdGeneratorSpec
                spanId <- idGeneratorSpecGenSpanId defaultIdGeneratorSpec
                pure (traceId, spanId)
          SpanParentChildOf scParent ->
            fmap (spanContextTraceId scParent,) genSpanId
              `catchAny` \(SomeException ex) -> do
                logError $ "Falling back to default span ID gen due to exception" :#
                  [ "exception" .= displayException ex ]
                spanId <- idGeneratorSpecGenSpanId defaultIdGeneratorSpec
                pure (spanContextTraceId scParent, spanId)

    pure emptySpanContext
      { spanContextTraceId
      , spanContextSpanId
      , spanContextTraceFlags = TraceFlags 0 -- TODO: Populate correctly
      , spanContextTraceState = TraceState [] -- TODO: Populate correctly
      , spanContextIsRemote = False -- TODO: Populate correctly
      }

  spanUpdater
    :: ContextKey (Span AttrsBuilder)
    -> UpdateSpanSpec
    -> SpanProcessorM (Span Attrs)
  spanUpdater spanKey updateSpanSpec =
    flip runContextT (unSpanBackend ctxBackendSpan) do
      updater <- buildSpanUpdater (liftIO now) updateSpanSpec
      frozenAt <- liftIO now
      fmap (freezeSpan frozenAt spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits) do
        updateContext spanKey updater

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
    , tracerProviderSpecSpanAttrsLimits = spanAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = spanEventAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = spanLinkAttrsLimits
    , tracerProviderSpecSpanBackend = ctxBackendSpan
    } = tracerProviderSpec

shutdownTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
shutdownTracerProvider = liftIO . tracerProviderShutdown

forceFlushTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
forceFlushTracerProvider = liftIO . tracerProviderForceFlush

data SpanProcessor = SpanProcessor
  { spanProcessorOnSpanStart
      :: Maybe SpanContext -- Parent span context pulled from the context
      -> (UpdateSpanSpec -> SpanProcessorM (Span Attrs))
      -> IO ()
  , spanProcessorOnSpanEnd
      :: Span Attrs
      -> IO ()
  , spanProcessorShutdown
      :: IO ()
  , spanProcessorForceFlush
      :: IO ()
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
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanProcessorSpec
  -> IO SpanProcessor
buildSpanProcessor logger spanProcessorSpec = do
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanProcessor shutdownRef
  where
  spanProcessor shutdownRef =
    SpanProcessor
      { spanProcessorOnSpanStart = \mParentSpanContext spanUpdater -> do
          unlessShutdown (readTVar shutdownRef) do
            pure do
              run defaultTimeout metaOnSpanStart do
                spanProcessorSpecOnSpanStart mParentSpanContext spanUpdater
      , spanProcessorOnSpanEnd = \endedSpan -> do
          unlessShutdown (readTVar shutdownRef) do
            pure do
              run defaultTimeout metaOnSpanEnd do
                spanProcessorSpecOnSpanEnd endedSpan
      , spanProcessorShutdown = do
          unlessShutdown (readTVar shutdownRef) do
            writeTVar shutdownRef True
            pure do
              run shutdownTimeout metaShutdown do
                spanProcessorSpecShutdown
      , spanProcessorForceFlush = do
          unlessShutdown (readTVar shutdownRef) do
            pure do
              run forceFlushTimeout metaForceFlush do
                spanProcessorSpecForceFlush
      }

  run :: Int -> [SeriesElem] -> SpanProcessorM () -> IO ()
  run = runSpanProcessorM logger onTimeout onSyncEx

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
    , spanProcessorSpecOnException = onSyncEx
    } = spanProcessorSpec

-- TODO: Implement!
simpleSpanProcessor :: SpanProcessorSpec
simpleSpanProcessor =
  defaultSpanProcessorSpec
    { spanProcessorSpecName = "simple"
    , spanProcessorSpecOnSpanStart = mempty
    , spanProcessorSpecOnSpanEnd = mempty
    , spanProcessorSpecShutdown = mempty
    , spanProcessorSpecForceFlush = mempty
    }

data SimpleSpanProcessorSpec = SimpleSpanProcessorSpec
  { simpleSpanProcessorSpecExporter :: SpanExporter (Batch (Span Attrs))
  , simpleSpanProcessorSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

defaultSimpleSpanProcessorSpec :: SimpleSpanProcessorSpec
defaultSimpleSpanProcessorSpec =
  SimpleSpanProcessorSpec
    { simpleSpanProcessorSpecExporter = noopSpanExporter
    , simpleSpanProcessorSpecLogger = mempty
    }

withSimpleSpanProcessor
  :: (MonadUnliftIO m)
  => SimpleSpanProcessorSpec
  -> (SpanProcessorSpec -> m a)
  -> m a
withSimpleSpanProcessor spec action =
  withRunInIO \runInIO -> withSimpleSpanProcessorIO spec (runInIO . action)

withSimpleSpanProcessorIO
  :: SimpleSpanProcessorSpec
  -> (SpanProcessorSpec -> IO a)
  -> IO a
withSimpleSpanProcessorIO simpleSpanProcessorSpec action = do
  commandQueue <- liftIO $ newTBMQueueIO maxQueueSize
  withBatchingSpanProcessorIO commandQueue batchingSpanProcessorSpec \spanProcessorSpec -> do
    action $ patchSpanProcessor commandQueue spanProcessorSpec
  where
  patchSpanProcessor commandQueue spanProcessorSpec =
    spanProcessorSpec
      { spanProcessorSpecOnSpanEnd = \span -> do
          unlessShutdown (isClosedTBMQueue commandQueue) do
            add <- sendCommand_ logger commandQueue (BatchCommandAddItem span) metaOnSpanEnd
            process <- sendCommand_ logger commandQueue BatchCommandProcessBatch metaOnSpanEnd
            pure $ add <> process
      }

  metaOnSpanEnd :: [SeriesElem]
  metaOnSpanEnd = mkLoggingMeta "onSpanEnd"

  -- This and the equivalent one in 'withBatchingSpanProcessorIO' should get
  -- factored out at some point.
  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "spanProcessorSpec" .= object
        [ "name" .= spanProcessorSpecName
        , "method" .= method
        ]
    , "maxQueueSize" .= maxQueueSize
    , "scheduledDelayMicros" .= scheduledDelayMicros
    , "exportTimeoutMicros" .= exportTimeoutMicros
    , "maxExportBatchSize" .= maxExportBatchSize
    ]

  BatchingSpanProcessorSpec
    { batchingSpanProcessorSpecName = spanProcessorSpecName
    , batchingSpanProcessorSpecMaxQueueSize = maxQueueSize
    , batchingSpanProcessorSpecScheduledDelayMicros = scheduledDelayMicros
    , batchingSpanProcessorSpecExportTimeoutMicros = exportTimeoutMicros
    , batchingSpanProcessorSpecMaxExportBatchSize = maxExportBatchSize
    } = batchingSpanProcessorSpec

  batchingSpanProcessorSpec =
    defaultBatchingSpanProcessorSpec
      { batchingSpanProcessorSpecExporter = spanExporter
        -- This delay config is critical as it disables the export timer thread
        -- in the batching internals. We do not need an export timer from the
        -- simple span processor, as this processor will export immediately
        -- after each span is added.
      , batchingSpanProcessorSpecScheduledDelayMicros = 0
      , batchingSpanProcessorSpecMaxExportBatchSize = 1
      , batchingSpanProcessorSpecName = "simple"
      , batchingSpanProcessorSpecLogger = logger
      }

  SimpleSpanProcessorSpec
    { simpleSpanProcessorSpecExporter = spanExporter
    , simpleSpanProcessorSpecLogger = logger
    } = simpleSpanProcessorSpec

data BatchingSpanProcessorSpec = BatchingSpanProcessorSpec
  { batchingSpanProcessorSpecExporter :: SpanExporter (Batch (Span Attrs))
  , batchingSpanProcessorSpecMaxQueueSize :: Int
  , batchingSpanProcessorSpecScheduledDelayMicros :: Int
  , batchingSpanProcessorSpecExportTimeoutMicros :: Int
  , batchingSpanProcessorSpecMaxExportBatchSize :: Int
  , batchingSpanProcessorSpecName :: Text
  , batchingSpanProcessorSpecLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

defaultBatchingSpanProcessorSpec :: BatchingSpanProcessorSpec
defaultBatchingSpanProcessorSpec =
  BatchingSpanProcessorSpec
    { batchingSpanProcessorSpecExporter = noopSpanExporter
    , batchingSpanProcessorSpecMaxQueueSize = 2_048
    , batchingSpanProcessorSpecScheduledDelayMicros = 5_000_000
    , batchingSpanProcessorSpecExportTimeoutMicros = 30_000_000
    , batchingSpanProcessorSpecMaxExportBatchSize = 512
    , batchingSpanProcessorSpecName = "batching"
    , batchingSpanProcessorSpecLogger = mempty
    }

withBatchingSpanProcessor
  :: (MonadUnliftIO m)
  => BatchingSpanProcessorSpec
  -> (SpanProcessorSpec -> m a)
  -> m a
withBatchingSpanProcessor spec action = do
  commandQueue <- liftIO $ newTBMQueueIO maxQueueSize
  withRunInIO \runInIO -> do
    withBatchingSpanProcessorIO commandQueue spec $ runInIO . action
  where
  BatchingSpanProcessorSpec
    { batchingSpanProcessorSpecMaxQueueSize = maxQueueSize
    } = spec

withBatchingSpanProcessorIO
  :: TBMQueue (BatchCommand (Span Attrs))
  -> BatchingSpanProcessorSpec
  -> (SpanProcessorSpec -> IO a)
  -> IO a
withBatchingSpanProcessorIO commandQueue batchingSpanProcessorSpec action = do
  withAsync checkedExportTimer \exportTimerThread -> do
    withAsync processCommandQueue \processorThread -> do
      let spanProcessorSpec = mkSpanProcessorSpec exportTimerThread processorThread
      action spanProcessorSpec `finally` do
        unlessShutdown (isClosedTBMQueue commandQueue) do
          pure $ shutdown exportTimerThread processorThread
  where
  mkSpanProcessorSpec
    :: Async ()
    -> Async ()
    -> SpanProcessorSpec
  mkSpanProcessorSpec exportTimerThread processorThread =
    defaultSpanProcessorSpec
      { spanProcessorSpecName
      , spanProcessorSpecOnSpanEnd = \span -> do
          unlessShutdown (isClosedTBMQueue commandQueue) do
            sendCommand_ logger commandQueue (BatchCommandAddItem span) metaOnSpanEnd
      , spanProcessorSpecShutdown = do
          unlessShutdown (isClosedTBMQueue commandQueue) do
            pure $ liftIO $ shutdown exportTimerThread processorThread
      , spanProcessorSpecForceFlush = do
          unlessShutdown (isClosedTBMQueue commandQueue) do
            sendCommand_ logger commandQueue BatchCommandForceFlush metaForceFlush
      }

  processCommandQueue :: IO ()
  processCommandQueue = go mempty
    where
    go :: DList (Span Attrs) -> IO ()
    go !spans = do
      join $ atomically do
        readTBMQueue commandQueue >>= \case
          Nothing -> do
            pure do
              forceFlush spans
              spanExporterShutdown
          Just BatchCommandForceFlush -> do
            pure do
              forceFlush spans
              go mempty
          Just (BatchCommandAddItem span) -> do
            pure $ go (DList.snoc spans span)
          Just BatchCommandProcessBatch -> do
            pure do
              let (batch, rest) = List.splitAt maxExportBatchSize $ DList.toList spans
              unless (List.null batch) do
                processBatch batch
              go (DList.fromList rest)

    processBatch :: [Span Attrs] -> IO ()
    processBatch spans = do
      mResult <- timeout exportTimeoutMicros $ spanExporterExport $ batchFromList spans
      flip runLoggingT logger do
        case mResult of
          Nothing -> do
            logError $ "Processor timed out when exporting batch" :# metaProcessBatch
          Just Nothing -> do
            logDebug $ "Processor cannot export batch as exporter is shut down" :# metaProcessBatch
          Just (Just SpanExportResultFailure) -> do
            logError $ "Processor cannot export batch due to exporter failure" :# metaProcessBatch
          Just (Just SpanExportResultSuccess) -> pure ()

    forceFlush :: DList (Span Attrs) -> IO ()
    forceFlush spans = do
      let batches = Split.chunksOf maxExportBatchSize $ DList.toList spans
      traverse_ processBatch batches
      spanExporterForceFlush

  -- If the configured export delay is not positive, then this function returns
  -- immediately. This is useful behavior, considering that the simple span
  -- processor also provided by this SDK is implemented in terms of this
  -- batching span processor, just with a batch size of 1. In the simple span
  -- processor case, we don't want a timer thread to be pumping process-batch
  -- commands into the queue at all, and instead, the simple span processor
  -- also sends a process-batch command after every span is added.
  checkedExportTimer :: IO ()
  checkedExportTimer =
    when (scheduledDelayMicros > 0) do
      exportTimer

  exportTimer :: IO ()
  exportTimer = go
    where
    go :: IO ()
    go = do
      threadDelay scheduledDelayMicros
      unlessShutdown (isClosedTBMQueue commandQueue) do
        sender <- sendCommand logger commandQueue BatchCommandProcessBatch metaExportTimer
        pure do
          sender >>= \case
            Nothing -> pure ()
            Just True -> go
            Just False -> go

  shutdown :: Async () -> Async () -> IO ()
  shutdown exportTimerThread processorThread= do
    flip runLoggingT logger do
      -- Closing the queue ultimately signals shutdown.
      liftIO $ atomically $ closeTBMQueue commandQueue
      -- We cancel the export timer thread, as we don't want it to be stuck
      -- in a 'threadDelay' forcing us to wait for that delay in our cleanup.
      liftIO $ uninterruptibleCancel exportTimerThread
      liftIO (waitCatch processorThread) >>= \case
        Right () -> pure ()
        Left (SomeException ex) -> do
          logError $ "Unhandled exception when stopping batch export timer" :#
            "exception" .= displayException ex : metaCleanup
      liftIO (waitCatch exportTimerThread) >>= \case
        Right () -> pure ()
        Left someEx@(SomeException ex) -> do
          case fromException someEx of
            Nothing -> do
              logError $ "Unhandled exception when stopping batch export timer" :#
                "exception" .= displayException ex : metaCleanup
            Just AsyncCancelled -> do
              logDebug $ "Successfully cancelled batch export timer" :# metaCleanup

  metaOnSpanEnd :: [SeriesElem]
  metaOnSpanEnd = mkLoggingMeta "onSpanEnd"

  metaForceFlush :: [SeriesElem]
  metaForceFlush = mkLoggingMeta "forceFlush"

  metaExportTimer :: [SeriesElem]
  metaExportTimer = mkLoggingMeta "exportTimer"

  metaCleanup :: [SeriesElem]
  metaCleanup = mkLoggingMeta "cleanup"

  metaProcessBatch :: [SeriesElem]
  metaProcessBatch = mkLoggingMeta "processBatch"

  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "spanProcessorSpec" .= object
        [ "name" .= spanProcessorSpecName
        , "method" .= method
        ]
    , "maxQueueSize" .= maxQueueSize
    , "scheduledDelayMicros" .= scheduledDelayMicros
    , "exportTimeoutMicros" .= exportTimeoutMicros
    , "maxExportBatchSize" .= maxExportBatchSize
    ]

  BatchingSpanProcessorSpec
    { batchingSpanProcessorSpecExporter =
        SpanExporter
          { spanExporterExport
          , spanExporterShutdown
          , spanExporterForceFlush
          }
    , batchingSpanProcessorSpecMaxQueueSize = maxQueueSize
    , batchingSpanProcessorSpecScheduledDelayMicros = scheduledDelayMicros
    , batchingSpanProcessorSpecExportTimeoutMicros = exportTimeoutMicros
    , batchingSpanProcessorSpecMaxExportBatchSize = maxExportBatchSize
    , batchingSpanProcessorSpecName = spanProcessorSpecName
    , batchingSpanProcessorSpecLogger = logger
    } = batchingSpanProcessorSpec

data BatchCommand a
  = BatchCommandAddItem a
  | BatchCommandProcessBatch
  | BatchCommandForceFlush

sendCommand
  :: (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> TBMQueue (BatchCommand (Span Attrs))
  -> BatchCommand (Span Attrs)
  -> [SeriesElem]
  -> STM (m (Maybe Bool))
sendCommand logger commandQueue command pairs = do
  mResult <- tryWriteTBMQueue commandQueue command
  pure do
    flip runLoggingT logger do
      case mResult of
        Just True -> pure ()
        Just False -> logError $ "Batch queue is full" :# pairs
        Nothing -> logDebug $ "Cannot write to batch queue as it is closed" :# pairs
      pure mResult

sendCommand_
  :: (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> TBMQueue (BatchCommand (Span Attrs))
  -> BatchCommand (Span Attrs)
  -> [SeriesElem]
  -> STM (m ())
sendCommand_ logger commandQueue command pairs = do
  io <- sendCommand logger commandQueue command pairs
  pure $ void io

type SpanProcessorM :: Type -> Type
newtype SpanProcessorM a = SpanProcessorM
  { unSpanProcessorM :: LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (LoggingT IO)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (LoggingT IO) a)

runSpanProcessorM
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnTimeout a
  -> OnSynchronousException a
  -> Int
  -> [SeriesElem]
  -> SpanProcessorM a
  -> IO a
runSpanProcessorM logger onTimeout onSyncEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanProcessorM action `catchAny` \someEx -> do
          runOnSynchronousException onSyncEx someEx pairs
    case mResult of
      Just x -> pure x
      Nothing -> runOnTimeout onTimeout timeoutMicros pairs

data SpanProcessorSpec = SpanProcessorSpec
  { spanProcessorSpecName :: Text
  , spanProcessorSpecOnSpanStart
      :: Maybe SpanContext -- Parent span context pulled from the context
      -> (UpdateSpanSpec -> SpanProcessorM (Span Attrs))
      -> SpanProcessorM ()
  , spanProcessorSpecOnSpanEnd :: Span Attrs -> SpanProcessorM ()
  , spanProcessorSpecShutdown :: SpanProcessorM ()
  , spanProcessorSpecShutdownTimeout :: Int
  , spanProcessorSpecForceFlush :: SpanProcessorM ()
  , spanProcessorSpecForceFlushTimeout :: Int
  , spanProcessorSpecOnTimeout :: OnTimeout ()
  , spanProcessorSpecOnException :: OnSynchronousException ()
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

type IdGeneratorM :: Type -> Type
newtype IdGeneratorM a = IdGeneratorM
  { unIdGeneratorM :: PRNG -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
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

newtype OnSynchronousException a = OnSynchronousException
  { runOnSynchronousException :: SomeException -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (ReaderT SomeException (ReaderT [SeriesElem] (LoggingT IO)))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT SomeException (ReaderT [SeriesElem] (LoggingT IO))) a)

askException :: OnSynchronousException SomeException
askException = OnSynchronousException \someEx _pairs -> pure someEx

askExceptionMetadata :: OnSynchronousException [SeriesElem]
askExceptionMetadata = OnSynchronousException \_someEx pairs -> pure pairs

newtype OnTimeout a = OnTimeout
  { runOnTimeout :: Int -> [SeriesElem] -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (ReaderT Int (ReaderT [SeriesElem] (LoggingT IO)))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Int (ReaderT [SeriesElem] (LoggingT IO))) a)

askTimeoutMicros :: OnTimeout Int
askTimeoutMicros = OnTimeout \timeoutMicros _pairs -> pure timeoutMicros

askTimeoutMetadata :: OnTimeout [SeriesElem]
askTimeoutMetadata = OnTimeout \_timeoutMicros pairs -> pure pairs

data SpanExportResult
  = SpanExportResultSuccess
  | SpanExportResultFailure

instance Semigroup SpanExportResult where
  x <> y =
    case (x, y) of
      (SpanExportResultFailure, _) -> SpanExportResultFailure
      (SpanExportResultSuccess, _) -> y

instance Monoid SpanExportResult where
  mempty = SpanExportResultSuccess

-- Export function returns 'Nothing' only if the exporter is shutdown.
data SpanExporter spans = SpanExporter
  { spanExporterExport :: spans -> IO (Maybe SpanExportResult)
  , spanExporterShutdown :: IO ()
  , spanExporterForceFlush :: IO ()
  }

noopSpanExporter :: SpanExporter spans
noopSpanExporter =
  SpanExporter
    { spanExporterExport = mempty
    , spanExporterShutdown = mempty
    , spanExporterForceFlush = mempty
    }

buildSpanExporter
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanExporterSpec spans
  -> IO (SpanExporter spans)
buildSpanExporter logger spanExporterSpec = do
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanExporter shutdownRef
  where
  spanExporter shutdownRef =
    SpanExporter
      { spanExporterExport = \batch -> do
          unlessShutdown (readTVar shutdownRef) do
            pure do
              fmap Just do
                runSpanExporterM logger onTimeout' onSyncEx' defaultTimeout metaExport do
                  spanExporterSpecExport batch
      , spanExporterShutdown = do
          unlessShutdown (readTVar shutdownRef) do
            writeTVar shutdownRef True
            pure do
              runSpanExporterM logger onTimeout onSyncEx shutdownTimeout metaShutdown do
                spanExporterSpecShutdown
      , spanExporterForceFlush = do
          unlessShutdown (readTVar shutdownRef) do
            pure do
              runSpanExporterM logger onTimeout onSyncEx forceFlushTimeout metaForceFlush do
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

  onTimeout' :: OnTimeout SpanExportResult
  onTimeout' = SpanExportResultFailure <$ onTimeout

  onSyncEx' :: OnSynchronousException SpanExportResult
  onSyncEx' = SpanExportResultFailure <$ onSyncEx

  SpanExporterSpec
    { spanExporterSpecName
    , spanExporterSpecExport
    , spanExporterSpecShutdown
    , spanExporterSpecShutdownTimeout = shutdownTimeout
    , spanExporterSpecForceFlush
    , spanExporterSpecForceFlushTimeout = forceFlushTimeout
    , spanExporterSpecOnTimeout = onTimeout
    , spanExporterSpecOnException = onSyncEx
    } = spanExporterSpec

type SpanExporterM :: Type -> Type
newtype SpanExporterM a = SpanExporterM
  { unSpanExporterM :: LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (LoggingT IO)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (LoggingT IO) a)

runSpanExporterM
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> OnTimeout a
  -> OnSynchronousException a
  -> Int
  -> [SeriesElem]
  -> SpanExporterM a
  -> IO a
runSpanExporterM logger onTimeout onSyncEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanExporterM action `catchAny` \someEx -> do
          runOnSynchronousException onSyncEx someEx pairs
    case mResult of
      Just x -> pure x
      Nothing -> runOnTimeout onTimeout timeoutMicros pairs

data SpanExporterSpec spans = SpanExporterSpec
  { spanExporterSpecName :: Text
  , spanExporterSpecExport :: spans -> SpanExporterM SpanExportResult
  , spanExporterSpecShutdown :: SpanExporterM ()
  , spanExporterSpecShutdownTimeout :: Int
  , spanExporterSpecForceFlush :: SpanExporterM ()
  , spanExporterSpecForceFlushTimeout :: Int
  , spanExporterSpecOnTimeout :: OnTimeout ()
  , spanExporterSpecOnException :: OnSynchronousException ()
  }

defaultSpanExporterSpec :: SpanExporterSpec spans
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

newtype Batch a = Batch
  { unBatch :: Vector a
  } deriving (Eq, Monoid, Semigroup, Show) via (Vector a)
    deriving (Foldable, Functor, Applicative, Monad) via Vector

instance Traversable Batch where
  traverse f (Batch xs) = fmap Batch $ traverse f xs

batchFromList :: [a] -> Batch a
batchFromList = Batch. Vector.fromList

unlessShutdown :: (MonadIO m, Monoid (m a)) => STM Bool -> STM (m a) -> m a
unlessShutdown isShutdownSTM actionSTM =
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
