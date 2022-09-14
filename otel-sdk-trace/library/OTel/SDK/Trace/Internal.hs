{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Monoid (Ap(..))
import Data.Text (Text)
import Data.Vector (Vector)
import OTel.API.Context (ContextT(runContextT), ContextKey, updateContext)
import OTel.API.Context.Internal (unsafeNewContextKey)
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
      fmap MutableSpan $ unsafeNewContextKey "spanKey" span

    when (spanIsRecording span) do
      -- TODO: Fetch baggage from context and pass along too
      spanProcessorOnSpanStart spanProcessor mParentSpanContext \updateSpanSpec -> do
        spanUpdater spanKey updateSpanSpec

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
    -> IO (Span Attrs)
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
                spanProcessorSpecOnSpanStart mParentSpanContext \updateSpanSpec -> do
                  liftIO $ spanUpdater updateSpanSpec
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
          let idSpan = Identity { runIdentity = span }
          liftIO $ spanExporterExport spanExporter idSpan \spanExportResult -> do
            logger <- askLoggerIO
            liftIO $ flip runLoggingT logger do
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
runSpanProcessorM logger onTimeout onSyncEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanProcessorM action `catchAny` \someEx -> do
          runOnException onSyncEx someEx pairs
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

data SpanExportResult
  = SpanExportResultSuccess
  | SpanExportResultFailure

-- Export function returns 'Nothing' only if the exporter is shutdown.
data SpanExporter (f :: Type -> Type) = SpanExporter
  { spanExporterExport
      :: f (Span Attrs)
      -> (SpanExportResult -> SpanProcessorM ())
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
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> SpanExporterSpec f
  -> IO (SpanExporter f)
buildSpanExporter logger spanExporterSpec = do
  shutdownRef <- liftIO $ newTVarIO False
  pure $ spanExporter shutdownRef
  where
  spanExporter shutdownRef =
    SpanExporter
      { spanExporterExport = \spans onSpansExported -> do
          unlessShutdown (readTVar shutdownRef) do
            pure do
              runSpanExporterM logger onTimeout onSyncEx defaultTimeout metaExport do
                spanExporterSpecExport spans \spanExportResult -> do
                  onSpansExported spanExportResult
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
runSpanExporterM logger onTimeout onSyncEx timeoutMicros pairs action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanExporterM action `catchAny` \someEx -> do
          runOnException onSyncEx someEx pairs
    case mResult of
      Just x -> pure x
      Nothing -> runOnTimeout onTimeout timeoutMicros pairs

data SpanExporterSpec (f :: Type -> Type) = SpanExporterSpec
  { spanExporterSpecName :: Text
  , spanExporterSpecExport
      :: f (Span Attrs)
      -> (SpanExportResult -> SpanProcessorM ())
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
