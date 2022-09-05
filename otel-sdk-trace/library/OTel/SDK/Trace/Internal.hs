{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.SDK.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    module OTel.SDK.Trace.Internal -- TODO: Explicit exports
  ) where

import Control.Applicative (Applicative(..))
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception.Safe
  ( Exception(..), SomeException(..), MonadCatch, MonadMask, MonadThrow, catchAny
  )
import Control.Monad ((<=<), join, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( LoggingT(..), Message(..), MonadLoggerIO(askLoggerIO), (.=), Loc, LogLevel, LogSource, LogStr
  , MonadLogger, SeriesElem, logError
  )
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (object)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import Data.Text (Text)
import OTel.API.Context
  ( ContextT(runContextT), ContextBackend, getContext, updateContext, withContextBackend
  )
import OTel.API.Context.Internal (newContextKey)
import OTel.API.Core
  ( EndedSpan(..), SpanParent(..), SpanSpec(..), SpanStatus(..), SpanAttrsLimits, SpanContext
  , SpanEventAttrsLimits, SpanId, SpanLinkAttrsLimits, Timestamp, TraceId, UpdateSpanSpec
  , defaultAttrsLimits, emptySpanContext, spanContextIsRemote, spanContextSpanId
  , spanContextTraceFlags, spanContextTraceId, spanContextTraceState, spanIdFromWords
  , timestampFromNanoseconds, traceIdFromWords
  )
import OTel.API.Core.Internal
  ( MutableSpan(..), Span(..), TraceFlags(..), TraceState(..), Tracer(..), TracerProvider(..)
  , buildSpanUpdater
  )
import Prelude
import System.Clock (Clock(Realtime), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (Variate(..), GenIO, Seed, createSystemSeed, fromSeed, initialize, uniform)
import System.Timeout (timeout)
import qualified Control.Exception.Safe as Exception

data TracerProviderSpec = TracerProviderSpec
  { tracerProviderSpecNow :: IO Timestamp
  , tracerProviderSpecSeed :: Seed
  , tracerProviderSpecIdGenerator :: IdGeneratorSpec
  , tracerProviderSpecSpanProcessors :: [SpanProcessorSpec]
  , tracerProviderSpecSpanAttrsLimits :: SpanAttrsLimits
  , tracerProviderSpecSpanEventAttrsLimits :: SpanEventAttrsLimits
  , tracerProviderSpecSpanLinkAttrsLimits :: SpanLinkAttrsLimits
  }

defaultTracerProviderSpec :: TracerProviderSpec
defaultTracerProviderSpec =
  TracerProviderSpec
    { tracerProviderSpecNow =
        fmap (timestampFromNanoseconds . toNanoSecs) $ getTime Realtime
    , tracerProviderSpecSeed = defaultSystemSeed
    , tracerProviderSpecIdGenerator = defaultIdGeneratorSpec
    , tracerProviderSpecSpanProcessors = mempty
    , tracerProviderSpecSpanAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = defaultAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = defaultAttrsLimits
    }

withTracerProvider
  :: forall m a
   . (MonadMask m, MonadLoggerIO m)
  => TracerProviderSpec
  -> (TracerProvider -> m a)
  -> m a
withTracerProvider spanProcessor f = do
  withContextBackend \ctxBackendTrace -> do
    Exception.bracket
      (newTracerProvider ctxBackendTrace spanProcessor)
      shutdownTracerProvider
      f

newTracerProvider
  :: forall m
   . (MonadLoggerIO m)
  => ContextBackend Span
  -> TracerProviderSpec
  -> m TracerProvider
newTracerProvider ctxBackendTrace tracerProviderSpec = do
  logger <- askLoggerIO
  shutdownRef <- liftIO $ newTVarIO False
  prngRef <- newPRNGRef seed
  spanProcessor <- liftIO $ foldMap (buildSpanProcessor logger) spanProcessorSpecs
  pure TracerProvider
    { tracerProviderGetTracer = getTracerWith prngRef logger spanProcessor
    , tracerProviderShutdown = do
        unlessShutdown shutdownRef do
          writeTVar shutdownRef True
          pure $ spanProcessorShutdown spanProcessor
    , tracerProviderForceFlush = do
        unlessShutdown shutdownRef do
          pure $ spanProcessorForceFlush spanProcessor
    }
  where
  getTracerWith prngRef logger spanProcessor scope =
    pure Tracer
      { tracerInstrumentationScope = scope
      , tracerNow = now
      , tracerStartSpan = startSpan prngRef logger spanProcessor
      , tracerProcessSpan = spanProcessorOnSpanEnd spanProcessor
      , tracerContextBackend = ctxBackendTrace
      , tracerSpanAttrsLimits = spanAttrsLimits
      , tracerSpanEventAttrsLimits = spanEventAttrsLimits
      , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
      }

  startSpan prngRef logger spanProcessor spanSpec = do
    let spanParent = spanSpecParent spanSpec
    freshSpanContext <- newSpanContext spanParent
    mutableSpan@MutableSpan { mutableSpanSpanKey = spanKey } <- do
      fmap MutableSpan $ newContextKey Span
        { spanParent
        , spanContext = freshSpanContext
        , spanName = spanSpecName spanSpec
        , spanStatus = SpanStatusUnset
        , spanStart = spanSpecStart spanSpec
        , spanEnd = Nothing
        , spanKind = spanSpecKind spanSpec
        , spanAttrs = spanSpecAttrs spanSpec
        , spanLinks = spanSpecLinks spanSpec
        , spanEvents = mempty
        , spanIsRecording = True -- TODO: Implement!
        }

    let getSpan = runContextT (getContext spanKey) ctxBackendTrace
    let updateSpan updatedSpan =
          void $ flip runContextT ctxBackendTrace do
            updateContext spanKey <=< buildSpanUpdater (liftIO now) $ updatedSpan

    -- TODO: Double-check this - might not be right
    parentSpanContext <- fmap spanContext getSpan
    -- TODO: Fetch baggage from context and pass along too
    spanProcessorOnSpanStart
      spanProcessor
      parentSpanContext
      updateSpan
      getSpan
    pure mutableSpan
    where
    newSpanContext spanParent = do
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
            SpanParentChildOf spanParentContext ->
              liftA2 (,) (pure $ spanContextTraceId spanParentContext) genSpanId
                `catchAny` \(SomeException ex) -> do
                  logError $ "Falling back to default span ID gen due to exception" :#
                    [ "exception" .= displayException ex ]
                  spanId <- idGeneratorSpecGenSpanId defaultIdGeneratorSpec
                  pure (spanContextTraceId spanParentContext, spanId)

      pure emptySpanContext
        { spanContextTraceId
        , spanContextSpanId
        , spanContextTraceFlags = TraceFlags 0 -- TODO: Populate correctly
        , spanContextTraceState = TraceState [] -- TODO: Populate correctly
        , spanContextIsRemote = False -- TODO: Populate correctly
        }

  TracerProviderSpec
    { tracerProviderSpecNow = now
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
    } = tracerProviderSpec

shutdownTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
shutdownTracerProvider = liftIO . tracerProviderShutdown

forceFlushTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
forceFlushTracerProvider = liftIO . tracerProviderForceFlush

data SpanProcessor = SpanProcessor
  { spanProcessorOnSpanStart
      :: SpanContext -- Parent span context pulled from the context
      -> (UpdateSpanSpec -> IO ())
      -> IO Span
      -> IO ()
  , spanProcessorOnSpanEnd
      :: EndedSpan
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
      { spanProcessorOnSpanStart = \parentSpanContext updateSpan getSpan -> do
          unlessShutdown shutdownRef $ pure do
            runSpanProcessorM defaultTimeout loggingMeta'onSpanStart logger onTimeout onSyncEx do
              spanProcessorSpecOnSpanStart parentSpanContext updateSpan getSpan
      , spanProcessorOnSpanEnd = \endedSpan -> do
          unlessShutdown shutdownRef $ pure do
            runSpanProcessorM defaultTimeout loggingMeta'onSpanEnd logger onTimeout onSyncEx do
              spanProcessorSpecOnSpanEnd endedSpan
      , spanProcessorShutdown = do
          unlessShutdown shutdownRef do
            writeTVar shutdownRef True
            pure do
              runSpanProcessorM spanProcessorSpecShutdownTimeout loggingMeta'shutdown logger onTimeout onSyncEx do
                spanProcessorSpecShutdown
      , spanProcessorForceFlush = do
          unlessShutdown shutdownRef $ pure do
            runSpanProcessorM spanProcessorSpecForceFlushTimeout loggingMeta'forceFlush logger onTimeout onSyncEx do
              spanProcessorSpecForceFlush
      }

  defaultTimeout :: Int
  defaultTimeout = 10000000

  loggingMeta'onSpanStart = mkLoggingMeta "onSpanStart"
  loggingMeta'onSpanEnd = mkLoggingMeta "onSpanEnd"
  loggingMeta'shutdown = mkLoggingMeta "shutdown"
  loggingMeta'forceFlush = mkLoggingMeta "forceFlush"

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
    , spanProcessorSpecShutdownTimeout
    , spanProcessorSpecForceFlush
    , spanProcessorSpecForceFlushTimeout
    , spanProcessorSpecOnTimeout = onTimeout
    , spanProcessorSpecOnSynchronousException = onSyncEx
    } = spanProcessorSpec

simpleSpanProcessor :: SpanProcessor
simpleSpanProcessor = mempty -- TODO: Implement!

batchedSpanProcessor :: SpanProcessor
batchedSpanProcessor = mempty -- TODO: Implement!

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
  :: Int
  -> [SeriesElem]
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> ([SeriesElem] -> OnTimeout ())
  -> ([SeriesElem] -> OnSynchronousException ())
  -> SpanProcessorM ()
  -> IO ()
runSpanProcessorM timeoutMicros pairs logger onTimeout onSyncEx action = do
  flip runLoggingT logger do
    mResult <- withRunInIO \runInIO -> do
      timeout timeoutMicros $ runInIO do
        unSpanProcessorM action `catchAny` \someEx -> do
          runOnSynchronousException (onSyncEx pairs) someEx
    case mResult of
      Just () -> pure ()
      Nothing -> runOnTimeout (onTimeout pairs) timeoutMicros

data SpanProcessorSpec = SpanProcessorSpec
  { spanProcessorSpecName
      :: Text
  , spanProcessorSpecOnSpanStart
      :: SpanContext -- Parent span context pulled from the context
      -> (UpdateSpanSpec -> IO ())
      -> IO Span
      -> SpanProcessorM ()
  , spanProcessorSpecOnSpanEnd
      :: EndedSpan
      -> SpanProcessorM ()
  , spanProcessorSpecShutdown
      :: SpanProcessorM ()
  , spanProcessorSpecShutdownTimeout
      :: Int
  , spanProcessorSpecForceFlush
      :: SpanProcessorM ()
  , spanProcessorSpecForceFlushTimeout
      :: Int
  , spanProcessorSpecOnTimeout
      :: [SeriesElem] -> OnTimeout ()
  , spanProcessorSpecOnSynchronousException
      :: [SeriesElem] -> OnSynchronousException ()
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
    , spanProcessorSpecOnTimeout = \pairs -> do
        timeoutMicros <- askTimeoutMicros
        logError $ "Action did not complete within timeout" :#
          "timeoutMicros" .= timeoutMicros : pairs
    , spanProcessorSpecOnSynchronousException = \pairs -> do
        SomeException ex <- askException
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

newPRNGRef :: (MonadIO m) => Seed -> m (MVar PRNG)
newPRNGRef seed = do
  liftIO do
    prng <- fmap PRNG $ initialize $ fromSeed seed
    newMVar prng

newtype OnSynchronousException a = OnSynchronousException
  { runOnSynchronousException :: SomeException -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (ReaderT SomeException (LoggingT IO))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT SomeException (LoggingT IO)) a)

askException :: OnSynchronousException SomeException
askException = OnSynchronousException \someEx -> pure someEx

newtype OnTimeout a = OnTimeout
  { runOnTimeout :: Int -> LoggingT IO a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (ReaderT Int (LoggingT IO))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Int (LoggingT IO)) a)

askTimeoutMicros :: OnTimeout Int
askTimeoutMicros = OnTimeout \timeoutMicros -> pure timeoutMicros

unlessShutdown :: (Monoid a) => TVar Bool -> STM (IO a) -> IO a
unlessShutdown shutdownRef action =
  join $ atomically $ readTVar shutdownRef >>= \case
    True -> pure mempty
    False -> action

defaultSystemSeed :: Seed
defaultSystemSeed = unsafePerformIO createSystemSeed
{-# NOINLINE defaultSystemSeed #-}

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
