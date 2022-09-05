{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.SDK.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    withTracerProvider
  , newTracerProvider
  , shutdownTracerProvider
  , forceFlushTracerProvider

  , SpanProcessor(..)
  , buildSpanProcessor
  , simpleSpanProcessor
  , batchedSpanProcessor
  , SpanProcessorSpec(..)
  ) where

import Control.Exception (Exception(..))
import Control.Exception.Safe (MonadCatch, MonadMask, catchAny)
import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Logger.Aeson
  ( LoggingT(..), Message(..), MonadLoggerIO(askLoggerIO), (.=), MonadLogger, SeriesElem, logError
  )
import Control.Monad.Reader (ReaderT(..))
import Data.Function ((&))
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import Data.Text (Text)
import OTel.API.Context
  ( ContextSnapshot(contextSnapshotValue), ContextT(runContextT), ContextBackend, getContext
  , updateContext, withContextBackend
  )
import OTel.API.Core
  ( EndedSpan(..), InstrumentationScope(..), InstrumentationScopeName(..)
  , Version(..), SpanSpec(..), SpanStatus(..), Span, SpanAttrsLimits
  , SpanEventAttrsLimits, SpanLinkAttrsLimits, Timestamp, UpdateSpanSpec, schemaURLToText, OpentracingRefType (OpentracingRefTypeChildOf)
  )
import OTel.API.Core.Internal
  ( InstrumentationScope(InstrumentationScope), MutableSpan(..), Span(..), Tracer(..)
  , TracerProvider(..), buildSpanUpdater
  )
import Prelude
import System.Timeout (timeout)
import qualified Control.Exception.Safe as Exception
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Concurrent (MVar)

data TracerProviderSpec = TracerProviderSpec
  { tracerProviderSpecInstrumentationScope :: InstrumentationScope
  , tracerProviderSpecNow :: IO Timestamp
  , tracerProviderSpecSpanProcessor :: [SpanProcessorSpec]
  , tracerProviderSpecSpanAttrsLimits :: SpanAttrsLimits
  , tracerProviderSpecSpanEventAttrsLimits :: SpanEventAttrsLimits
  , tracerProviderSpecSpanLinkAttrsLimits :: SpanLinkAttrsLimits
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
  pure TracerProvider
    { tracerProviderGetTracer = \instrumentationScope ->
        pure Tracer
          { tracerInstrumentationScope = instrumentationScope
          , tracerNow = now
          , tracerStartSpan = \spanSpec -> do
              pure Span
                { spanParent = spanSpecParent spanSpec
                , spanContext = undefined -- TODO: Implement!
                , spanName = spanSpecName spanSpec
                , spanStatus = SpanStatusUnset
                , spanStart = spanSpecStart spanSpec
                , spanKind = spanSpecKind spanSpec
                , spanAttrs = spanSpecAttrs spanSpec
                , spanLinks = spanSpecLinks spanSpec
                , spanEvents = mempty
                , spanIsRecording = False
                }
          , tracerOnSpanStart = \case
              MutableSpan { mutableSpanSpanKey = spanKey } -> do
                spanProcessorOnSpanStart spanProcessor updateSpan getSpan
                  & flip runLoggingT logger
                where
                getSpan :: LoggingT IO Span
                getSpan = flip runContextT ctxBackendTrace do
                  fmap contextSnapshotValue $ getContext spanKey
                updateSpan :: UpdateSpanSpec -> LoggingT IO ()
                updateSpan updatedSpan = flip runContextT ctxBackendTrace do
                  void $ updateContext spanKey <=< buildSpanUpdater (liftIO now) $ updatedSpan
          , tracerOnSpanEnd = \endedSpan ->
              spanProcessorOnSpanEnd spanProcessor endedSpan
                & flip runLoggingT logger
          , tracerContextBackend = ctxBackendTrace
          , tracerSpanAttrsLimits = spanAttrsLimits
          , tracerSpanEventAttrsLimits = spanEventAttrsLimits
          , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
          }
    , tracerProviderShutdown =
        spanProcessorShutdown spanProcessor
          & mkTimeoutSafe (mkLoggingMeta "shutdown")
          & flip runTimeoutSafe 30000000 -- TODO: Make configurable?
          & flip runLoggingT logger
    , tracerProviderForceFlush =
        spanProcessorForceFlush spanProcessor
          & mkTimeoutSafe (mkLoggingMeta "forceFlush")
          & flip runTimeoutSafe 30000000 -- TODO: Make configurable?
          & flip runLoggingT logger
    }
  where
  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "instrumentationScopeName" .=
        unInstrumentationScopeName instrumentationScopeName
    , "instrumentationScopeVersion" .=
        fmap unVersion instrumentationScopeVersion
    , "instrumentationScopeSchemaURL" .=
        fmap schemaURLToText instrumentationScopeSchemaURL
    , "tracerProviderMethod" .= method
    ]

  spanProcessor = foldMap buildSpanProcessor spanProcessorSpecs

  TracerProviderSpec
    { tracerProviderSpecInstrumentationScope =
        InstrumentationScope
          { instrumentationScopeName
          , instrumentationScopeVersion
          , instrumentationScopeSchemaURL
          }
    , tracerProviderSpecNow = now
    , tracerProviderSpecSpanProcessor = spanProcessorSpecs
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
      :: (UpdateSpanSpec -> LoggingT IO ())
      -> LoggingT IO Span
      -> LoggingT IO ()
  , spanProcessorOnSpanEnd
      :: EndedSpan
      -> LoggingT IO ()
  , spanProcessorShutdown
      :: LoggingT IO ()
  , spanProcessorForceFlush
      :: LoggingT IO ()
  }

instance Semigroup SpanProcessor where
  sp1 <> sp2 =
    SpanProcessor
      { spanProcessorOnSpanStart =
          spanProcessorOnSpanStart sp1 *> spanProcessorOnSpanStart sp2
      , spanProcessorOnSpanEnd =
          spanProcessorOnSpanEnd sp1 *> spanProcessorOnSpanEnd sp2
      , spanProcessorShutdown =
          spanProcessorShutdown sp1 *> spanProcessorShutdown sp2
      , spanProcessorForceFlush =
          spanProcessorForceFlush sp1 *> spanProcessorForceFlush sp2
      }

instance Monoid SpanProcessor where
  mempty =
    SpanProcessor
      { spanProcessorOnSpanStart = const $ const $ pure mempty
      , spanProcessorOnSpanEnd = const $ pure mempty
      , spanProcessorShutdown = pure mempty
      , spanProcessorForceFlush = pure mempty
      }

buildSpanProcessor :: SpanProcessorSpec -> SpanProcessor
buildSpanProcessor spanProcessorSpec = spanProcessor
  where
  spanProcessor =
    SpanProcessor
      { spanProcessorOnSpanStart = \updateSpan getSpan -> do
          let loggingMeta = mkLoggingMeta "onSpanStart"
          spanProcessorSpecOnSpanStart spanProcessorSpec updateSpan getSpan
            & mkExceptionSafe loggingMeta
            & runExceptionSafe
      , spanProcessorOnSpanEnd = \endedSpan -> do
          let loggingMeta = mkLoggingMeta "onSpanEnd"
          spanProcessorSpecOnSpanEnd spanProcessorSpec endedSpan
            & mkExceptionSafe loggingMeta
            & runExceptionSafe
      , spanProcessorShutdown = do
          let loggingMeta = mkLoggingMeta "shutdown"
          spanProcessorSpecShutdown spanProcessorSpec
            & mkExceptionSafe loggingMeta
            & mkTimeoutSafe loggingMeta
            & flip runTimeoutSafe 30000000 -- TODO: Make configurable?
            & runExceptionSafe
      , spanProcessorForceFlush = do
          let loggingMeta = mkLoggingMeta "forceFlush"
          spanProcessorSpecForceFlush spanProcessorSpec
            & mkExceptionSafe loggingMeta
            & mkTimeoutSafe loggingMeta
            & flip runTimeoutSafe 30000000 -- TODO: Make configurable?
            & runExceptionSafe
      }

  mkLoggingMeta :: Text -> [SeriesElem]
  mkLoggingMeta method =
    [ "spanProcessorName" .= spanProcessorSpecName spanProcessorSpec
    , "spanProcessorMethod" .= method
    ]

simpleSpanProcessor :: SpanProcessor
simpleSpanProcessor = mempty -- TODO: Implement!

batchedSpanProcessor :: SpanProcessor
batchedSpanProcessor = mempty -- TODO: Implement!

data SpanProcessorSpec = SpanProcessorSpec
  { spanProcessorSpecName
      :: Text
  , spanProcessorSpecOnSpanStart
      :: (UpdateSpanSpec -> LoggingT IO ())
      -> LoggingT IO Span
      -> LoggingT IO ()
  , spanProcessorSpecOnSpanEnd
      :: EndedSpan
      -> LoggingT IO ()
  , spanProcessorSpecShutdown
      :: LoggingT IO ()
  , spanProcessorSpecShutdownTimeout
      :: Int
  , spanProcessorSpecForceFlush
      :: LoggingT IO ()
  , spanProcessorSpecForceFlushTimeout
      :: Int
  }

type ExceptionSafe :: (Type -> Type) -> Type -> Type
newtype ExceptionSafe m a = ExceptionSafe
  { runExceptionSafe :: m a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via m
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap m a)

mkExceptionSafe
  :: forall m
   . (MonadCatch m, MonadLogger m)
  => [SeriesElem]
  -> m ()
  -> ExceptionSafe m ()
mkExceptionSafe pairs action =
  ExceptionSafe do
    action `catchAny` \ex -> do
      logError $ "Ignoring exception" :#
        ("exception" .= displayException ex) : pairs

type TimeoutSafe :: (Type -> Type) -> Type -> Type
newtype TimeoutSafe m a = TimeoutSafe
  { runTimeoutSafe :: Int -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (ReaderT Int m)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT Int m) a)

mkTimeoutSafe
  :: forall m
   . (MonadLogger m, MonadUnliftIO m)
  => [SeriesElem] -- ^ Metadata to log if action times out
  -> m ()
  -> TimeoutSafe m ()
mkTimeoutSafe pairs action =
  TimeoutSafe \microseconds -> do
    mResult <- withRunInIO \runInIO -> do
      timeout microseconds $ runInIO action
    case mResult of
      Just () -> pure ()
      Nothing -> do
        logError $ "Action did not complete within timeout" :#
          ("timeoutMicroseconds" .= microseconds) : pairs

type Once :: (Type -> Type) -> Type -> Type
newtype Once m a = Once
  { runOnce :: IORef Bool -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadIO -- @base@
      , MonadUnliftIO -- @unliftio-core@
      , MonadLogger -- @monad-logger@
      ) via (ReaderT (IORef Bool) m)
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (IORef Bool) m) a)

mkOnce
  :: forall m
   . (MonadCatch m, MonadLogger m, MonadIO m)
  => m ()
  -> Once m ()
mkOnce action =
  Once \ref -> do
    liftIO (readIORef ref) >>= \case
      False -> do
        wrappedAction
        liftIO $ writeIORef ref True
      True -> pure ()
  where
  wrappedAction = runExceptionSafe $ mkExceptionSafe [] action

--runExactlyOnce :: forall m a. (MonadIO m) => m a -> m a
--runExactlyOnce action = do
--  ref <- liftIO $ newIORef Nothing
--  action

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
