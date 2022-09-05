{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.SDK.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    withTracerProvider
  , newTracerProvider
  , shutdownTracerProvider
  , forceFlushTracerProvider

  , SpanProcessor(..)
  , simpleSpanProcessor
  , batchedSpanProcessor
  ) where

import Control.Exception.Safe (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (LoggingT(..), MonadLoggerIO(askLoggerIO))
import Data.Function ((&))
import OTel.API.Context (ContextBackend, withContextBackend)
import OTel.API.Core
  ( EndedSpan(..), SpanSpec(..), SpanStatus(..), Span, SpanAttrsLimits, SpanEventAttrsLimits
  , SpanLinkAttrsLimits, Timestamp
  )
import OTel.API.Core.Internal (MutableSpan(..), Span(..), Tracer(..), TracerProvider(..))
import Prelude
import qualified Control.Exception.Safe as Exception

data TracerProviderSpec = TracerProviderSpec
  { tracerProviderSpecNow :: IO Timestamp
  , tracerProviderSpecSpanProcessor :: SpanProcessor
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
          , tracerOnSpanStart = \mutableSpan ->
              flip runLoggingT logger do
                spanProcessorOnSpanStart spanProcessor mutableSpan
          , tracerOnSpanEnd = \endedSpan ->
              flip runLoggingT logger do
                spanProcessorOnSpanEnd spanProcessor endedSpan
          , tracerContextBackend = ctxBackendTrace
          , tracerSpanAttrsLimits = spanAttrsLimits
          , tracerSpanEventAttrsLimits = spanEventAttrsLimits
          , tracerSpanLinkAttrsLimits = spanLinkAttrsLimits
          }
    , tracerProviderShutdown =
        spanProcessorShutdown spanProcessor
          & flip runLoggingT logger
    , tracerProviderForceFlush =
        spanProcessorForceFlush spanProcessor
          & flip runLoggingT logger
    }
 where
  TracerProviderSpec
    { tracerProviderSpecNow = now
    , tracerProviderSpecSpanProcessor = spanProcessor
    , tracerProviderSpecSpanAttrsLimits = spanAttrsLimits
    , tracerProviderSpecSpanEventAttrsLimits = spanEventAttrsLimits
    , tracerProviderSpecSpanLinkAttrsLimits = spanLinkAttrsLimits
    } = tracerProviderSpec

shutdownTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
shutdownTracerProvider = liftIO . tracerProviderShutdown

forceFlushTracerProvider :: forall m. (MonadIO m) => TracerProvider -> m ()
forceFlushTracerProvider = liftIO . tracerProviderForceFlush

data SpanProcessor = SpanProcessor
  { spanProcessorOnSpanStart :: MutableSpan -> LoggingT IO ()
  , spanProcessorOnSpanEnd :: EndedSpan -> LoggingT IO ()
  , spanProcessorShutdown :: LoggingT IO ()
  , spanProcessorForceFlush :: LoggingT IO ()
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
      { spanProcessorOnSpanStart = const $ pure mempty
      , spanProcessorOnSpanEnd = const $ pure mempty
      , spanProcessorShutdown = pure mempty
      , spanProcessorForceFlush = pure mempty
      }

simpleSpanProcessor :: SpanProcessor
simpleSpanProcessor = mempty -- TODO: Implement!

batchedSpanProcessor :: SpanProcessor
batchedSpanProcessor = mempty -- TODO: Implement!

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
