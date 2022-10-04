module OTel.SDK.Trace.SpanProcessor
  ( Internal.SpanProcessor

  , Internal.SpanProcessorSpec
  , Internal.defaultSpanProcessorSpec
  , Internal.spanProcessorSpecName
  , Internal.spanProcessorSpecExporter
  , Internal.spanProcessorSpecOnSpanStart
  , Internal.spanProcessorSpecOnSpanEnd
  , Internal.spanProcessorSpecShutdown
  , Internal.spanProcessorSpecShutdownTimeout
  , Internal.spanProcessorSpecForceFlush
  , Internal.spanProcessorSpecForceFlushTimeout
  , Internal.spanProcessorSpecOnTimeout
  , Internal.spanProcessorSpecOnException

  , Internal.SpanProcessorM
  , Internal.askSpanExporter

  , module OTel.SDK.Trace.SpanProcessor.Simple
  ) where

import OTel.SDK.Trace.SpanProcessor.Simple
import qualified OTel.SDK.Trace.Internal as Internal
