module OTel.SDK.Trace
  ( -- * Synopsis
    -- $synopsis
    Internal.withTracerProvider
  , Internal.getTracingBackend
  , Internal.getTracer
  , Internal.shutdownTracerProvider
  , Internal.forceFlushTracerProvider

  , Internal.TracerProviderSpec
  , Internal.defaultTracerProviderSpec
  , Internal.tracerProviderSpecNow
  , Internal.tracerProviderSpecLogger
  , Internal.tracerProviderSpecSeed
  , Internal.tracerProviderSpecIdGenerator
  , Internal.tracerProviderSpecSpanProcessors
  , Internal.tracerProviderSpecSampler
  , Internal.tracerProviderSpecSpanAttrsLimits
  , Internal.tracerProviderSpecSpanEventAttrsLimits
  , Internal.tracerProviderSpecSpanLinkAttrsLimits
  , Internal.tracerProviderSpecCallStackAttrs
  , Internal.tracerProviderSpecResource

  , module OTel.SDK.Trace.Common
  , module OTel.SDK.Trace.Handlers
  , module OTel.SDK.Trace.IdGenerator
  , module OTel.SDK.Trace.Sampler
  , module OTel.SDK.Trace.SpanExporter
  , module OTel.SDK.Trace.SpanProcessor
  ) where

import OTel.SDK.Trace.Common
import OTel.SDK.Trace.Handlers
import OTel.SDK.Trace.IdGenerator
import OTel.SDK.Trace.Sampler
import OTel.SDK.Trace.SpanExporter
import OTel.SDK.Trace.SpanProcessor
import qualified OTel.SDK.Trace.Internal as Internal

-- $synopsis
--
-- @otel-sdk-trace@ STUB
--
