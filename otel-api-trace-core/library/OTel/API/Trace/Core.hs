module OTel.API.Trace.Core
  ( -- * Synopsis
    -- $synopsis
    Internal.Tracer

  , Internal.TracerProvider
  , Internal.getTracer
  , Internal.shutdownTracerProvider
  , Internal.forceFlushTracerProvider

  , module OTel.API.Trace.Core.Attributes
  , module OTel.API.Trace.Core.Class
  , module OTel.API.Trace.Core.Context
  , module OTel.API.Trace.Core.Span
  , module OTel.API.Trace.Core.SpanContext
  , module OTel.API.Trace.Core.SpanId
  , module OTel.API.Trace.Core.TraceFlags
  , module OTel.API.Trace.Core.TraceId
  , module OTel.API.Trace.Core.TraceState
  , module OTel.API.Trace.Core.TraceState.Errors

  , (.@)
  ) where

import OTel.API.Common (KV((.@)))
import OTel.API.Trace.Core.Attributes
import OTel.API.Trace.Core.Class
import OTel.API.Trace.Core.Context
import OTel.API.Trace.Core.Span
import OTel.API.Trace.Core.SpanContext
import OTel.API.Trace.Core.SpanId
import OTel.API.Trace.Core.TraceFlags
import OTel.API.Trace.Core.TraceId
import OTel.API.Trace.Core.TraceState
import OTel.API.Trace.Core.TraceState.Errors
import qualified OTel.API.Trace.Core.Internal as Internal

-- $synopsis
--
-- @otel-api-trace-core@ STUB
--
