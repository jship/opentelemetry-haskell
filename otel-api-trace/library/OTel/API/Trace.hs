module OTel.API.Trace
  ( -- * Synopsis
    -- $synopsis
    Internal.TracingT(..)
  , Internal.mapTracingT

  , Internal.TracingBackend
  , Internal.toTracingBackend

  , Internal.getTracingBackend
  , Internal.getTracer
  , Internal.shutdownTracerProvider
  , Internal.forceFlushTracerProvider

  , module OTel.API.Trace.Core
  ) where

import OTel.API.Trace.Core
import qualified OTel.API.Trace.Internal as Internal

-- $synopsis
--
-- @otel-api-trace@ STUB
--
