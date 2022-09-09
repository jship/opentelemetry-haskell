module OTel.API.Trace
  ( -- * Synopsis
    -- $synopsis
    module OTel.API.Trace.Core

  , Internal.TracingT
  , Internal.runTracingT
  , Internal.mapTracingT

  , Internal.SpanBackend
  , Internal.defaultSpanBackend
  ) where

import OTel.API.Trace.Core
import qualified OTel.API.Trace.Internal as Internal

-- $synopsis
--
-- @otel-api-trace@ STUB
--
