module OTel.API.Trace
  ( -- * Synopsis
    -- $synopsis
    Internal.withTracing

  , Internal.TracingT(..)
  , Internal.mapTracingT

  , Internal.SpanBackend
  , Internal.defaultSpanBackend

  , module OTel.API.Trace.Core
  ) where

import OTel.API.Trace.Core
import qualified OTel.API.Trace.Internal as Internal

-- $synopsis
--
-- @otel-api-trace@ STUB
--
