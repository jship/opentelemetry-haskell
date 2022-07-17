module OTel.API.Trace
  ( -- * Synopsis
    -- $synopsis
    MonadTracing(..)
  , trace
  , traceCS

  , TracingT(..)
  , runTracingT
  , mapTracingT

  , NoTracingT
  , runNoTracingT
  , mapNoTracingT

  , Tracer(..)

  , Span

  , SpanContext
  , TraceId
  , SpanId
  , TraceFlags
  , TraceState

  , Nanoseconds

  , Attributes

  , SpanEvents

  , SpanLinks
  ) where

import OTel.API.Trace.Internal

-- $synopsis
--
-- @hotel-api-trace@ STUB
