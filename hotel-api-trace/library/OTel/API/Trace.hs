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

  , Context
  , useSpanContext

  , SpanDetails
  , defaultSpanDetails

  , Span

  , SpanLineageSource(..)
  , implicitSpanLineageSource
  , explicitSpanLineageSource

  , SpanLineage
  , rootSpanLineage
  , childSpanLineage

  , SpanContext
  , TraceId
  , SpanId
  , TraceFlags
  , TraceState

  , Timestamp

  , Attributes

  , SpanEvents

  , SpanLinks
  ) where

import OTel.API.Trace.Internal

-- $synopsis
--
-- @hotel-api-trace@ STUB
--
