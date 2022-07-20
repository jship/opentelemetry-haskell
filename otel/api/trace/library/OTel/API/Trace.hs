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
  , activateSpan

  , SpanDetails -- TODO: Export fields
  , defaultSpanDetails

  , SpanUpdate
  , defaultSpanUpdate -- TODO: Export fields

  , Span(..)
  , EndedSpan(..)

  , SpanLineageSource
  , implicitSpanLineageSource
  , explicitSpanLineageSource

  , SpanLineage
  , rootSpanLineage
  , childSpanLineage

  , SpanKind
  , serverSpanKind
  , clientSpanKind
  , producerSpanKind
  , consumerSpanKind
  , internalSpanKind

  , SpanStatus
  , unsetSpanStatus
  , okSpanStatus
  , errorSpanStatus

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
