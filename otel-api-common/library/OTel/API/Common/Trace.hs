module OTel.API.Common.Trace
  ( Tracer(..)

  , SpanContext
      ( spanContextTraceId
      , spanContextSpanId
      , spanContextTraceFlags
      , spanContextTraceState
      , spanContextIsRemote
      )
  , defaultSpanContext
  , spanContextIsValid

  , TraceId
  , SpanId
  , TraceFlags
  , TraceState

  , SpanSpec
      ( spanSpecLineageSource
      , spanSpecStart
      , spanSpecKind
      , spanSpecAttributes
      , spanSpecLinks
      )
  , defaultSpanSpec
  , buildSpanUpdater
  , recordException

  , SpanUpdateSpec
      ( spanUpdateSpecName
      , spanUpdateSpecStatus
      , spanUpdateSpecAttributes
      , spanUpdateSpecEvents
      )
  , defaultSpanUpdateSpec

  , SpanEventSpec
      ( spanEventSpecName
      , spanEventSpecTimestamp
      , spanEventSpecAttributes
      )
  , defaultSpanEventSpec

  , SpanName(..)
  , Span(..)

  , EndedSpan
      ( endedSpanLineage
      , endedSpanContext
      , endedSpanName
      , endedSpanStatus
      , endedSpanStart
      , endedSpanEnd
      , endedSpanKind
      , endedSpanAttributes
      , endedSpanLinks
      , endedSpanEvents
      , endedSpanSrcLoc
      )
  , toEndedSpan

  , SpanLineageSource(..)
  , SpanLineage(..)

  , SpanKind(..)

  , SpanStatus(..)

  , SpanEvents
  , SpanEventSpecs(..) -- TODO: Constructor exposed :(

  , SpanLinks
  ) where

import OTel.API.Context (ContextBackend)
import OTel.API.Common.Internal
import Prelude

--data TracerProvider = TracerProvider
--  { tracerProviderGetTracer :: IO Tracer
--  , tracerProviderShutdown :: IO ()
--  }

data Tracer = Tracer
  { tracerGetCurrentTimestamp :: IO Timestamp
  , tracerStartSpan :: SpanName -> SpanSpec -> IO Span
  , tracerProcessSpan :: EndedSpan -> IO ()
  , tracerContextBackend :: ContextBackend Span
  }
