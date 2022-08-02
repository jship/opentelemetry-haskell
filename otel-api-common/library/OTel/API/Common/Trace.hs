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

  , SpanSpec(..)

  , NewSpanSpec
      ( newSpanSpecLineageSource
      , newSpanSpecStart
      , newSpanSpecKind
      , newSpanSpecAttributes
      , newSpanSpecLinks
      )
  , defaultNewSpanSpec
  , buildSpanUpdater
  , recordException

  , UpdateSpanSpec
      ( updateSpanSpecName
      , updateSpanSpecStatus
      , updateSpanSpecAttributes
      , updateSpanSpecEvents
      )
  , defaultUpdateSpanSpec

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

import OTel.API.Common.Internal
