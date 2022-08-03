module OTel.API.Core.Trace
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
      ( newSpanSpecParentSource
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
      ( endedSpanParent
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

  , SpanParentSource(..)
  , SpanParent(..)

  , SpanKind(..)

  , SpanStatus(..)

  , SpanEvents
  , SpanEventSpecs(..) -- TODO: Constructor exposed :(

  , SpanLinks
  ) where

import OTel.API.Core.Internal
