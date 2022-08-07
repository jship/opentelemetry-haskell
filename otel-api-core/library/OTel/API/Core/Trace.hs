module OTel.API.Core.Trace
  ( Tracer(..)

  , SpanContext
      ( spanContextTraceId
      , spanContextSpanId
      , spanContextTraceFlags
      , spanContextTraceState
      , spanContextIsRemote
      )
  , emptySpanContext
  , spanContextIsValid
  , TraceId
  , emptyTraceId
  , SpanId
  , emptySpanId
  , TraceFlags
  , TraceState

  , SpanEvents
  , spanEventsFromList
  , spanEventsToList

  , SpanEvent(..)

  , SpanEventSpecs
  , singletonSpanEventSpecs
  , spanEventSpecsFromList
  , spanEventSpecsToList

  , SpanEventSpec
      ( spanEventSpecName
      , spanEventSpecTimestamp
      , spanEventSpecAttrs
      )
  , defaultSpanEventSpec

  , SpanEventName(..)

  , SpanLinks
  , spanLinksFromList
  , spanLinksToList

  , SpanLinkSpecs
  , singletonSpanLinkSpecs
  , spanLinkSpecsFromList
  , spanLinkSpecsToList

  , SpanLink(..)

  , SpanLinkName(..)

  , SpanLinkSpec
      ( spanLinkSpecSpanContext
      , spanLinkSpecAttrs
      )
  , defaultSpanLinkSpec

  , SpanSpec
      ( spanSpecParent
      , spanSpecName
      , spanSpecStart
      , spanSpecKind
      , spanSpecAttrs
      , spanSpecLinks
      )
  , buildSpanSpec

  , NewSpanSpec
      ( newSpanSpecName
      , newSpanSpecParentSource
      , newSpanSpecStart
      , newSpanSpecKind
      , newSpanSpecAttrs
      , newSpanSpecLinks
      )
  , defaultNewSpanSpec

  , UpdateSpanSpec
      ( updateSpanSpecName
      , updateSpanSpecStatus
      , updateSpanSpecAttrs
      , updateSpanSpecEvents
      )
  , defaultUpdateSpanSpec
  , buildSpanUpdater
  , recordException

  , SpanName(..)

  , Span
      ( spanParent
      , spanContext
      , spanName
      , spanStatus
      , spanStart
      , spanKind
      , spanAttrs
      , spanLinks
      , spanEvents
      , spanIsRecording
      )
  , emptySpan

  , EndedSpan
      ( endedSpanParent
      , endedSpanContext
      , endedSpanName
      , endedSpanStatus
      , endedSpanStart
      , endedSpanEnd
      , endedSpanKind
      , endedSpanAttrs
      , endedSpanLinks
      , endedSpanEvents
      )
  , toEndedSpan

  , SpanParentSource(..)
  , SpanParent(..)

  , SpanKind(..)

  , SpanStatus(..)
  ) where

import OTel.API.Core.Internal
