module OTel.API.Core.Trace
  ( Tracer(..)

  , SpanContext
  , emptySpanContext
  , spanContextTraceId
  , spanContextSpanId
  , spanContextTraceFlags
  , spanContextTraceState
  , spanContextIsRemote
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
  , defaultSpanEventSpec
  , spanEventSpecName
  , spanEventSpecTimestamp
  , spanEventSpecAttrs

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
  , defaultSpanLinkSpec
  , spanLinkSpecSpanContext
  , spanLinkSpecAttrs

  , SpanSpec
  , buildSpanSpec
  , spanSpecParent
  , spanSpecName
  , spanSpecStart
  , spanSpecKind
  , spanSpecAttrs
  , spanSpecLinks

  , NewSpanSpec
  , defaultNewSpanSpec
  , newSpanSpecName
  , newSpanSpecParentSource
  , newSpanSpecStart
  , newSpanSpecKind
  , newSpanSpecAttrs
  , newSpanSpecLinks

  , UpdateSpanSpec
  , defaultUpdateSpanSpec
  , updateSpanSpecName
  , updateSpanSpecStatus
  , updateSpanSpecAttrs
  , updateSpanSpecEvents
  , buildSpanUpdater
  , recordException

  , SpanName(..)

  , Span
  , emptySpan
  , spanParent
  , spanContext
  , spanName
  , spanStatus
  , spanStart
  , spanKind
  , spanAttrs
  , spanLinks
  , spanEvents
  , spanIsRecording

  , EndedSpan
  , toEndedSpan
  , endedSpanParent
  , endedSpanContext
  , endedSpanName
  , endedSpanStatus
  , endedSpanStart
  , endedSpanEnd
  , endedSpanKind
  , endedSpanAttrs
  , endedSpanLinks
  , endedSpanEvents

  , SpanParentSource(..)
  , SpanParent(..)

  , SpanKind(..)

  , SpanStatus(..)
  ) where

import OTel.API.Core.Internal
