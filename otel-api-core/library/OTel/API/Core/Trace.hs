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

  , SpanEventSpecs
  , singletonSpanEventSpecs
  , spanEventSpecsFromList
  , spanEventSpecsToList

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
  , spanEventsFromList
  , spanEventsToList

  , SpanEvent(..)
  , SpanEventName(..)

  , SpanLinks
  , spanLinksFromList
  , spanLinksToList

  , SpanLink
      ( spanLinkSpanContext
      , spanLinkAttributes
      )
  , SpanLinkName(..)

  , SpanLinkSpecs(..)
  , singletonSpanLinkSpecs
  , spanLinkSpecsFromList
  , spanLinkSpecsToList

  , SpanLinkSpec
      ( spanLinkSpecSpanContext
      , spanLinkSpecAttributes
      )
  , defaultSpanLinkSpec
  ) where

import OTel.API.Core.Internal
