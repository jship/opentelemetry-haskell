module OTel.API.Trace.Core.Span
  ( Internal.MutableSpan

  , Internal.Span
  , Internal.spanLineage
  , Internal.spanContext
  , Internal.spanName
  , Internal.spanStatus
  , Internal.spanStart
  , Internal.spanFrozenAt
  , Internal.spanKind
  , Internal.spanAttrs
  , Internal.spanLinks
  , Internal.spanEvents
  , Internal.spanIsRecording
  , Internal.spanInstrumentationScope
  , Internal.spanIsRemote
  , Internal.spanIsSampled
  , Internal.spanIsRoot
  , Internal.spanIsChildOf

  , Internal.SpanName(..)
  , Internal.SpanLineage(..)
  , Internal.SpanKind(..)
  , Internal.SpanStatus(..)
  , Internal.SpanFrozenAt
  , Internal.SpanFrozenTimestamp(..)
  , Internal.frozenTimestamp

  , Internal.SpanSpec
  , Internal.defaultSpanSpec
  , Internal.spanSpecName
  , Internal.spanSpecParentContext
  , Internal.spanSpecStart
  , Internal.spanSpecKind
  , Internal.spanSpecAttrs
  , Internal.spanSpecLinks

  , Internal.UpdateSpanSpec
  , Internal.defaultUpdateSpanSpec
  , Internal.updateSpanSpecName
  , Internal.updateSpanSpecStatus
  , Internal.updateSpanSpecAttrs
  , Internal.updateSpanSpecEvents
  , Internal.recordException
  , Internal.exceptionEvent

  , Internal.SpanEvents
  , Internal.spanEventsFromList
  , Internal.spanEventsToList

  , Internal.SpanEvent(..)
  , Internal.SpanEventName(..)

  , Internal.SpanEventSpecs
  , Internal.singletonSpanEventSpecs
  , Internal.spanEventSpecsFromList
  , Internal.spanEventSpecsToList

  , Internal.SpanEventSpec
  , Internal.defaultSpanEventSpec
  , Internal.spanEventSpecName
  , Internal.spanEventSpecTimestamp
  , Internal.spanEventSpecAttrs

  , Internal.SpanLinks
  , Internal.spanLinksFromList
  , Internal.spanLinksToList

  , Internal.SpanLink(..)
  , Internal.SpanLinkName(..)

  , Internal.SpanLinkSpecs
  , Internal.singletonSpanLinkSpecs
  , Internal.spanLinkSpecsFromList
  , Internal.spanLinkSpecsToList

  , Internal.SpanLinkSpec
  , Internal.defaultSpanLinkSpec
  , Internal.spanLinkSpecSpanContext
  , Internal.spanLinkSpecAttrs
  ) where

import qualified OTel.API.Trace.Core.Internal as Internal
