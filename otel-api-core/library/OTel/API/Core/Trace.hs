{-# LANGUAGE DataKinds #-}
module OTel.API.Core.Trace
  ( TracerProvider
  , getTracer

  , Tracer

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

  , recordException
  , exceptionEvent

  , SpanName(..)

  , MutableSpan
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

import Control.Exception (SomeException(..))
import OTel.API.Core.Attributes.Trace
import OTel.API.Core.Internal
import Prelude hiding (span)
import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable

recordException
  :: SomeException
  -> Bool
  -> TimestampSource
  -> AttrsBuilder 'AttrsForSpanEvent
  -> UpdateSpanSpec
recordException someEx escaped timestamp attributes =
  defaultUpdateSpanSpec
    { updateSpanSpecEvents =
        Just $ spanEventSpecsFromList
          [ exceptionEvent someEx escaped timestamp attributes
          ]
    }

exceptionEvent
  :: SomeException
  -> Bool
  -> TimestampSource
  -> AttrsBuilder 'AttrsForSpanEvent
  -> SpanEventSpec
exceptionEvent (SomeException e) escaped timestamp attributes =
  SpanEventSpec
    { spanEventSpecName = EXCEPTION_EVENT_NAME
    , spanEventSpecTimestamp = timestamp
    , spanEventSpecAttrs =
        attributes
          <> EXCEPTION_TYPE .@ show (Typeable.typeOf e)
          <> EXCEPTION_MESSAGE .@ Exception.displayException e
          <> EXCEPTION_ESCAPED .@ escaped
    }
