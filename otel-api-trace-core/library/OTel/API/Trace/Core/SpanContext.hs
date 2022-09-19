module OTel.API.Trace.Core.SpanContext
  ( Internal.SpanContext
  , Internal.emptySpanContext
  , Internal.spanContextTraceId
  , Internal.spanContextSpanId
  , Internal.spanContextTraceFlags
  , Internal.spanContextTraceState
  , Internal.spanContextIsRemote
  , Internal.spanContextIsValid
  , Internal.spanContextIsSampled
  ) where

import qualified OTel.API.Trace.Core.Internal as Internal
