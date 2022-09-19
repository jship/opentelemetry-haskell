module OTel.API.Trace.Core.TraceFlags
  ( Internal.TraceFlags
  , Internal.traceFlagsToHexText
  , Internal.traceFlagsToHexBuilder
  , Internal.emptyTraceFlags
  , Internal.setSampledFlag
  , Internal.isSampledFlagSet
  ) where

import qualified OTel.API.Trace.Core.Internal as Internal
