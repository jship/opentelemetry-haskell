module OTel.API.Trace
  ( -- * Synopsis
    -- $synopsis
    Core.trace
  , Core.MonadTracing(..)
  , Core.MonadTraceContext(..)

  , TracingT
  , runTracingT
  , mapTracingT
  ) where

import qualified OTel.API.Trace.Core as Core
import OTel.API.Trace.Internal

-- $synopsis
--
-- @hotel-api-trace@ STUB
--
