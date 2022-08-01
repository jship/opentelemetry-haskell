module OTel.API.Trace.Core.Internal
  ( MutableSpan(..)
  ) where

import OTel.API.Common (Span)
import OTel.API.Context (ContextKey)

newtype MutableSpan = MutableSpan
  { spanKey :: ContextKey Span
  }
