module OTel.API.Trace.Core.Internal
  ( MutableSpan(..)
  ) where

import OTel.API.Core (AttrsBuilder, Span)
import OTel.API.Context (ContextKey)

newtype MutableSpan = MutableSpan
  { spanKey :: ContextKey (Span AttrsBuilder)
  }
