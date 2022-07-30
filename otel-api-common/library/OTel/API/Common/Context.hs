{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
module OTel.API.Common.Context
  ( ContextValue(..)
  , HasTracingContext
      ( _tracingContextSpan
      )
  ) where

import OTel.API.Common.Internal (Span)
import Prelude

newtype ContextValue = ContextValue
  { contextValueSpan :: Span
  }

class HasTracingContext ctx where
  _tracingContextSpan :: Functor f => (Span -> f Span) -> ctx -> f ctx

instance HasTracingContext ContextValue where
  _tracingContextSpan
    :: Functor f
    => (Span -> f Span)
    -> ContextValue
    -> f ContextValue
  _tracingContextSpan f contextValue = fmap set $ f get
    where
    set :: Span -> ContextValue
    set newSpan =
      contextValue { contextValueSpan = newSpan }

    get :: Span
    get = contextValueSpan contextValue
