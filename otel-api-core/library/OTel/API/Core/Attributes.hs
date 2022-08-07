module OTel.API.Core.Attributes
  ( Attrs
  , attrsToBuilder
  , nullAttrs
  , emptyAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs

  , AttrsBuilder
  , runAttrsBuilder

  , SpanAttrs
  , SpanAttrsBuilder

  , SpanEventAttrs
  , SpanEventAttrsBuilder

  , SpanLinkAttrs
  , SpanLinkAttrsBuilder

  , AttrsFor(..)

  , AttrsLimits
      ( attrsLimitsCount
      , attrsLimitsValueLength
      )
  , defaultAttrsLimits

  , SomeAttr(..)
  , Attr(..)

  , AttrVals

  , AttrType(..)

  , KnownAttrType(..)
  , ToAttrVal(..)
  ) where

import OTel.API.Core.Internal
