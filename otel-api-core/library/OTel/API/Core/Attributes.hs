module OTel.API.Core.Attributes
  ( module OTel.API.Core.Attributes.Trace

  , Attrs
  , nullAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs

  , AttrsBuilder

  , AttrsFor(..)

  , SpanAttrsLimits
  , SpanEventAttrsLimits
  , SpanLinkAttrsLimits

  , AttrsLimits
  , attrsLimitsCount
  , attrsLimitsValueLength
  , defaultAttrsLimits

  , SomeAttr(..)
  , Attr(..)

  , AttrVals

  , AttrType(..)

  , KnownAttrType(..)
  , ToAttrVal(..)
  ) where

import OTel.API.Core.Attributes.Trace
import OTel.API.Core.Internal
