module OTel.API.Core.Attributes
  ( Attrs
  , nullAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs

  , AttrsBuilder

  , AttrsFor(..)

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

import OTel.API.Core.Internal
