module OTel.API.Common.Attributes
  ( Attrs
  , emptyAttrs
  , nullAttrs
  , sizeAttrs
  , memberAttrs
  , lookupAttrs
  , foldMapWithKeyAttrs
  , droppedAttrsCount

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

import OTel.API.Common.Internal
