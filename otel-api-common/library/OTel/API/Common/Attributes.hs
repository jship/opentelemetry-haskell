module OTel.API.Common.Attributes
  ( Internal.Attrs
  , Internal.emptyAttrs
  , Internal.nullAttrs
  , Internal.sizeAttrs
  , Internal.memberAttrs
  , Internal.lookupAttrs
  , Internal.foldMapWithKeyAttrs
  , Internal.droppedAttrsCount

  , Internal.AttrsBuilder

  , Internal.AttrsFor(..)

  , Internal.AttrsLimits
  , Internal.defaultAttrsLimits
  , Internal.attrsLimitsCount
  , Internal.attrsLimitsValueLength

  , Internal.SomeAttr(..)
  , Internal.Attr(..)

  , Internal.AttrVals

  , Internal.AttrType(..)

  , Internal.KnownAttrType(..)
  , Internal.ToAttrVal(..)
  ) where

import qualified OTel.API.Common.Internal as Internal
