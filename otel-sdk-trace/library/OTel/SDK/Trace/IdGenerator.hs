module OTel.SDK.Trace.IdGenerator
  ( Internal.IdGenerator

  , Internal.IdGeneratorSpec
  , Internal.defaultIdGeneratorSpec
  , Internal.idGeneratorSpecName
  , Internal.idGeneratorSpecGenTraceId
  , Internal.idGeneratorSpecGenSpanId

  , Internal.IdGeneratorM
  , Internal.genUniform
  ) where

import qualified OTel.SDK.Trace.Internal as Internal
