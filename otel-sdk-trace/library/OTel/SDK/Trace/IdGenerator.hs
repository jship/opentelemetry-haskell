module OTel.SDK.Trace.IdGenerator
  ( Internal.IdGeneratorSpec
  , Internal.defaultIdGeneratorSpec
  , Internal.idGeneratorSpecGenTraceId
  , Internal.idGeneratorSpecGenSpanId

  , Internal.IdGeneratorM
  , Internal.genUniform
  ) where

import qualified OTel.SDK.Trace.Internal as Internal
