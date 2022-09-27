module OTel.SDK.Trace.SpanExporter.OTLP
  ( Internal.otlpSpanExporter

  , Internal.OTLPSpanExporterSpec
  , Internal.defaultOTLPSpanExporterSpec
  , Internal.otlpSpanExporterSpecManager
  , Internal.otlpSpanExporterSpecEndpoint
  , Internal.otlpSpanExporterSpecTimeout
  , Internal.otlpSpanExporterSpecProtocol
  , Internal.otlpSpanExporterSpecLogger
  , Internal.otlpSpanExporterSpecWorkerQueueSize
  , Internal.otlpSpanExporterSpecWorkerCount

  , Internal.OTLPProtocol
  , Internal.httpProtobufProtocol
  ) where

import qualified OTel.SDK.Trace.Internal as Internal