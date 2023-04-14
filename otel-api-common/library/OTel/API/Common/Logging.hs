module OTel.API.Common.Logging
  ( Internal.Logger

  , Internal.withBufferedLogger

  , Internal.BufferedLoggerSpec
  , Internal.defaultBufferedLoggerSpec
  , Internal.bufferedLoggerSpecShouldBuffer
  , Internal.bufferedLoggerSpecLogger
  , Internal.bufferedLoggerSpecFlushPeriod
  , Internal.bufferedLoggerSpecFlushTimeout
  , Internal.bufferedLoggerSpecOnFlushTimeout
  , Internal.bufferedLoggerSpecOnFlushException
  , Internal.bufferedLoggerSpecOnFlushExceptionLogger
  , Internal.bufferedLoggerSpecIncludeLogAggregate

  , Internal.includeLogAggregateViaAeson

  , Internal.BufferedLogs
  , Internal.BufferedLog
  , Internal.bufferedLogLoc
  , Internal.bufferedLogLogSource
  , Internal.bufferedLogLogLevel
  , Internal.bufferedLogLogStr
  , Internal.BufferedLogAgg
  , Internal.bufferedLogAggCount
  , Internal.bufferedLogAggMetas
  ) where

import qualified OTel.API.Common.Internal as Internal
