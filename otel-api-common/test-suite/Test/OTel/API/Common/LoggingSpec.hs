{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.OTel.API.Common.LoggingSpec
  ( spec
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Control.Exception (ArithException(..), throwIO)
import Control.Monad (guard, replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger.Aeson
  ( Loc(..), LogLevel(LevelDebug, LevelError, LevelInfo), LoggingT(runLoggingT), Message(..)
  , ToLogStr(toLogStr), (.=), LogSource, LogStr, logDebug, logError, logInfo
  )
import Data.Aeson (Value(Array), object)
import Data.Text (Text)
import OTel.API.Common.Logging
  ( BufferedLoggerSpec
    ( bufferedLoggerSpecFlushPeriod, bufferedLoggerSpecFlushTimeout
    , bufferedLoggerSpecIncludeLogAggregate, bufferedLoggerSpecLogger
    , bufferedLoggerSpecOnFlushExceptionLogger, bufferedLoggerSpecShouldBuffer
    )
  , defaultBufferedLoggerSpec, includeLogAggregateViaAeson, withBufferedLogger
  )
import Prelude
import System.Timeout (timeout)
import Test.Hspec (HasCallStack, Spec, describe, expectationFailure, it, shouldBe)

testLogging
  :: forall m
   . (MonadIO m)
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> m ()
testLogging logger = do
 flip runLoggingT logger do
   logError $ "Ruh roh" :# ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
   logDebug "Doing stuff..."
   logInfo $ "Something of interest" :# ["howImportant" .= ("very" :: Text)]

-- Note that these tests do not test the period-based flushing of the buffered
-- logger, as that would likely just result in flaky tests. Instead, we rely on
-- the shutdown flush to check buffered logs.
spec :: Spec
spec = do
  describe "withBufferedLogger" do

    it "Unbuffered logs are logged immediately" do
      queue <- newTQueueIO
      let bufferedLoggerSpec =
            defaultBufferedLoggerSpec
              { bufferedLoggerSpecLogger = stmLogger queue
              , bufferedLoggerSpecFlushPeriod = maxBound @Int
              , bufferedLoggerSpecOnFlushExceptionLogger =
                  \_loc _logSource _logLevel _logStr ->
                    expectationFailure $
                      "bufferedLoggerSpecOnFlushExceptionLogger should not "
                        <> "have been called"
              }
      withBufferedLogger bufferedLoggerSpec \logger -> do
        replicateM_ 3 $ testLogging logger
        let expectedLogs =
              [ (l (43, 4) (43, 12), "", LevelError, "Ruh roh" :# ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)])
              , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
              , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
              ]
        expectedQueueElems False queue $ mconcat [expectedLogs, expectedLogs, expectedLogs]
      expectedQueueElems False queue $ mconcat []

    it "Buffered logs are logged on flush" do
      queue <- newTQueueIO
      let bufferedLoggerSpec =
            defaultBufferedLoggerSpec
              { bufferedLoggerSpecShouldBuffer =
                  \_loc _logSource logLevel _logStr ->
                    logLevel >= LevelError
              , bufferedLoggerSpecLogger = stmLogger queue
              , bufferedLoggerSpecFlushPeriod = maxBound @Int
              , bufferedLoggerSpecOnFlushExceptionLogger =
                  \_loc _logSource _logLevel _logStr ->
                    expectationFailure $
                      "bufferedLoggerSpecOnFlushExceptionLogger should not "
                        <> "have been called"
              , bufferedLoggerSpecIncludeLogAggregate =
                  includeLogAggregateViaAeson Just
              }
      withBufferedLogger bufferedLoggerSpec \logger -> do
        replicateM_ 3 $ testLogging logger
        -- All logs less than error level are unbuffered and logged immediately.
        expectedQueueElems False queue
          [ (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          ]
      -- A flush happens when @withBufferedLogger@ returns.
      expectedQueueElems False queue
        [ (l (43, 4) (43, 12), "", LevelError, "Ruh roh" :#
            [ "bufferedLogAgg" .= object
                [ "count" .= (3 :: Int)
                , "metas" .=  Array
                    [ object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                    , object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                    , object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                    ]
                ]
            , "bufferedLogger" .= object
                [ "flushPeriod" .= maxBound @Int
                , "flushTimeout" .= bufferedLoggerSpecFlushTimeout defaultBufferedLoggerSpec
                ]
            ]
          )
        ]

    it "Exceptions are handled" do
      queue <- newTQueueIO
      let bufferedLoggerSpec =
            defaultBufferedLoggerSpec
              { bufferedLoggerSpecShouldBuffer =
                  \_loc _logSource logLevel _logStr ->
                    logLevel >= LevelError
              , bufferedLoggerSpecLogger = \loc logSource logLevel logStr ->
                  if logLevel >= LevelError then do
                    throwIO Overflow
                  else do
                    stmLogger queue loc logSource logLevel logStr
              , bufferedLoggerSpecFlushPeriod = maxBound @Int
              , bufferedLoggerSpecOnFlushExceptionLogger = stmLogger queue
              }
      withBufferedLogger bufferedLoggerSpec \logger -> do
        replicateM_ 3 $ testLogging logger
        -- All logs less than error level are unbuffered and logged immediately.
        expectedQueueElems False queue
          [ (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          ]
      -- A flush happens when @withBufferedLogger@ returns.
      expectedQueueElems True queue
        [ (lInternal (1_007, 9) (1_007, 17), "", LevelError, "Ignoring exception from flushing buffered log" :#
            [ "exception" .= ("arithmetic overflow" :: Text)
            , "bufferedLog" .= object
                [ "loc" .= object
                    [ "char" .= (4 :: Int)
                    , "file" .= ("test-suite/Test/OTel/API/Common/LoggingSpec.hs" :: Text)
                    , "line" .= (43 :: Int)
                    , "module" .= ("Test.OTel.API.Common.LoggingSpec" :: Text)
                    , "package" .= ("main" :: Text)
                    ]
                , "level" .= ("error" :: Text)
                , "source" .= ("" :: Text)
                , "text" .= ("Ruh roh" :: Text)
                ]
            , "bufferedLogAgg" .= object
                [ "count" .= (3 :: Int)
                , "metas" .=  Array
                    [ object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                    , object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                    , object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                    ]
                ]
            , "bufferedLogger" .= object
                [ "flushPeriod" .= maxBound @Int
                , "flushTimeout" .= bufferedLoggerSpecFlushTimeout defaultBufferedLoggerSpec
                ]
            ]
          )
        ]

    it "Timeouts are handled" do
      queue <- newTQueueIO
      let bufferedLoggerSpec =
            defaultBufferedLoggerSpec
              { bufferedLoggerSpecShouldBuffer =
                  \_loc _logSource logLevel _logStr ->
                    logLevel >= LevelError
              , bufferedLoggerSpecLogger = stmLogger queue
              , bufferedLoggerSpecFlushPeriod = maxBound @Int
              , bufferedLoggerSpecFlushTimeout = 0
              , bufferedLoggerSpecOnFlushExceptionLogger = stmLogger queue
              }
      withBufferedLogger bufferedLoggerSpec \logger -> do
        replicateM_ 3 $ testLogging logger
        -- All logs less than error level are unbuffered and logged immediately.
        expectedQueueElems False queue
          [ (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          , (l (44, 4) (44, 12), "", LevelDebug, "Doing stuff...")
          , (l (45, 4) (45, 11), "", LevelInfo, "Something of interest" :# ["howImportant" .= ("very" :: Text)])
          ]
      -- A flush happens when @withBufferedLogger@ returns.
      expectedQueueElems True queue
        [ (lInternal (992, 9) (992, 17), "", LevelError, "Flushing buffered logs took too long" :#
            [ "timeoutMicros" .= (0 :: Int)
            , "unflushedLogs" .= Array
                [ object
                    [ "bufferedLog" .= object
                        [ "loc" .= object
                            [ "char" .= (4 :: Int)
                            , "file" .= ("test-suite/Test/OTel/API/Common/LoggingSpec.hs" :: Text)
                            , "line" .= (43 :: Int)
                            , "module" .= ("Test.OTel.API.Common.LoggingSpec" :: Text)
                            , "package" .= ("main" :: Text)
                            ]
                        , "level" .= ("error" :: Text)
                        , "source" .= ("" :: Text)
                        , "text" .= ("Ruh roh" :: Text)
                        ]
                    , "bufferedLogAgg" .= object
                        [ "count" .= (3 :: Int)
                        , "metas" .=  Array
                            [ object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                            , object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                            , object ["floop" .= ("bloorp" :: Text), "bonk" .= (42 :: Int)]
                            ]
                        ]
                    ]
                ]
            , "bufferedLogger" .= object
                [ "flushPeriod" .= maxBound @Int
                , "flushTimeout" .= (0 :: Int)
                ]
            ]
          )
        ]

expectedQueueElems
  :: (HasCallStack)
  => Bool
  -> TQueue (Loc, LogSource, LogLevel, LogStr)
  -> [(Loc, LogSource, LogLevel, Message)]
  -> IO ()
expectedQueueElems shouldIgnoreLoc queue expectedElems = do
  mResult <- timeout 3_000_000 $ atomically do
    actualElems <- getActualElems
    guard $ fmap locUpdate actualElems == expectedElems'
  case mResult of
    Just () -> pure ()
    Nothing -> do
      actualElems <- atomically getActualElems
      -- If a test is failing and the output is hard to decipher, it can be
      -- convenient to temporarily change the @expectationFailure@ to the
      -- following:
      --
      actualElems `shouldBe` expectedElems'
      expectationFailure $
        "expectedQueueElems timed out: "
          <> "expected=" <> show expectedElems' <> ", "
          <> "actual=" <> show actualElems
  where
  getActualElems = fmap locUpdate <$> flushTQueue queue

  expectedElems' =
    flip fmap expectedElems \(loc, logSource, logLevel, msg) ->
      locUpdate (loc, logSource, logLevel, toLogStr msg)

  locUpdate (loc, logSource, logLevel, msg)
    | shouldIgnoreLoc =
        (dummyLoc, logSource, logLevel, msg)
    | otherwise = (loc, logSource, logLevel, msg)

stmLogger
  :: TQueue (Loc, LogSource, LogLevel, LogStr)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
stmLogger queue loc logSource logLevel logStr = do
  atomically $ writeTQueue queue (loc, logSource, logLevel, logStr)

l :: (Int, Int) -> (Int, Int) -> Loc
l start end =
  Loc
    { loc_filename = "test-suite/Test/OTel/API/Common/LoggingSpec.hs"
    , loc_package = "main"
    , loc_module = "Test.OTel.API.Common.LoggingSpec"
    , loc_start = start
    , loc_end = end
    }

lInternal :: (Int, Int) -> (Int, Int) -> Loc
lInternal start end =
  Loc
    { loc_filename = "library/OTel/API/Common/Internal.hs"
    , loc_package = "TESTS SHOULD IGNORE THIS FIELD VIA expectedQueueElems INPUT"
    , loc_module = "OTel.API.Common.Internal"
    , loc_start = start
    , loc_end = end
    }

dummyLoc :: Loc
dummyLoc =
  Loc
    { loc_filename = "DUMMY FILENAME"
    , loc_package = "DUMMY PACKAGE"
    , loc_module = "DUMMY MODULE"
    , loc_start = (1, 2)
    , loc_end = (3, 4)
    }
