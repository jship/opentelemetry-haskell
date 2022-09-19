{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer
    trace
  , trace_
  , MonadTracing(..)
  , MonadTracingContext(..)
  , MonadTracingIO(..)

  , TracerProvider(..)
  , getTracer

  , Tracer(..)

  , SpanBackend(..)
  , defaultSpanBackend
  , spanContextKey

  , SpanContext(..)
  , emptySpanContext
  , spanContextIsValid
  , spanContextIsSampled

  , TraceId(..)
  , traceIdToHexText
  , traceIdToBytesVector
  , traceIdToHexBuilder
  , traceIdToBytesBuilder
  , emptyTraceId
  , traceIdFromWords

  , SpanId(..)
  , spanIdToHexText
  , spanIdToBytesVector
  , spanIdToHexBuilder
  , spanIdToBytesBuilder
  , emptySpanId
  , spanIdFromWords

  , TraceFlags(..)
  , traceFlagsToHexText
  , traceFlagsToHexBuilder
  , emptyTraceFlags
  , setSampledFlag
  , isSampledFlagSet

  , TraceState(..)
  , emptyTraceState
  , nullTraceState
  , sizeTraceState
  , memberTraceState
  , lookupTraceState
  , findWithDefaultTraceState
  , deleteTraceState
  , filterTraceState
  , filterWithKeyTraceState
  , foldMapWithKeyTraceState
  , toListTraceState
  , TraceStateBuilder(..)
  , buildTraceState
  , buildTraceStatePure

  , SpanEvents(..)
  , spanEventsFromList
  , spanEventsToList
  , freezeAllSpanEventAttrs
  , SpanEvent(..)
  , freezeSpanEventAttrs
  , SpanEventSpecs(..)
  , singletonSpanEventSpecs
  , spanEventSpecsFromList
  , spanEventSpecsToList
  , SpanEventSpec(..)
  , defaultSpanEventSpec
  , SpanEventName(..)
  , SpanLinks(..)
  , spanLinksFromList
  , spanLinksToList
  , freezeAllSpanLinkAttrs
  , SpanLinkSpecs(..)
  , singletonSpanLinkSpecs
  , spanLinkSpecsFromList
  , spanLinkSpecsToList
  , SpanLink(..)
  , freezeSpanLinkAttrs
  , SpanLinkName(..)
  , SpanLinkSpec(..)
  , defaultSpanLinkSpec
  , NewSpanSpec(..)
  , defaultNewSpanSpec
  , UpdateSpanSpec(..)
  , defaultUpdateSpanSpec
  , buildSpanUpdater
  , recordException
  , exceptionEvent
  , SpanName(..)
  , MutableSpan(..)
  , Span(..)
  , spanIsRemote
  , spanIsSampled
  , SpanFrozenAt
  , freezeSpan
  , SpanParent(.., Root, ChildOf)
  , SpanKind(.., Server, Client, Producer, Consumer, Internal)
  , SpanStatus(.., Unset, OK, Error)
  ) where

import Control.Applicative (Applicative(..))
import Control.Exception (SomeException(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadTransControl(liftWith, restoreT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (KeyValue((.=)), ToJSON(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bits (Bits((.|.), testBit), Ior(..))
import Data.ByteString.Builder (Builder)
import Data.DList (DList)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word64, Word8)
import GHC.Stack (CallStack, HasCallStack, callStack)
import OTel.API.Context.Core (Context, ContextBackend, ContextKey)
import OTel.API.Context.Core.Internal
  ( ContextBackend(contextBackendValueKey), unsafeNewContextBackend
  )
import OTel.API.Core
  ( AttrsFor(..), KV(..), Key(..), TimestampSource(..), Attrs, AttrsBuilder, AttrsLimits
  , InstrumentationScope, IsTextKV, Timestamp
  )
import OTel.API.Core.Internal (runAttrsBuilder)
import OTel.API.Trace.Core.Attributes
  ( pattern EXCEPTION_ESCAPED, pattern EXCEPTION_MESSAGE, pattern EXCEPTION_TYPE
  )
import OTel.API.Trace.Core.TraceState.Errors
  ( TraceStateError(..), TraceStateErrors(..), TraceStateKeyTypeUnknownError(..)
  , TraceStateSimpleKeyContainsInvalidCharsError(..), TraceStateSimpleKeyIsEmptyError(..)
  , TraceStateSimpleKeyTooLongError(..), TraceStateSystemIdContainsInvalidCharsError(..)
  , TraceStateSystemIdIsEmptyError(..), TraceStateSystemIdTooLongError(..)
  , TraceStateTenantIdContainsInvalidCharsError(..), TraceStateTenantIdIsEmptyError(..)
  , TraceStateTenantIdTooLongError(..), TraceStateValueContainsInvalidCharsError(..)
  , TraceStateValueIsEmptyError(..), TraceStateValueTooLongError(..)
  )
import Prelude hiding (span)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Traversable as Traversable
import qualified Data.Typeable as Typeable
import qualified Data.Vector.Unboxed as Unboxed

trace
  :: (MonadTracing m, HasCallStack)
  => NewSpanSpec
  -> (MutableSpan -> m a)
  -> m a
trace = traceCS callStack

trace_
  :: (MonadTracing m, HasCallStack)
  => NewSpanSpec
  -> m a
  -> m a
trace_ newSpanSpec = traceCS callStack newSpanSpec . const

class (Monad m) => MonadTracing m where
  traceCS :: CallStack -> NewSpanSpec -> (MutableSpan -> m a) -> m a

  default traceCS
    :: (MonadTransControl t, MonadTracing n, m ~ t n)
    => CallStack
    -> NewSpanSpec
    -> (MutableSpan -> m a)
    -> m a
  traceCS cs newSpanSpec f = do
    restoreT . pure
      =<< liftWith \run -> traceCS cs newSpanSpec (run . f)

instance (MonadTracing m) => MonadTracing (ExceptT e m)
instance (MonadTracing m) => MonadTracing (IdentityT m)
instance (MonadTracing m) => MonadTracing (MaybeT m)
instance (MonadTracing m) => MonadTracing (ReaderT r m)
instance (MonadTracing m) => MonadTracing (State.Lazy.StateT r m)
instance (MonadTracing m) => MonadTracing (State.Strict.StateT r m)
instance (MonadTracing m, Monoid w) => MonadTracing (RWS.Lazy.RWST r w s m)
instance (MonadTracing m, Monoid w) => MonadTracing (RWS.Strict.RWST r w s m)
instance (MonadTracing m, Monoid w) => MonadTracing (Writer.Lazy.WriterT w m)
instance (MonadTracing m, Monoid w) => MonadTracing (Writer.Strict.WriterT w m)
instance (MonadTracing m) => MonadTracing (LoggingT m)
instance (MonadTracing m, MonadUnliftIO m) => MonadTracing (ResourceT m) where
  traceCS cs newSpanSpec f = do
    withRunInIO \runInIO -> do
      runInIO $ traceCS cs newSpanSpec f

class (MonadTracing m) => MonadTracingContext m where
  getSpanContext :: MutableSpan -> m SpanContext
  updateSpan :: MutableSpan -> UpdateSpanSpec -> m ()

  default getSpanContext
    :: (MonadTrans t, MonadTracingContext n, m ~ t n)
    => MutableSpan
    -> m SpanContext
  getSpanContext = lift . getSpanContext

  default updateSpan
    :: (MonadTrans t, MonadTracingContext n, m ~ t n)
    => MutableSpan
    -> UpdateSpanSpec
    -> m ()
  updateSpan mutableSpan = lift . updateSpan mutableSpan

instance (MonadTracingContext m) => MonadTracingContext (ExceptT e m)
instance (MonadTracingContext m) => MonadTracingContext (IdentityT m)
instance (MonadTracingContext m) => MonadTracingContext (MaybeT m)
instance (MonadTracingContext m) => MonadTracingContext (ReaderT r m)
instance (MonadTracingContext m) => MonadTracingContext (State.Lazy.StateT r m)
instance (MonadTracingContext m) => MonadTracingContext (State.Strict.StateT r m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (RWS.Lazy.RWST r w s m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (RWS.Strict.RWST r w s m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (Writer.Lazy.WriterT w m)
instance (MonadTracingContext m, Monoid w) => MonadTracingContext (Writer.Strict.WriterT w m)
instance (MonadTracingContext m) => MonadTracingContext (LoggingT m)
instance (MonadTracingContext m, MonadUnliftIO m) => MonadTracingContext (ResourceT m) where
  getSpanContext mutableSpan = do
    withRunInIO \runInIO -> do
      runInIO $ getSpanContext mutableSpan

  updateSpan mutableSpan updateSpanSpec = do
    withRunInIO \runInIO -> do
      runInIO $ updateSpan mutableSpan updateSpanSpec

class (MonadTracing m, MonadIO m) => MonadTracingIO m where
  askTracer :: m Tracer

  default askTracer
    :: (MonadTrans t, MonadTracingIO n, m ~ t n)
    => m Tracer
  askTracer = lift askTracer

instance (MonadTracingIO m) => MonadTracingIO (ExceptT e m)
instance (MonadTracingIO m) => MonadTracingIO (IdentityT m)
instance (MonadTracingIO m) => MonadTracingIO (MaybeT m)
instance (MonadTracingIO m) => MonadTracingIO (ReaderT r m)
instance (MonadTracingIO m) => MonadTracingIO (State.Lazy.StateT r m)
instance (MonadTracingIO m) => MonadTracingIO (State.Strict.StateT r m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (RWS.Lazy.RWST r w s m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (RWS.Strict.RWST r w s m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (Writer.Lazy.WriterT w m)
instance (MonadTracingIO m, Monoid w) => MonadTracingIO (Writer.Strict.WriterT w m)
instance (MonadTracingIO m) => MonadTracingIO (LoggingT m)
instance (MonadTracingIO m, MonadUnliftIO m) => MonadTracingIO (ResourceT m) where
  askTracer = do
    withRunInIO \runInIO -> do
      runInIO askTracer

data TracerProvider = TracerProvider
  { tracerProviderGetTracer :: InstrumentationScope -> IO Tracer
  , tracerProviderShutdown :: IO ()
  , tracerProviderForceFlush :: IO ()
  }

getTracer
  :: forall m
   . (MonadIO m)
  => TracerProvider
  -> InstrumentationScope
  -> m Tracer
getTracer tracerProvider = liftIO . tracerProviderGetTracer tracerProvider

data Tracer = Tracer
  { tracerInstrumentationScope :: InstrumentationScope
  , tracerNow :: IO Timestamp
  , tracerStartSpan :: Context -> NewSpanSpec -> IO MutableSpan
  , tracerProcessSpan :: Span Attrs -> IO ()
  , tracerSpanBackend :: SpanBackend
  , tracerSpanAttrsLimits :: AttrsLimits 'AttrsForSpan
  , tracerSpanEventAttrsLimits :: AttrsLimits 'AttrsForSpanEvent
  , tracerSpanLinkAttrsLimits :: AttrsLimits 'AttrsForSpanLink
  }

newtype SpanBackend = SpanBackend
  { unSpanBackend :: ContextBackend MutableSpan
  }

defaultSpanBackend :: SpanBackend
defaultSpanBackend =
  unsafePerformIO $ liftIO $ fmap SpanBackend unsafeNewContextBackend
{-# NOINLINE defaultSpanBackend #-}

spanContextKey :: ContextKey MutableSpan
spanContextKey = contextBackendValueKey $ unSpanBackend defaultSpanBackend

data SpanContext = SpanContext
  { spanContextTraceId :: TraceId
  , spanContextSpanId :: SpanId
  , spanContextTraceFlags :: TraceFlags
  , spanContextTraceState :: TraceState
  , spanContextIsRemote :: Bool
  } deriving stock (Eq, Show)

instance ToJSON SpanContext where
  toJSON spanContext =
    Aeson.object
      [ "traceId" .= spanContextTraceId
      , "spanId" .= spanContextSpanId
      , "traceFlags" .= spanContextTraceFlags
      , "traceState" .= spanContextTraceState
      , "isRemote" .= spanContextIsRemote
      ]
    where
    SpanContext
      { spanContextTraceId
      , spanContextSpanId
      , spanContextTraceFlags
      , spanContextTraceState
      , spanContextIsRemote
      } = spanContext

emptySpanContext :: SpanContext
emptySpanContext =
  SpanContext
    { spanContextTraceId = emptyTraceId
    , spanContextSpanId = emptySpanId
    , spanContextTraceFlags = mempty
    , spanContextTraceState = emptyTraceState
    , spanContextIsRemote = False
    }

spanContextIsValid :: SpanContext -> Bool
spanContextIsValid spanContext =
  spanContextTraceId /= emptyTraceId && spanContextSpanId /= emptySpanId
  where
  SpanContext { spanContextTraceId, spanContextSpanId } = spanContext

spanContextIsSampled :: SpanContext -> Bool
spanContextIsSampled spanContext = isSampledFlagSet spanContextTraceFlags
  where
  SpanContext { spanContextTraceFlags } = spanContext

data TraceId = TraceId
  { traceIdHi :: Word64
  , traceIdLo :: Word64
  } deriving stock (Eq, Show)

instance ToJSON TraceId where
  toJSON = toJSON . traceIdToHexText

traceIdToHexText :: TraceId -> Text
traceIdToHexText traceId =
  Text.Encoding.decodeUtf8
    $ ByteString.toStrict
    $ Builder.toLazyByteString
    $ traceIdToHexBuilder traceId

traceIdToBytesVector :: TraceId -> Unboxed.Vector Word8
traceIdToBytesVector traceId =
  Unboxed.fromList
    $ ByteString.Lazy.unpack
    $ Builder.toLazyByteString
    $ traceIdToBytesBuilder traceId

traceIdToHexBuilder :: TraceId -> Builder
traceIdToHexBuilder traceId =
  Builder.word64HexFixed traceIdHi <> Builder.word64HexFixed traceIdLo
  where
  TraceId { traceIdHi, traceIdLo } = traceId

traceIdToBytesBuilder :: TraceId -> Builder
traceIdToBytesBuilder traceId =
  Builder.word64LE traceIdHi <> Builder.word64LE traceIdLo
  where
  TraceId { traceIdHi, traceIdLo } = traceId

emptyTraceId :: TraceId
emptyTraceId = TraceId { traceIdHi = 0, traceIdLo = 0 }

traceIdFromWords :: Word64 -> Word64 -> TraceId
traceIdFromWords = TraceId

newtype SpanId = SpanId
  { spanIdLo :: Word64
  } deriving stock (Eq, Show)

instance ToJSON SpanId where
  toJSON = toJSON . spanIdToHexText

spanIdToHexText :: SpanId -> Text
spanIdToHexText spanId =
  Text.Encoding.decodeUtf8
    $ ByteString.toStrict
    $ Builder.toLazyByteString
    $ spanIdToHexBuilder spanId

spanIdToBytesVector :: SpanId -> Unboxed.Vector Word8
spanIdToBytesVector spanId =
  Unboxed.fromList
    $ ByteString.Lazy.unpack
    $ Builder.toLazyByteString
    $ spanIdToBytesBuilder spanId

spanIdToHexBuilder :: SpanId -> Builder
spanIdToHexBuilder spanId =
  Builder.word64HexFixed spanIdLo
  where
  SpanId { spanIdLo } = spanId

spanIdToBytesBuilder :: SpanId -> Builder
spanIdToBytesBuilder spanId =
  Builder.word64LE spanIdLo
  where
  SpanId { spanIdLo } = spanId

emptySpanId :: SpanId
emptySpanId = SpanId { spanIdLo = 0 }

spanIdFromWords :: Word64 -> SpanId
spanIdFromWords = SpanId

newtype TraceFlags = TraceFlags
  { unTraceFlags :: Word8
  } deriving stock (Eq, Show)
    deriving (Semigroup, Monoid) via (Ior Word8)

instance ToJSON TraceFlags where
  toJSON = toJSON . traceFlagsToHexText

traceFlagsToHexText :: TraceFlags -> Text
traceFlagsToHexText traceFlags =
  Text.Encoding.decodeUtf8
    $ ByteString.toStrict
    $ Builder.toLazyByteString
    $ traceFlagsToHexBuilder traceFlags

traceFlagsToHexBuilder :: TraceFlags -> Builder
traceFlagsToHexBuilder traceFlags =
  Builder.word8HexFixed $ unTraceFlags traceFlags

emptyTraceFlags :: TraceFlags
emptyTraceFlags = TraceFlags { unTraceFlags = 0 }

setSampledFlag :: TraceFlags -> TraceFlags
setSampledFlag traceFlags =
  traceFlags
    { unTraceFlags = 1 .|. unTraceFlags traceFlags
    }

isSampledFlagSet :: TraceFlags -> Bool
isSampledFlagSet traceFlags =
  unTraceFlags traceFlags `testBit` 0

newtype TraceState = TraceState
  { unTraceState :: HashMap Text Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (HashMap Text Text)

emptyTraceState :: TraceState
emptyTraceState = TraceState { unTraceState = mempty }

nullTraceState :: TraceState -> Bool
nullTraceState = HashMap.null . unTraceState

sizeTraceState :: TraceState -> Int
sizeTraceState = HashMap.size . unTraceState

memberTraceState :: Key Text -> TraceState -> Bool
memberTraceState key = HashMap.member (unKey key) . unTraceState

lookupTraceState :: Key Text -> TraceState -> Maybe Text
lookupTraceState key = HashMap.lookup (unKey key) . unTraceState

findWithDefaultTraceState :: Text -> Key Text -> TraceState -> Text
findWithDefaultTraceState defVal key =
  HashMap.findWithDefault defVal (unKey key) . unTraceState

deleteTraceState :: Key Text -> TraceState -> TraceState
deleteTraceState key = TraceState . HashMap.delete (unKey key) . unTraceState

filterTraceState :: (Text -> Bool) -> TraceState -> TraceState
filterTraceState f = TraceState . HashMap.filter f . unTraceState

filterWithKeyTraceState :: (Key Text -> Text -> Bool) -> TraceState -> TraceState
filterWithKeyTraceState f = TraceState . HashMap.filterWithKey f' . unTraceState
  where
  f' keyText val = f (Key keyText) val

foldMapWithKeyTraceState
  :: forall m
   . (Monoid m)
  => (Key Text -> Text -> m)
  -> TraceState
  -> m
foldMapWithKeyTraceState f traceState =
  flip HashMap.foldMapWithKey (unTraceState traceState) \keyText val ->
    f (Key keyText) val

toListTraceState :: TraceState -> [(Key Text, Text)]
toListTraceState traceState = foldMapWithKeyTraceState (\k v -> ((k, v) :)) traceState []

newtype TraceStateBuilder a = TraceStateBuilder
  { unTraceStateBuilder :: Either (DList TraceStateError) a
  } deriving
      ( Functor -- @base@
      ) via (Either (DList TraceStateError))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap TraceStateBuilder a)

instance Applicative TraceStateBuilder where
  pure = TraceStateBuilder . Right
  liftA2 f (TraceStateBuilder mx) (TraceStateBuilder my) =
    TraceStateBuilder $ case (mx, my) of
      (Left ex, Left ey) -> Left $ ex <> ey
      (Left ex, Right {}) -> Left ex
      (Right {}, Left ey) -> Left ey
      (Right x, Right y) -> Right $ f x y

instance KV (TraceStateBuilder TraceState) where
  type KVConstraints (TraceStateBuilder TraceState) = IsTextKV
  (.@) = go
    where
    go :: Key Text -> Text -> TraceStateBuilder TraceState
    go (Key keyText) valText = do
      traceStateKey <- fmap unKey parseKey
      traceStateVal <- parseValue
      pure $ TraceState $ HashMap.singleton traceStateKey traceStateVal
      where
      parseKey :: TraceStateBuilder (Key Text)
      parseKey =
        TraceStateBuilder do
          case Text.splitOn "@" keyText of
            [] -> error "TraceStateBuilder: parseKey - impossible!"
            [simpleKeyText] -> do
              if Text.null simpleKeyText then do
                Left $ DList.singleton $ TraceStateSimpleKeyIsEmpty TraceStateSimpleKeyIsEmptyError
                  { rawValue = valText
                  }
              else if Text.length simpleKeyText > 256 then do
                Left $ DList.singleton $ TraceStateSimpleKeyTooLong TraceStateSimpleKeyTooLongError
                  { rawKey = Key simpleKeyText
                  , rawValue = valText
                  }
              else if not (isFirstSimpleKeyCharValid $ Text.head simpleKeyText) then do
                Left $ DList.singleton $ TraceStateSimpleKeyContainsInvalidChars TraceStateSimpleKeyContainsInvalidCharsError
                  { rawKey = Key simpleKeyText
                  , rawValue = valText
                  , invalidChars = Text.singleton $ Text.head simpleKeyText
                  }
              else if not (Text.null invalidChars) then do
                Left $ DList.singleton $ TraceStateSimpleKeyContainsInvalidChars TraceStateSimpleKeyContainsInvalidCharsError
                  { rawKey = Key simpleKeyText
                  , rawValue = valText
                  , invalidChars
                  }
              else do
                Right $ Key keyText
              where
              invalidChars = Text.filter (not . isValidKeyChar) simpleKeyText
            [tenantIdText, systemIdText] -> do
              if Text.null tenantIdText then do
                Left $ DList.singleton $ TraceStateTenantIdIsEmpty TraceStateTenantIdIsEmptyError
                  { rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if Text.length tenantIdText > 241 then do
                Left $ DList.singleton $ TraceStateTenantIdTooLong TraceStateTenantIdTooLongError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if not (isFirstTenantIdCharValid $ Text.head tenantIdText) then do
                Left $ DList.singleton $ TraceStateTenantIdContainsInvalidChars TraceStateTenantIdContainsInvalidCharsError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = Text.singleton $ Text.head tenantIdText
                  }
              else if not (Text.null invalidTenantIdChars) then do
                Left $ DList.singleton $ TraceStateTenantIdContainsInvalidChars TraceStateTenantIdContainsInvalidCharsError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = invalidTenantIdChars
                  }
              else if Text.null systemIdText then do
                Left $ DList.singleton $ TraceStateSystemIdIsEmpty TraceStateSystemIdIsEmptyError
                  { rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if Text.length systemIdText > 14 then do
                Left $ DList.singleton $ TraceStateSystemIdTooLong TraceStateSystemIdTooLongError
                  { rawTenantId = tenantIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  }
              else if not (isFirstSystemIdCharValid $ Text.head systemIdText) then do
                Left $ DList.singleton $ TraceStateSystemIdContainsInvalidChars TraceStateSystemIdContainsInvalidCharsError
                  { rawTenantId = systemIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = Text.singleton $ Text.head systemIdText
                  }
              else if not (Text.null invalidSystemIdChars) then do
                Left $ DList.singleton $ TraceStateSystemIdContainsInvalidChars TraceStateSystemIdContainsInvalidCharsError
                  { rawTenantId = systemIdText
                  , rawSystemId = systemIdText
                  , rawValue = valText
                  , invalidChars = invalidSystemIdChars
                  }
              else do
                Right $ Key keyText
              where
              invalidTenantIdChars = Text.filter (not . isValidKeyChar) tenantIdText
              invalidSystemIdChars = Text.filter (not . isValidKeyChar) systemIdText
            _texts -> do
              Left $ DList.singleton $ TraceStateKeyTypeUnknown TraceStateKeyTypeUnknownError
                { rawKey = Key keyText
                , rawValue = valText
                }

      parseValue :: TraceStateBuilder Text
      parseValue =
        TraceStateBuilder do
          if Text.null valText then do
            Left $ DList.singleton $ TraceStateValueIsEmpty TraceStateValueIsEmptyError
              { rawKey = Key keyText
              }
          else if Text.length valText > 256 then do
            Left $ DList.singleton $ TraceStateValueTooLong TraceStateValueTooLongError
              { rawKey = Key keyText
              , rawValue = valText
              }
          else if not (isLastValueCharValid $ Text.last valText) then do
            Left $ DList.singleton $ TraceStateValueContainsInvalidChars TraceStateValueContainsInvalidCharsError
              { rawKey = Key keyText
              , rawValue = valText
              , invalidChars = Text.singleton $ Text.last valText
              }
          else if not (Text.null invalidChars) then do
            Left $ DList.singleton $ TraceStateValueContainsInvalidChars TraceStateValueContainsInvalidCharsError
              { rawKey = Key keyText
              , rawValue = valText
              , invalidChars
              }
          else do
            pure valText
        where
        invalidChars = Text.filter (not . isValidValueChar) valText

      isValidKeyChar :: Char -> Bool
      isValidKeyChar c
        | c == '_' || c == '-' || c == '*' || c == '/' = True
        | otherwise = n >= 0x61 && n <= 0x7a
        where
        n = Char.ord c

      isFirstSimpleKeyCharValid :: Char -> Bool
      isFirstSimpleKeyCharValid c = n >= 0x61 && n <= 0x7a
        where
        n = Char.ord c

      isFirstTenantIdCharValid :: Char -> Bool
      isFirstTenantIdCharValid c = (n >= 0x61 && n <= 0x7a) || Char.isDigit c
        where
        n = Char.ord c

      isFirstSystemIdCharValid :: Char -> Bool
      isFirstSystemIdCharValid c = n >= 0x61 && n <= 0x7a
        where
        n = Char.ord c

      isValidValueChar :: Char -> Bool
      isValidValueChar c
        | n >= 0x20 && n <= 0x2b = True
        | n >= 0x2d && n <= 0x3c = True
        | n >= 0x3e && n <= 0x7e = True
        | otherwise = False
        where
        n = Char.ord c

      isLastValueCharValid :: Char -> Bool
      isLastValueCharValid c = isValidValueChar c && n /= 0x20
        where
        n = Char.ord c

buildTraceState
  :: forall m
   . (MonadThrow m)
  => TraceStateBuilder TraceState
  -> m TraceState
buildTraceState builder =
  case buildTraceStatePure builder of
    Left err -> throwM err
    Right x -> pure x

buildTraceStatePure :: TraceStateBuilder TraceState -> Either TraceStateErrors TraceState
buildTraceStatePure = first (TraceStateErrors . DList.toList) . unTraceStateBuilder

newtype SpanEvents (attrs :: AttrsFor -> Type) = SpanEvents
  { unSpanEvents :: DList (SpanEvent attrs)
  }

instance ToJSON (SpanEvents Attrs) where
  toJSON = toJSON . unSpanEvents

deriving stock instance (Eq (attrs 'AttrsForSpanEvent)) => Eq (SpanEvents attrs)
deriving stock instance (Show (attrs 'AttrsForSpanEvent)) => Show (SpanEvents attrs)
deriving via (DList (SpanEvent attrs)) instance Monoid (SpanEvents attrs)
deriving via (DList (SpanEvent attrs)) instance Semigroup (SpanEvents attrs)

spanEventsFromList :: [SpanEvent attrs] -> SpanEvents attrs
spanEventsFromList = SpanEvents . DList.fromList

spanEventsToList :: SpanEvents attrs -> [SpanEvent attrs]
spanEventsToList = Foldable.toList . unSpanEvents

freezeAllSpanEventAttrs
  :: AttrsLimits 'AttrsForSpanEvent
  -> SpanEvents AttrsBuilder
  -> SpanEvents Attrs
freezeAllSpanEventAttrs attrsLimits spanEvent =
  SpanEvents
    $ fmap (freezeSpanEventAttrs attrsLimits)
    $ unSpanEvents spanEvent

data SpanEvent (attrs :: AttrsFor -> Type) = SpanEvent
  { spanEventName :: SpanEventName
  , spanEventTimestamp :: Timestamp
  , spanEventAttrs :: attrs 'AttrsForSpanEvent
  }

instance ToJSON (SpanEvent Attrs) where
  toJSON spanEvent =
    Aeson.object
      [ "name" .= spanEventName
      , "timestamp" .= spanEventTimestamp
      , "attributes" .= spanEventAttrs
      ]
    where
    SpanEvent
      { spanEventName
      , spanEventTimestamp
      , spanEventAttrs
      } = spanEvent

deriving stock instance (Eq (attrs 'AttrsForSpanEvent)) => Eq (SpanEvent attrs)
deriving stock instance (Show (attrs 'AttrsForSpanEvent)) => Show (SpanEvent attrs)

freezeSpanEventAttrs
  :: AttrsLimits 'AttrsForSpanEvent
  -> SpanEvent AttrsBuilder
  -> SpanEvent Attrs
freezeSpanEventAttrs attrsLimits spanEvent =
  spanEvent
    { spanEventAttrs = runAttrsBuilder (spanEventAttrs spanEvent) attrsLimits
    }

newtype SpanEventSpecs = SpanEventSpecs
  { unSpanEventSpecs :: DList SpanEventSpec
  } deriving (Monoid, Semigroup) via (DList SpanEventSpec)

singletonSpanEventSpecs :: SpanEventSpec -> SpanEventSpecs
singletonSpanEventSpecs = SpanEventSpecs . DList.singleton

spanEventSpecsFromList :: [SpanEventSpec] -> SpanEventSpecs
spanEventSpecsFromList = SpanEventSpecs . DList.fromList

spanEventSpecsToList :: SpanEventSpecs -> [SpanEventSpec]
spanEventSpecsToList = Foldable.toList . unSpanEventSpecs

data SpanEventSpec = SpanEventSpec
  { spanEventSpecName :: SpanEventName
  , spanEventSpecTimestamp :: TimestampSource
  , spanEventSpecAttrs :: AttrsBuilder 'AttrsForSpanEvent
  }

defaultSpanEventSpec :: SpanEventSpec
defaultSpanEventSpec =
  SpanEventSpec
    { spanEventSpecName = ""
    , spanEventSpecTimestamp = TimestampSourceNow
    , spanEventSpecAttrs = mempty
    }

instance IsString SpanEventSpec where
  fromString s =
    defaultSpanEventSpec
      { spanEventSpecName = fromString s
      }

newtype SpanEventName = SpanEventName
  { unSpanEventName :: Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (Text)

instance IsString SpanEventName where
  fromString = SpanEventName . Text.pack

newtype SpanLinks (attrs :: AttrsFor -> Type) = SpanLinks
  { unSpanLinks :: DList (SpanLink attrs)
  }

instance ToJSON (SpanLinks Attrs) where
  toJSON = toJSON . unSpanLinks

deriving stock instance (Eq (attrs 'AttrsForSpanLink)) => Eq (SpanLinks attrs)
deriving stock instance (Show (attrs 'AttrsForSpanLink)) => Show (SpanLinks attrs)
deriving via (DList (SpanLink attrs)) instance Monoid (SpanLinks attrs)
deriving via (DList (SpanLink attrs)) instance Semigroup (SpanLinks attrs)

spanLinksFromList :: [SpanLink attrs] -> SpanLinks attrs
spanLinksFromList = SpanLinks . DList.fromList

spanLinksToList :: SpanLinks attrs -> [SpanLink attrs]
spanLinksToList = Foldable.toList . unSpanLinks

freezeAllSpanLinkAttrs
  :: AttrsLimits 'AttrsForSpanLink
  -> SpanLinks AttrsBuilder
  -> SpanLinks Attrs
freezeAllSpanLinkAttrs attrsLimits spanLink =
  SpanLinks
    $ fmap (freezeSpanLinkAttrs attrsLimits)
    $ unSpanLinks spanLink

newtype SpanLinkSpecs = SpanLinkSpecs
  { unSpanLinkSpecs :: DList SpanLinkSpec
  } deriving (Monoid, Semigroup) via (DList SpanLinkSpec)

singletonSpanLinkSpecs :: SpanLinkSpec -> SpanLinkSpecs
singletonSpanLinkSpecs = SpanLinkSpecs . DList.singleton

spanLinkSpecsFromList :: [SpanLinkSpec] -> SpanLinkSpecs
spanLinkSpecsFromList = SpanLinkSpecs . DList.fromList

spanLinkSpecsToList :: SpanLinkSpecs -> [SpanLinkSpec]
spanLinkSpecsToList = Foldable.toList . unSpanLinkSpecs

data SpanLink (attrs :: AttrsFor -> Type) = SpanLink
  { spanLinkSpanContext :: SpanContext
  , spanLinkAttrs :: attrs 'AttrsForSpanLink
  }

instance ToJSON (SpanLink Attrs) where
  toJSON spanLink =
    Aeson.object
      [ "spanContext" .= spanLinkSpanContext
      , "attributes" .= spanLinkAttrs
      ]
    where
    SpanLink
      { spanLinkSpanContext
      , spanLinkAttrs
      } = spanLink

deriving stock instance (Eq (attrs 'AttrsForSpanLink)) => Eq (SpanLink attrs)
deriving stock instance (Show (attrs 'AttrsForSpanLink)) => Show (SpanLink attrs)

freezeSpanLinkAttrs
  :: AttrsLimits 'AttrsForSpanLink
  -> SpanLink AttrsBuilder
  -> SpanLink Attrs
freezeSpanLinkAttrs attrsLimits spanLink =
  spanLink
    { spanLinkAttrs = runAttrsBuilder (spanLinkAttrs spanLink) attrsLimits
    }

newtype SpanLinkName = SpanLinkName
  { unSpanLinkName :: Text
  } deriving stock (Eq, Show)

instance IsString SpanLinkName where
  fromString = SpanLinkName . Text.pack

data SpanLinkSpec = SpanLinkSpec
  { spanLinkSpecSpanContext :: SpanContext
  , spanLinkSpecAttrs :: AttrsBuilder 'AttrsForSpanLink
  }

defaultSpanLinkSpec :: SpanLinkSpec
defaultSpanLinkSpec =
  SpanLinkSpec
    { spanLinkSpecSpanContext = emptySpanContext
    , spanLinkSpecAttrs = mempty
    }

data NewSpanSpec = NewSpanSpec
  { newSpanSpecName :: SpanName
  , newSpanSpecParentContext :: Maybe Context
  , newSpanSpecStart :: TimestampSource
  , newSpanSpecKind :: SpanKind
  , newSpanSpecAttrs :: AttrsBuilder 'AttrsForSpan
  , newSpanSpecLinks :: SpanLinkSpecs
  }

instance IsString (NewSpanSpec) where
  fromString s =
    defaultNewSpanSpec
      { newSpanSpecName = fromString s
      }

defaultNewSpanSpec :: NewSpanSpec
defaultNewSpanSpec =
  NewSpanSpec
    { newSpanSpecName = ""
    , newSpanSpecParentContext = Nothing
    , newSpanSpecStart = TimestampSourceNow
    , newSpanSpecKind = SpanKindInternal
    , newSpanSpecAttrs = mempty
    , newSpanSpecLinks = mempty
    }

data UpdateSpanSpec = UpdateSpanSpec
  { updateSpanSpecName :: Maybe SpanName
  , updateSpanSpecStatus :: Maybe SpanStatus
  , updateSpanSpecAttrs :: Maybe (AttrsBuilder 'AttrsForSpan)
  , updateSpanSpecEvents :: Maybe SpanEventSpecs
  }

defaultUpdateSpanSpec :: UpdateSpanSpec
defaultUpdateSpanSpec =
  UpdateSpanSpec
    { updateSpanSpecName = Nothing
    , updateSpanSpecStatus = Nothing
    , updateSpanSpecAttrs = Nothing
    , updateSpanSpecEvents = Nothing
    }

buildSpanUpdater
  :: forall m
   . (Monad m)
  => m Timestamp
  -> UpdateSpanSpec
  -> m (Span AttrsBuilder -> Span AttrsBuilder)
buildSpanUpdater getTimestamp updateSpanSpec = do
  newSpanEvents <- do
    fmap SpanEvents do
      case updateSpanSpecEvents of
        Nothing -> pure mempty
        Just spanEventSpecs -> do
          Traversable.for (unSpanEventSpecs spanEventSpecs) \spanEventSpec -> do
            spanEventTimestamp <- do
              case spanEventSpecTimestamp spanEventSpec of
                TimestampSourceAt timestamp -> pure timestamp
                TimestampSourceNow -> getTimestamp
            pure SpanEvent
              { spanEventName = spanEventSpecName spanEventSpec
              , spanEventTimestamp
              , spanEventAttrs = spanEventSpecAttrs spanEventSpec
              }
  pure \span ->
    if not $ spanIsRecording span then
      span
    else
      span
        { spanName =
            Maybe.fromMaybe (spanName span) updateSpanSpecName
        , spanStatus =
            Maybe.fromMaybe (spanStatus span) updateSpanSpecStatus
        , spanAttrs =
            case updateSpanSpecAttrs of
              Nothing -> spanAttrs span
              Just attrsBuilder ->
                attrsBuilder <> spanAttrs span
        , spanEvents =
            spanEvents span <> newSpanEvents
        }
  where
  UpdateSpanSpec
    { updateSpanSpecName
    , updateSpanSpecStatus
    , updateSpanSpecAttrs
    , updateSpanSpecEvents
    } = updateSpanSpec

recordException
  :: SomeException
  -> Bool
  -> TimestampSource
  -> AttrsBuilder 'AttrsForSpanEvent
  -> UpdateSpanSpec
recordException someEx escaped timestamp attributes =
  defaultUpdateSpanSpec
    { updateSpanSpecEvents =
        Just $ spanEventSpecsFromList
          [ exceptionEvent someEx escaped timestamp attributes
          ]
    }

exceptionEvent
  :: SomeException
  -> Bool
  -> TimestampSource
  -> AttrsBuilder 'AttrsForSpanEvent
  -> SpanEventSpec
exceptionEvent (SomeException e) escaped timestamp attributes =
  SpanEventSpec
    { spanEventSpecName = "exception"
    , spanEventSpecTimestamp = timestamp
    , spanEventSpecAttrs =
        attributes
          <> EXCEPTION_TYPE .@ show (Typeable.typeOf e)
          <> EXCEPTION_MESSAGE .@ Exception.displayException e
          <> EXCEPTION_ESCAPED .@ escaped
    }

newtype SpanName = SpanName
  { unSpanName :: Text
  } deriving stock (Eq, Show)
    deriving (ToJSON) via (Text)

instance IsString SpanName where
  fromString = SpanName . Text.pack

-- TODO: Need dedicated interface for getting/updating
newtype MutableSpan = MutableSpan
  { unMutableSpan :: IORef (Span AttrsBuilder)
  }

-- TODO: Add reference to Resource?
-- See https://opentelemetry.io/docs/reference/specification/trace/sdk/#additional-span-interfaces
data Span (attrs :: AttrsFor -> Type) = Span
  { spanParent :: SpanParent
  , spanContext :: SpanContext
  , spanName :: SpanName
  , spanStatus :: SpanStatus
  , spanStart :: Timestamp
  , spanFrozenAt :: SpanFrozenAt attrs
  , spanKind :: SpanKind
  , spanAttrs :: attrs 'AttrsForSpan
  , spanLinks :: SpanLinks attrs
  , spanEvents :: SpanEvents attrs
  , spanIsRecording :: Bool
  , spanInstrumentationScope :: InstrumentationScope
  }

spanIsRemote :: Span attrs -> Bool
spanIsRemote span = spanContextIsRemote spanContext
  where
  Span { spanContext } = span

spanIsSampled :: Span attrs -> Bool
spanIsSampled span = spanContextIsSampled spanContext
  where
  Span { spanContext } = span

instance ToJSON (Span Attrs) where
  toJSON span =
    Aeson.object
      [ "parent" .= spanParent
      , "spanContext" .= spanContext
      , "name" .= spanName
      , "status" .= spanStatus
      , "start" .= spanStart
      , "frozenAt" .= spanFrozenAt
      , "kind" .= spanKind
      , "attributes" .= spanAttrs
      , "links" .= spanLinks
      , "events" .= spanEvents
      , "isRecording" .= spanIsRecording
      , "instrumentationScope" .= spanInstrumentationScope
      ]
    where
    Span
      { spanParent
      , spanContext
      , spanName
      , spanStatus
      , spanStart
      , spanFrozenAt
      , spanKind
      , spanAttrs
      , spanLinks
      , spanEvents
      , spanIsRecording
      , spanInstrumentationScope
      } = span

type family SpanFrozenAt (attrs :: AttrsFor -> Type) :: Type where
  SpanFrozenAt AttrsBuilder = Maybe Timestamp
  SpanFrozenAt Attrs = Timestamp

freezeSpan
  :: Timestamp
  -> AttrsLimits 'AttrsForSpanLink
  -> AttrsLimits 'AttrsForSpanEvent
  -> AttrsLimits 'AttrsForSpan
  -> Span AttrsBuilder
  -> Span Attrs
freezeSpan defaultSpanFrozenAt spanLinkAttrsLimits spanEventAttrsLimits spanAttrsLimits span =
  span
    { spanFrozenAt = Maybe.fromMaybe defaultSpanFrozenAt $ spanFrozenAt span
    , spanAttrs =
        runAttrsBuilder (spanAttrs span) spanAttrsLimits
    , spanLinks =
        freezeAllSpanLinkAttrs spanLinkAttrsLimits $ spanLinks span
    , spanEvents =
        freezeAllSpanEventAttrs spanEventAttrsLimits $ spanEvents span
    }

-- TODO: Rename back to SpanLineage?
data SpanParent
  = SpanParentRoot
  | SpanParentChildOf SpanContext
  deriving stock (Eq, Show)

instance ToJSON SpanParent where
  toJSON = \case
    SpanParentRoot ->
      Aeson.object
        [ "tag" .= ("root" :: Text)
        ]
    SpanParentChildOf spanContext ->
      Aeson.object
        [ "tag" .= ("childOf" :: Text)
        , "content" .= toJSON spanContext
        ]

pattern Root :: SpanParent
pattern Root <- SpanParentRoot where
  Root = SpanParentRoot

pattern ChildOf :: SpanContext -> SpanParent
pattern ChildOf sc <- SpanParentChildOf sc where
  ChildOf sc = SpanParentChildOf sc

{-# COMPLETE Root, ChildOf :: SpanParent #-}

data SpanKind
  = SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  | SpanKindInternal
  deriving stock (Eq, Show)

instance ToJSON SpanKind where
  toJSON = \case
    SpanKindServer -> Aeson.object ["tag" .= ("server" :: Text)]
    SpanKindClient -> Aeson.object ["tag" .= ("client" :: Text)]
    SpanKindProducer -> Aeson.object ["tag" .= ("producer" :: Text)]
    SpanKindConsumer -> Aeson.object ["tag" .= ("consumer" :: Text)]
    SpanKindInternal -> Aeson.object ["tag" .= ("internal" :: Text)]

pattern Server :: SpanKind
pattern Server <- SpanKindServer where
  Server = SpanKindServer

pattern Client :: SpanKind
pattern Client <- SpanKindClient where
  Client = SpanKindClient

pattern Producer :: SpanKind
pattern Producer <- SpanKindProducer where
  Producer = SpanKindProducer

pattern Consumer :: SpanKind
pattern Consumer <- SpanKindConsumer where
  Consumer = SpanKindConsumer

pattern Internal :: SpanKind
pattern Internal <- SpanKindInternal where
  Internal = SpanKindInternal

{-# COMPLETE Server, Client, Producer, Consumer, Internal :: SpanKind #-}

data SpanStatus
  = SpanStatusUnset
  | SpanStatusOk
  | SpanStatusError Text
  deriving stock (Eq, Show)

instance ToJSON SpanStatus where
  toJSON = \case
    SpanStatusUnset ->
      Aeson.object
        [ "tag" .= ("unset" :: Text)
        ]
    SpanStatusOk ->
      Aeson.object
        [ "tag" .= ("ok" :: Text)
        ]
    SpanStatusError errText ->
      Aeson.object
        [ "tag" .= ("error" :: Text)
        , "content" .= toJSON errText
        ]

pattern Unset :: SpanStatus
pattern Unset <- SpanStatusUnset where
  Unset = SpanStatusUnset

pattern OK :: SpanStatus
pattern OK <- SpanStatusOk where
  OK = SpanStatusOk

pattern Error :: Text -> SpanStatus
pattern Error errText <- SpanStatusError errText where
  Error errText = SpanStatusError errText

{-# COMPLETE Unset, OK, Error :: SpanStatus #-}

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.