{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    MonadTracing(..)
  , trace
  , traceCS
  , updateSpan

  , TracingT(..)
  , runTracingT
  , mapTracingT

  , NoTracingT(..)
  , runNoTracingT
  , mapNoTracingT

  , Tracer(..)

  , Context(..)
  , activateSpan

  , SpanDetails(..)
  , defaultSpanDetails

  , SpanUpdate(..)
  , defaultSpanUpdate

  , Span(..)
  , EndedSpan(..)

  , SpanLineageSource(..)
  , implicitSpanLineageSource
  , explicitSpanLineageSource

  , SpanLineage(..)
  , rootSpanLineage
  , childSpanLineage

  , SpanKind(..)
  , serverSpanKind
  , clientSpanKind
  , producerSpanKind
  , consumerSpanKind
  , internalSpanKind

  , SpanStatus(..)
  , unsetSpanStatus
  , okSpanStatus
  , errorSpanStatus

  , SpanContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceFlags(..)
  , TraceState(..)

  , TimestampSource(..)
  , nowTimestampSource
  , atTimestampSource

  , Timestamp(..)

  , Attributes(..)

  , SpanEvents(..)

  , SpanLinks(..)
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.State (StateT)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, HasCallStack, SrcLoc, callStack)
import Prelude hiding (span)
import qualified Context
import qualified Context.Internal
import qualified Context.Internal as Context.Internal.Store (Store(ref))
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception.Safe as Exceptions
import qualified Control.Monad.Accum as MTL.Accum
import qualified Control.Monad.Cont as MTL.Cont
import qualified Control.Monad.Except as MTL.Except
import qualified Control.Monad.IO.Unlift as Unlift
import qualified Control.Monad.RWS.Class as MTL.RWS.Class
import qualified Control.Monad.Reader as MTL.Reader
import qualified Control.Monad.Select as MTL.Select
import qualified Control.Monad.State as MTL.State
import qualified Control.Monad.Trans.Accum as Trans.Accum
import qualified Control.Monad.Trans.Cont as Trans.Cont
import qualified Control.Monad.Trans.Except as Trans.Except
import qualified Control.Monad.Trans.Identity as Trans.Identity
import qualified Control.Monad.Trans.Maybe as Trans.Maybe
import qualified Control.Monad.Trans.RWS.CPS as Trans.RWS.CPS
import qualified Control.Monad.Trans.RWS.Lazy as Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as Trans.RWS.Strict
import qualified Control.Monad.Trans.Reader as Trans.Reader
import qualified Control.Monad.Trans.Select as Trans.Select
import qualified Control.Monad.Trans.State as Trans.State
import qualified Control.Monad.Trans.Writer.CPS as Trans.Writer.CPS
import qualified Control.Monad.Trans.Writer.Lazy as Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.Writer.Strict
import qualified Control.Monad.Writer.Class as MTL.Writer.Class
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

data Context = Context
  { contextStoreSpan :: Context.Store Span
  }

activateSpan
  :: forall m a
   . (MonadIO m, Exceptions.MonadMask m)
  => Context
  -> Span
  -> m a
  -> m a
activateSpan Context { contextStoreSpan } = Context.use contextStoreSpan

--foo :: (MonadTracing m) => m ()
--foo = do
--  trace "abc" defaultSpanDetails do
--    trace "def" defaultSpanDetails do
--
--      pure ()

class (Monad m) => MonadTracing m where
  monadTracingTrace :: CallStack -> SpanName -> SpanDetails -> m a -> m a
  monadTracingUpdateSpan :: SpanUpdate -> Span -> m (Maybe Span)

trace
  :: forall m a
   . (MonadTracing m, HasCallStack)
  => SpanName
  -> SpanDetails
  -> m a
  -> m a
trace = traceCS callStack

traceCS
  :: forall m a
   . (MonadTracing m)
  => CallStack
  -> SpanName
  -> SpanDetails
  -> m a
  -> m a
traceCS = monadTracingTrace

updateSpan
  :: forall m
   . (MonadTracing m)
  => SpanUpdate
  -> Span
  -> m (Maybe Span)
updateSpan = monadTracingUpdateSpan

-- TODO: Account for change where now the Span is passed to callback
instance (MonadTracing m, Monoid w) => MonadTracing (AccumT w m) where
  monadTracingTrace cs spanName = Trans.Accum.mapAccumT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (ContT r m) where
  monadTracingTrace cs spanName = Trans.Cont.mapContT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (ExceptT e m) where
  monadTracingTrace cs spanName = Trans.Except.mapExceptT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (IdentityT m) where
  monadTracingTrace cs spanName = Trans.Identity.mapIdentityT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (MaybeT m) where
  monadTracingTrace cs spanName = Trans.Maybe.mapMaybeT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (ReaderT r m) where
  monadTracingTrace cs spanName = Trans.Reader.mapReaderT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (StateT r m) where
  monadTracingTrace cs spanName = Trans.State.mapStateT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.CPS.RWST r w s m) where
  monadTracingTrace cs spanName = Trans.RWS.CPS.mapRWST . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.Lazy.RWST r w s m) where
  monadTracingTrace cs spanName = Trans.RWS.Lazy.mapRWST . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.Strict.RWST r w s m) where
  monadTracingTrace cs spanName = Trans.RWS.Strict.mapRWST . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m) => MonadTracing (SelectT r m) where
  monadTracingTrace cs spanName = Trans.Select.mapSelectT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.CPS.WriterT w m) where
  monadTracingTrace cs spanName = Trans.Writer.CPS.mapWriterT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.Lazy.WriterT w m) where
  monadTracingTrace cs spanName = Trans.Writer.Lazy.mapWriterT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.Strict.WriterT w m) where
  monadTracingTrace cs spanName = Trans.Writer.Strict.mapWriterT . monadTracingTrace cs spanName
  monadTracingUpdateSpan spanUpdate = lift . monadTracingUpdateSpan spanUpdate

newtype TracingT m a = TracingT
  { unTracingT :: ReaderT Tracer m a
  } deriving newtype
      ( Applicative
      , Functor
      , Monad
      , MonadFail
      , MonadIO
      , Exceptions.MonadCatch
      , Exceptions.MonadMask
      , Exceptions.MonadThrow
      , MTL.Accum.MonadAccum w
      , MTL.Cont.MonadCont
      , MTL.Except.MonadError e
      , MTL.RWS.Class.MonadRWS r w s
      , MTL.Select.MonadSelect r
      , MTL.State.MonadState s
      , MTL.Writer.Class.MonadWriter w
      , Unlift.MonadUnliftIO
      -- TODO: MonadLogger instance
      -- TODO: Add @since annotations
      )

instance MonadTrans TracingT where
  lift = TracingT . ReaderT . const

instance (MTL.Reader.MonadReader r m) => MTL.Reader.MonadReader r (TracingT m) where
  ask = lift MTL.Reader.ask
  reader = lift . MTL.Reader.reader
  local = mapTracingT . MTL.Reader.local

instance (MonadIO m, Exceptions.MonadMask m) => MonadTracing (TracingT m) where
  monadTracingTrace _cs spanName spanDetails action =
    TracingT $ ReaderT \tracer -> do
      Exceptions.bracket (start tracer) (end tracer) \span -> do
        activateSpan (tracerContext tracer) span do
          runTracingT action tracer
    where
    start tracer = liftIO $ tracerStartSpan tracer spanName spanDetails
    end tracer span = liftIO $ tracerEndSpan tracer span

  monadTracingUpdateSpan spanUpdate span = do
    TracingT $ ReaderT \tracer -> do
      liftIO $ veryUnsafeSpanReplace (tracerContext tracer) span'
    where
    span' =
      span
        { spanName =
            Maybe.fromMaybe (spanName span) spanUpdateName
        , spanStatus =
            Maybe.fromMaybe (spanStatus span) $ spanUpdateStatus
        , spanAttributes =
            maybe id (\as -> (<> as)) spanUpdateAttributes $ spanAttributes span
        , spanEvents =
            maybe id (\es -> (<> es)) spanUpdateEvents $ spanEvents span
        }
    SpanUpdate
      { spanUpdateName
      , spanUpdateStatus
      , spanUpdateAttributes
      , spanUpdateEvents
      } = spanUpdate

runTracingT :: TracingT m a -> Tracer -> m a
runTracingT action = runReaderT (unTracingT action)

mapTracingT :: (m a -> n b) -> TracingT m a -> TracingT n b
mapTracingT f action =
  TracingT $ ReaderT \tracer -> do
    f $ runTracingT action tracer

veryUnsafeSpanReplace :: Context -> Span -> IO (Maybe Span)
veryUnsafeSpanReplace context replacementSpan = do
  threadId <- Concurrent.myThreadId
  IORef.atomicModifyIORef' ref
    \state@Context.Internal.State { Context.Internal.stacks } ->
      case Map.lookup threadId stacks of
        Nothing ->
          (state, Nothing)
        Just [] -> bug "veryUnsafeSpanReplace"
        Just spans ->
          ( state
              { Context.Internal.stacks =
                  -- TODO: Change to a fold
                  flip (Map.insert threadId) stacks $ flip fmap spans \span ->
                    if spanContext span /= spanContext replacementSpan then
                      span
                    else
                      replacementSpan
              }
          , Just undefined -- TODO: Make sure we actually find the thing in the fold above
          )
  where
  Context.Internal.Store { Context.Internal.Store.ref } = contextStoreSpan
  Context { contextStoreSpan } = context

newtype NoTracingT m a = NoTracingT
  { unNoTracingT :: m a
  } deriving newtype
      ( Applicative
      , Functor
      , Monad
      , MonadFail
      , MonadIO
      , Exceptions.MonadCatch
      , Exceptions.MonadMask
      , Exceptions.MonadThrow
      , MTL.Accum.MonadAccum w
      , MTL.Cont.MonadCont
      , MTL.Except.MonadError e
      , MTL.RWS.Class.MonadRWS r w s
      , MTL.Reader.MonadReader r
      , MTL.Select.MonadSelect r
      , MTL.State.MonadState s
      , MTL.Writer.Class.MonadWriter w
      , Unlift.MonadUnliftIO
      )

instance MonadTrans NoTracingT where
  lift = NoTracingT

instance (Monad m) => MonadTracing (NoTracingT m) where
  monadTracingTrace _cs _spanName _spanDetails action = action
  monadTracingUpdateSpan _spanUpdate _span = pure Nothing

runNoTracingT :: NoTracingT m a -> m a
runNoTracingT = unNoTracingT

mapNoTracingT :: (m a -> n b) -> NoTracingT m a -> NoTracingT n b
mapNoTracingT f action = do
  NoTracingT $ f $ runNoTracingT action

data Tracer = Tracer
  { tracerStartSpan :: SpanName -> SpanDetails -> IO Span
  , tracerEndSpan :: Span -> IO ()
  , tracerContext :: Context
  }

data SpanDetails = SpanDetails
  { spanDetailsLineageSource :: SpanLineageSource
  , spanDetailsStart :: TimestampSource
  , spanDetailsKind :: SpanKind
  , spanDetailsAttributes :: Attributes
  , spanDetailsLinks :: SpanLinks
  } deriving stock (Eq, Generic, Show)

defaultSpanDetails :: SpanDetails
defaultSpanDetails =
  SpanDetails
    { spanDetailsLineageSource = implicitSpanLineageSource
    , spanDetailsStart = nowTimestampSource
    , spanDetailsKind = internalSpanKind
    , spanDetailsAttributes = mempty
    , spanDetailsLinks = mempty
    }

data SpanUpdate = SpanUpdate
  { spanUpdateName :: Maybe Text
  , spanUpdateStatus :: Maybe SpanStatus
  , spanUpdateAttributes :: Maybe Attributes
  , spanUpdateEvents :: Maybe SpanEvents
  }

defaultSpanUpdate :: SpanUpdate
defaultSpanUpdate =
  SpanUpdate
    { spanUpdateName = Nothing
    , spanUpdateStatus = Nothing
    , spanUpdateAttributes = Nothing
    , spanUpdateEvents = Nothing
    }

newtype SpanName = SpanName
  { unSpanName :: Text
  } deriving stock (Eq, Show, Generic)
    deriving newtype (Semigroup)

instance IsString SpanName where
  fromString = SpanName . Text.pack

data Span = Span
  { spanLineage :: SpanLineage
  , spanContext :: SpanContext
  , spanName :: Text
  , spanStatus :: SpanStatus
  , spanStart :: Timestamp
  , spanKind :: SpanKind
  , spanAttributes :: Attributes
  , spanEvents :: SpanEvents
  , spanLinks :: SpanLinks
  , spanIsRecording :: Bool
  , spanSrcLoc :: SrcLoc
  } deriving stock (Eq, Generic, Show)

data EndedSpan = EndedSpan
  { endedSpanLineage :: SpanLineage
  , endedSpanContext :: SpanContext
  , endedSpanName :: Text
  , endedSpanStatus :: SpanStatus
  , endedSpanStart :: Timestamp
  , endedSpanEnd :: Timestamp
  , endedSpanKind :: SpanKind
  , endedSpanAttributes :: Attributes
  , endedSpanEvents :: SpanEvents
  , endedSpanLinks :: SpanLinks
  , endedSpanSrcLoc :: SrcLoc
  } deriving stock (Eq, Generic, Show)

data SpanLineageSource
  = SpanLineageSourceImplicit
  | SpanLineageSourceExplicit SpanLineage
  deriving stock (Eq, Generic, Show)

implicitSpanLineageSource :: SpanLineageSource
implicitSpanLineageSource = SpanLineageSourceImplicit

explicitSpanLineageSource :: SpanLineage -> SpanLineageSource
explicitSpanLineageSource = SpanLineageSourceExplicit

data SpanLineage
  = SpanLineageRoot
  | SpanLineageChildOf SpanContext
  deriving stock (Eq, Generic, Show)

rootSpanLineage :: SpanLineage
rootSpanLineage = SpanLineageRoot

childSpanLineage :: SpanContext -> SpanLineage
childSpanLineage = SpanLineageChildOf

data SpanKind
  = SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  | SpanKindInternal
  deriving stock (Eq, Generic, Show)

serverSpanKind :: SpanKind
serverSpanKind = SpanKindServer

clientSpanKind :: SpanKind
clientSpanKind = SpanKindClient

producerSpanKind :: SpanKind
producerSpanKind = SpanKindProducer

consumerSpanKind :: SpanKind
consumerSpanKind = SpanKindConsumer

internalSpanKind :: SpanKind
internalSpanKind = SpanKindInternal

data SpanStatus
  = SpanStatusUnset
  | SpanStatusOk
  | SpanStatusError Text
  deriving stock (Eq, Generic, Show)

unsetSpanStatus :: SpanStatus
unsetSpanStatus = SpanStatusUnset

okSpanStatus :: SpanStatus
okSpanStatus = SpanStatusOk

errorSpanStatus :: Text -> SpanStatus
errorSpanStatus = SpanStatusError

data SpanContext = SpanContext
  { spanContextTraceId :: TraceId
  , spanContextSpanId :: SpanId
  , spanContextTraceFlags :: TraceFlags
  , spanContextTraceState :: TraceState
  } deriving stock (Eq, Generic, Show)

data TraceId = TraceId
  { traceIdHi :: Word64
  , traceIdLo :: Word64
  } deriving stock (Eq, Generic, Show)

data SpanId = SpanId
  { spanIdLo :: Word64
  } deriving stock (Eq, Generic, Show)

newtype TraceFlags = TraceFlags
  { unTraceFlags :: Word8
  } deriving stock (Eq, Generic, Show)

newtype TraceState = TraceState
  { unTraceState :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)

data TimestampSource
  = TimestampSourceNow
  | TimestampSourceAt Timestamp
  deriving stock (Eq, Generic, Show)

nowTimestampSource :: TimestampSource
nowTimestampSource = TimestampSourceNow

atTimestampSource :: Timestamp -> TimestampSource
atTimestampSource = TimestampSourceAt

newtype Timestamp = Timestamp
  { unTimestamp :: Word64 -- ^ nanoseconds
  } deriving stock (Eq, Generic, Show)

newtype Attributes = Attributes
  { unAttributes :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)
    deriving newtype (Semigroup, Monoid)

newtype SpanEvents = SpanEvents
  { unSpanEvents :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)
    deriving newtype (Semigroup, Monoid)

newtype SpanLinks = SpanLinks
  { unSpanLinks :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)
    deriving newtype (Semigroup, Monoid)

bug :: HasCallStack => String -> a
bug prefix =
  error $
    "OTel.API.Trace.Internal." <> prefix <> ": Impossible! (if you see this "
      <> "message, please report it as a bug at "
      <> "https://github.com/jship/opentelemetry-haskell)"

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
