{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Internal
  ( -- * Disclaimer
    -- $disclaimer
    MonadTracing(..)
  , trace
  , traceCS

  , TracingT(..)
  , runTracingT
  , mapTracingT

  , NoTracingT(..)
  , runNoTracingT
  , mapNoTracingT

  , Tracer(..)

  , Context(..)
  , useSpanContext

  , SpanDetails(..)
  , defaultSpanDetails

  , Span(..)

  , SpanLineageSource(..)
  , implicitSpanLineageSource
  , explicitSpanLineageSource

  , SpanLineage(..)
  , rootSpanLineage
  , childSpanLineage

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
import Data.Time (UTCTime)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, HasCallStack, callStack)
import Prelude hiding (span)
import qualified Context
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
import qualified Data.Text as Text

class (Monad m) => MonadTracing m where
  monadTracingTrace
    :: CallStack
    -> SpanName
    -> SpanDetails
    -> (Span -> m a)
    -> m a

trace
  :: forall m a
   . (MonadTracing m, HasCallStack)
  => SpanName
  -> SpanDetails
  -> (Span -> m a)
  -> m a
trace = traceCS callStack

traceCS
  :: forall m a
   . (MonadTracing m)
  => CallStack
  -> SpanName
  -> SpanDetails
  -> (Span -> m a)
  -> m a
traceCS cs spanName = monadTracingTrace cs spanName

-- TODO: Account for change where now the Span is passed to callback
instance (MonadTracing m, Monoid w) => MonadTracing (AccumT w m) where
  monadTracingTrace cs spanName = Trans.Accum.mapAccumT . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (ContT r m) where
  monadTracingTrace cs spanName = Trans.Cont.mapContT . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (ExceptT e m) where
  monadTracingTrace cs spanName = Trans.Except.mapExceptT . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (IdentityT m) where
  monadTracingTrace cs spanName = Trans.Identity.mapIdentityT . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (MaybeT m) where
  monadTracingTrace cs spanName = Trans.Maybe.mapMaybeT . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (ReaderT r m) where
  monadTracingTrace cs spanName = Trans.Reader.mapReaderT . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (StateT r m) where
  monadTracingTrace cs spanName = Trans.State.mapStateT . monadTracingTrace cs spanName

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.CPS.RWST r w s m) where
  monadTracingTrace cs spanName = Trans.RWS.CPS.mapRWST . monadTracingTrace cs spanName

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.Lazy.RWST r w s m) where
  monadTracingTrace cs spanName = Trans.RWS.Lazy.mapRWST . monadTracingTrace cs spanName

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.Strict.RWST r w s m) where
  monadTracingTrace cs spanName = Trans.RWS.Strict.mapRWST . monadTracingTrace cs spanName

instance (MonadTracing m) => MonadTracing (SelectT r m) where
  monadTracingTrace cs spanName = Trans.Select.mapSelectT . monadTracingTrace cs spanName

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.CPS.WriterT w m) where
  monadTracingTrace cs spanName = Trans.Writer.CPS.mapWriterT . monadTracingTrace cs spanName

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.Lazy.WriterT w m) where
  monadTracingTrace cs spanName = Trans.Writer.Lazy.mapWriterT . monadTracingTrace cs spanName

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.Strict.WriterT w m) where
  monadTracingTrace cs spanName = Trans.Writer.Strict.mapWriterT . monadTracingTrace cs spanName

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
        let spanCtx = spanContext span
        useSpanContext (tracerContext tracer) spanCtx do
          runTracingT action tracer
    where
    start tracer = liftIO $ tracerStartSpan tracer spanName spanDetails
    end tracer span = liftIO $ tracerEndSpan tracer span

runTracingT :: TracingT m a -> Tracer -> m a
runTracingT action = runReaderT (unTracingT action)

mapTracingT :: (m a -> n b) -> TracingT m a -> TracingT n b
mapTracingT f action =
  TracingT $ ReaderT \tracer -> do
    f $ runTracingT action tracer

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

newtype Context = Context
  { contextStore :: Context.Store SpanContext
  }

useSpanContext
  :: forall m a
   . (MonadIO m, Exceptions.MonadMask m)
  => Context
  -> SpanContext
  -> m a
  -> m a
useSpanContext Context { contextStore } = Context.use contextStore

data SpanDetails = SpanDetails
  { spanDetailsLineageSource :: SpanLineageSource
  , spanDetailsStart :: TimestampSource
  , spanDetailsAttributes :: Attributes
  , spanDetailsLinks :: SpanLinks
  } deriving stock (Eq, Generic, Show)

defaultSpanDetails :: SpanDetails
defaultSpanDetails =
  SpanDetails
    { spanDetailsLineageSource = implicitSpanLineageSource
    , spanDetailsStart = nowTimestampSource
    , spanDetailsAttributes = mempty
    , spanDetailsLinks = mempty
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
  , spanStart :: Timestamp
  , spanEnd :: UTCTime
  , spanAttributes :: Attributes
  , spanEvents :: SpanEvents
  , spanLinks :: SpanLinks
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
  { unTimestamp :: Word64
  } deriving stock (Eq, Generic, Show)

newtype Attributes = Attributes
  { unAttributes :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)
    deriving newtype (Semigroup, Monoid)

newtype SpanEvents = SpanEvents
  { unSpanEvents :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)

newtype SpanLinks = SpanLinks
  { unSpanLinks :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)
    deriving newtype (Semigroup, Monoid)

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
