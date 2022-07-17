{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Trace.Internal
  ( -- * Synopsis
    -- $synopsis
    MonadTracing(..)
  , trace
  , traceCS

  , SpanContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceFlags(..)
  , TraceState
  , Span(..)

  , Tracer(..)
  , TracingT(..)
  , mapTracingT

  , NoTracingT(..)
  , mapNoTracingT
  ) where

import Data.Text (Text)
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
import Data.Time (UTCTime)
import Data.Word (Word8, Word64)
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
import qualified System.IO.Unsafe as IO.Unsafe

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

data Span = Span
  { spanParent :: Maybe SpanId
  , spanContext :: SpanContext
  , spanName :: Text
  , spanStartNanoseconds :: Word64
  , spanEnd :: UTCTime
  , spanAttributes :: Attributes
  , spanEvents :: SpanEvents
  , spanLinks :: SpanLinks
  } deriving stock (Eq, Generic, Show)

newtype Nanoseconds = Nanoseconds
  { unNanoseconds :: Word64
  } deriving stock (Eq, Generic, Show)

newtype Attributes = Attributes
  { unAttributes :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)

newtype SpanEvents = SpanEvents
  { unSpanEvents :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)

newtype SpanLinks = SpanLinks
  { unSpanLinks :: [(Text, Text)] -- TODO: Better type
  } deriving stock (Eq, Generic, Show)

-------------------------------------------------------------------------------

class (Monad m) => MonadTracing m where
  monadTracingTrace :: CallStack -> m a -> m a

trace :: (MonadTracing m, HasCallStack) => m a -> m a
trace = traceCS callStack

traceCS :: (MonadTracing m) => CallStack -> m a -> m a
traceCS = monadTracingTrace

-------------------------------------------------------------------------------

instance (MonadTracing m, Monoid w) => MonadTracing (AccumT w m) where
  monadTracingTrace = Trans.Accum.mapAccumT . monadTracingTrace

instance (MonadTracing m) => MonadTracing (ContT r m) where
  monadTracingTrace = Trans.Cont.mapContT . monadTracingTrace

instance (MonadTracing m) => MonadTracing (ExceptT e m) where
  monadTracingTrace = Trans.Except.mapExceptT . monadTracingTrace

instance (MonadTracing m) => MonadTracing (IdentityT m) where
  monadTracingTrace = Trans.Identity.mapIdentityT . monadTracingTrace

instance (MonadTracing m) => MonadTracing (MaybeT m) where
  monadTracingTrace = Trans.Maybe.mapMaybeT . monadTracingTrace

instance (MonadTracing m) => MonadTracing (ReaderT r m) where
  monadTracingTrace = Trans.Reader.mapReaderT . monadTracingTrace

instance (MonadTracing m) => MonadTracing (StateT r m) where
  monadTracingTrace = Trans.State.mapStateT . monadTracingTrace

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.CPS.RWST r w s m) where
  monadTracingTrace = Trans.RWS.CPS.mapRWST . monadTracingTrace

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.Lazy.RWST r w s m) where
  monadTracingTrace = Trans.RWS.Lazy.mapRWST . monadTracingTrace

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.RWS.Strict.RWST r w s m) where
  monadTracingTrace = Trans.RWS.Strict.mapRWST . monadTracingTrace

instance (MonadTracing m) => MonadTracing (SelectT r m) where
  monadTracingTrace = Trans.Select.mapSelectT . monadTracingTrace

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.CPS.WriterT w m) where
  monadTracingTrace = Trans.Writer.CPS.mapWriterT . monadTracingTrace

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.Lazy.WriterT w m) where
  monadTracingTrace = Trans.Writer.Lazy.mapWriterT . monadTracingTrace

instance (MonadTracing m, Monoid w) => MonadTracing (Trans.Writer.Strict.WriterT w m) where
  monadTracingTrace = Trans.Writer.Strict.mapWriterT . monadTracingTrace

-------------------------------------------------------------------------------

data Tracer = Tracer
  { startTrace :: Maybe SpanContext -> IO Span
  , reportTrace :: Span -> IO ()
  }

newtype TracingT m a = TracingT
  { runTracingT :: ReaderT Tracer m a
  } deriving
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
      ) via (ReaderT Tracer m)

instance MonadTrans TracingT where
  lift = TracingT . ReaderT . const

instance (MTL.Reader.MonadReader r m) => MTL.Reader.MonadReader r (TracingT m) where
  ask = lift MTL.Reader.ask
  reader = lift . MTL.Reader.reader
  local = mapTracingT . MTL.Reader.local

instance (MonadIO m, Exceptions.MonadMask m) => MonadTracing (TracingT m) where
  monadTracingTrace :: CallStack -> TracingT m a -> TracingT m a
  monadTracingTrace _cs action =
    TracingT $ ReaderT \tracer -> do
      Exceptions.bracket (start tracer) (end tracer) \span -> do
        Context.use spanContextStore (spanContext span) do
          runReaderT (runTracingT action) tracer
    where
    start tracer = do
      mSpanContext <- Context.mineMay spanContextStore
      liftIO $ startTrace tracer mSpanContext
    end tracer span = liftIO $ reportTrace tracer span

mapTracingT :: (m a -> n b) -> TracingT m a -> TracingT n b
mapTracingT f x = TracingT $ ReaderT \tracer ->
  f $ runReaderT (runTracingT x) tracer

spanContextStore :: Context.Store SpanContext
spanContextStore =
  IO.Unsafe.unsafePerformIO $ Context.newStore Context.noPropagation $ Nothing
{-# NOINLINE spanContextStore #-}

-------------------------------------------------------------------------------

newtype NoTracingT m a = NoTracingT
  { runNoTracingT :: m a
  } deriving
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
      ) via (m)

instance MonadTrans NoTracingT where
  lift = NoTracingT

instance (Monad m) => MonadTracing (NoTracingT m) where
  monadTracingTrace _cs action = action

mapNoTracingT :: (m a -> n b) -> NoTracingT m a -> NoTracingT n b
mapNoTracingT f = NoTracingT . f . runNoTracingT

-- $synopsis
--
-- @monad-tracer@ STUB
