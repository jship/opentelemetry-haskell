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
module OTel.Tracer
  ( -- * Synopsis
    -- $synopsis
    MonadTracer(..)
  , trace
  , traceCS

  , SpanContext(..)
  , TraceId(..)
  , SpanId(..)
  , Span(..)

  , Tracer(..)
  , TracingT(..)
  , mapTracingT

  , NoTracingT(..)
  , mapNoTracingT

  , main
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.State (StateT)
import Data.Time (UTCTime)
import Data.Word (Word64)
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
  , spanContextParentSpanId :: SpanId
  } deriving stock (Eq, Generic, Show)

data TraceId = TraceId
  { traceIdHi :: Word64
  , traceIdLo :: Word64
  } deriving stock (Eq, Generic, Show)

data SpanId = SpanId
  { spanIdLo :: Word64
  } deriving stock (Eq, Generic, Show)

data Span = Span
  { spanContext :: SpanContext
  , spanId :: SpanId
  , spanStart :: UTCTime
  }

-------------------------------------------------------------------------------

class (Monad m) => MonadTracer m where
  monadTracerTrace :: CallStack -> m a -> m a

trace :: (MonadTracer m, HasCallStack) => m a -> m a
trace = traceCS callStack

traceCS :: (MonadTracer m) => CallStack -> m a -> m a
traceCS = monadTracerTrace

-------------------------------------------------------------------------------

instance (MonadTracer m, Monoid w) => MonadTracer (AccumT w m) where
  monadTracerTrace = Trans.Accum.mapAccumT . monadTracerTrace

instance (MonadTracer m) => MonadTracer (ContT r m) where
  monadTracerTrace = Trans.Cont.mapContT . monadTracerTrace

instance (MonadTracer m) => MonadTracer (ExceptT e m) where
  monadTracerTrace = Trans.Except.mapExceptT . monadTracerTrace

instance (MonadTracer m) => MonadTracer (IdentityT m) where
  monadTracerTrace = Trans.Identity.mapIdentityT . monadTracerTrace

instance (MonadTracer m) => MonadTracer (MaybeT m) where
  monadTracerTrace = Trans.Maybe.mapMaybeT . monadTracerTrace

instance (MonadTracer m) => MonadTracer (ReaderT r m) where
  monadTracerTrace = Trans.Reader.mapReaderT . monadTracerTrace

instance (MonadTracer m) => MonadTracer (StateT r m) where
  monadTracerTrace = Trans.State.mapStateT . monadTracerTrace

instance (MonadTracer m, Monoid w) => MonadTracer (Trans.RWS.CPS.RWST r w s m) where
  monadTracerTrace = Trans.RWS.CPS.mapRWST . monadTracerTrace

instance (MonadTracer m, Monoid w) => MonadTracer (Trans.RWS.Lazy.RWST r w s m) where
  monadTracerTrace = Trans.RWS.Lazy.mapRWST . monadTracerTrace

instance (MonadTracer m, Monoid w) => MonadTracer (Trans.RWS.Strict.RWST r w s m) where
  monadTracerTrace = Trans.RWS.Strict.mapRWST . monadTracerTrace

instance (MonadTracer m) => MonadTracer (SelectT r m) where
  monadTracerTrace = Trans.Select.mapSelectT . monadTracerTrace

instance (MonadTracer m, Monoid w) => MonadTracer (Trans.Writer.CPS.WriterT w m) where
  monadTracerTrace = Trans.Writer.CPS.mapWriterT . monadTracerTrace

instance (MonadTracer m, Monoid w) => MonadTracer (Trans.Writer.Lazy.WriterT w m) where
  monadTracerTrace = Trans.Writer.Lazy.mapWriterT . monadTracerTrace

instance (MonadTracer m, Monoid w) => MonadTracer (Trans.Writer.Strict.WriterT w m) where
  monadTracerTrace = Trans.Writer.Strict.mapWriterT . monadTracerTrace

-------------------------------------------------------------------------------

data Tracer = Tracer
  { startTrace :: Maybe SpanContext -> IO Span
  , reportTrace :: Span -> IO ()
  }

newtype TracingT m a = TracingT
  { runTracingT :: Tracer -> m a
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
  lift = TracingT . const

instance (MTL.Reader.MonadReader r m) => MTL.Reader.MonadReader r (TracingT m) where
  ask = lift MTL.Reader.ask
  reader = lift . MTL.Reader.reader
  local = mapTracingT . MTL.Reader.local

instance (MonadIO m, Exceptions.MonadMask m) => MonadTracer (TracingT m) where
  monadTracerTrace :: CallStack -> TracingT m a -> TracingT m a
  monadTracerTrace _cs action =
    TracingT \tracer -> do
      Exceptions.bracket (start tracer) (end tracer) \span -> do
        Context.use spanContextStore (spanContext span) do
          runTracingT action tracer
    where
    start tracer = do
      mSpanContext <- Context.mineMay spanContextStore
      liftIO $ startTrace tracer mSpanContext
    end tracer span = liftIO $ reportTrace tracer span

mapTracingT :: (m a -> n b) -> TracingT m a -> TracingT n b
mapTracingT f x = TracingT $ f . runTracingT x

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

instance (Monad m) => MonadTracer (NoTracingT m) where
  monadTracerTrace _cs action = action

mapNoTracingT :: (m a -> n b) -> NoTracingT m a -> NoTracingT n b
mapNoTracingT f = NoTracingT . f . runNoTracingT

-------------------------------------------------------------------------------

doPureStuff :: (MonadTracer m) => m ()
doPureStuff = do
  trace do
    pure ()

doStuff :: (MonadIO m, MonadTracer m) => Int -> m ()
doStuff x = do
  trace do
    liftIO $ putStrLn $ "Doing stuff: " <> show x

main :: IO ()
main = do
  runNoTracingT do
    doStuff 42
    doPureStuff
    trace do
      liftIO $ putStrLn "Done"

-- $synopsis
--
-- @monad-tracer@ STUB
