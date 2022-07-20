{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Context.Internal
  ( -- * Disclaimer
    -- $disclaimer
    MonadContext(..)
  , attachContext
  , getContext

  , ContextT(..)
  , runContextT
  , mapContextT

  , ContextBackend(..)
  , withContextBackend
  ) where

import Context (Store)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.State (StateT)
import Data.Kind (Type)
import Prelude
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

class (Monad m) => MonadContext ctx m | m -> ctx where
  monadContextAttachContext :: ctx -> m a -> m a
  monadContextGetContext :: m ctx

attachContext :: forall m ctx a. (MonadContext ctx m) => ctx -> m a -> m a
attachContext = monadContextAttachContext

getContext :: forall m ctx. (MonadContext ctx m) => m ctx
getContext = monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (AccumT w m) where
  monadContextAttachContext = Trans.Accum.mapAccumT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (ContT r m) where
  monadContextAttachContext = Trans.Cont.mapContT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (ExceptT e m) where
  monadContextAttachContext = Trans.Except.mapExceptT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (IdentityT m) where
  monadContextAttachContext = Trans.Identity.mapIdentityT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (MaybeT m) where
  monadContextAttachContext = Trans.Maybe.mapMaybeT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (ReaderT r m) where
  monadContextAttachContext = Trans.Reader.mapReaderT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (StateT r m) where
  monadContextAttachContext = Trans.State.mapStateT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.CPS.RWST r w s m) where
  monadContextAttachContext = Trans.RWS.CPS.mapRWST . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.Lazy.RWST r w s m) where
  monadContextAttachContext = Trans.RWS.Lazy.mapRWST . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.Strict.RWST r w s m) where
  monadContextAttachContext = Trans.RWS.Strict.mapRWST . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m) => MonadContext ctx (SelectT r m) where
  monadContextAttachContext = Trans.Select.mapSelectT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.CPS.WriterT w m) where
  monadContextAttachContext = Trans.Writer.CPS.mapWriterT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.Lazy.WriterT w m) where
  monadContextAttachContext = Trans.Writer.Lazy.mapWriterT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.Strict.WriterT w m) where
  monadContextAttachContext = Trans.Writer.Strict.mapWriterT . monadContextAttachContext
  monadContextGetContext = lift monadContextGetContext

-- TODO: LoggingT instance

type ContextT :: Type -> (Type -> Type) -> Type -> Type

newtype ContextT ctx m a = ContextT
  { unContextT :: ContextBackend ctx -> m a
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
      , MTL.Select.MonadSelect r
      , MTL.State.MonadState s
      , MTL.Writer.Class.MonadWriter w
      , Unlift.MonadUnliftIO
        -- TODO: MonadLogger instance
      ) via (ReaderT (ContextBackend ctx) m)

instance MonadTrans (ContextT ctx) where
  lift = ContextT . const

instance (MTL.Reader.MonadReader r m) => MTL.Reader.MonadReader r (ContextT ctx m) where
  ask = lift MTL.Reader.ask
  reader = lift . MTL.Reader.reader
  local = mapContextT . MTL.Reader.local

instance (MTL.RWS.Class.MonadRWS r w s m) => MTL.RWS.Class.MonadRWS r w s (ContextT ctx m)

instance (MonadIO m, Exceptions.MonadMask m) => MonadContext ctx (ContextT ctx m) where
  monadContextAttachContext ctx action =
    ContextT \contextBackend ->
      Context.use (contextBackendStore contextBackend) ctx do
        runContextT action contextBackend

  monadContextGetContext =
    ContextT \contextBackend ->
      Context.mine $ contextBackendStore contextBackend

runContextT
  :: forall ctx m a
   . ContextT ctx m a
  -> ContextBackend ctx
  -> m a
runContextT = unContextT

mapContextT
  :: forall m n ctx a b
   . (m a -> n b)
  -> ContextT ctx m a
  -> ContextT ctx n b
mapContextT f action = ContextT $ f . runContextT action

newtype ContextBackend ctx = ContextBackend
  { contextBackendStore :: Store ctx
  }

withContextBackend
  :: forall m ctx a
   . (MonadIO m, Exceptions.MonadMask m)
  => ctx
  -> (ContextBackend ctx -> m a)
  -> m a
withContextBackend ctx action = do
  Context.withStore Context.noPropagation (Just ctx) \contextBackendStore ->
    action ContextBackend { contextBackendStore }

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
