{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Context.Internal
  ( -- * Disclaimer
    -- $disclaimer
    attachContext
  , updateContext
  , getAttachedContext
  , getAttachedContextKey
  , getContextStatus

  , MonadContext(..)

  , ContextT(..)
  , runContextT
  , mapContextT

  , ContextKey(..)
  , ContextRef(..)
  , newContextRef
  , updateContextRef
  , markAsDetached
  , updateContextRefStatus
  , updateContextRefValue
  , ContextState(..)
  , ContextStatus(..)
  , contextStatusAttached
  , contextStatusDetached

  , ContextBackend(..)
  , withContextBackend
  ) where

import Context (Store)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Select (MonadSelect)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Writer.Class (MonadWriter)
import Data.IORef (IORef)
import Data.Kind (Type)
import Prelude
import qualified Context
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.RWS.Class as MTL.RWS.Class
import qualified Control.Monad.Reader as MTL.Reader
import qualified Control.Monad.Trans.Control as Trans.Control
import qualified Control.Monad.Trans.RWS.Lazy as Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as Trans.RWS.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Trans.Writer.Strict
import qualified Data.IORef as IORef

attachContext
  :: forall m ctx a
   . (MonadContext ctx m)
  => ctx
  -> (ContextKey ctx -> m a)
  -> m a
attachContext = monadContextAttachContext

updateContext
  :: forall m ctx
   . (MonadContext ctx m)
  => ContextKey ctx
  -> (ctx -> ctx)
  -> m ctx
updateContext = monadContextUpdateContext

getAttachedContext
  :: forall m ctx
   . (MonadContext ctx m)
  => m (Maybe ctx)
getAttachedContext = do
  getAttachedContextKey >>= \case
   Nothing -> pure Nothing
   Just ctxKey -> fmap Just $ updateContext ctxKey id

getAttachedContextKey
  :: forall m ctx
   . (MonadContext ctx m)
  => m (Maybe (ContextKey ctx))
getAttachedContextKey = monadContextGetAttachedContextKey

getContextStatus
  :: forall m ctx
   . (MonadContext ctx m)
  => ContextKey ctx
  -> m ContextStatus
getContextStatus = monadContextGetContextStatus

class (Monad m) => MonadContext ctx m | m -> ctx where
  monadContextAttachContext :: ctx -> (ContextKey ctx -> m a) -> m a
  monadContextUpdateContext :: ContextKey ctx -> (ctx -> ctx) -> m ctx
  monadContextGetAttachedContextKey :: m (Maybe (ContextKey ctx))
  monadContextGetContextStatus :: ContextKey ctx -> m ContextStatus

  default monadContextAttachContext
    :: (Trans.Control.MonadTransControl t, MonadContext ctx n, m ~ t n)
    => ctx
    -> (ContextKey ctx -> m a)
    -> m a
  monadContextAttachContext ctx action = do
    Trans.Control.restoreT . pure
      =<< Trans.Control.liftWith \run -> monadContextAttachContext ctx (run . action)

  default monadContextUpdateContext
    :: (MonadTrans t, MonadContext ctx n, m ~ t n)
    => ContextKey ctx
    -> (ctx -> ctx)
    -> m ctx
  monadContextUpdateContext ctxKey =
    lift . monadContextUpdateContext ctxKey

  default monadContextGetAttachedContextKey
    :: (MonadTrans t, MonadContext ctx n, m ~ t n)
    => m (Maybe (ContextKey ctx))
  monadContextGetAttachedContextKey =
    lift monadContextGetAttachedContextKey

  default monadContextGetContextStatus
    :: (MonadTrans t, MonadContext ctx n, m ~ t n)
    => ContextKey ctx
    -> m ContextStatus
  monadContextGetContextStatus =
    lift . monadContextGetContextStatus

instance (MonadContext ctx m) => MonadContext ctx (ExceptT e m)
instance (MonadContext ctx m) => MonadContext ctx (IdentityT m)
instance (MonadContext ctx m) => MonadContext ctx (MaybeT m)
instance (MonadContext ctx m) => MonadContext ctx (ReaderT r m)
instance (MonadContext ctx m) => MonadContext ctx (StateT r m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.Lazy.RWST r w s m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.RWS.Strict.RWST r w s m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.Lazy.WriterT w m)
instance (MonadContext ctx m, Monoid w) => MonadContext ctx (Trans.Writer.Strict.WriterT w m)
instance (MonadContext ctx m) => MonadContext ctx (LoggingT m)

instance (MonadContext ctx m, MonadUnliftIO m) => MonadContext ctx (ResourceT m) where
  monadContextAttachContext ctx action = do
    withRunInIO \runInIO -> do
      runInIO $ monadContextAttachContext ctx action
  monadContextUpdateContext ctxKey = lift . monadContextUpdateContext ctxKey
  monadContextGetAttachedContextKey = lift monadContextGetAttachedContextKey
  monadContextGetContextStatus = lift . monadContextGetContextStatus

type ContextT :: Type -> (Type -> Type) -> Type -> Type

newtype ContextT ctx m a = ContextT
  { unContextT :: ContextBackend ctx -> m a
  } deriving
      ( Applicative
      , Functor
      , Monad
      , MonadFail
      , MonadIO

      , MonadCatch -- @exceptions@
      , MonadMask -- @exceptions@
      , MonadThrow -- @exceptions@

      , MonadAccum w -- @mtl@
      , MonadCont -- @mtl@
      , MonadError e -- @mtl@
      , MonadSelect r -- @mtl@
      , MonadState s -- @mtl@
      , MonadWriter w -- @mtl@

      , MonadUnliftIO -- @unliftio-core@

      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@

      , MonadLogger -- @monad-logger@

      , MonadResource -- @resourcet@
      ) via (ReaderT (ContextBackend ctx) m)
    deriving
      ( MonadTrans
      , MonadTransControl -- @monad-control@
      ) via (ReaderT (ContextBackend ctx))

instance (MTL.Reader.MonadReader r m) => MTL.Reader.MonadReader r (ContextT ctx m) where
  ask = lift MTL.Reader.ask
  reader = lift . MTL.Reader.reader
  local = mapContextT . MTL.Reader.local

instance (MTL.RWS.Class.MonadRWS r w s m) => MTL.RWS.Class.MonadRWS r w s (ContextT ctx m)

instance (MonadIO m, MonadMask m) => MonadContext ctx (ContextT ctx m) where
  monadContextAttachContext ctx f =
    ContextT \contextBackend -> do
      ctxRef <- liftIO $ newContextRef ContextStatusAttached ctx
      Context.use (contextBackendStore contextBackend) ctxRef do
        runContextT (f ContextKey { contextKeyRef = ctxRef }) contextBackend
        `Exception.finally` liftIO (markAsDetached ctxRef)

  monadContextUpdateContext ctxKey updater =
    ContextT \_contextBackend ->
      liftIO $ updateContextRefValue (contextKeyRef ctxKey) updater

  monadContextGetAttachedContextKey =
    ContextT \contextBackend -> do
      Context.minesMay (contextBackendStore contextBackend) ContextKey

  monadContextGetContextStatus ctxKey =
    ContextT \_contextBackend -> do
      liftIO $ updateContextRefStatus (contextKeyRef ctxKey) id

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

newtype ContextKey ctx = ContextKey
  { contextKeyRef :: ContextRef ctx
  }

newtype ContextRef ctx = ContextRef
  { unContextRef :: IORef (ContextState ctx)
  }

newContextRef :: ContextStatus -> ctx -> IO (ContextRef ctx)
newContextRef contextStateStatus contextStateValue = do
  fmap ContextRef $ IORef.newIORef ContextState
    { contextStateStatus
    , contextStateValue
    }

updateContextRef
  :: ContextRef ctx
  -> (ContextState ctx -> ContextState ctx)
  -> IO (ContextState ctx)
updateContextRef ctxRef updater = do
  IORef.atomicModifyIORef' (unContextRef ctxRef) \ctxState ->
    let ctxState' = updater ctxState
     in (ctxState', ctxState')

markAsDetached :: ContextRef ctx -> IO ()
markAsDetached ctxRef =
  () <$ updateContextRefStatus ctxRef (const ContextStatusDetached)

updateContextRefStatus
  :: ContextRef ctx
  -> (ContextStatus -> ContextStatus)
  -> IO ContextStatus
updateContextRefStatus ctxRef updater =
  fmap contextStateStatus $ updateContextRef ctxRef \ctxState ->
    ctxState
      { contextStateStatus = updater $ contextStateStatus ctxState
      }

updateContextRefValue
  :: ContextRef ctx
  -> (ctx -> ctx)
  -> IO ctx
updateContextRefValue ctxRef updater =
  fmap contextStateValue $ updateContextRef ctxRef \ctxState ->
    ctxState
      { contextStateValue = updater $ contextStateValue ctxState
      }

data ContextState ctx = ContextState
  { contextStateStatus :: ContextStatus
  , contextStateValue :: ctx
  }

data ContextStatus
  = ContextStatusDetached
  | ContextStatusAttached
  deriving stock (Eq, Show)

contextStatusDetached :: ContextStatus
contextStatusDetached = ContextStatusDetached

contextStatusAttached :: ContextStatus
contextStatusAttached = ContextStatusAttached

newtype ContextBackend ctx = ContextBackend
  { contextBackendStore :: Store (ContextRef ctx)
  }

withContextBackend
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => (ContextBackend ctx -> m a)
  -> m a
withContextBackend action = do
  Context.withStore Context.noPropagation Nothing \contextBackendStore ->
    action ContextBackend { contextBackendStore }

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
