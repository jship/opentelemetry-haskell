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
    MonadKeyedContext(..)
  , MonadContext(..)
  , ContextT(..)
  , runContextT
  , mapContextT

  , ContextKey(..)
  , ContextSnapshot(..)
  , ContextStatus(..)
  , ContextBackend(..)
  , withContextBackend

  , ContextRef(..)
  , newContextRef
  , updateContextRef
  , markAsDetached
  , updateContextRefStatus
  , updateContextRefValue
  ) where

import Context (Store)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader)
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
import Data.Monoid (Ap(..))
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

class (Monad m) => MonadKeyedContext ctx m | m -> ctx where
  getContext :: ContextKey ctx -> m (ContextSnapshot ctx)
  updateContext :: ContextKey ctx -> (ctx -> ctx) -> m (ContextSnapshot ctx)

  default getContext
    :: (MonadTrans t, MonadKeyedContext ctx n, m ~ t n)
    => ContextKey ctx
    -> m (ContextSnapshot ctx)
  getContext =
    lift . getContext

  default updateContext
    :: (MonadTrans t, MonadKeyedContext ctx n, m ~ t n)
    => ContextKey ctx
    -> (ctx -> ctx)
    -> m (ContextSnapshot ctx)
  updateContext ctxKey =
    lift . updateContext ctxKey

instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (ExceptT e m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (IdentityT m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (MaybeT m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (ReaderT r m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (StateT r m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.RWS.Lazy.RWST r w s m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.RWS.Strict.RWST r w s m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.Writer.Lazy.WriterT w m)
instance (MonadKeyedContext ctx m, Monoid w) => MonadKeyedContext ctx (Trans.Writer.Strict.WriterT w m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (LoggingT m)
instance (MonadKeyedContext ctx m) => MonadKeyedContext ctx (ResourceT m)

class (MonadKeyedContext ctx m) => MonadContext ctx m | m -> ctx where
  attachContext :: ctx -> (ContextKey ctx -> m a) -> m a
  getAttachedContextKey :: m (Maybe (ContextKey ctx))

  default attachContext
    :: (Trans.Control.MonadTransControl t, MonadContext ctx n, m ~ t n)
    => ctx
    -> (ContextKey ctx -> m a)
    -> m a
  attachContext ctx action = do
    Trans.Control.restoreT . pure
      =<< Trans.Control.liftWith \run -> attachContext ctx (run . action)

  default getAttachedContextKey
    :: (MonadTrans t, MonadContext ctx n, m ~ t n)
    => m (Maybe (ContextKey ctx))
  getAttachedContextKey =
    lift getAttachedContextKey

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
  attachContext ctx action = do
    withRunInIO \runInIO -> do
      runInIO $ attachContext ctx action
  getAttachedContextKey = lift getAttachedContextKey

type ContextT :: Type -> (Type -> Type) -> Type -> Type

newtype ContextT ctx m a = ContextT
  { unContextT :: ContextBackend ctx -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
      , MonadAccum w, MonadCont, MonadError e, MonadSelect r, MonadState s, MonadWriter w -- @mtl@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      ) via (ReaderT (ContextBackend ctx) m)
    deriving
      ( MonadTrans -- @base@
      , MonadTransControl -- @monad-control@
      ) via (ReaderT (ContextBackend ctx))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (ContextBackend ctx) m) a)

instance (MTL.Reader.MonadReader r m) => MonadReader r (ContextT ctx m) where
  ask = lift MTL.Reader.ask
  reader = lift . MTL.Reader.reader
  local = mapContextT . MTL.Reader.local

instance (MTL.RWS.Class.MonadRWS r w s m) => MTL.RWS.Class.MonadRWS r w s (ContextT ctx m)

instance (MonadIO m) => MonadKeyedContext ctx (ContextT ctx m) where
  updateContext ctxKey updater =
    ContextT \_contextBackend ->
      liftIO $ updateContextRefValue (contextKeyRef ctxKey) updater

  getContext ctxKey = updateContext ctxKey id

instance (MonadIO m, MonadMask m) => MonadContext ctx (ContextT ctx m) where
  attachContext ctx f =
    ContextT \contextBackend -> do
      ctxRef <- liftIO $ newContextRef ContextStatusAttached ctx
      Context.use (contextBackendStore contextBackend) ctxRef do
        runContextT (f ContextKey { contextKeyRef = ctxRef }) contextBackend
        `Exception.finally` liftIO (markAsDetached ctxRef)

  getAttachedContextKey =
    ContextT \contextBackend -> do
      Context.minesMay (contextBackendStore contextBackend) ContextKey

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

data ContextSnapshot ctx = ContextSnapshot
  { contextSnapshotStatus :: ContextStatus
  , contextSnapshotValue :: ctx
  }

data ContextStatus
  = ContextStatusDetached
  | ContextStatusAttached
  deriving stock (Eq, Show)

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

newtype ContextRef ctx = ContextRef
  { unContextRef :: IORef (ContextSnapshot ctx)
  }

newContextRef :: ContextStatus -> ctx -> IO (ContextRef ctx)
newContextRef contextSnapshotStatus contextSnapshotValue = do
  fmap ContextRef $ IORef.newIORef ContextSnapshot
    { contextSnapshotStatus
    , contextSnapshotValue
    }

updateContextRef
  :: ContextRef ctx
  -> (ContextSnapshot ctx -> ContextSnapshot ctx)
  -> IO (ContextSnapshot ctx)
updateContextRef ctxRef updater = do
  IORef.atomicModifyIORef' (unContextRef ctxRef) \ctxSnapshot ->
    let ctxSnapshot' = updater ctxSnapshot
     in (ctxSnapshot', ctxSnapshot')

markAsDetached :: ContextRef ctx -> IO ()
markAsDetached ctxRef =
  () <$ updateContextRefStatus ctxRef (const ContextStatusDetached)

updateContextRefStatus
  :: ContextRef ctx
  -> (ContextStatus -> ContextStatus)
  -> IO (ContextSnapshot ctx )
updateContextRefStatus ctxRef updater =
  updateContextRef ctxRef \ctxSnapshot ->
    ctxSnapshot
      { contextSnapshotStatus = updater $ contextSnapshotStatus ctxSnapshot
      }

updateContextRefValue
  :: ContextRef ctx
  -> (ctx -> ctx)
  -> IO (ContextSnapshot ctx)
updateContextRefValue ctxRef updater =
  updateContextRef ctxRef \ctxSnapshot ->
    ctxSnapshot
      { contextSnapshotValue = updater $ contextSnapshotValue ctxSnapshot
      }

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
