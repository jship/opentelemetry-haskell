{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Context.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer
    ContextT(..)
  , runContextT
  , mapContextT

  , updateContext
  , getContext
  , attachContext
  , getAttachedContextKey

  , ContextBackend(..)
  , withContextBackend

  , ContextKey(..)
  , ContextSnapshot(..)
  , ContextStatus(..)

    -- ** Extremely internal things below
  , ContextRef(..)
  , newContextRef
  , updateContextRef
  , markAsDetached
  , updateContextRefStatus
  , updateContextRefValue
  ) where

import Context (Store)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Select (MonadSelect)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import Prelude
import qualified Context
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.RWS.Class as MTL.RWS.Class
import qualified Control.Monad.Reader as MTL.Reader
import qualified Data.IORef as IORef

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

updateContext
  :: forall m ctx
   . (MonadIO m)
  => ContextKey ctx
  -> (ctx -> ctx)
  -> ContextT ctx m (ContextSnapshot ctx)
updateContext ctxKey updater =
  ContextT \_contextBackend ->
    liftIO $ updateContextRefValue (contextKeyRef ctxKey) updater

getContext
  :: forall m ctx
   . (MonadIO m)
  => ContextKey ctx
  -> ContextT ctx m (ContextSnapshot ctx)
getContext ctxKey = updateContext ctxKey id

attachContext
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => ctx
  -> (ContextKey ctx -> ContextT ctx m a)
  -> ContextT ctx m a
attachContext ctx f =
  ContextT \contextBackend -> do
    ctxRef <- liftIO $ newContextRef ContextStatusAttached ctx
    Context.use (contextBackendStore contextBackend) ctxRef do
      runContextT (f ContextKey { contextKeyRef = ctxRef }) contextBackend
      `Catch.finally` liftIO (markAsDetached ctxRef)

getAttachedContextKey
  :: forall m ctx
   . (MonadIO m)
  => ContextT ctx m (Maybe (ContextKey ctx))
getAttachedContextKey =
  ContextT \contextBackend -> do
    Context.minesMay (contextBackendStore contextBackend) ContextKey

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

newtype ContextKey ctx = ContextKey
  { contextKeyRef :: ContextRef ctx
  }

data ContextSnapshot ctx = ContextSnapshot
  { contextSnapshotStatus :: ContextStatus
  , contextSnapshotValue :: ctx
  } deriving stock (Eq, Show)

data ContextStatus
  = ContextStatusDetached
  | ContextStatusAttached
  deriving stock (Eq, Show)

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
