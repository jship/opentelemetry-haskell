{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Context.Internal
  ( -- * Disclaimer
    -- $disclaimer
    ContextT(..)
  , mapContextT

  , updateContext
  , getContext
  , attachContext
  , getAttachedContextKey

  , ContextBackend(..)
  , withContextBackend
  , unsafeNewContextBackend

  , ContextKey(..)

    -- ** Extremely internal things below
  , newContextKey
  , updateContextAtKey
  , unsafeAttachContext
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
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader (MonadReader(ask, local, reader), MonadReader)
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
import qualified Data.IORef as IORef

type ContextT :: Type -> (Type -> Type) -> Type -> Type

newtype ContextT ctx m a = ContextT
  { runContextT :: ContextBackend ctx -> m a
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

instance (MonadReader r m) => MonadReader r (ContextT ctx m) where
  ask = lift ask
  reader = lift . reader
  local = mapContextT . local

instance (MonadRWS r w s m) => MonadRWS r w s (ContextT ctx m)

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
  -> ContextT ctx m ctx
updateContext ctxKey updater =
  ContextT \_ctxBackend ->
    liftIO $ updateContextAtKey ctxKey updater

getContext
  :: forall m ctx
   . (MonadIO m)
  => ContextKey ctx
  -> ContextT ctx m ctx
getContext ctxKey = updateContext ctxKey id

attachContext
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => ctx
  -> (ContextKey ctx -> ContextT ctx m a)
  -> ContextT ctx m a
attachContext ctx f =
  ContextT \ctxBackend -> do
    ctxKey <- liftIO $ newContextKey ctx
    Context.use (ctxBackendStore ctxBackend) ctxKey do
      runContextT (f ctxKey) ctxBackend

unsafeAttachContext
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => ContextKey ctx
  -> ContextT ctx m a
  -> ContextT ctx m a
unsafeAttachContext ctxKey action =
  ContextT \ctxBackend -> do
    Context.use (ctxBackendStore ctxBackend) ctxKey do
      runContextT action ctxBackend

getAttachedContextKey
  :: forall m ctx
   . (MonadIO m)
  => ContextT ctx m (Maybe (ContextKey ctx))
getAttachedContextKey =
  ContextT \ctxBackend -> do
    Context.mineMay $ ctxBackendStore ctxBackend

newtype ContextBackend ctx = ContextBackend
  { ctxBackendStore :: Store (ContextKey ctx)
  }

withContextBackend
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => (ContextBackend ctx -> m a)
  -> m a
withContextBackend action = do
  Context.withStore Context.noPropagation Nothing \ctxBackendStore ->
    action ContextBackend { ctxBackendStore }

unsafeNewContextBackend
  :: forall m ctx
   . (MonadIO m)
  => m (ContextBackend ctx)
unsafeNewContextBackend = do
  fmap ContextBackend $ Context.newStore Context.noPropagation Nothing

newtype ContextKey ctx = ContextKey
  { contextKeyRef :: IORef ctx
  }

newContextKey :: ctx -> IO (ContextKey ctx)
newContextKey = fmap ContextKey . IORef.newIORef

updateContextAtKey :: ContextKey ctx -> (ctx -> ctx) -> IO ctx
updateContextAtKey ctxKey updater = do
  IORef.atomicModifyIORef' (contextKeyRef ctxKey) \ctx ->
    let ctx' = updater ctx
     in (ctx', ctx')

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
