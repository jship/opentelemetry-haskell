{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Context.Internal
  ( -- * Disclaimer
    -- $disclaimer
    ContextT(..)
  , mapContextT

  , attachContextValue
  , getAttachedContextValue
  , getAttachedContext
  ) where

import Control.Monad.Accum (MonadAccum)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader (MonadReader(ask, local, reader))
import Control.Monad.Select (MonadSelect)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import OTel.API.Context.Core
  ( Context, ContextBackend, attachContextValueUsing, getAttachedContextUsing
  , getAttachedContextValueUsing
  )
import Prelude

type ContextT :: Type -> (Type -> Type) -> Type -> Type

newtype ContextT c m a = ContextT
  { runContextT :: ContextBackend c -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
      , MonadAccum w, MonadCont, MonadError e, MonadSelect r, MonadState s, MonadWriter w -- @mtl@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      ) via (ReaderT (ContextBackend c) m)
    deriving
      ( MonadTrans -- @base@
      , MonadTransControl -- @monad-control@
      ) via (ReaderT (ContextBackend c))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (ContextBackend c) m) a)

instance (MonadReader r m) => MonadReader r (ContextT c m) where
  ask = lift ask
  reader = lift . reader
  local = mapContextT . local

instance (MonadRWS r w s m) => MonadRWS r w s (ContextT c m)

mapContextT
  :: forall m n c a b
   . (m a -> n b)
  -> ContextT c m a
  -> ContextT c n b
mapContextT f action = ContextT $ f . runContextT action

attachContextValue
  :: forall m a b
   . (MonadIO m, MonadMask m)
  => a
  -> ContextT a m b
  -> ContextT a m b
attachContextValue value action =
  ContextT \ctxBackend -> do
    attachContextValueUsing ctxBackend value do
      runContextT action ctxBackend

getAttachedContextValue
  :: forall m a
   . (MonadIO m, MonadMask m)
  => ContextT a m (Maybe a)
getAttachedContextValue =
  ContextT \ctxBackend -> do
    getAttachedContextValueUsing ctxBackend

getAttachedContext
  :: forall m a
   . (MonadIO m, MonadThrow m, forall x. (Monoid x) => Monoid (m x))
  => ContextT a m Context
getAttachedContext =
  ContextT \ctxBackend -> do
    getAttachedContextUsing ctxBackend

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
