{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
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

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader (MonadReader(ask, local, reader))
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

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Select (MonadSelect)
#endif

type ContextT :: Type -> (Type -> Type) -> Type -> Type

newtype ContextT c m a = ContextT
  { runContextT :: ContextBackend c -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadIO -- @base@
      , Alternative, MonadPlus -- @base@
      , MonadCont, MonadError e, MonadState s, MonadWriter w -- @mtl@
#if MIN_VERSION_mtl(2,3,0)
      , MonadAccum w, MonadSelect r -- @mtl@
#endif
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      ) via (ReaderT (ContextBackend c) m)
    deriving
      ( MonadTransControl -- @monad-control@
      ) via (ReaderT (ContextBackend c))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (ContextBackend c) m) a)

-- On GHC 9.6.2/transformers-0.6.0.1, including this 'MonadTrans' instance
-- in the cleaner way above, e.g.:
--
--   deriving
--     ( MonadTrans -- @transformers@
--     , MonadTransControl -- @monad-control@
--     ) via (ReaderT (ContextBackend c))
--
-- produces a redundant constraint warning:
--
-- error: [GHC-30606] [-Wredundant-constraints, Werror=redundant-constraints]
--       • Redundant constraint: Monad m
--       • When deriving the instance for (MonadTrans (ContextT c))
--      |
--   75 |       ( MonadTrans -- @transformers@
--      |         ^^^^^^^^^^
--
-- Strangely, doing the same style of deriving but using @-XStandaloneDeriving@
-- does not produce this warning.
deriving via (ReaderT (ContextBackend c)) instance MonadTrans (ContextT c)

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
   . (MonadIO m, MonadThrow m)
  => ContextT a m Context
getAttachedContext =
  ContextT \ctxBackend -> do
    getAttachedContextUsing ctxBackend

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
