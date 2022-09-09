{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module OTel.API.Baggage.Internal
  ( -- * Disclaimer
    -- $disclaimer
    BaggageT(..)
  , mapBaggageT
  ) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Accum (MonadAccum)
import Control.Monad.Base (MonadBase)
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
import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Kind (Type)
import Data.Monoid (Ap(..))
import OTel.API.Baggage.Core (MonadBaggage(..), Baggage)
import OTel.API.Context
  ( ContextT(..), ContextBackend, attachContext, getAttachedContextKey, getContext
  )
import Prelude hiding (span)

type BaggageT :: (Type -> Type) -> Type -> Type
newtype BaggageT m a = BaggageT
  { runBaggageT :: ContextBackend Baggage -> m a
  } deriving
      ( Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO -- @base@
      , MonadAccum w, MonadCont, MonadError e, MonadSelect r, MonadState s, MonadWriter w -- @mtl@
      , MonadCatch, MonadMask, MonadThrow -- @exceptions@
      , MonadUnliftIO -- @unliftio-core@
      , MonadBase b -- @transformers-base@
      , MonadBaseControl b -- @monad-control@
      , MonadLogger -- @monad-logger@
      , MonadResource -- @resourcet@
      ) via (ReaderT (ContextBackend Baggage) m)
    deriving
      ( MonadTrans -- @base@
      , MonadTransControl -- @monad-control@
      ) via (ReaderT (ContextBackend Baggage))
    deriving
      ( Semigroup, Monoid -- @base@
      ) via (Ap (ReaderT (ContextBackend Baggage) m) a)

instance (MonadReader r m) => MonadReader r (BaggageT m) where
  ask = lift ask
  reader = lift . reader
  local = mapBaggageT . local

instance (MonadRWS r w s m) => MonadRWS r w s (BaggageT m)

instance (MonadIO m, MonadMask m) => MonadBaggage (BaggageT m) where
  getBaggage =
    BaggageT \ctxBackend -> do
      flip runContextT ctxBackend do
        getAttachedContextKey >>= \case
          Nothing -> pure mempty
          Just ctxKey -> getContext ctxKey

  setBaggage baggage action =
    BaggageT \ctxBackend -> do
      flip runContextT ctxBackend do
        attachContext "baggage" baggage \_ctxKey -> do
          lift $ runBaggageT action ctxBackend

mapBaggageT
  :: forall m n a b
   . (m a -> n b)
  -> BaggageT m a
  -> BaggageT n b
mapBaggageT f action = BaggageT $ f . runBaggageT action

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
