{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module OTel.API.Context.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer
    Context(..)
  , emptyContext
  , lookupContext
  , insertContext
  , ContextKey(..)
  , contextKeyName
  , unsafeNewContextKey
  , attachContextValueUsing
  , getAttachedContextValueUsing
  , getAttachedContextUsing
  , ContextBackend(..)
  , unsafeNewContextBackend
  , SomeContextBackend(..)
  , ContextBackendRegistry(..)
  , emptyContextBackendRegistry
  , registerContextBackend
  , defaultContextBackendRegistry
  ) where

import Context (Store)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Unique.Really (Unique)
import Data.Vault.Strict (Vault)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import qualified Context
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import qualified Data.Typeable as Typeable
import qualified Data.Unique.Really as Unique
import qualified Data.Vault.Strict as Vault

newtype Context = Context
  { unContext :: Vault
  }

emptyContext :: Context
emptyContext = Context Vault.empty

lookupContext :: ContextKey a -> Context -> Maybe a
lookupContext contextKey context = Vault.lookup vaultKey vault
  where
  ContextKey { contextKeyVaultKey = vaultKey } = contextKey
  Context { unContext = vault } = context

insertContext :: ContextKey a -> a -> Context -> Context
insertContext contextKey value context =
  Context $ Vault.insert vaultKey value vault
  where
  ContextKey { contextKeyVaultKey = vaultKey } = contextKey
  Context { unContext  = vault } = context

data ContextKey a = ContextKey
  { contextKeyDebugName :: Text
  , contextKeyVaultKey :: Vault.Key a
  }

contextKeyName :: ContextKey a -> Text
contextKeyName = contextKeyDebugName

unsafeNewContextKey :: forall m a. (MonadIO m) => Text -> m (ContextKey a)
unsafeNewContextKey contextKeyDebugName = do
  contextKeyVaultKey <- liftIO Vault.newKey
  pure ContextKey
    { contextKeyDebugName
    , contextKeyVaultKey
    }

attachContextValueUsing
  :: forall m a b
   . (MonadIO m, MonadMask m)
  => ContextBackend a
  -> a
  -> m b
  -> m b
attachContextValueUsing contextBackend value =
  Context.adjust (contextBackendStore contextBackend)
    $ insertContext (contextBackendValueKey contextBackend) value

getAttachedContextValueUsing
  :: forall m a
   . (MonadIO m, MonadThrow m)
  => ContextBackend a
  -> m (Maybe a)
getAttachedContextValueUsing contextBackend = do
  context <- Context.mine $ contextBackendStore contextBackend
  pure $ lookupContext (contextBackendValueKey contextBackend) context

getAttachedContextUsing
  :: forall m a
   . (MonadIO m, MonadThrow m)
  => ContextBackend a
  -> m Context
getAttachedContextUsing contextBackend = do
  someContextBackends <- do
    liftIO $ IORef.readIORef $ unContextBackendRegistry $ contextBackendRegistry contextBackend
  fmap (Context . mconcat) do
    Traversable.for (HashMap.elems someContextBackends) \case
      SomeContextBackend registeredContextBackend -> do
        context <- Context.mine $ contextBackendStore registeredContextBackend
        pure $ unContext context

data ContextBackend a = ContextBackend
  { contextBackendStore :: Store Context
  , contextBackendValueKey :: ContextKey a
  , contextBackendRegistry :: ContextBackendRegistry
  }

unsafeNewContextBackend :: forall m a. (MonadIO m, Typeable a) => m (ContextBackend a)
unsafeNewContextBackend = do
  liftIO do
    contextBackend <- do
      contextBackendValueKey <- do
        unsafeNewContextKey $ Text.pack $ show $ Typeable.typeRep $ Proxy @a
      contextBackendStore <- do
        Context.newStore Context.noPropagation $ Just emptyContext
      pure ContextBackend
        { contextBackendStore
        , contextBackendValueKey
        , contextBackendRegistry = defaultContextBackendRegistry
        }
    contextBackendRegistryKey <- Unique.newUnique
    registerContextBackend contextBackendRegistryKey contextBackend
      $ contextBackendRegistry contextBackend
    pure contextBackend

data SomeContextBackend where
  SomeContextBackend :: ContextBackend a -> SomeContextBackend

newtype ContextBackendRegistry = ContextBackendRegistry
  { unContextBackendRegistry :: IORef (HashMap Unique SomeContextBackend)
  }

emptyContextBackendRegistry :: IO ContextBackendRegistry
emptyContextBackendRegistry = do
  fmap ContextBackendRegistry $ IORef.newIORef HashMap.empty

registerContextBackend :: Unique -> ContextBackend a -> ContextBackendRegistry -> IO ()
registerContextBackend registryKey contextBackend registry = do
  IORef.atomicModifyIORef' ref \contextBackends ->
    ( HashMap.insert registryKey (SomeContextBackend contextBackend) contextBackends
    , ()
    )
  where
  ContextBackendRegistry
    { unContextBackendRegistry = ref
    } = registry

defaultContextBackendRegistry :: ContextBackendRegistry
defaultContextBackendRegistry = unsafePerformIO emptyContextBackendRegistry
{-# NOINLINE defaultContextBackendRegistry #-}

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
