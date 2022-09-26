{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module OTel.SDK.Resource.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer
    Resource(..)
  , defaultResource
  , fromSpecificSchema

  , ResourceBuilder(..)
  , buildResource
  , buildResourcePure

  , ResourceMergeError(..)
  ) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)
import Data.Text (Text)
import OTel.API.Common
  ( AttrsFor(..), KV(..), Attrs, AttrsBuilder, SchemaURL, ToAttrVal, attrsLimitsCount
  , attrsLimitsValueLength, defaultAttrsLimits
  )
import OTel.API.Common.Internal (runAttrsBuilder)
import OTel.SDK.Resource.Core.Attributes (pattern RESOURCE_SCHEMA_URL, pattern SERVICE_NAME)
import Prelude
import qualified Data.HashMap.Strict as HashMap

data Resource (attrs :: AttrsFor -> Type) = Resource
  { resourceAttrs :: attrs 'AttrsForResource
  , resourceSchemaURL :: Maybe SchemaURL
  }

deriving stock instance Eq (Resource Attrs)
deriving stock instance Show (Resource Attrs)

defaultResource :: ResourceBuilder
defaultResource =
  fromSpecificSchema RESOURCE_SCHEMA_URL $
    SERVICE_NAME .@ ("unknown_service" :: Text)

fromSpecificSchema :: SchemaURL -> ResourceBuilder -> ResourceBuilder
fromSpecificSchema schemaURL resourceBuilder =
  case unResourceBuilder resourceBuilder of
    Left {} -> resourceBuilder
    Right resource ->
      ResourceBuilder $ Right resource { resourceSchemaURL = Just schemaURL }

newtype ResourceBuilder = ResourceBuilder
  { unResourceBuilder
      :: Either
           (HashMap SchemaURL (Resource AttrsBuilder))
           (Resource AttrsBuilder)
  }

instance KV ResourceBuilder where
  type KVConstraints ResourceBuilder = ToAttrVal
  k .@ v =
    ResourceBuilder $ Right Resource
      { resourceAttrs = k .@ v
      , resourceSchemaURL = Nothing
      }

instance Semigroup ResourceBuilder where
  rb1 <> rb2 =
    case (unResourceBuilder rb1, unResourceBuilder rb2) of
      (Left e1, Left e2) -> ResourceBuilder $ Left $ e1 <> e2
      (Left e1, Right {}) -> ResourceBuilder $ Left e1
      (Right {}, Left e2) -> ResourceBuilder $ Left e2
      (Right r1, Right r2)
        | Nothing <- schema1 -> res schema2
        | Nothing <- schema2 -> res schema1
        | Just s1 <- schema1, Just s2 <- schema2, s1 == s2 -> res schema1
        | Just s1 <- schema1, Just s2 <- schema2 ->
            ResourceBuilder
              $ Left
              $ HashMap.unionWith
                  (\x y -> x { resourceAttrs = resourceAttrs x <> resourceAttrs y })
                  (HashMap.singleton s1 r1)
                  (HashMap.singleton s2 r2)
        where
        res schemaURL =
          ResourceBuilder $ Right Resource
            { resourceAttrs = attrs1 <> attrs2
            , resourceSchemaURL = schemaURL
            }
        Resource { resourceAttrs = attrs1 , resourceSchemaURL = schema1 } = r1
        Resource { resourceAttrs = attrs2 , resourceSchemaURL = schema2 } = r2

instance Monoid ResourceBuilder where
  mempty =
    ResourceBuilder $ Right Resource
      { resourceAttrs = mempty
      , resourceSchemaURL = Nothing
      }

buildResource
  :: forall m
   . (MonadThrow m)
  => ResourceBuilder
  -> m (Resource Attrs)
buildResource resourceBuilder =
  case buildResourcePure resourceBuilder of
    Left err -> throwM err
    Right x -> pure x

buildResourcePure
  :: ResourceBuilder
  -> Either ResourceMergeError (Resource Attrs)
buildResourcePure resourceBuilder =
  case unResourceBuilder resourceBuilder of
    Left hashMap -> Left $ ResourceMergeError
      { resourceMergeErrorSchemas = fmap go hashMap
      }
    Right resource -> Right $ go resource
  where
  go resource =
    resource
      { resourceAttrs =
          runAttrsBuilder (resourceAttrs resource) defaultAttrsLimits
            { attrsLimitsCount = Nothing
            , attrsLimitsValueLength = Nothing
            }
      }

newtype ResourceMergeError = ResourceMergeError
  { resourceMergeErrorSchemas :: HashMap SchemaURL (Resource Attrs)
  } deriving stock (Eq, Show)
    deriving anyclass (Exception)

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
