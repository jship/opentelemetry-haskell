{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module OTel.SDK.Resource.Core.Internal
  ( -- * Disclaimer
    -- $disclaimer
    Resource(..)
  , defaultResourceBuilder
  , forSpecificSchema
  , resourceBuilderFromAttrs

  , ResourceBuilder(..)
  , buildResource
  , buildResourcePure

  , ResourceMergeError(..)
  ) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Data.Aeson (ToJSON(..), (.=), object)
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

instance ToJSON (Resource Attrs) where
  toJSON resource =
    object
      [ "attributes" .= resourceAttrs
      , "schemaURL" .= resourceSchemaURL
      ]
    where
    Resource { resourceAttrs, resourceSchemaURL } = resource

defaultResourceBuilder :: Text -> ResourceBuilder
defaultResourceBuilder serviceName =
  forSpecificSchema RESOURCE_SCHEMA_URL $
    SERVICE_NAME .@ serviceName

forSpecificSchema :: SchemaURL -> ResourceBuilder -> ResourceBuilder
forSpecificSchema schemaURL resourceBuilder =
  case unResourceBuilder resourceBuilder of
    Left {} -> resourceBuilder
    Right resource ->
      ResourceBuilder $ Right resource { resourceSchemaURL = Just schemaURL }

resourceBuilderFromAttrs :: AttrsBuilder 'AttrsForResource -> ResourceBuilder
resourceBuilderFromAttrs resourceAttrs =
  ResourceBuilder $ Right Resource
    { resourceAttrs
    , resourceSchemaURL = Nothing
    }

newtype ResourceBuilder = ResourceBuilder
  { unResourceBuilder
      :: Either
           (HashMap SchemaURL (AttrsBuilder 'AttrsForResource))
           (Resource AttrsBuilder)
  }

instance KV ResourceBuilder where
  type KVConstraints ResourceBuilder = ToAttrVal
  k .@ v = resourceBuilderFromAttrs $ k .@ v

instance Semigroup ResourceBuilder where
  rb1 <> rb2 =
    case (unResourceBuilder rb1, unResourceBuilder rb2) of
      (Left e1, Left e2) -> ResourceBuilder $ Left $ unionAppendAttrs e1 e2
      (Left e1, Right {}) -> ResourceBuilder $ Left e1
      (Right {}, Left e2) -> ResourceBuilder $ Left e2
      (Right r1, Right r2)
        | Nothing <- schema1 -> res schema2
        | Nothing <- schema2 -> res schema1
        | Just s1 <- schema1, Just s2 <- schema2, s1 == s2 -> res schema1
        | Just s1 <- schema1, Just s2 <- schema2 ->
            ResourceBuilder
              $ Left
              $ unionAppendAttrs
                  (HashMap.singleton s1 $ resourceAttrs r1)
                  (HashMap.singleton s2 $ resourceAttrs r2)
        where
        res schemaURL =
          ResourceBuilder $ Right Resource
            { resourceAttrs = attrs1 <> attrs2
            , resourceSchemaURL = schemaURL
            }
        Resource { resourceAttrs = attrs1 , resourceSchemaURL = schema1 } = r1
        Resource { resourceAttrs = attrs2 , resourceSchemaURL = schema2 } = r2
    where
    unionAppendAttrs = HashMap.unionWith (<>)

instance Monoid ResourceBuilder where
  mempty = resourceBuilderFromAttrs mempty

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
      { resourceMergeErrorSchemas = fmap build hashMap
      }
    Right resource -> Right $ go resource
  where
  go resource = resource { resourceAttrs = build $ resourceAttrs resource }

  build attrs =
    runAttrsBuilder attrs defaultAttrsLimits
      { attrsLimitsCount = Nothing
      , attrsLimitsValueLength = Nothing
      }

newtype ResourceMergeError = ResourceMergeError
  { resourceMergeErrorSchemas :: HashMap SchemaURL (Attrs 'AttrsForResource)
  } deriving stock (Eq, Show)
    deriving anyclass (Exception)

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
