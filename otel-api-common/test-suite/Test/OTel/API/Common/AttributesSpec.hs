{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.OTel.API.Common.AttributesSpec
  ( spec
  ) where

import Data.Aeson (ToJSON(..), Value(..), (.=), object)
import Data.Text (Text)
import OTel.API.Common
import OTel.API.Common.Internal (runAttrsBuilder)
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.OTel.API.Common.IsTest (IsTest(..))
import qualified Data.Scientific as Scientific

spec :: Spec
spec = do
  describe "attributes from JSON" do
    it "null" do
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ ("(null)" :: Text)
        , initKeyText = "acme"
        , value = Null
        }
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ ("(null)" :: Text)
        , initKeyText = "acme"
        , value = Nothing @Int
        }
    it "bool" do
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ False
        , initKeyText = "acme"
        , value = False
        }
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ True
        , initKeyText = "acme"
        , value = True
        }
    it "string" do
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ ("foo" :: Text)
        , initKeyText = "acme"
        , value = "foo" :: Text
        }
    it "number" do
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ (42 :: Int)
        , initKeyText = "acme"
        , value = 42 :: Int
        }
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ (27.5 :: Double)
        , initKeyText = "acme"
        , value = 27.5 :: Double
        }
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ ("1.0e9000000000000000000" :: Text)
        , initKeyText = "acme"
        , value = Scientific.scientific 1 9000000000000000000
        }
    it "array" do
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ ("(empty array)" :: Text)
        , initKeyText = "acme"
        , value = [] :: [Int]
        }
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme.0" .@ (1 :: Int)
        , initKeyText = "acme"
        , value = [1] :: [Int]
        }
      runTest JSONAttrsTestCase
        { expectedAttrs =
            mconcat
              [ Key "acme.0" .@ (1 :: Int)
              , Key "acme.1" .@ (2 :: Int)
              , Key "acme.2" .@ (3 :: Int)
              ]
        , initKeyText = "acme"
        , value = [1.. 3] :: [Int]
        }
      runTest JSONAttrsTestCase
        { expectedAttrs =
            mconcat
              [ Key "acme.0" .@ (1 :: Int)
              , Key "acme.1" .@ ("foo" :: Text)
              , Key "acme.2" .@ (False :: Bool)
              , Key "acme.3" .@ ("(null)" :: Text)
              ]
        , initKeyText = "acme"
        , value = Array [Number 1, "foo", Bool False, Null]
        }
    it "object" do
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme" .@ ("(empty object)" :: Text)
        , initKeyText = "acme"
        , value = Object []
        }
      runTest JSONAttrsTestCase
        { expectedAttrs = Key "acme.stuff" .@ ("things" :: Text)
        , initKeyText = "acme"
        , value = Object [("stuff", "things")]
        }
      runTest JSONAttrsTestCase
        { expectedAttrs =
            mconcat
              [ Key "acme.foo.id" .@ (42 :: Int)
              , Key "acme.foo.name" .@ ("bloorp" :: Text)
              , Key "acme.foo.description" .@ ("foo bar didgeridoo" :: Text)
              , Key "acme.foo.valid" .@ True
              , Key "acme.foo.option" .@ (27.5 :: Double)
              ]
        , initKeyText = "acme.foo"
        , value =
            Foo
              { fooId = 42
              , fooName = "bloorp"
              , fooDescription = "foo bar didgeridoo"
              , fooValid = True
              , fooOption = Just 27.5
              }
        }

data JSONAttrsTestCase a = JSONAttrsTestCase
  { expectedAttrs :: AttrsBuilder 'AttrsForSpan
  , initKeyText :: Text
  , value :: a
  }

instance (ToJSON a) => IsTest (JSONAttrsTestCase a) where
  runTest testCase = do
    build (jsonAttrs initKeyText value) `shouldBe` build expectedAttrs
    where
    build attrs =
      runAttrsBuilder attrs defaultAttrsLimits
        { attrsLimitsCount = Nothing
        , attrsLimitsValueLength = Nothing
        }

    JSONAttrsTestCase
      { expectedAttrs
      , initKeyText
      , value
      } = testCase

data Foo = Foo
  { fooId :: Int
  , fooName :: Text
  , fooDescription :: Text
  , fooValid :: Bool
  , fooOption :: Maybe Double
  }

instance ToJSON Foo where
  toJSON foo =
    object
      [ "id" .= fooId
      , "name" .= fooName
      , "description" .= fooDescription
      , "valid" .= fooValid
      , "option" .= fooOption
      ]
    where
    Foo
      { fooId
      , fooName
      , fooDescription
      , fooValid
      , fooOption
      } = foo
