module Test.OTel.API.Common.IsTest
  ( IsTest(..)
  ) where

import GHC.Stack (HasCallStack)
import Prelude

class IsTest a where
  runTest :: (HasCallStack) => a -> IO ()
