module Test.OTel.SDK.Common
  ( IsTest(..)
  ) where

import GHC.Stack (HasCallStack)
import Prelude

class IsTest a where
  runTest :: (HasCallStack) => a -> IO ()
