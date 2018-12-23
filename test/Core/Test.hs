module Core.Test
  ( module Core.Test.Descript
  , module Core.Test.HUnit
  , module Test.Hspec
  , module Test.QuickCheck
  , module Test.HUnit
  ) where

import Core.Test.Descript
import Core.Test.HUnit
import Test.Hspec
import Test.QuickCheck hiding (Result, Success, Failure, Testable)
import Test.HUnit hiding (Testable)
