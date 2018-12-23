{-# LANGUAGE OverloadedStrings #-}

module Core.Test.Descript.Spec
  ( assertFailureText
  , shouldFailTo
  , shouldBeReducePrintOf
  ) where

import Descript

import Control.Monad
import Data.List
import qualified Data.Text as T
import Test.Hspec
import Test.HUnit

-- | 'assertFailure' but takes the message in 'Text'.
assertFailureText :: (HasCallStack) => T.Text -> Expectation
assertFailureText = assertFailure . T.unpack

-- | Asserts the result should be a failure, and the error should have the given message.
shouldFailTo :: (HasCallStack, Printable a) => Result a -> T.Text -> Expectation
shouldFailTo (ResultFail aerr) eerr = errorMsg aerr `shouldBe` eerr
shouldFailTo (ResultSuccess val) _ = assertFailureText $ "Unexpected success: " <> pprint val

-- | Specifies that the left string should be a subsequence of the right.
shouldBeReducePrintOf :: (HasCallStack) => T.Text -> T.Text -> Expectation
rp `shouldBeReducePrintOf` full
  = unless (T.unpack rp `isSubsequenceOf` T.unpack full) $ assertFailureText failMsg
  where failMsg
           = "not a reduce print:\n"
          <> rp
          <> "\n----- isn't a reduce print of -----\n"
          <> full
