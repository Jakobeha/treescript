{-# LANGUAGE OverloadedStrings #-}

module Core.Test.TreeScript.Spec
  ( assertFailureText
  , assertNoErrors
  , shouldFailTo
  , shouldBeReducePrintOf
  )
where

import           TreeScript

import           Control.Monad
import           Data.Foldable
import           Data.Char
import           Data.List
import qualified Data.Text                     as T
import           Test.Hspec
import           Test.HUnit

-- | 'assertFailure' but takes the message in 'Text'.
assertFailureText :: (HasCallStack) => T.Text -> Expectation
assertFailureText = assertFailure . T.unpack

-- | Asserts that the input is empty.
assertNoErrors :: (HasCallStack, Foldable t) => t Error -> Expectation
assertNoErrors errs
  | null errs = pure ()
  | otherwise = assertFailureText $ T.unlines $ "Unexpected errors:\n" : map
    pprint
    (toList errs)

-- | Asserts the result should be a fatal error with the given message.
shouldFailTo
  :: (HasCallStack, Printable a) => EResult a -> T.Text -> Expectation
shouldFailTo (ResultFail aerr) eerr = errorMsg aerr `shouldBe` eerr
shouldFailTo res@(Result _ _) _ =
  assertFailureText $ "Unexpected non-fatal result: " <> pprint res

reducePrint :: T.Text -> String
reducePrint = T.unpack . T.filter (not . isSpace)

-- | Specifies that the left string should be a subsequence of the right.
shouldBeReducePrintOf :: (HasCallStack) => T.Text -> T.Text -> Expectation
rp `shouldBeReducePrintOf` full =
  unless (reducePrint rp `isSubsequenceOf` reducePrint full)
    $ assertFailureText failMsg
 where
  failMsg =
    "not a reduce print:\n"
      <> rp
      <> "\n----- isn't a reduce print of -----\n"
      <> full
