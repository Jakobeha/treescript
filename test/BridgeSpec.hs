{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that serialized programs match the interpreter's specification.
module BridgeSpec
       ( spec ) where

import Core.Test
import TreeScript

import qualified Data.Text.IO as T
import System.Process

input :: FilePath
input = "test-resources/Serialize.tscr"

outputJson :: FilePath
outputJson = "treescript-interpreter/test-resources/program/SerializeHaskell.json"

interpreterOutputJson :: FilePath
interpreterOutputJson = "treescript-interpreter/test-resources/program/SerializeRust.json"

spec :: Spec
spec = do
  exampleEnv <- runIO $ fmap forceSuccess $ runPreSessionRes $ getInitialEnv

  describe "The compiler" $
    it "Compiles and serializes" $ do
      res <- runSessionResVirtual exampleEnv $ compileRaw input outputJson
      case res of
        ResultFail err ->
          assertFailureText $ pprint err
        Result errs () ->
          assertNoErrors errs
  describe "The serialized file" $ do
    -- WARN: Be careful of order
    it "Is valid json" $ do
      pretty <- readProcess "jq" [".", outputJson] ""
      writeFile outputJson pretty
    it "Matches the format expected by the interpreter" $ do
      txt <- T.readFile outputJson
      itxt <- T.readFile interpreterOutputJson
      assertBool "format doesn't match" $ txt == itxt -- `shouldBe` exhausts heap
