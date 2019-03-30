{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that serialized programs match the interpreter's specification.
module BridgeSpec
       ( spec ) where

import Core.Test
import TreeScript

import System.Process
import qualified Data.Text.IO as T

input :: FilePath
input = "test-resources/Serialize.tscr"

outputMsgpack :: FilePath
outputMsgpack = "treescript-interpreter/test-resources/program/SerializeHaskell.msgpack"

outputJson :: FilePath
outputJson = "treescript-interpreter/test-resources/program/SerializeHaskell.json"

interpreterOutputJson :: FilePath
interpreterOutputJson = "treescript-interpreter/test-resources/program/SerializeRust.json"

spec :: Spec
spec = do
  exampleEnv <- runIO $ fmap forceSuccess $ runPreSessionRes $ getInitialEnv

  describe "The compiler" $
    it "Compiles and serializes" $ do
      res <- runSessionResVirtual exampleEnv $ compileRaw input outputMsgpack
      case res of
        ResultFail err ->
          assertFailureText $ pprint err
        Result errs () ->
          assertNoErrors errs
  describe "The serialized file" $ do
    it "Is valid msgpack" $
      -- WARN: Be careful of order
      callProcess "msgpack2json" ["-i", outputMsgpack, "-o", outputJson, "-p"]
    it "Matches the format expected by the interpreter" $ do
      txt <- T.readFile outputJson
      itxt <- T.readFile interpreterOutputJson
      txt `shouldBe` itxt
