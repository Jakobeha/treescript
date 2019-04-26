{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that serialized programs match the interpreter's specification.
module BridgeSpec
       ( spec ) where

import Core.Test
import TreeScript
import qualified TreeScript.Ast.Core as C

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as T
import System.Process

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
  describe "The compiler" $ do
    -- WARN: Be careful of order
    it "Compiles and serializes" $ do
      res <- runSessionResVirtual exampleEnv $ compileRaw input outputMsgpack
      case res of
        ResultFail err ->
          assertFailureText $ pprint err
        Result errs () ->
          assertNoErrors errs
    it "Can deserialize" $ do
      text <- B.readFile outputMsgpack
      let res :: Maybe (C.PFProgram)
          res = C.deserialize text
      case res of
        Nothing -> assertFailureText "couldn't deserialize"
        Just prog -> do
          let out = C.serialize prog
          assertBool "didn't deserialize correctly" $ text == out
  describe "The serialized file" $ do
    -- WARN: Be careful of order
    it "Is valid msgpack" $
      callProcess "msgpack2json" ["-i", outputMsgpack, "-o", outputJson, "-p"]
    it "Matches the format expected by the interpreter" $ do
      txt <- T.readFile outputJson
      itxt <- T.readFile interpreterOutputJson
      assertBool "format doesn't match" $ txt == itxt -- `shouldBe` exhausts heap
