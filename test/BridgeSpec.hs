{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that serialized programs match the interpreter's specification.
module BridgeSpec
  ( spec
  )
where

import           Core.Test
import           TreeScript
import qualified TreeScript.Ast.Core           as C

import qualified Data.ByteString.Lazy          as B
import qualified Data.Text.IO                  as T
import           System.Process

input :: FilePath
input = "test-resources/Serialize.tscr"

outputProg :: FilePath
outputProg =
  "treescript-interpreter/test-resources/program/SerializeHaskell.tprg"

outputMsgpack :: FilePath
outputMsgpack =
  "treescript-interpreter/test-resources/program/SerializeHaskell.msgpack"

outputJson :: FilePath
outputJson =
  "treescript-interpreter/test-resources/program/SerializeHaskell.json"

interpreterOutputMsgpack :: FilePath
interpreterOutputMsgpack =
  "treescript-interpreter/test-resources/program/SerializeRust.msgpack"

interpreterOutputJson :: FilePath
interpreterOutputJson =
  "treescript-interpreter/test-resources/program/SerializeRust.json"

validate :: Result a -> IO a
validate (ResultFail err) = do
  assertFailureText $ pprint err
  pure undefined
validate (Result errs x) = do
  assertNoErrors errs
  pure x

spec :: Spec
spec = do
  exampleEnv <- runIO $ fmap forceSuccess $ runPreSessionRes $ getInitialEnv
  describe "program serialization" $ do
    -- WARN: Needs this order
    it "serializes" $ do
      resProg <- runSessionResVirtual exampleEnv $ compile input outputProg
      validate resProg
    it "deserializes" $ do
      text <- B.readFile outputProg
      prog <- validate $ C.decompile text
      let text' = C.export prog
      assertBool "didn't deserialize correctly" $ text == text'
  describe "interpreter serialization" $ do
    -- WARN: Needs this order
    it "serializes" $ do
      resInterp <- runSessionResVirtual exampleEnv
        $ compileInterp input outputMsgpack
      validate resInterp
    it "is valid msgpack" $ callProcess
      "msgpack2json"
      ["-i", outputMsgpack, "-o", outputJson, "-p"]
    it "matches the format expected by the interpreter" $ do
      jtxt  <- T.readFile outputJson
      ijtxt <- T.readFile interpreterOutputJson
      assertBool "JSON format doesn't match" $ jtxt == ijtxt -- `shouldBe` exhausts heap
      mtxt  <- B.readFile outputMsgpack
      imtxt <- B.readFile interpreterOutputMsgpack
      assertBool "Msgpack format doesn't match (but JSON does)" $ mtxt == imtxt
