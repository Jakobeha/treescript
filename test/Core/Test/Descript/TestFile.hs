{-# LANGUAGE OverloadedStrings #-}

module Core.Test.Descript.TestFile
  ( TestFile (..)
  , TestInfo (..)
  , loadFilesInDir
  ) where

import Descript

import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml
import System.Directory
import System.FilePath

data TestFile
  = TestFile
  { testFileSrcFile :: File
  , testFileTestInfo :: TestInfo
  } deriving (Eq, Ord, Read, Show)

data TestInfo
  = TestInfo
  { testInfoPrintLex :: Bool
  , testInfoPrintSugar :: Bool
  , testInfoPrintCore :: Bool
  , testInfoIsLexable :: Bool
  , testInfoIsParseable :: Bool
  , testInfoIsDesugarable :: Bool
  , testInfoFatalErrorMsg :: T.Text
  , testInfoErrorMsgs :: [T.Text]
  } deriving (Eq, Ord, Read, Show)

instance FromJSON TestInfo where
  parseJSON = withObject "TestInfo" $ \x -> TestInfo
    <$> x .:? "printLex?" .!= False
    <*> x .:? "printSugar?" .!= False
    <*> x .:? "printCore?" .!= False
    <*> x .:? "lexes?" .!= True
    <*> x .:? "parses?" .!= True
    <*> x .:? "desugars?" .!= True
    <*> x .:? "error" .!= ""
    <*> x .:? "errors" .!= []

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suf = fmap reverse . stripPrefix (reverse suf) . reverse

-- | Reads the source file in the given directory with the given name.
loadSrcFileInDir :: FilePath -> String -> IO File
loadSrcFileInDir dir name' = loadFile $ dir </> name' <.> "dscr"

-- | Reads the source file in the given directory with the given name.
loadTestInfoInDir :: FilePath -> String -> IO TestInfo
loadTestInfoInDir dir name' = do
  let path = dir </> name' <.> "out.yaml"
  result <- decodeFileEither path
  case result of
    Left err
      -> fail
       $ "Error while decoding test info for "
      ++ name'
      ++ ": "
      ++ prettyPrintParseException err
    Right x -> pure x

-- | Reads the test file in the given directory with the given name.
loadTestFileInDir :: FilePath -> String -> IO TestFile
loadTestFileInDir dir name'
  = TestFile <$> loadSrcFileInDir dir name' <*> loadTestInfoInDir dir name'

-- | Gets the all test files in the directory, assuming the directory doesn't
-- contain any sub-directories.
loadFilesInDir :: FilePath -> IO [TestFile]
loadFilesInDir dir
    = traverse (loadTestFileInDir dir)
    . sort
    . mapMaybe (stripSuffix ".dscr")
  =<< listDirectory dir
