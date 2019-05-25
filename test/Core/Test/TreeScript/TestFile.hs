{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Core.Test.TreeScript.TestFile
  ( ExecTest(..)
  , TestFile(..)
  , TestInfo(..)
  , loadFilesInDir
  )
where

import           TreeScript

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Yaml
import           System.Directory
import           System.FilePath

data ExecTest
  = ExecTest
  { execTestName :: T.Text
  , execTestInputPath :: FilePath
  , execTestInput :: T.Text
  , execTestInputExt :: T.Text
  , execTestOutput :: T.Text
  , execTestOutputExt :: T.Text
  } deriving (Eq, Ord, Read, Show)

data TestFile
  = TestFile
  { testFileSrcFile :: File
  , testFileTestInfo :: TestInfo
  , testFileExecTests :: [ExecTest]
  } deriving (Eq, Ord, Read, Show)

data TestInfo
  = TestInfo
  { testInfoSkip :: Bool
  , testInfoSkipRun :: [T.Text]
  , testInfoPrintLex :: Bool
  , testInfoPrintSugar :: Bool
  , testInfoPrintCore :: Bool
  , testInfoPrintTranslate :: Bool
  , testInfoIsLexable :: Bool
  , testInfoIsParseable :: Bool
  , testInfoIsDesugarable :: Bool
  , testInfoIsTranslatable :: Bool
  , testInfoIsCompilable :: Bool
  , testInfoCantRun :: [T.Text]
  , testInfoFatalErrorMsg :: T.Text
  , testInfoErrorMsgs :: [T.Text]
  , testInfoRunEnv :: [(String, String)]
  } deriving (Eq, Ord, Read, Show)

instance FromJSON TestInfo where
  parseJSON (Object x) =
    TestInfo
      <$> (x .:? "skip?" .!= False)
      <*> (x .:? "skipRun" .!= [])
      <*> (x .:? "printLex?" .!= False)
      <*> (x .:? "printSugar?" .!= False)
      <*> (x .:? "printCore?" .!= False)
      <*> (x .:? "printTranslate?" .!= False)
      <*> (x .:? "lexes?" .!= True)
      <*> (x .:? "parses?" .!= True)
      <*> (x .:? "desugars?" .!= True)
      <*> (x .:? "translates?" .!= True)
      <*> (x .:? "compiles?" .!= True)
      <*> (x .:? "cantRun" .!= [])
      <*> (x .:? "error" .!= "")
      <*> (x .:? "errors" .!= [])
      <*> (M.toList <$> x .:? "runEnv" .!= M.empty)
  parseJSON Null = pure TestInfo { testInfoSkip           = False
                                 , testInfoSkipRun        = []
                                 , testInfoPrintLex       = False
                                 , testInfoPrintSugar     = False
                                 , testInfoPrintCore      = False
                                 , testInfoPrintTranslate = False
                                 , testInfoIsLexable      = True
                                 , testInfoIsParseable    = True
                                 , testInfoIsDesugarable  = True
                                 , testInfoIsTranslatable = True
                                 , testInfoIsCompilable   = True
                                 , testInfoCantRun        = []
                                 , testInfoFatalErrorMsg  = ""
                                 , testInfoErrorMsgs      = []
                                 , testInfoRunEnv         = []
                                 }
  parseJSON _ = fail "expected object or null"

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suf = fmap reverse . stripPrefix (reverse suf) . reverse

-- | Reads the source file in the given directory with the given name.
loadSrcFileInDir :: FilePath -> String -> IO File
loadSrcFileInDir dir name' = loadFile $ dir </> name' <.> "tscr"

-- | Reads the source file in the given directory with the given name.
loadTestInfoInDir :: FilePath -> String -> IO TestInfo
loadTestInfoInDir dir name' = do
  let path = dir </> name' <.> "out.yaml"
  result <- decodeFileEither path
  case result of
    Left err ->
      fail
        $  "Error while decoding test info for "
        ++ name'
        ++ ": "
        ++ prettyPrintParseException err
    Right x -> pure x

mkExecTest :: FilePath -> (String, String) -> (String, String) -> IO ExecTest
mkExecTest dir (inName, inExt) (outName, outExt)
  | inName /= outName
  = error
    $  "executable test input/output pair mismatch: '"
    ++ inName
    ++ "' vs '"
    ++ outName
    ++ "'"
  | otherwise
  = do
    let inputPath  = dir </> inName <.> "in" <.> inExt
        outputPath = dir </> outName <.> "out" <.> outExt
    input  <- T.readFile inputPath
    output <- T.readFile outputPath
    pure ExecTest { execTestName      = T.pack inName
                  , execTestInputPath = inputPath
                  , execTestInput     = input
                  , execTestInputExt  = T.pack inExt
                  , execTestOutput    = output
                  , execTestOutputExt = T.pack outExt
                  }

-- | Finds executable input/output tests in the given directory with the given name.
loadExecTestsInDir :: FilePath -> String -> IO [ExecTest]
loadExecTestsInDir dir name' = do
  let execTestsDir = dir </> name'
  hasExecTests <- doesDirectoryExist execTestsDir
  if not hasExecTests
    then pure []
    else do
      execTestPaths <- map splitExtensions . sort <$> listDirectory execTestsDir
      let
        execInPaths = mapMaybe
          (\(name, ext) -> (name, ) <$> stripPrefix ".in." ext)
          execTestPaths
        execOutPaths = mapMaybe
          (\(name, ext) -> (name, ) <$> stripPrefix ".out." ext)
          execTestPaths
      zipWithM (mkExecTest execTestsDir) execInPaths execOutPaths

-- | Reads the test file in the given directory with the given name.
loadTestFileInDir :: FilePath -> String -> IO TestFile
loadTestFileInDir dir name' =
  TestFile
    <$> loadSrcFileInDir dir name'
    <*> loadTestInfoInDir dir name'
    <*> loadExecTestsInDir dir name'

-- | Gets the all test files in the directory, assuming the directory doesn't
-- contain any sub-directories.
loadFilesInDir :: FilePath -> IO [TestFile]
loadFilesInDir dir =
  traverse (loadTestFileInDir dir)
    .   sort
    .   mapMaybe (stripSuffix ".tscr")
    =<< listDirectory dir
