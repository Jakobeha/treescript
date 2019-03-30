{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests all parts of compiling and interpreting.
module IntegrSpec
       ( spec ) where

import Core.Test
import TreeScript
import qualified TreeScript.Ast.Translate as T
import qualified TreeScript.Ast.Core as C
import qualified TreeScript.Ast.Lex as L
import qualified TreeScript.Ast.Sugar as S

import Control.Concurrent.MVar
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO.Temp

type VarMap k v = M.Map k (MVar (Maybe v))

examplesDir :: FilePath
examplesDir = "test-resources/examples/"

getExampleFiles :: IO [TestFile]
getExampleFiles = loadFilesInDir examplesDir

mkVarMap :: (Ord k) => [k] -> IO (VarMap k v)
mkVarMap = fmap M.fromList . mapM (sequence . (, newMVar Nothing))

insertVarMap :: (Ord k) => VarMap k v -> k -> v -> IO ()
insertVarMap vars key x = do
  Nothing <- swapMVar (vars M.! key) $ Just x
  pure ()

spec :: Spec
spec = do
  sysTmpDir <- runIO getCanonicalTemporaryDirectory
  exampleEnv <- runIO $ fmap forceSuccess $ runPreSessionRes $ getInitialEnv
  exampleFiles <- runIO getExampleFiles
  exampleLexVars <- runIO $ mkVarMap exampleFiles
  exampleSugarVars <- runIO $ mkVarMap exampleFiles
  exampleCoreVars <- runIO $ mkVarMap exampleFiles
  exampleTranslateVars <- runIO $ mkVarMap exampleFiles
  exampleExecVars <- runIO $ mkVarMap exampleFiles
  let forExampleFile :: (TestFile -> IO ()) -> IO ()
      forExampleFile f =
        forM_ exampleFiles $ \file ->
          unless (testInfoSkip $ testFileTestInfo file) $
            denoteFailIn ("file " <> fileName (testFileSrcFile file)) $
              f file
      forExampleIntermediateIn :: M.Map TestFile (MVar (Maybe a)) -> (TestFile -> a -> IO ()) -> IO ()
      forExampleIntermediateIn xVars f =
        forM_ (M.toAscList xVars) $ \(file, xVar) ->
          denoteFailIn ("file " <> fileName (testFileSrcFile file)) $ withMVar xVar $ \case
            Nothing -> pure ()
            Just val -> f file val
      forExampleLex :: (TestFile -> L.Program Range -> IO ()) -> IO ()
      forExampleLex = forExampleIntermediateIn exampleLexVars
      forExampleSugar :: (TestFile -> S.Program Range -> IO ()) -> IO ()
      forExampleSugar = forExampleIntermediateIn exampleSugarVars
      forExampleCore :: (TestFile -> C.Program Range -> IO ()) -> IO ()
      forExampleCore = forExampleIntermediateIn exampleCoreVars
      forExampleTranslate :: (TestFile -> T.Program -> IO ()) -> IO ()
      forExampleTranslate = forExampleIntermediateIn exampleTranslateVars
      forExampleExec :: (TestFile -> FilePath -> IO ()) -> IO ()
      forExampleExec = forExampleIntermediateIn exampleExecVars
      codeToAstData :: T.Text -> T.Text -> ResultT IO T.Text
      codeToAstData txt ext
        | ext == "tast" = pure txt
        | otherwise = ResultT $ runSessionResVirtual exampleEnv $ do
          lang <- langWithExt undefined ext
          let parser = languageParser lang
          runCmdProgram parser txt
      astDataToCode :: T.Text -> T.Text -> ResultT IO T.Text
      astDataToCode txt ext
        | ext == "tast" = pure txt
        | otherwise = ResultT $ runSessionResVirtual exampleEnv $ do
          lang <- langWithExt undefined ext
          let printer = languagePrinter lang
          runCmdProgram printer txt
      assertProperFailure :: (Printable a) => TestInfo -> Result a -> IO ()
      assertProperFailure testInfo (ResultFail err) = do
        unless (T.null $ testInfoFatalErrorMsg testInfo) $
          pprint err `shouldBe` testInfoFatalErrorMsg testInfo
        [] `shouldBe` testInfoErrorMsgs testInfo
      assertProperFailure testInfo (Result errs src)
        | null errs = assertFailureText $ "Unexpected non-fatal result: " <> pprint src
        | otherwise = do
          unless (null $ testInfoErrorMsgs testInfo) $
              map pprint (sortOn errorRange errs) `shouldBe` testInfoErrorMsgs testInfo
          "" `shouldBe` testInfoFatalErrorMsg testInfo

  beforeAll (createTempDirectory sysTmpDir "treescript-tests") $ afterAll removeDirectoryRecursive $ do
    describe "The compiler" $ do
      it "Lexes" $ \_ ->
        forExampleFile $ \file@(TestFile srcFile testInfo _) -> do
          let fileStr = fileContents srcFile
              lexRes = L.parse fileStr
          when (testInfoPrintLex testInfo) $ do
            T.putStrLn $ fileName srcFile <> ":"
            T.putStrLn $ pprint lexRes
          if testInfoIsLexable testInfo then
            case lexRes of
              ResultFail lexErr ->
                assertFailureText $ pprint lexErr
              Result lexErrs lexSrc -> do
                insertVarMap exampleLexVars file lexSrc
                assertNoErrors lexErrs
                reducePrint lexSrc `shouldBeReducePrintOf` fileStr
          else
            assertProperFailure testInfo lexRes
      it "Parses" $ \_ ->
        forExampleLex $ \file@(TestFile srcFile testInfo _) lexSrc -> do
          let fileStr = fileContents srcFile
              sugarRes = S.parse lexSrc
          when (testInfoPrintSugar testInfo) $ do
            T.putStrLn $ fileName srcFile <> ":"
            T.putStrLn $ pprint sugarRes
          if testInfoIsParseable testInfo then
            case sugarRes of
              ResultFail sugarErr ->
                assertFailureText $ pprint sugarErr
              Result sugarErrs sugarSrc -> do
                insertVarMap exampleSugarVars file sugarSrc
                assertNoErrors sugarErrs
                reducePrint sugarSrc `shouldBeReducePrintOf` fileStr
          else
            assertProperFailure testInfo sugarRes
      it "Desugars" $ \_ ->
        forExampleSugar $ \file@(TestFile srcFile testInfo _) sugarSrc -> do
          coreRes <- runSessionResVirtual exampleEnv $ C.parse sugarSrc
          when (testInfoPrintCore testInfo) $ do
            T.putStrLn $ fileName srcFile <> ":"
            T.putStrLn $ pprint coreRes
          if testInfoIsDesugarable testInfo then
            case coreRes of
              ResultFail coreErr ->
                assertFailureText $ pprint coreErr
              Result coreErrs coreSrc -> do
                insertVarMap exampleCoreVars file coreSrc
                assertNoErrors coreErrs
          else
            assertProperFailure testInfo coreRes
      it "Translates" $ \_ ->
        forExampleCore $ \file@(TestFile srcFile testInfo _) coreSrc -> do
          translateRes <- runSessionResVirtual exampleEnv $ T.parse coreSrc
          when (testInfoPrintTranslate testInfo) $ do
            T.putStrLn $ fileName srcFile <> ":"
            T.putStrLn $ pprint translateRes
          if testInfoIsTranslatable testInfo then
            case translateRes of
              ResultFail translateErr ->
                assertFailureText $ pprint translateErr
              Result translateErrs translateSrc -> do
                insertVarMap exampleTranslateVars file translateSrc
                assertNoErrors translateErrs
          else
            assertProperFailure testInfo translateRes
      it "Compiles" $ \tmpDir ->
        forExampleTranslate $ \file@(TestFile srcFile testInfo _) translateSrc -> do
          let execPath = tmpDir </> T.unpack (fileName srcFile)
          execRes <- runSessionResVirtual exampleEnv $ T.exportFile execPath translateSrc
          if testInfoIsCompilable testInfo then
            case execRes of
              ResultFail execErr ->
                assertFailureText $ pprint execErr
              Result execErrs () -> do
                insertVarMap exampleExecVars file execPath
                assertNoErrors execErrs
          else
            assertProperFailure testInfo execRes
    describe "The compiled executable" $
      it "Transforms source code" $ \_ ->
        forExampleExec $ \(TestFile _ testInfo execTests) execPath ->
          forM_ execTests $ \(ExecTest name inPath inTxt inExt outTxt outExt) -> unless (name `elem` testInfoSkipRun testInfo) $ denoteFailIn ("executable test " <> name) $ do
            let execProg
                  = CmdProgram
                  { cmdProgramStage = StageRunning
                  , cmdProgramPath = execPath
                  , cmdProgramEnv = testInfoRunEnv testInfo
                  }
            progOutRes <- runResultT $ do
              inAstData <- codeToAstData inTxt inExt
              outAstData <- runCmdProgramArgs execProg [T.pack inPath, "--stdin", "--stdout"] inAstData
              astDataToCode outAstData outExt
            if name `elem` testInfoCantRun testInfo then
              assertProperFailure testInfo progOutRes
            else
              case progOutRes of
                ResultFail progErr ->
                  assertFailureText $ pprint progErr
                Result progErrs progOutTxt -> do
                  assertNoErrors progErrs
                  T.strip progOutTxt `shouldBe` T.strip outTxt
