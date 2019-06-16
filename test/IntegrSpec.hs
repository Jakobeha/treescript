{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests all parts of compiling and interpreting.
module IntegrSpec
  ( spec
  )
where

import           Core.Test
import           TreeScript
import qualified TreeScript.Ast.Core           as C
import qualified TreeScript.Ast.Lex            as L
import qualified TreeScript.Ast.Sugar          as S

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Directory
import           System.FilePath
import           System.IO.Temp

data PhaseRes a
  = PhaseResUnset
  | PhaseResFailure Bool -- ^ Was it intentional?
  | PhaseResSuccess a

type VarMap k v = M.Map k (MVar (PhaseRes v))

examplesDir :: FilePath
examplesDir = "test-resources/examples/"

getExampleFiles :: IO [TestFile]
getExampleFiles = loadFilesInDir examplesDir

mkVarMap :: (Ord k) => [k] -> IO (VarMap k v)
mkVarMap = fmap M.fromList . mapM (sequence . (, newMVar PhaseResUnset))

insertVarMapSuccess :: (Ord k) => VarMap k v -> k -> v -> IO ()
insertVarMapSuccess vars key x = do
  PhaseResUnset <- swapMVar (vars M.! key) $ PhaseResSuccess x
  pure ()

insertVarMapFailure :: (Ord k) => VarMap k v -> k -> Bool -> IO ()
insertVarMapFailure vars key intentional = do
  PhaseResUnset <- swapMVar (vars M.! key) $ PhaseResFailure intentional
  pure ()

spec :: Spec
spec = do
  sysTmpDir        <- runIO getCanonicalTemporaryDirectory
  exampleEnv       <- runIO $ fmap forceSuccess $ runPreSessionRes getInitialEnv
  exampleFiles     <- runIO getExampleFiles
  exampleLexVars   <- runIO $ mkVarMap exampleFiles
  exampleSugarVars <- runIO $ mkVarMap exampleFiles
  exampleCoreVars  <- runIO $ mkVarMap exampleFiles
  exampleExecVars  <- runIO $ mkVarMap exampleFiles
  exampleUnsetVars <- runIO $ mkVarMap exampleFiles
  let
    forExampleFile :: (TestFile -> SpecWith FilePath) -> SpecWith FilePath
    forExampleFile f = forM_ exampleFiles $ \file ->
      unless (testInfoSkip $ testFileTestInfo file)
        $ describe (T.unpack $ "In " <> fileName (testFileSrcFile file))
        $ f file
    forExampleIntermediateIn
      :: M.Map TestFile (MVar (PhaseRes a))
      -> M.Map TestFile (MVar (PhaseRes b))
      -> TestFile
      -> (a -> IO ())
      -> IO ()
    forExampleIntermediateIn curVars nextVars file f = do
      let curVar = curVars M.! file
      withMVar curVar $ \case
        PhaseResUnset -> pendingWith
          "WARNING: previous phase unset! This is a bug in the test suite"
        PhaseResFailure intentional -> do
          insertVarMapFailure nextVars file intentional
          unless intentional $ pendingWith "no previous phase"
        PhaseResSuccess val -> f val
    forExampleLex :: TestFile -> (L.Program Range -> IO ()) -> IO ()
    forExampleLex = forExampleIntermediateIn exampleLexVars exampleSugarVars
    forExampleSugar :: TestFile -> (S.Program Range -> IO ()) -> IO ()
    forExampleSugar = forExampleIntermediateIn exampleSugarVars exampleCoreVars
    forExampleCore :: TestFile -> (C.Program () -> IO ()) -> IO ()
    forExampleCore = forExampleIntermediateIn exampleCoreVars exampleExecVars
    forExampleExec :: TestFile -> (FilePath -> IO ()) -> IO ()
    forExampleExec = forExampleIntermediateIn exampleExecVars exampleUnsetVars
    assertProperFailure :: (Printable a) => TestInfo -> Result a -> IO ()
    assertProperFailure testInfo (ResultFail err) = do
      unless (T.null $ testInfoFatalErrorMsg testInfo)
        $          pprint err
        `shouldBe` testInfoFatalErrorMsg testInfo
      [] `shouldBe` testInfoErrorMsgs testInfo
    assertProperFailure testInfo (Result errs src)
      | null errs =  assertFailureText
      $  "Unexpected non-fatal result: "
      <> pprint src
      | otherwise = do
        unless (null $ testInfoErrorMsgs testInfo)
          $          map pprint (S.toList errs)
          `shouldBe` testInfoErrorMsgs testInfo
        "" `shouldBe` testInfoFatalErrorMsg testInfo

  beforeAll (createTempDirectory sysTmpDir "treescript-tests")
    $ afterAll removeDirectoryRecursive
    $ forExampleFile
    $ \file@(TestFile srcFile testInfo execTests) -> do
        describe "The parser" $ do
          it "Lexes" $ \_ -> do
            let fileStr = fileContents srcFile
                lexRes  = L.parse fileStr
            when (testInfoPrintLex testInfo) $ do
              T.putStrLn $ fileName srcFile <> ":"
              T.putStrLn $ pprint lexRes
            if testInfoIsLexable testInfo
              then case lexRes of
                ResultFail lexErr -> do
                  insertVarMapFailure exampleLexVars file False
                  assertFailureText $ pprint lexErr
                Result lexErrs lexSrc -> do
                  insertVarMapSuccess exampleLexVars file lexSrc
                  assertNoErrors lexErrs
                  reducePrint lexSrc `shouldBeReducePrintOf` fileStr
              else do
                insertVarMapFailure exampleLexVars file True
                assertProperFailure testInfo lexRes
          it "Parses" $ \_ -> forExampleLex file $ \lexSrc -> do
            let fileStr  = fileContents srcFile
                sugarRes = S.parse lexSrc
            when (testInfoPrintSugar testInfo) $ do
              T.putStrLn $ fileName srcFile <> ":"
              T.putStrLn $ pprint sugarRes
            if testInfoIsParseable testInfo
              then case sugarRes of
                ResultFail sugarErr -> do
                  insertVarMapFailure exampleSugarVars file False
                  assertFailureText $ pprint sugarErr
                Result sugarErrs sugarSrc -> do
                  insertVarMapSuccess exampleSugarVars file sugarSrc
                  assertNoErrors sugarErrs
                  reducePrint sugarSrc `shouldBeReducePrintOf` fileStr
              else do
                insertVarMapFailure exampleSugarVars file True
                assertProperFailure testInfo sugarRes
          it "Desugars" $ \_ -> forExampleSugar file $ \sugarSrc -> do
            coreRes <- runSessionResVirtual exampleEnv
              $ parse1_ examplesDir (fileName srcFile) sugarSrc
            when (testInfoPrintCore testInfo) $ do
              T.putStrLn $ fileName srcFile <> ":"
              T.putStrLn $ pprint $ coreRes
            if testInfoIsDesugarable testInfo
              then case coreRes of
                ResultFail coreErr -> do
                  insertVarMapFailure exampleCoreVars file False
                  assertFailureText $ pprint coreErr
                Result coreErrs coreSrc -> do
                  insertVarMapSuccess exampleCoreVars file coreSrc
                  assertNoErrors coreErrs
              else do
                insertVarMapFailure exampleCoreVars file True
                assertProperFailure testInfo $ coreRes
        describe "The interpreter" $ it "Evaluates" $ \_ ->
          forExampleCore file $ \prg ->
            forM_ execTests
              $ \(ExecTest name inPath inTxt inExt outTxt outExt) ->
                  unless (name `elem` testInfoSkipRun testInfo)
                    $ denoteFailIn ("executable test " <> name)
                    $ do
                        progOutRes <- runSessionResVirtual exampleEnv
                          $ evalFileOutText inPath (r0 <$ prg)
                        if name `elem` testInfoCantRun testInfo
                          then assertProperFailure testInfo progOutRes
                          else case progOutRes of
                            ResultFail progErr ->
                              assertFailureText $ pprint progErr
                            Result progErrs progOutTxt -> do
                              assertNoErrors progErrs
                              T.strip progOutTxt `shouldBe` T.strip outTxt
