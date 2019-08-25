{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests all parts of compiling and interpreting.
module IntegrSpec
  ( spec
  )
where

import           Core.Test
import           TreeScript

import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.FilePath
import           System.Directory
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
  -- exampleEnv       <- runIO $ fmap forceSuccess $ runPreSessionRes getInitialEnv
  exampleFiles     <- runIO getExampleFiles
  exampleParseVars <- runIO $ mkVarMap exampleFiles
  let
    forExampleFile :: (TestFile -> SpecWith FilePath) -> SpecWith FilePath
    forExampleFile f = forM_ exampleFiles $ \file ->
      unless (testInfoSkip $ testFileTestInfo file)
        $ describe ("In " ++ takeBaseName (testFileSrcPath file))
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
    -- forExampleParse :: TestFile -> (Program SrcAnn -> IO ()) -> IO ()
    -- forExampleParse =
    --   forExampleIntermediateIn exampleParseVars exampleSugarVars
    assertProperFailure :: (Printable a) => TestInfo -> EResult a -> IO ()
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
    $ \file@(TestFile srcPath srcContents testInfo _) ->
        describe "The parser" $ it "parses" $ \_ -> do
          parseRes :: EResult (Program SrcAnn) <- runResultT $ parseFile srcPath
          let rawParseRes = mapAnn (\_ -> NoAnn) <$> parseRes
          when (testInfoPrintParse testInfo) $ do
            T.putStrLn $ T.pack (takeBaseName srcPath) <> ":"
            T.putStrLn $ pprint rawParseRes
          if testInfoIsParseable testInfo
            then case parseRes of
              ResultFail parseErr -> do
                insertVarMapFailure exampleParseVars file False
                assertFailureText $ pprint parseErr
              Result parseErrs parseSrc -> do
                let rawParseSrc = mapAnn (\_ -> NoAnn) parseSrc
                insertVarMapSuccess exampleParseVars file parseSrc
                assertNoErrors parseErrs
                denoteFail "reused (with original source) - "
                  $                       pprint parseSrc
                  `shouldBeReducePrintOf` srcContents
                denoteFail "generated (without original source) - "
                  $                       pprint rawParseSrc
                  `shouldBeReducePrintOf` srcContents
            else do
              insertVarMapFailure exampleParseVars file True
              assertProperFailure testInfo parseRes
