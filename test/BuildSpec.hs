{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests all parts of building.
module BuildSpec
       ( spec ) where

import Core.Test
import Descript
import Descript.Ast.Core as C
import Descript.Ast.Lex as L
import Descript.Ast.Sugar as S

import Control.Concurrent.MVar
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
  exampleFiles <- runIO getExampleFiles
  exampleLexVars <- runIO $ mkVarMap exampleFiles
  exampleSugarVars <- runIO $ mkVarMap exampleFiles
  exampleCoreVars <- runIO $ mkVarMap exampleFiles
  let forExampleFile :: (TestFile -> IO ()) -> IO ()
      forExampleFile f =
        forM_ exampleFiles $ \file ->
          denoteFailIn ("file " <> fileName (testFileSrcFile file)) $ f file
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
      assertProperFailure :: (Printable a) => TestInfo -> Result a -> IO ()
      assertProperFailure testInfo (ResultFail err) = do
        unless (T.null $ testInfoFatalErrorMsg testInfo) $
          errorMsg err `shouldBe` testInfoFatalErrorMsg testInfo
        [] `shouldBe` testInfoErrorMsgs testInfo
      assertProperFailure testInfo (Result errs src)
        | null errs = assertFailureText $ "Unexpected non-fatal result: " <> pprint src
        | otherwise = do
          unless (null $ testInfoErrorMsgs testInfo) $
              map errorMsg (sortOn errorRange errs) `shouldBe` testInfoErrorMsgs testInfo
          "" `shouldBe` testInfoFatalErrorMsg testInfo

  describe "Read" $ do
    it "Lexes" $
      forExampleFile $ \file@(TestFile srcFile testInfo) -> do
        let fileStr = fileContents srcFile
            lexRes = L.parse fileStr
        when (testInfoPrintLex testInfo) $ do
          T.putStrLn $ fileName srcFile <> ":"
          T.putStrLn $ pprint lexRes
        if testInfoIsLexable testInfo then
          case lexRes of
            ResultFail lexErr ->
              assertFailureText $ errorMsg lexErr
            Result lexErrs lexSrc -> do
              insertVarMap exampleLexVars file lexSrc
              assertNoErrors lexErrs
              reducePrint lexSrc `shouldBeReducePrintOf` fileStr
        else
          assertProperFailure testInfo lexRes
    it "Parses" $
      forExampleLex $ \file@(TestFile srcFile testInfo) lexSrc -> do
        let fileStr = fileContents srcFile
            sugarRes = S.parse lexSrc
        when (testInfoPrintSugar testInfo) $ do
          T.putStrLn $ fileName srcFile <> ":"
          T.putStrLn $ pprint sugarRes
        if testInfoIsParseable testInfo then
          case sugarRes of
            ResultFail sugarErr ->
              assertFailureText $ errorMsg sugarErr
            Result sugarErrs sugarSrc -> do
              insertVarMap exampleSugarVars file sugarSrc
              assertNoErrors sugarErrs
              reducePrint sugarSrc `shouldBeReducePrintOf` fileStr
        else
          assertProperFailure testInfo sugarRes
    it "Desugars" $
      forExampleSugar $ \file@(TestFile srcFile testInfo) sugarSrc -> do
        let coreRes = C.parse sugarSrc
        when (testInfoPrintCore testInfo) $ do
          T.putStrLn $ fileName srcFile <> ":"
          T.putStrLn $ pprint coreRes
        if testInfoIsDesugarable testInfo then
          case coreRes of
            ResultFail coreErr ->
              assertFailureText $ errorMsg coreErr
            Result coreErrs coreSrc -> do
              insertVarMap exampleCoreVars file coreSrc
              assertNoErrors coreErrs
        else
          assertProperFailure testInfo coreRes
