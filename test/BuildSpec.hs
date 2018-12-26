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
      forExampleCore :: (TestFile -> C.Program -> IO ()) -> IO ()
      forExampleCore = forExampleIntermediateIn exampleCoreVars

  describe "Read" $ do
    it "Lexes" $
      forExampleFile $ \file@(TestFile srcFile testInfo) -> do
        let fileStr = fileContents srcFile
            lexRes = L.parse fileStr
        if testInfoIsLexable testInfo then do
          when (testInfoPrintLex testInfo) $ do
            T.putStrLn $ fileName srcFile <> ":"
            T.putStrLn $ pprint lexRes
          case lexRes of
            ResultFail lexErr -> assertFailureText $ errorMsg lexErr
            Result lexErrs lexSrc -> do
              insertVarMap exampleLexVars file lexSrc
              assertNoErrors lexErrs
              reducePrint lexSrc `shouldBeReducePrintOf` fileStr
        else
          unless (T.null $ testInfoErrorMsg testInfo) $ do
            lexRes `shouldFailTo` testInfoErrorMsg testInfo
    it "Parses" $
      forExampleLex $ \file@(TestFile srcFile testInfo) lexSrc -> do
        let fileStr = fileContents srcFile
            sugarRes = S.parse lexSrc
        if testInfoIsParseable testInfo then do
          when (testInfoPrintSugar testInfo) $ do
            T.putStrLn $ fileName srcFile <> ":"
            T.putStrLn $ pprint sugarRes
          case sugarRes of
            ResultFail sugarErr -> assertFailureText $ errorMsg sugarErr
            Result sugarErrs sugarSrc -> do
              insertVarMap exampleSugarVars file sugarSrc
              assertNoErrors sugarErrs
              reducePrint sugarSrc `shouldBeReducePrintOf` fileStr
        else
          unless (T.null $ testInfoErrorMsg testInfo) $ do
            sugarRes `shouldFailTo` testInfoErrorMsg testInfo
