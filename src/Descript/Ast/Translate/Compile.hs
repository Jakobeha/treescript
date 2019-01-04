{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Splice a Descript source file's generated C code into the template, and compile it into an executable.
module Descript.Ast.Translate.Compile
  ( compile
  ) where

import Descript.Ast.Translate.Types
import qualified Descript.Misc.Ext.Text as T
import Descript.Misc
import Descript.Plugin

import Control.Monad
import qualified Data.Text as T
import Shelly hiding (FilePath)

default (T.Text)

-- | Splice the C code into the template, and compile it to the output path.
compile :: Translated -> FilePath -> SessionRes ()
compile (Translated numProps reduceSurface) outPath = do
  env <- getSessionEnv
  let templateDir = fromText $ T.pack $ sessionEnvTemplateDir env
  -- TODO Handle exceptions
  catchExceptionToError StageCompiling $ shelly $ withTmpDir $ \tempDir -> do
    let projectDir = tempDir </> "project"
        splicePath = tempDir </> "splice"
    cp_r templateDir projectDir
    projectSrcPaths <- lsT projectDir
    let splice old new = do
          writefile splicePath $ T.indent new
          forM_ projectSrcPaths $ \projectSrcPath ->
            run_ "sed" ["-i", "''", "/\\/\\/ \\\\" <> old <> "/r " <> toTextIgnore splicePath, projectSrcPath]
    splice "get_record_num_props" numProps
    splice "reduce_surface" reduceSurface
    run_ "gcc" $ filter (".c" `T.isSuffixOf`) projectSrcPaths ++ ["-o", T.pack outPath]
