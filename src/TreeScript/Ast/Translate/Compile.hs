{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Splice a TreeScript source file's generated C code into the template, and compile it into an executable.
module TreeScript.Ast.Translate.Compile
  ( compile
  ) where

import TreeScript.Ast.Translate.Types
import qualified TreeScript.Misc.Ext.Text as T
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad
import qualified Data.Text as T
import Shelly hiding (FilePath)

default (T.Text)

-- | Splice the C code into the template, and compile it to the output path.
compile :: Translated -> FilePath -> SessionRes ()
compile (Translated maxNumBinds numProps reduceMain reduceExtras) outPath = do
  env <- getSessionEnv
  let templateDir = fromText $ T.pack $ sessionEnvTemplateDir env
  -- TODO Handle exceptions
  catchExceptionToError StageCompiling $ shelly $ withTmpDir $ \tempDir -> do
    let projectDir = tempDir </> "project"
        splicePath = tempDir </> "splice"
    cp_r templateDir projectDir
    projectSrcPaths <- lsT projectDir
    let splice old new = do
          writefile splicePath new
          forM_ projectSrcPaths $ \projectSrcPath ->
            run_ "sed" ["-i", "''", "/\\/\\/ \\\\" <> old <> "/r " <> toTextIgnore splicePath, projectSrcPath]
    splice "max_num_binds" $ "#undef MAX_NUM_BINDS\n#define MAX_NUM_BINDS " <> maxNumBinds
    splice "get_record_num_props" $ T.indent numProps
    splice "reduce_main" $ T.indent reduceMain
    splice "reduce_extras" reduceExtras
    run_ "gcc" $ filter (".c" `T.isSuffixOf`) projectSrcPaths ++ ["-o", T.pack outPath]
