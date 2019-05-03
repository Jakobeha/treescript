-- | Fully compile a TreeScript program.
module TreeScript.Compile
  ( compile
  , compileInterp
  ) where

import TreeScript.Ast.Core
import TreeScript.Misc
import TreeScript.Plugin

-- | Read the TreeScript source from the first path, compile it, and move the executable to the second path.
compile :: FilePath -> FilePath -> SessionRes ()
compile input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise = exportFile output =<< parse input

-- | Read the TreeScript source from the first path, serialize it, and move the raw data to the second path. For testing.
compileInterp :: FilePath -> FilePath -> SessionRes ()
compileInterp input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise = exportInterp output =<< parse input
