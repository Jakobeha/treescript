-- | Fully compile a TreeScript program.
module TreeScript.Compile
  ( compile
  , compileRaw
  ) where

import TreeScript.Ast.Core
import TreeScript.Misc
import TreeScript.Plugin

-- | Read the TreeScript source from the first path, compile it, and move the executable to the second path.
compile :: FilePath -> FilePath -> SessionRes ()
compile input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise = exportFile output =<< parseFromStart input

-- | Read the TreeScript source from the first path, serialize it, and move the raw data to the second path. For testing.
compileRaw :: FilePath -> FilePath -> SessionRes ()
compileRaw input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise = exportRaw output =<< parseFromStart input
