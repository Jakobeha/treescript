-- | Fully compile a TreeScript program.
module TreeScript.Compile
  ( compile
  , compileRaw
  ) where

import qualified TreeScript.Ast.Translate as T
import qualified TreeScript.Ast.Core as C
import qualified TreeScript.Ast.Lex as L
import qualified TreeScript.Ast.Sugar as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad
import qualified Data.Text.IO as T

-- | Read the TreeScript source from the first path, compile it, and move the executable to the second path.
compile :: FilePath -> FilePath -> SessionRes ()
compile input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise
  = ( T.exportFile output
  <=< T.parse
  <=< C.parse input
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile
    ) input

-- | Read the TreeScript source from the first path, serialize it, and move the raw data to the second path. For testing.
compileRaw :: FilePath -> FilePath -> SessionRes ()
compileRaw input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise
  = ( T.exportRaw output
  <=< T.parse
  <=< C.parse input
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile
    ) input
