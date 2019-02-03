-- | Fully compile a TreeScript program.
module TreeScript.Compile
  ( compile
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
  <=< C.parse
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile
    ) input
