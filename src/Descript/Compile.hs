-- | Fully compile a Descript program.
module Descript.Compile
  ( compile
  ) where

import qualified Descript.Ast.Translate as T
import qualified Descript.Ast.Core as C
import qualified Descript.Ast.Lex as L
import qualified Descript.Ast.Sugar as S
import Descript.Misc
import Descript.Plugin

import Control.Monad
import qualified Data.Text.IO as T

-- | Read the Descript source from the first path, compile it, and move the executable to the second path.
compile :: FilePath -> FilePath -> SessionRes ()
compile input output
  | input == output = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise
  = ( (`T.compile` output)
  <=< T.translate
  <=< C.parse
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile
    ) input
