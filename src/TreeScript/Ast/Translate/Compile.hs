{-# LANGUAGE OverloadedStrings #-}

-- | Serialize compiled code into a file.
module TreeScript.Ast.Translate.Compile
  ( export
  , exportFile
  ) where

import TreeScript.Ast.Translate.Types
import TreeScript.Misc
import TreeScript.Plugin

import qualified Data.ByteString.Lazy.Char8 as B
import Data.MessagePack
import System.Posix.Files
import System.Posix.Types

-- | Serialize the code, and add a shebang which makes it run as an executable.
export :: Program -> B.ByteString
export prog = "#! /usr/bin/env treescript-interpreter\n" <> pack prog

exportFileMode :: FileMode
exportFileMode = CMode 0o755 -- Everyone can read and execute, owner can write

-- | Serialize the code as an executable into the output path.
exportFile :: FilePath -> Program -> SessionRes ()
exportFile outPath prog = liftIOAndCatch StageWriteCompiled $ do
  B.writeFile outPath $ export prog
  setFileMode outPath exportFileMode
