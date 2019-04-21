{-# LANGUAGE OverloadedStrings #-}

-- | Serialize compiled code into a file.
module TreeScript.Ast.Core.Compile
  ( export
  , exportFile
  , exportRaw
  ) where

import TreeScript.Ast.Core.Serialize
import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import qualified Data.ByteString.Lazy.Char8 as B
import System.Posix.Files
import System.Posix.Types

-- | Serialize the program, and add a shebang which makes it run as an executable.
export :: Program () () () -> B.ByteString
export prog = "#! /usr/bin/env treescript-interpreter\n" <> serialize prog

exportFileMode :: FileMode
exportFileMode = CMode 0o755 -- Everyone can read and execute, owner can write

-- | Serialize the program as an executable into the output path.
exportFile :: FilePath -> Program () () () -> SessionRes ()
exportFile outPath prog = liftIOAndCatch StageWriteCompiled $ do
  B.writeFile outPath $ export prog
  setFileMode outPath exportFileMode

-- | Serialize the program's data into the output path, for testing.
exportRaw :: FilePath -> Program () () () -> SessionRes ()
exportRaw outPath = liftIOAndCatch StageWriteCompiled . B.writeFile outPath . serialize
