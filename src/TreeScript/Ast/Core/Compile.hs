{-# LANGUAGE OverloadedStrings #-}

-- | Serialize compiled code into a file.
module TreeScript.Ast.Core.Compile
  ( export
  , exportFile
  , exportRaw
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import System.Posix.Files
import System.Posix.Types

-- | Serialize the module, and add a shebang which makes it run as an executable.
export :: Module () () -> B.ByteString
export mdl = "#! /usr/bin/env treescript-interpreter\n" <> A.encode mdl

exportFileMode :: FileMode
exportFileMode = CMode 0o755 -- Everyone can read and execute, owner can write

-- | Serialize the module as an executable into the output path.
exportFile :: FilePath -> Module () () -> SessionRes ()
exportFile outPath mdl = liftIOAndCatch StageWriteCompiled $ do
  B.writeFile outPath $ export mdl
  setFileMode outPath exportFileMode

-- | Serialize the module's data into the output path, for testing.
exportRaw :: FilePath -> Module () () -> SessionRes ()
exportRaw outPath = liftIOAndCatch StageWriteCompiled . B.writeFile outPath . A.encode
