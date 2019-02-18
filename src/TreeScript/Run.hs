{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Run
  ( run
  ) where

import qualified TreeScript.Ast.Flat as F
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import System.FilePath

-- | Parse the input code, run the TreeScript executable at the given path, print its output, and return the result.
runCode :: FilePath -> Code -> SessionRes Code
runCode execPath (Code inExt inText) = do
  inLang <- langWithExt StageReadArgs inExt
  let inParser = languageParser inLang
  inAstData <- runCmdProgram inParser inText
  let execProg
        = CmdProgram
        { cmdProgramStage = StageRunning
        , cmdProgramPath = execPath
        }
  outAstData <- runCmdProgramArgs execProg ["--stdin", "--stdout"] inAstData
  outLang <- F.langFromAstData outAstData
  let outExt = langSpecExtension $ languageSpec outLang
      outPrinter = languagePrinter outLang
  outText <- runCmdProgram outPrinter outAstData
  pure Code
    { codeLangExt = outExt
    , codeContent = outText
    }

-- | Run the treescript executable with the given arguments.
run :: FilePath -> [T.Text] -> SessionRes ()
run execPath args = () <$ runCmdProgramArgs execProg args T.empty
  where execProg = CmdProgram
          { cmdProgramStage = StageRunning
          , cmdProgramPath = execPath
          }
