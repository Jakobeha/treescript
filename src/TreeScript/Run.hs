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
  outAstData <- runCmdProgram execProg inAstData
  outLang <- F.langFromAstData outAstData
  let outExt = langSpecExtension $ languageSpec outLang
      outPrinter = languagePrinter outLang
  outText <- runCmdProgram outPrinter outAstData
  pure Code
    { codeLangExt = outExt
    , codeContent = outText
    }

-- | Parse the input (second arg), run the TreeScript executable (first arg), and print its output (third path, defaults to input path with output extension).
run :: FilePath -> FilePath -> Maybe FilePath -> SessionRes ()
run exec input optOutput
  | Just input == optOutput = mkFail $ mkOverlapInOutError StageReadArgs
  | otherwise = do
    let (inBase, inExtFull) = splitExtension input
        inExt
          = case inExtFull of
              '.' : inExt' -> T.pack inExt'
              _ -> T.empty -- Will raise error later
    inText <- liftIOAndCatch StageReadInput $ T.readFile input
    Code outExt outText <- runCode exec $ Code inExt inText
    let outExt' = T.unpack outExt
        output = inBase <.> outExt' `fromMaybe` optOutput
    when (input == output) $
      mkFail $ mkOverlapInOutError StageRunning
    liftIOAndCatch StageWriteOutput $ T.writeFile output outText
