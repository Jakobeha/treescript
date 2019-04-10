{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Run
  ( runExec
  , run
  ) where

import TreeScript.Compile
import TreeScript.Misc
import TreeScript.Plugin

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Directory
import System.IO.Temp

-- | Run the treescript executable with the given arguments.
runExec :: FilePath -> [T.Text] -> SessionRes ()
runExec execPath args = () <$ runCmdProgramArgs execProg args T.empty
  where execProg = CmdProgram
          { cmdProgramStage = StageRunning
          , cmdProgramPath = execPath
          , cmdProgramEnv = []
          }

-- | Interpret the treescript source - compile it and run with the given arguments.
run :: FilePath -> [T.Text] -> SessionRes ()
run srcPath args = do
  let liftRunIO = liftIOAndCatch StageRunning
  execPath <- liftRunIO $ emptySystemTempFile "treescript"
  compile srcPath execPath
  runExec execPath args
  liftIO $ removeFile execPath `catch` ignoreException
