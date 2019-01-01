{-# LANGUAGE OverloadedStrings #-}

-- | Allows the Descript compiler to use external command-line programs.
module Descript.Plugin.CmdProgram
  ( CmdProgram (..)
  , runCmdProgram
  ) where

import Descript.Misc

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit
import System.IO
import System.Process

-- | Represents an external command-line program.
newtype CmdProgram = CmdProgram{ cmdProgramPath :: FilePath } deriving (Read, Show)

convertErr :: T.Text -> Error
convertErr err
  = Error
    { errorStage = StagePluginUse
    , errorRange = Nothing
    , errorMsg = err
    }

-- | Runs the command-line program, passing the text to stdin, and returning stdout.
runCmdProgram :: (MonadIO m, MonadResult m) => CmdProgram -> T.Text -> m T.Text
runCmdProgram (CmdProgram ppath) inp = do
  let pproc
        = (proc ppath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (Just pin, Just pout, Just perr, phandle) <- liftIO $ createProcess pproc
  liftIO $ T.hPutStr pin inp
  liftIO $ hClose pin
  exitCode <- liftIO $ waitForProcess phandle
  out <- liftIO $ T.hGetContents pout
  errs <- map convertErr . T.lines <$> liftIO (T.hGetContents perr)
  case exitCode of
    ExitSuccess -> do
      tellErrors errs
      pure out
    ExitFailure code ->
      case errs of
        [] -> mkFail $ convertErr $ "unknown error (code " <> pprint code <> ")"
        _ -> do
          tellErrors $ init errs
          mkFail $ last errs
