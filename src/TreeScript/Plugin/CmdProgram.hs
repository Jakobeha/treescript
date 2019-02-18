{-# LANGUAGE OverloadedStrings #-}

-- | Allows the TreeScript compiler to use external command-line programs.
module TreeScript.Plugin.CmdProgram
  ( CmdProgram (..)
  , runCmdProgramArgs
  , runCmdProgram
  ) where

import TreeScript.Misc

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit
import System.IO
import System.Process

-- | Represents an external command-line program.
data CmdProgram
  = CmdProgram
  { cmdProgramStage :: Stage
  , cmdProgramPath :: FilePath
  } deriving (Read, Show)

convertErr :: Stage -> T.Text -> Error
convertErr stage err
  = Error
    { errorStage = stage
    , errorRange = Nothing
    , errorMsg = err
    }

-- | Runs the command-line program, passing the text to stdin, and returning stdout.
runCmdProgramArgs :: (MonadIO m, MonadCatch m, MonadResult m) => CmdProgram -> [T.Text] -> T.Text -> m T.Text
runCmdProgramArgs (CmdProgram stage ppath) args inp = do
  let liftCmdIO = liftIOAndCatch stage
      pproc
        = (proc ppath $ map T.unpack args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (Just pin, Just pout, Just perr, phandle) <- liftCmdIO $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftCmdIO $ hSetEncoding pout latin1
  liftCmdIO $ hSetEncoding perr latin1
  liftCmdIO $ T.hPutStr pin inp
  liftCmdIO $ hClose pin
  exitCode <- liftCmdIO $ waitForProcess phandle
  out <- liftCmdIO $ T.hGetContents pout
  err <- liftCmdIO $ T.hGetContents perr
  case exitCode of
    ExitSuccess -> do
      let errs = filter (not . T.null . T.strip) $ T.lines err
      tellErrors $ map (convertErr stage) errs
      pure out
    ExitFailure code
      | T.null $ T.strip err ->
        mkFail $ convertErr stage $ "unknown error (code " <> pprint code <> ")"
      | otherwise -> do
        mkFail $ convertErr stage $ T.strip err

-- | Runs the command-line program without arguments.
runCmdProgram :: (MonadIO m, MonadCatch m, MonadResult m) => CmdProgram -> T.Text -> m T.Text
runCmdProgram prog = runCmdProgramArgs prog []
