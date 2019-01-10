{-# LANGUAGE OverloadedStrings #-}

-- | Allows the TreeScript compiler to use external command-line programs.
module TreeScript.Plugin.CmdProgram
  ( CmdProgram (..)
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
runCmdProgram :: (MonadIO m, MonadCatch m, MonadResult m) => CmdProgram -> T.Text -> m T.Text
runCmdProgram (CmdProgram stage ppath) inp = do
  let liftCmdIO = liftIOAndCatch stage
      pproc
        = (proc ppath [])
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
  errs <- map (convertErr stage) . filter (not . T.null . T.strip) . T.lines <$> liftCmdIO (T.hGetContents perr)
  case exitCode of
    ExitSuccess -> do
      tellErrors errs
      pure out
    ExitFailure code
      | null errs ->
        mkFail $ convertErr stage $ "unknown error (code " <> pprint code <> ")"
      | otherwise -> do
        tellErrors $ init errs
        mkFail $ last errs
