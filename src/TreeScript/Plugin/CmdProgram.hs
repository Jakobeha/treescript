{-# LANGUAGE OverloadedStrings #-}

-- | Allows the TreeScript compiler to use external command-line programs.
module TreeScript.Plugin.CmdProgram
  ( CmdProgram (..)
  , runCmdProgramArgs
  , runCmdProgram
  ) where

import TreeScript.Misc

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Exit
import System.IO
import System.Process

-- | Represents an external command-line program.
data CmdProgram
  = CmdProgram
  { cmdProgramStage :: Stage
  , cmdProgramPath :: FilePath
  , cmdProgramEnv :: [(String, String)]
  } deriving (Read, Show)

convertErr :: Stage -> T.Text -> Error
convertErr stage err
  = Error
    { errorStage = stage
    , errorRange = Nothing
    , errorMsg = err
    }

-- | Runs the command-line program, passing the text to stdin, and returning stdout.
runCmdProgramArgs :: (MonadIO m, MonadCatch m, MonadFail m, MonadResult m) => CmdProgram -> [T.Text] -> T.Text -> m T.Text
runCmdProgramArgs (CmdProgram stage ppath progEnv) args inp = do
  let liftCmdIO = liftIOAndCatch stage
  parentEnv <- liftCmdIO getEnvironment
  let pproc
        = (proc ppath $ map T.unpack args)
        { env = Just $ parentEnv ++ progEnv
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (Just pin, Just pout, Just perr, phandle) <- liftCmdIO $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftCmdIO $ hSetEncoding pout latin1
  liftCmdIO $ hSetEncoding perr latin1
  liftCmdIO $ T.hPutStr pin inp
  liftCmdIO $ hClose pin
  err <- liftCmdIO $ redirectExceptLast perr
  exitCode <- liftCmdIO $ waitForProcess phandle
  case exitCode of
    ExitSuccess -> do
      unless (T.null $ T.strip err) $
        liftCmdIO $ T.putStrLn err
      liftCmdIO $ T.hGetContents pout
    ExitFailure code
      | T.null $ T.strip err ->
          mkFail $ convertErr stage $ "unknown error (code " <> pprint code <> ")"
      | otherwise ->
          mkFail $ convertErr stage $ T.strip err

-- | Runs the command-line program without arguments.
runCmdProgram :: (MonadIO m, MonadCatch m, MonadFail m, MonadResult m) => CmdProgram -> T.Text -> m T.Text
runCmdProgram prog = runCmdProgramArgs prog []
