{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | Allows the TreeScript compiler to use external command-line programs.
module TreeScript.Misc.IO.CmdProgram
  ( CmdProgram(..)
  , runCmdProgramArgs
  , runCmdProgramStreamArgs
  , runCmdProgramFileArgs
  , runCmdProgram
  , runCmdProgramStream
  , runCmdProgramFile
  )
where

import           TreeScript.Misc.Error

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Environment
import           System.Exit
import           System.IO
import qualified System.IO.Streams             as S
import           System.Process

-- | Represents an external command-line program.
data CmdProgram
  = CmdProgram
  { cmdProgramStage :: Stage
  , cmdProgramPath :: FilePath
  , cmdProgramEnv :: [(String, String)]
  } deriving (Read, Show)

convertErr :: Stage -> T.Text -> Error
convertErr stage err =
  Error { errorStage = stage, errorRange = r0, errorMsg = err }

-- | Runs the command-line program, passing the text to stdin, and returning stdout.
runCmdProgramArgs
  :: (MonadResult m) => CmdProgram -> [T.Text] -> T.Text -> m T.Text
runCmdProgramArgs (CmdProgram stage ppath progEnv) args inp = do
  let liftCmdIO = liftIOAndCatch stage
  parentEnv <- liftCmdIO getEnvironment
  let pproc = (proc ppath $ map T.unpack args) { env     = Just
                                                 $  parentEnv
                                                 ++ progEnv
                                               , std_in  = CreatePipe
                                               , std_out = CreatePipe
                                               , std_err = CreatePipe
                                               }
  (Just pin, Just pout, Just perr, phandle) <- liftCmdIO $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftCmdIO $ hSetEncoding pout latin1
  liftCmdIO $ hSetEncoding perr latin1
  liftCmdIO $ T.hPutStr pin inp
  liftCmdIO $ hClose pin
  err      <- liftCmdIO $ redirectExceptLast perr
  exitCode <- liftCmdIO $ waitForProcess phandle
  case exitCode of
    ExitSuccess -> do
      unless (T.null $ T.strip err) $ liftCmdIO $ T.putStrLn err
      liftCmdIO $ T.hGetContents pout
    ExitFailure code
      | T.null $ T.strip err
      -> mkFail
        $  convertErr stage
        $  "unknown error (code "
        <> pprint code
        <> ")"
      | otherwise
      -> mkFail $ convertErr stage $ T.strip err

-- | Runs the command-line program, passing the file to stdin, and returning a stream from stdout.
runCmdProgramStreamArgs
  :: (MonadResult m)
  => CmdProgram
  -> [T.Text]
  -> S.OutputStream B.ByteString
  -> m (S.OutputStream B.ByteString)
runCmdProgramStreamArgs (CmdProgram stage ppath progEnv) args out = do
  let liftCmdIO = liftIOAndCatch stage
  parentEnv <- liftCmdIO getEnvironment
  let pproc = (proc ppath $ map T.unpack args) { env     = Just
                                                 $  parentEnv
                                                 ++ progEnv
                                               , std_in  = CreatePipe
                                               , std_out = CreatePipe
                                               , std_err = Inherit
                                               }
  -- TODO: Use handle for result output stream
  (Just pin, Just pout, Nothing, _) <- liftCmdIO $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftCmdIO $ hSetEncoding pin latin1
  liftCmdIO $ hSetEncoding pout latin1
  sout <-
    liftCmdIO
    $   S.lockingInputStream
    =<< S.atEndOfInput (hClose pout)
    =<< S.handleToInputStream pout
  let finish = do
        hClose pin
        S.connect sout out
  liftCmdIO
    $   S.lockingOutputStream
    =<< S.atEndOfOutput finish
    =<< S.handleToOutputStream pin

-- | Runs the command-line program, passing the file to stdin, and returning a stream from stdout.
runCmdProgramFileArgs
  :: (MonadResult m)
  => CmdProgram
  -> [T.Text]
  -> FilePath
  -> m (ResultInputStream T.Text)
runCmdProgramFileArgs (CmdProgram stage ppath progEnv) args inp = do
  let liftCmdIO = liftIOAndCatch stage
  parentEnv <- liftCmdIO getEnvironment
  pin       <- liftCmdIO $ openFile inp ReadMode
  let pproc = (proc ppath $ map T.unpack args) { env     = Just
                                                 $  parentEnv
                                                 ++ progEnv
                                               , std_in  = UseHandle pin
                                               , std_out = CreatePipe
                                               , std_err = Inherit
                                               }
  (Nothing, Just pout, Nothing, phandle) <- liftCmdIO $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftCmdIO $ hSetEncoding pout latin1
  out <- liftCmdIO $ S.decodeUtf8 =<< S.handleToInputStream pout
  processInputStream out phandle

-- | Runs the command-line program without arguments.
runCmdProgram :: (MonadResult m) => CmdProgram -> T.Text -> m T.Text
runCmdProgram prog = runCmdProgramArgs prog []

-- | Runs the command-line program, passing the output to stdin, and returning a stream from stdout.
runCmdProgramStream
  :: (MonadResult m)
  => CmdProgram
  -> S.OutputStream B.ByteString
  -> m (S.OutputStream B.ByteString)
runCmdProgramStream prog = runCmdProgramStreamArgs prog []

-- | Runs the command-line program, passing the file to stdin, and returning a stream from stdout.
runCmdProgramFile
  :: (MonadResult m) => CmdProgram -> FilePath -> m (ResultInputStream T.Text)
runCmdProgramFile prog = runCmdProgramFileArgs prog []
