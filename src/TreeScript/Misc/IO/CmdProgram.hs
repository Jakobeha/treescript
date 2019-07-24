{-# LANGUAGE FlexibleContexts #-}
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
import           TreeScript.Misc.Ext

import           Control.Monad
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
  { cmdProgramPath :: FilePath
  , cmdProgramEnv :: [(String, String)]
  } deriving (Read, Show)

-- | Runs the command-line program, passing the text to stdin, and returning stdout.
runCmdProgramArgs
  :: (MonadIOEResult m) => CmdProgram -> [T.Text] -> T.Text -> m T.Text
runCmdProgramArgs (CmdProgram ppath progEnv) args inp = do
  parentEnv <- liftIOCatch getEnvironment
  let pproc = (proc ppath $ map T.unpack args) { env     = Just
                                                 $  parentEnv
                                                 ++ progEnv
                                               , std_in  = CreatePipe
                                               , std_out = CreatePipe
                                               , std_err = CreatePipe
                                               }
  (Just pin, Just pout, Just perr, phandle) <- liftIOCatch $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftIOCatch $ hSetEncoding pout latin1
  liftIOCatch $ hSetEncoding perr latin1
  liftIOCatch $ T.hPutStr pin inp
  liftIOCatch $ hClose pin
  err      <- liftIOCatch $ redirectExceptLast perr
  exitCode <- liftIOCatch $ waitForProcess phandle
  case exitCode of
    ExitSuccess -> do
      unless (T.null $ T.strip err) $ liftIOCatch $ T.putStrLn err
      liftIOCatch $ T.hGetContents pout
    ExitFailure code
      | T.null $ T.strip err
      -> mkFail $ Error Nothing $ "unknown error (code " <> T.pack (show code) <> ")"
      | otherwise
      -> mkFail $ Error Nothing $ T.strip err

-- | Runs the command-line program, passing the file to stdin, and returning a stream from stdout.
runCmdProgramStreamArgs
  :: (MonadIOEResult m)
  => CmdProgram
  -> [T.Text]
  -> S.OutputStream B.ByteString
  -> m (S.OutputStream B.ByteString)
runCmdProgramStreamArgs (CmdProgram ppath progEnv) args out = do
  parentEnv <- liftIOCatch getEnvironment
  let pproc = (proc ppath $ map T.unpack args) { env     = Just
                                                 $  parentEnv
                                                 ++ progEnv
                                               , std_in  = CreatePipe
                                               , std_out = CreatePipe
                                               , std_err = Inherit
                                               }
  -- TODO: Use handle for result output stream
  (Just pin, Just pout, Nothing, _) <- liftIOCatch $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftIOCatch $ hSetEncoding pin latin1
  liftIOCatch $ hSetEncoding pout latin1
  sout <-
    liftIOCatch
    $   S.lockingInputStream
    =<< S.atEndOfInput (hClose pout)
    =<< S.handleToInputStream pout
  let finish = do
        hClose pin
        S.connect sout out
  liftIOCatch
    $   S.lockingOutputStream
    =<< S.atEndOfOutput finish
    =<< S.handleToOutputStream pin

-- | Runs the command-line program, passing the file to stdin, and returning a stream from stdout.
runCmdProgramFileArgs
  :: (MonadIOEResult m)
  => CmdProgram
  -> [T.Text]
  -> FilePath
  -> m (EResultInputStream T.Text)
runCmdProgramFileArgs (CmdProgram ppath progEnv) args inp = do
  parentEnv <- liftIOCatch getEnvironment
  pin       <- liftIOCatch $ openFile inp ReadMode
  let pproc = (proc ppath $ map T.unpack args) { env     = Just
                                                 $  parentEnv
                                                 ++ progEnv
                                               , std_in  = UseHandle pin
                                               , std_out = CreatePipe
                                               , std_err = Inherit
                                               }
  (Nothing, Just pout, Nothing, phandle) <- liftIOCatch $ createProcess pproc
  -- Not sure why but not setting these encodings sometimes causes an "invalid byte sequence" error
  liftIOCatch $ hSetEncoding pout latin1
  out <- liftIOCatch $ S.decodeUtf8 =<< S.handleToInputStream pout
  processInputStream out phandle

-- | Runs the command-line program without arguments.
runCmdProgram :: (MonadIOEResult m) => CmdProgram -> T.Text -> m T.Text
runCmdProgram prog = runCmdProgramArgs prog []

-- | Runs the command-line program, passing the output to stdin, and returning a stream from stdout.
runCmdProgramStream
  :: (MonadIOEResult m)
  => CmdProgram
  -> S.OutputStream B.ByteString
  -> m (S.OutputStream B.ByteString)
runCmdProgramStream prog = runCmdProgramStreamArgs prog []

-- | Runs the command-line program, passing the file to stdin, and returning a stream from stdout.
runCmdProgramFile
  :: (MonadIOEResult m)
  => CmdProgram
  -> FilePath
  -> m (EResultInputStream T.Text)
runCmdProgramFile prog = runCmdProgramFileArgs prog []
