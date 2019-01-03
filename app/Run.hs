{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Runs actions.
module Run
  ( run
  ) where

import Action
import Descript
import qualified Descript.Misc.Ext.Text as T

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Rainbow
import System.FilePath
import System.FSNotify hiding (Action)

default (T.Text)

run :: Action -> IO ()
run ActionServe = runServe
run (ActionCompile compile') = runCompile compile'
run (ActionRun run') = runRun run'

runServe :: IO ()
runServe = error "TODO implement"

runCompile :: Compile -> IO ()
runCompile (Compile input output True)
  = runWatching input $ runCompile $ Compile input output False
runCompile (Compile input output False) = runSessionRes $ compile input output

runRun :: Run -> IO ()
runRun (Run exec input output True)
  = runWatching input $ runRun $ Run exec input output False
runRun (Run _ _ _ False) = error "TODO implement"

runWatching :: FilePath -> IO () -> IO ()
runWatching input action = withManager $ \mgr -> do
  stop <- watchDir mgr (takeDirectory input) rerunForEvt runEvt
  action
  T.putStrLn "Watcher started. Press 'enter' to stop watching."
  _ <- getLine
  stop
  where runEvt evt
          | path == input = do
            T.putStrLn "Watcher detected change. Re-running..."
            action
          | otherwise = pure ()
          where path = eventPath evt
        rerunForEvt (Added _ _ _) = True
        rerunForEvt (Modified _ _ _) = True
        rerunForEvt (Removed _ _ _) = False
        rerunForEvt (Unknown _ _ _) = False

runSessionRes :: SessionRes () -> IO ()
runSessionRes session = do
  res <- runSessionResReal session
  case res of
    ResultFail err -> do
      putChunkLn $ fore red $ chunk "Fatal error:"
      T.putStrLn $ pprint err
    Result errs ()
      | not (null errs) -> do
        putChunkLn $ fore red $ chunk "Errors:"
        forM_ errs $ \err ->
          T.putStrLn $ T.bullet $ pprint err
      | otherwise ->
        putChunkLn $ fore green $ chunk "Success."
