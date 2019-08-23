{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Runs actions.
module Run
  ( runAction
  )
where

import           Action
import           TreeScript

import           Control.Monad
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Rainbow
import           System.FilePath
import           System.FSNotify         hiding ( Action )

default (T.Text)

runAction :: Action -> IO ()
runAction ActionServe              = runServe
runAction (ActionCompile compile') = runCompile compile'
runAction (ActionEval    run'    ) = runEval run'

runServe :: IO ()
runServe = fail "TODO implement"

runCompile :: Compile -> IO ()
runCompile (Compile inp out True) =
  runWatching inp $ runCompile $ Compile inp out False
runCompile (Compile inp out False) = fail "TODO implement"

runEval :: Eval -> IO ()
runEval (Eval prg inp out True) =
  runWatching inp $ runEval $ Eval prg inp out False
runEval (Eval prg inp out False) = fail "TODO implement"

runWatching :: FilePath -> IO () -> IO ()
runWatching inp action = withManager $ \mgr -> do
  stop <- watchDir mgr (takeDirectory inp) rerunForEvt runEvt
  action
  T.putStrLn "Watcher started. Press 'enter' to stop watching."
  _ <- getLine
  stop
 where
  runEvt evt
    | path == inp = do
      T.putStrLn "Watcher detected change. Re-running..."
      action
    | otherwise = pure ()
    where path = eventPath evt
  rerunForEvt (Added    _ _ _) = True
  rerunForEvt (Modified _ _ _) = True
  rerunForEvt (Removed  _ _ _) = False
  rerunForEvt (Unknown  _ _ _) = False

{- runSession :: Session () -> IO ()
runSession session = do
  res <- runSessionReal session
  case res of
    ResultFail err -> do
      putChunkLn $ fore red $ chunk "Fatal error:"
      T.putStrLn $ pprint err
    Result errs ()
      | not (null errs) -> do
        putChunkLn $ fore red $ chunk "Errors:"
        forM_ errs $ \err -> T.putStrLn $ T.bullet $ pprint err
      | otherwise -> putChunkLn $ fore green $ chunk "Success." -}
