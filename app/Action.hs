{-# LANGUAGE DeriveFunctor #-}

-- | Defines different actions and their arguments.
module Action
  ( module Action
  ) where

import Data.Maybe
import qualified Data.Text as T
import System.FilePath

data Action
  = ActionServe
  | ActionCompile Compile
  | ActionRun Run

data Compile
  = Compile
  { compileInput :: FilePath
  , compileOutput :: FilePath
  , compileWatch :: Bool
  }

data Run
  = Run
  { runExec :: FilePath
  , runArgs :: [T.Text]
  }

mkCompile :: FilePath -> Maybe FilePath -> Bool -> Compile
mkCompile input output watch
  = Compile
  { compileInput = input
  , compileOutput = defaultOut `fromMaybe` output
  , compileWatch = watch
  }
  where defaultOut = input -<.> "tprg"
