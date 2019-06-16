{-# LANGUAGE DeriveFunctor #-}

-- | Defines different actions and their arguments.
module Action
  ( module Action
  )
where

import           Data.Maybe
import           System.FilePath

data Action
  = ActionServe
  | ActionCompile Compile
  | ActionEval Eval

data Compile
  = Compile
  { compileInput :: FilePath
  , compileOutput :: FilePath
  , compileWatch :: Bool
  }

data Eval
  = Eval
  { evalProgram :: FilePath
  , evalInput :: FilePath
  , evalOutput :: FilePath
  , evalWatch :: Bool
  }

mkCompile :: FilePath -> Maybe FilePath -> Bool -> Compile
mkCompile input output watch = Compile
  { compileInput  = input
  , compileOutput = defaultOut `fromMaybe` output
  , compileWatch  = watch
  }
  where defaultOut = input -<.> "tprg"

mkEval :: FilePath -> FilePath -> Maybe FilePath -> Bool -> Eval
mkEval prg input output watch = Eval { evalProgram = prg
                                     , evalInput   = input
                                     , evalOutput  = input `fromMaybe` output
                                     , evalWatch   = watch
                                     }
