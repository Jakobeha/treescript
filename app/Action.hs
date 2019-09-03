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
  | ActionCompile CompileAction
  | ActionEval EvalAction

data CompileAction
  = CompileAction
  { compileActionInput :: FilePath
  , compileActionOutput :: FilePath
  , compileActionWatch :: Bool
  }

data EvalAction
  = EvalAction
  { evalActionProgram :: FilePath
  , evalActionInput :: FilePath
  , evalActionOutput :: FilePath
  , evalActionWatch :: Bool
  }

mkCompileAction :: FilePath -> Maybe FilePath -> Bool -> CompileAction
mkCompileAction input output watch = CompileAction
  { compileActionInput  = input
  , compileActionOutput = defaultOut `fromMaybe` output
  , compileActionWatch  = watch
  }
  where defaultOut = input -<.> "tprg"

mkEvalAction :: FilePath -> FilePath -> Maybe FilePath -> Bool -> EvalAction
mkEvalAction prg input output watch = EvalAction
  { evalActionProgram = prg
  , evalActionInput   = input
  , evalActionOutput  = input `fromMaybe` output
  , evalActionWatch   = watch
  }
