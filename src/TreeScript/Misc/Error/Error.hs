{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data types for errors.
module TreeScript.Misc.Error.Error
  ( Stage(..)
  , Error(..)
  , mkOverlapInOutError
  , exceptionToError
  , prependMsgToErr
  , addRangeToErr
  )
where

import           TreeScript.Misc.Loc
import           TreeScript.Misc.Print

import           Control.Monad.Catch
import qualified Data.Text                     as T

-- | A step in compiling.
data Stage
  = StagePluginLoad
  | StageLex
  | StageParse
  | StageDesugar
  | StageValidate
  | StageType
  | StageCompile
  | StageEval
  | StageStartLib
  | StageReadInput
  | StageEvalStx
  | StageWriteOutput
  | StageShutdown
  deriving (Eq, Ord, Read, Show)

-- | An error which occurs while compiling a program. Fatal and nonfatal errors share this type.
data Error
  = Error
  { errorStage :: Stage -- ^ Compile stage the error occurred.
  , errorRange :: Range -- ^ Where the error occurred. A singleton range if it occurred at a location.
  , errorMsg :: T.Text -- ^ Text displayed to the user.
  } deriving (Eq, Ord, Read, Show)

instance Printable Stage where
  pprint StagePluginLoad  = "loading plugins"
  pprint StageLex         = "lexing"
  pprint StageParse       = "parsing"
  pprint StageDesugar     = "desugaring"
  pprint StageValidate    = "validating"
  pprint StageType        = "type checking / casting"
  pprint StageEval        = "evaluating"
  pprint StageCompile     = "compiling"
  pprint StageStartLib    = "starting librares"
  pprint StageReadInput   = "reading input"
  pprint StageEvalStx     = "evaluating syntax"
  pprint StageWriteOutput = "writing output"
  pprint StageShutdown    = "shutting down"

instance Printable Error where
  pprint (Error stage _ msg) = "while " <> pprint stage <> " - " <> msg

-- | Creates an error which occurs when trying to perform an operation which would overwrite its input.
mkOverlapInOutError :: Stage -> Error
mkOverlapInOutError stage = Error
  { errorStage = stage
  , errorRange = r0
  , errorMsg   =
    "input and output are the same, so output would overwrite - will not perform this operation"
  }

-- | Converts the exception into an error.
exceptionToError :: Stage -> SomeException -> Error
exceptionToError stage exc =
  Error { errorStage = stage, errorRange = r0, errorMsg = pprint exc }

-- | Prepends to the error message.
prependMsgToErr :: T.Text -> Error -> Error
prependMsgToErr new (Error stage rng msg) =
  Error { errorStage = stage, errorRange = rng, errorMsg = new <> " - " <> msg }

-- | Denotes that the error occurred in the given range. Changes its description. Fails if the error has a range.
addRangeToErr :: Range -> Error -> Error
addRangeToErr rng err@(Error stage rng' msg)
  | rng' == r0
  = Error { errorStage = stage
          , errorRange = rng
          , errorMsg   = "at " <> pprint rng <> " - " <> msg
          }
  | otherwise
  = error
    $  "tried to add range to error which already has one ("
    ++ T.unpack (pprint err)
    ++ ")"
