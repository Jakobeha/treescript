{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data types for errors.
module TreeScript.Misc.Error.Error
  ( Stage(..)
  , SError(..)
  , Error(..)
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
  = StageSetup
  | StageLex
  | StageBalance
  | StageParse
  | StageEval
  deriving (Eq, Ord, Read, Show)

-- | Error with a stage.
data SError = SError
  { serrorStage :: Stage
  , serror :: Error
  } deriving (Eq, Ord, Read, Show)

-- | An error which occurs while compiling a program. Fatal and nonfatal errors share this type.
data Error
  = Error
  { errorRange :: Maybe Range -- ^ Where the error occurred.
  , errorMsg :: T.Text -- ^ Text displayed to the user.
  } deriving (Eq, Ord, Read, Show)

instance Printable Stage where
  pprint StageSetup   = "setting up"
  pprint StageLex     = "parsing"
  pprint StageBalance = "parsing"
  pprint StageParse   = "parsing"
  pprint StageEval    = "evaluating"

instance Printable Error where
  pprint (Error Nothing    msg) = msg
  pprint (Error (Just rng) msg) = "at " <> pprint rng <> " - " <> msg

instance Printable SError where
  pprint (SError stage err) = "(while " <> pprint stage <> ") " <> pprint err


-- | Converts the exception into an error.
exceptionToError :: SomeException -> Error
exceptionToError exc = Error { errorRange = Nothing, errorMsg = pprint exc }

-- | Prepends to the error message.
prependMsgToErr :: T.Text -> Error -> Error
prependMsgToErr new (Error rng msg) =
  Error { errorRange = rng, errorMsg = new <> " - " <> msg }

-- | Denotes that the error occurred in the given range. Changes its description. Fails if the error has a range.
addRangeToErr :: Range -> Error -> Error
addRangeToErr rng (Error Nothing msg) = Error
  { errorRange = Just rng
  , errorMsg   = "at " <> pprint rng <> " - " <> msg
  }
addRangeToErr _ err@(Error (Just _) _) =
  error
    $  "tried to add range to error which already has one ("
    ++ T.unpack (pprint err)
    ++ ")"
