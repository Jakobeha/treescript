{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data types for errors.
module Descript.Misc.Error
  ( Stage (..)
  , Error (..)
  , Result (..)
  , MonadResult (..)
  , isSuccess
  , eitherToResult
  ) where

import qualified Descript.Misc.Ext.Text as T
import Descript.Misc.Print

import qualified Data.Text as T

-- | A step in compiling.
data Stage
  = StageLexing
  | StageParsing
  | StageExtracting

-- | An error which can occur while compiling a program. Fatal and nonfatal errors share this type.
data Error
  = Error
  { errorStage :: Stage
  , errorMsg :: T.Text
  }

-- | A value which can fail to be created because of a fatal error, or it can be created but have nonfatal errors.
data Result a
  = ResultFail Error -- ^ Failed to get a result because of a fatal error.
  | Result [Error] a -- ^ Got a result but maybe some errors.
  deriving (Functor)

-- | 'Result' monad class.
class MonadResult m where
  -- | Failed to get a result because of a fatal error.
  mkFail :: Error -> m a
  -- | Raise errors but don't stop computing the result.
  tellErrors :: [Error] -> m ()
  -- | Raise an error but don't stop computing the result.
  tellError :: Error -> m ()
  tellError err = tellErrors [err]

instance Printable Stage where
  pprint StageLexing = "lexing"
  pprint StageParsing = "parsing"
  pprint StageExtracting = "extracting"

instance Printable Error where
  pprint (Error stage msg) = "while " <> pprint stage <> " - " <> msg

instance (Printable a) => Printable (Result a) where
  pprint (ResultFail err) = "fatal error: " <> pprint err
  pprint (Result [] res) = "success: " <> pprint res
  pprint (Result errs res)
    = T.unlines
    ( "result:"
    : pprint res
    : "errors:"
    : map (T.bullet . pprint) errs
    )

instance Applicative Result where
  pure = Result []
  ResultFail err <*> _ = ResultFail err
  _ <*> ResultFail err = ResultFail err
  Result fErrs f <*> Result xErrs x = Result (fErrs ++ xErrs) $ f x

instance Monad Result where
  return = pure
  ResultFail err >>= _ = ResultFail err
  Result xErrs x >>= f
    = case f x of
        ResultFail err -> ResultFail err
        Result errs y -> Result (xErrs ++ errs) y

instance MonadResult Result where
  mkFail = ResultFail
  tellErrors errs = Result errs ()

-- | Is the result a success?
isSuccess :: Result a -> Bool
isSuccess (ResultFail _) = False
isSuccess (Result errs _) = null errs

-- | Converts a simple 'Either' into a 'Result'. This makes its meaning more explicit.
eitherToResult :: Stage -> Either T.Text a -> Result a
eitherToResult stage (Left msg)
  = ResultFail Error
  { errorStage = stage
  , errorMsg = msg
  }
eitherToResult _ (Right res) = Result [] res
