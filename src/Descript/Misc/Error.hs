{-# LANGUAGE DeriveFunctor #-}

-- | Data types for errors.
module Descript.Misc.Error
  ( Stage (..)
  , Error (..)
  , Result (..)
  , eitherToResult
  ) where

import qualified Data.Text as T

-- | A step in compiling.
data Stage
  = StageLexing
  | StageParsing

-- | An error which can occur while compiling a program.
data Error
  = Error
  { errorStage :: Stage
  , errorMsg :: T.Text
  }

-- | A value which can contain errors.
data Result a
  = ResultFail Error
  | ResultSuccess a
  deriving (Functor)

instance Applicative Result where
  pure = ResultSuccess
  ResultFail err <*> _ = ResultFail err
  _ <*> ResultFail err = ResultFail err
  ResultSuccess f <*> ResultSuccess x = ResultSuccess $ f x

instance Monad Result where
  return = pure
  ResultFail err >>= _ = ResultFail err
  ResultSuccess x >>= f = f x

-- | Converts a simple 'Either' into a 'Result'. This makes its meaning more explicit.
eitherToResult :: Stage -> Either T.Text a -> Result a
eitherToResult stage (Left msg)
  = ResultFail Error
  { errorStage = stage
  , errorMsg = msg
  }
eitherToResult _ (Right res) = ResultSuccess res
