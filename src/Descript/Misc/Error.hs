{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data types for errors.
module Descript.Misc.Error
  ( Stage (..)
  , Error (..)
  , Result (..)
  , ResultT (..)
  , MonadResult (..)
  , isSuccess
  , traverseDropFatals
  ) where

import Descript.Misc.Ext
import qualified Descript.Misc.Ext.Text as T
import Descript.Misc.Loc
import Descript.Misc.Print

import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Text as T

-- | A step in compiling.
data Stage
  = StageLexing
  | StageParsing
  | StageExtracting

-- | An error which can occur while compiling a program. Fatal and nonfatal errors share this type.
data Error
  = Error
  { errorStage :: Stage -- ^ Compile stage the error occurred.
  , errorRange :: Range -- ^ Where the error occurred. A singleton range if it occurred at a location.
  , errorMsg :: T.Text -- ^ Text displayed to the user.
  }

-- | A value which can fail to be created because of a fatal error, or it can be created but have nonfatal errors.
data Result a
  = ResultFail Error -- ^ Failed to get a result because of a fatal error.
  | Result [Error] a -- ^ Got a result but maybe some errors.
  deriving (Functor)

-- | 'Result' monad transformer.
newtype ResultT u a = ResultT{ runResultT :: u (Result a) } deriving (Functor)

-- | 'Result' monad class.
class MonadResult m where
  -- | Failed to get a result because of a fatal error.
  mkFail :: Error -> m a
  -- | Raise errors but don't stop computing the result.
  tellErrors :: [Error] -> m ()
  -- | Raise an error but don't stop computing the result.
  tellError :: Error -> m ()
  tellError err = tellErrors [err]
  -- | Converts fatal results into nonfatal 'Nothing' results, and the rest into 'Just' results.
  downgradeFatal :: m a -> m (Maybe a)

instance Printable Stage where
  pprint StageLexing = "lexing"
  pprint StageParsing = "parsing"
  pprint StageExtracting = "extracting"

instance Printable Error where
  pprint (Error stage _ msg) = "while " <> pprint stage <> " - " <> msg

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

instance (Applicative u) => Applicative (ResultT u) where
  pure = ResultT . pure2
  ResultT f <*> ResultT x = ResultT $ f <<*>> x

instance Monad Result where
  return = pure
  ResultFail err >>= _ = ResultFail err
  Result xErrs x >>= f
    = case f x of
        ResultFail err -> ResultFail err
        Result errs y -> Result (xErrs ++ errs) y

instance (Monad u) => Monad (ResultT u) where
  return = pure
  ResultT x >>= f = ResultT $ x >>= f'
    where f' (ResultFail err) = pure $ ResultFail err
          f' (Result errs x') = prependErrs errs <$> runResultT (f x')
          prependErrs _ (ResultFail err) = ResultFail err
          prependErrs xErrs (Result yErrs y) = Result (xErrs ++ yErrs) y

instance MonadResult Result where
  mkFail = ResultFail
  tellErrors errs = Result errs ()
  downgradeFatal (ResultFail err) = Result [err] Nothing
  downgradeFatal (Result errs x) = Result errs $ Just x

instance (Applicative u) => MonadResult (ResultT u) where
  mkFail = ResultT . pure . mkFail
  tellErrors = ResultT . pure . tellErrors
  downgradeFatal (ResultT x) = ResultT $ downgradeFatal <$> x

instance (Monad u, MonadResult u) => MonadResult (StateT s u) where
  mkFail = lift . mkFail
  tellErrors = lift . tellErrors
  downgradeFatal x = StateT run
    where run s = fmap fillState $ downgradeFatal $ runStateT x s
            where fillState Nothing = (Nothing, s)
                  fillState (Just (res, s')) = (Just res, s')

-- | Is the result a success?
isSuccess :: Result a -> Bool
isSuccess (ResultFail _) = False
isSuccess (Result errs _) = null errs

-- | Like 'traverse', but when an element raises a fatal error, instead of completely failing, the element is removed and the error becomes nonfatal.
traverseDropFatals :: (Applicative w, MonadResult w) => (a -> w b) -> [a] -> w [b]
traverseDropFatals f
  = fmap catMaybes . traverse (downgradeFatal . f)
