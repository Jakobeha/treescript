{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data types for errors.
module TreeScript.Misc.Error
  ( Stage (..)
  , Error (..)
  , Result (..)
  , ResultT (..)
  , MonadResult (..)
  , mkOverlapInOutError
  , exceptionToError
  , prependMsgToErr
  , addRangeToErr
  , isSuccess
  , forceSuccess
  , traverseDropFatals
  , mapResultT
  , catchExceptionToError
  , liftIOAndCatch
  ) where

import TreeScript.Misc.Ext
import qualified TreeScript.Misc.Ext.Text as T
import TreeScript.Misc.Loc
import TreeScript.Misc.Print

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Text as T
import Control.Monad.Logger

-- | A step in compiling.
data Stage
  = StageReadArgs
  | StageReadInput
  | StagePluginLoad
  | StagePluginUse
  | StageLexing
  | StageParsing
  | StageExtracting
  | StageCompiling
  | StageRunning
  | StageWriteOutput
  deriving (Eq, Ord, Read, Show)

-- | An error which occurs while compiling a program. Fatal and nonfatal errors share this type.
data Error
  = Error
  { errorStage :: Stage -- ^ Compile stage the error occurred.
  , errorRange :: Maybe Range -- ^ Where the error occurred. A singleton range if it occurred at a location.
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
  -- | Transforms all errors in the result.
  overErrors :: (Error -> Error) -> m a -> m a
  -- | Converts fatal results into nonfatal 'Nothing' results, and the rest into 'Just' results.
  downgradeFatal :: m a -> m (Maybe a)

instance Printable Stage where
  pprint StageReadArgs = "reading arguments"
  pprint StageReadInput = "reading input"
  pprint StagePluginLoad = "loading plugins"
  pprint StagePluginUse = "using a plugin"
  pprint StageLexing = "lexing"
  pprint StageParsing = "parsing"
  pprint StageExtracting = "extracting"
  pprint StageCompiling = "compiling"
  pprint StageRunning = "running"
  pprint StageWriteOutput = "writing output"

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

  overErrors f (ResultFail err) = ResultFail $ f err
  overErrors f (Result errs x) = Result (map f errs) x

  downgradeFatal (ResultFail err) = Result [err] Nothing
  downgradeFatal (Result errs x) = Result errs $ Just x

instance (Applicative u) => MonadResult (ResultT u) where
  mkFail = ResultT . pure . mkFail
  tellErrors = ResultT . pure . tellErrors
  overErrors f (ResultT x) = ResultT $ overErrors f <$> x
  downgradeFatal (ResultT x) = ResultT $ downgradeFatal <$> x

instance (Monad u, MonadResult u) => MonadResult (ReaderT r u) where
  mkFail = lift . mkFail
  tellErrors = lift . tellErrors
  overErrors = mapReaderT . overErrors
  downgradeFatal x = ReaderT $ downgradeFatal . runReaderT x

instance (Monad u, MonadResult u) => MonadResult (StateT s u) where
  mkFail = lift . mkFail
  tellErrors = lift . tellErrors
  overErrors = mapStateT . overErrors
  downgradeFatal x = StateT run
    where run s = fmap fillState $ downgradeFatal $ runStateT x s
            where fillState Nothing = (Nothing, s)
                  fillState (Just (res, s')) = (Just res, s')

instance (MonadReader r u) => MonadReader r (ResultT u) where
  ask = ResultT $ pure <$> ask
  local f (ResultT x) = ResultT $ local f x

instance (MonadLogger u) => MonadLogger (ResultT u) where
  monadLoggerLog loc src lvl = ResultT . fmap pure . monadLoggerLog loc src lvl

instance (MonadIO u) => MonadIO (ResultT u) where
  liftIO = ResultT . fmap pure . liftIO

instance (MonadLoggerIO u) => MonadLoggerIO (ResultT u) where
  askLoggerIO = ResultT $ pure <$> askLoggerIO

instance (MonadThrow u) => MonadThrow (ResultT u) where
  throwM = ResultT . fmap pure . throwM

instance (MonadCatch u) => MonadCatch (ResultT u) where
  ResultT x `catch` f = ResultT $ x `catch` f'
    where f' = runResultT . f

-- | Creates an error which occurs when trying to perform an operation which would overwrite its input.
mkOverlapInOutError :: Stage -> Error
mkOverlapInOutError stage
  = Error
  { errorStage = stage
  , errorRange = Nothing
  , errorMsg = "input and output are the same, so output would overwrite - will not perform this operation"
  }

-- | Converts the exception into an error.
exceptionToError :: Stage -> SomeException -> Error
exceptionToError stage exc
  = Error
  { errorStage = stage
  , errorRange = Nothing
  , errorMsg = pprint exc
  }

-- | Prepends to the error message.
prependMsgToErr :: T.Text -> Error -> Error
prependMsgToErr new (Error stage rng msg)
  = Error
  { errorStage = stage
  , errorRange = rng
  , errorMsg = new <> " - " <> msg
  }

-- | Denotes that the error occurred in the given range. Changes its description. Fails if the error has a range.
addRangeToErr :: Range -> Error -> Error
addRangeToErr rng (Error stage Nothing msg)
  = Error
  { errorStage = stage
  , errorRange = Just rng
  , errorMsg = "at " <> pprint rng <> " - " <> msg
  }
addRangeToErr _ err@(Error _ (Just _) _)
  = error $ "tried to add range to error which already has one (" ++ T.unpack (pprint err) ++ ")"

-- | Is the result a success?
isSuccess :: Result a -> Bool
isSuccess (ResultFail _) = False
isSuccess (Result errs _) = null errs

-- | Raises an error if the result has any errors.
forceSuccess :: Result a -> a
forceSuccess (ResultFail err) = error $ "unexpected fatal error:\n" <> T.unpack (pprint err)
forceSuccess (Result errs x)
  | null errs = x
  | otherwise = error $ "unexpected nonfatal errors:\n" <> T.unpack (T.unlines $ map pprint errs)

-- | Like 'traverse', but when an element raises a fatal error, instead of completely failing, the element is removed and the error becomes nonfatal.
traverseDropFatals :: (Applicative w, MonadResult w) => (a -> w b) -> [a] -> w [b]
traverseDropFatals f
  = fmap catMaybes . traverse (downgradeFatal . f)

-- | Transforms the underlying monad in a 'ResultT'.
mapResultT :: (u1 (Result a) -> u2 (Result b)) -> ResultT u1 a -> ResultT u2 b
mapResultT f (ResultT x) = ResultT $ f x

-- | If an exception is thrown, will catch it and convert it into a fatal error with the given stage.
catchExceptionToError :: (MonadCatch w, MonadResult w) => Stage -> w a -> w a
catchExceptionToError stage x
  = x `catch` (mkFail . exceptionToError stage)

-- | Lift the I/O action into a 'ResultT' /and/ catch exceptions.
liftIOAndCatch :: (MonadIO w, MonadCatch w, MonadResult w) => Stage -> IO a -> w a
liftIOAndCatch stage = catchExceptionToError stage . liftIO
