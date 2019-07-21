{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data types for errors.
module TreeScript.Misc.Error.Result
  ( EResult
  , SResult
  , EResultT
  , SResultT
  , MonadEResult
  , MonadSResult
  , MonadIOEResult
  , Result(..)
  , ResultT(..)
  , MonadResult(..)
  , isSuccess
  , forceSuccess
  , justSuccess
  , failIfNothing
  , traverseDropFatals
  , mapResultT
  , mapErrors
  , mapErrorsT
  , addStage
  , catchExceptionToError
  , liftIOCatch
  )
where

import           TreeScript.Misc.Error.Error
import           TreeScript.Misc.Ext
import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Misc.Print

import           Control.Monad.Catch
import qualified Control.Monad.Fail            as F
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Control.Monad.RWS.Strict
import           Data.Maybe
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Control.Monad.Logger


type EResult = Result Error
type SResult = Result SError
type EResultT = ResultT Error
type SResultT = ResultT SError
type MonadEResult m = MonadResult Error m
type MonadSResult m = MonadResult SError m
type MonadIOEResult m = (MonadCatch m, F.MonadFail m, MonadIO m, MonadEResult m)

-- | A value which can fail to be created because of a fatal error, or it can be created but have nonfatal errors.
data Result e a
  = ResultFail e -- ^ Failed to get a result because of a fatal error.
  | Result (S.Set e) a -- ^ Got a result but maybe some errors.
  deriving (Functor)

-- | 'Result' monad transformer.
newtype ResultT e u a = ResultT{ runResultT :: u (Result e a) } deriving (Functor)

-- | 'Result' monad class. Note that 'Result' error handling is separate from 'IO' error handling to prevent unexpected exception mixing.
class (Ord e, Monad m) => MonadResult e m | m -> e where
  -- | Failed to get a result because of a fatal error.
  mkFail :: e -> m a
  -- | Raise errors but don't stop computing the result.
  tellErrors :: [e] -> m ()
  -- | Raise an error but don't stop computing the result.
  tellError :: e -> m ()
  tellError err = tellErrors [err]
  -- | Transforms all errors in the result.
  overErrors :: (e -> e) -> m a -> m a
  -- | Converts fatal results into nonfatal 'Nothing' results, and the rest into 'Just' results.
  downgradeFatal :: m a -> m (Maybe a)

instance (Ord e, Printable e, Printable a) => Printable (Result e a) where
  pprint (ResultFail err ) = "fatal error: " <> pprint err
  pprint (Result []   res) = "success: " <> pprint res
  pprint (Result errs res) = T.unlines
    ("result:" : pprint res : "errors:" : map (T.bullet . pprint)
                                              (S.toList errs)
    )

instance (Ord e) => Applicative (Result e) where
  pure = Result []
  ResultFail err <*> _              = ResultFail err
  _              <*> ResultFail err = ResultFail err
  Result fErrs f <*> Result xErrs x = Result (fErrs <> xErrs) $ f x

instance (Ord e, Applicative u) => Applicative (ResultT e u) where
  pure = ResultT . pure2
  ResultT f <*> ResultT x = ResultT $ f <<*>> x

instance (Ord e) => Monad (Result e) where
  return = pure
  ResultFail err >>= _ = ResultFail err
  Result xErrs x >>= f = case f x of
    ResultFail err -> ResultFail err
    Result errs y  -> Result (xErrs <> errs) y

instance (Ord e, Monad u) => Monad (ResultT e u) where
  return = pure
  ResultT x >>= f = ResultT $ x >>= f'
   where
    f' (ResultFail err) = pure $ ResultFail err
    f' (Result errs x') = prependErrs errs <$> runResultT (f x')
    prependErrs _     (ResultFail err) = ResultFail err
    prependErrs xErrs (Result yErrs y) = Result (xErrs <> yErrs) y

instance (Ord e) => MonadResult e (Result e) where
  mkFail = ResultFail
  tellErrors errs = Result (S.fromList errs) ()
  overErrors = mapErrors
  downgradeFatal (ResultFail err) = Result [err] Nothing
  downgradeFatal (Result errs x ) = Result errs $ Just x

instance (Ord e, Monad u) => MonadResult e (ResultT e u) where
  mkFail     = ResultT . pure . mkFail
  tellErrors = ResultT . pure . tellErrors
  overErrors f (ResultT x) = ResultT $ overErrors f <$> x
  downgradeFatal (ResultT x) = ResultT $ downgradeFatal <$> x

instance (Ord e) => MonadTrans (ResultT e) where
  lift = ResultT . fmap (Result [])

instance (MonadResult e u) => MonadResult e (ReaderT r u) where
  mkFail     = lift . mkFail
  tellErrors = lift . tellErrors
  overErrors = mapReaderT . overErrors
  downgradeFatal x = ReaderT $ downgradeFatal . runReaderT x

instance (MonadResult e u) => MonadResult e (StateT s u) where
  mkFail     = lift . mkFail
  tellErrors = lift . tellErrors
  overErrors = mapStateT . overErrors
  downgradeFatal x = StateT run
   where
    run s = fmap fillWrite $ downgradeFatal $ runStateT x s
     where
      fillWrite Nothing          = (Nothing, s)
      fillWrite (Just (res, s')) = (Just res, s')

instance (Monoid w, MonadResult e u) => MonadResult e (WriterT w u) where
  mkFail     = lift . mkFail
  tellErrors = lift . tellErrors
  overErrors = mapWriterT . overErrors
  downgradeFatal x = WriterT run
   where
    run = fmap fillWrite $ downgradeFatal $ runWriterT x
     where
      fillWrite Nothing          = (Nothing, mempty)
      fillWrite (Just (res, w')) = (Just res, w')

instance (Monoid w, MonadResult e u) => MonadResult e (RWST r w s u) where
  mkFail     = lift . mkFail
  tellErrors = lift . tellErrors
  overErrors = mapRWST . overErrors
  downgradeFatal x = RWST run
   where
    run r s = fmap fillWrite $ downgradeFatal $ runRWST x r s
     where
      fillWrite Nothing              = (Nothing, s, mempty)
      fillWrite (Just (res, s', w')) = (Just res, s', w')

instance (Ord e, MonadReader r u) => MonadReader r (ResultT e u) where
  ask = ResultT $ asks pure
  local f (ResultT x) = ResultT $ local f x

instance (Ord e, MonadLogger u) => MonadLogger (ResultT e u) where
  monadLoggerLog loc src lvl = ResultT . fmap pure . monadLoggerLog loc src lvl

instance (Ord e, MonadIO u) => MonadIO (ResultT e u) where
  liftIO = ResultT . fmap pure . liftIO

instance (Ord e, MonadLoggerIO u) => MonadLoggerIO (ResultT e u) where
  askLoggerIO = ResultT $ pure <$> askLoggerIO

instance (Ord e, F.MonadFail u) => F.MonadFail (ResultT e u) where
  fail = ResultT . fmap pure . F.fail

instance (Ord e, MonadThrow u) => MonadThrow (ResultT e u) where
  throwM = ResultT . fmap pure . throwM

instance (Ord e, MonadCatch u) => MonadCatch (ResultT e u) where
  ResultT x `catch` f = ResultT $ x `catch` f' where f' = runResultT . f

instance (Ord e, MonadMask u) => MonadMask (ResultT e u) where
  -- I don't think GHC accepts this without type signatures
  mask
    :: forall b
     . ((forall a . ResultT e u a -> ResultT e u a) -> ResultT e u b)
    -> ResultT e u b
  mask f = ResultT $ mask f'
   where
    f' :: (forall a . u a -> u a) -> u (Result e b)
    f' g = runResultT $ f g'
     where
      g' :: forall a . ResultT e u a -> ResultT e u a
      g' = ResultT . g . runResultT
-- I don't think GHC accepts this without type signatures
  uninterruptibleMask
    :: forall b
     . ((forall a . ResultT e u a -> ResultT e u a) -> ResultT e u b)
    -> ResultT e u b
  uninterruptibleMask f = ResultT $ uninterruptibleMask f'
   where
    f' :: (forall a . u a -> u a) -> u (Result e b)
    f' g = runResultT $ f g'
     where
      g' :: forall a . ResultT e u a -> ResultT e u a
      g' = ResultT . g . runResultT
  generalBracket aqr rls use = ResultT $ do
    let -- resource won't leak because it failed to aquire
        rls' (ResultFail aerr) _    = pure $ ResultFail aerr
        -- if use' fails outside result than everything fails outside result, so no longer care about errors
        -- use' reports aquire errors
        rls' (Result _ rsc   ) excs = runResultT $ case excs of
          ExitCaseSuccess   (ResultFail _ ) -> rls rsc ExitCaseAbort
          -- errors reported later
          ExitCaseSuccess   (Result _ used) -> rls rsc $ ExitCaseSuccess used
          ExitCaseException exc             -> rls rsc $ ExitCaseException exc
          ExitCaseAbort                     -> rls rsc ExitCaseAbort
        use' (ResultFail aerr ) = pure $ ResultFail aerr
        use' (Result aerrs rsc) = do
          res <- runResultT $ use rsc
          case res of
            ResultFail uerr -> pure $ ResultFail uerr
            Result uerrs x  -> pure $ Result (aerrs <> uerrs) x
    (ures, rres) <- generalBracket (runResultT aqr) rls' use'
    case (ures, rres) of
      (ResultFail _uerr, ResultFail rerr) ->
        -- roughly equivalent to 'tellError uerr; mkFail rerr' (that's an excuse for being lazy)
        pure $ ResultFail rerr
      (Result _ _     , ResultFail rerr) -> pure $ ResultFail rerr
      (ResultFail uerr, Result _ _     ) -> pure $ ResultFail uerr
      (Result uerrs used, Result rerrs rlsd) ->
        pure $ Result (uerrs <> rerrs) (used, rlsd)

-- | Is the result a success?
isSuccess :: Result e a -> Bool
isSuccess (ResultFail _ ) = False
isSuccess (Result errs _) = null errs

-- | Raises an error if the result has any errors.
forceSuccess :: (Ord e, Printable e) => Result e a -> a
forceSuccess (ResultFail err) =
  error $ "unexpected fatal error:\n" <> T.unpack (pprint err)
forceSuccess (Result errs x)
  | null errs = x
  | otherwise = error $ "unexpected nonfatal errors:\n" <> T.unpack
    (T.unlines $ map pprint $ S.toList errs)

-- | @Nothing@ if there's any failure (even if success).
justSuccess :: (Ord e) => Result e a -> Maybe a
justSuccess (ResultFail _) = Nothing
justSuccess (Result [] x ) = Just x
justSuccess (Result _  _ ) = Nothing

-- | Fail with the given error if @Nothing@.
failIfNothing :: (MonadResult e m) => e -> Maybe a -> m a
failIfNothing err Nothing  = mkFail err
failIfNothing _   (Just x) = pure x

-- | Like 'traverse', but when an element raises a fatal error, instead of completely failing, the element is removed and the error becomes nonfatal.
traverseDropFatals :: (MonadResult e m) => (a -> m b) -> [a] -> m [b]
traverseDropFatals f = fmap catMaybes . traverse (downgradeFatal . f)

-- | Transforms the underlying monad in a 'ResultT'.
mapResultT
  :: (u1 (Result e1 a) -> u2 (Result e2 b))
  -> ResultT e1 u1 a
  -> ResultT e2 u2 b
mapResultT f (ResultT x) = ResultT $ f x

-- | Unline 'overErrors', allows changing the error type.
mapErrors :: (Ord e2) => (e1 -> e2) -> Result e1 a -> Result e2 a
mapErrors f (ResultFail err) = ResultFail $ f err
mapErrors f (Result errs x ) = Result (S.map f errs) x

-- | Unline 'overErrors', allows changing the error type.
mapErrorsT
  :: (Ord e2, Functor u) => (e1 -> e2) -> ResultT e1 u a -> ResultT e2 u a
mapErrorsT = mapResultT . fmap . mapErrors

-- | Adds stage to errors.
addStage :: (Functor u) => Stage -> EResultT u a -> SResultT u a
addStage = mapErrorsT . SError

-- | If an exception is thrown, will catch it and convert it into a fatal error.
catchExceptionToError :: (MonadIOEResult m) => m a -> m a
catchExceptionToError x = x `catch` (mkFail . exceptionToError)

-- | Lift the I/O action into a 'ResultT' /and/ catch exceptions.
-- The default implementation doesn't catch exceptions because 'Result' error handling is separate from 'IO' error handling
liftIOCatch :: (MonadIOEResult m) => IO a -> m a
liftIOCatch = catchExceptionToError . liftIO
