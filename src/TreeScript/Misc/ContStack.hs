{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Provides the ability to embed @letCont@ / @callCont@ statements without passing continuations around,
-- except in @callCont@ you must handle the case where no @letCont@ was actually defined.
module TreeScript.Misc.ContStack
  ( ContStackT(..)
  , MonadContStack(..)
  )
where

import           TreeScript.Misc.Ext.List
import           TreeScript.Misc.RCont

import           Control.Monad.State.Strict

type ContFrame m e = e -> m VoidCont

-- | Implements @MonadContStack@ with built-in continuation monad and strict state monad.
newtype ContStackT u e a = ContStackT{ unContStackT :: StateT [ContFrame (ContStackT u e) e] (RContT u e) a } deriving (Functor, Applicative, Monad)

-- | Continuation monad and state monad containing a stack of continuations.
class (MonadRCont e m) => MonadContStack e m | m -> e where
  -- | Push continuation onto the stack, run, then pop continuation.
  bumpCont :: ContFrame m e -> m a -> m a
  topCont :: m (Maybe (ContFrame m e))

instance (Monad u) => MonadRCont e (ContStackT u e) where
  mkCont f = ContStackT $ StateT $ \s ->
    let f' k' =
            (do
              let k                     = ContStackT . lift . k'
                  ContStackT (StateT x) = f k
              fst <$> x s
            )
    in  (, s) <$> mkCont f'

instance (Monad u) => MonadContStack e (ContStackT u e) where
  bumpCont k action = ContStackT $ do
    ks <- get
    put $ k : ks
    res <- unContStackT action
    put ks
    pure res
  topCont = ContStackT $ headOpt <$> get
