{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the ability to embed @letCont@ / @callCont@ statements without passing continuations around,
-- except in @callCont@ you must handle the case where no @letCont@ was actually defined.
module TreeScript.Misc.ContStack
  ( ContStackT(..)
  , MonadContStack(..)
  , runContStackT
  )
where

import           TreeScript.Misc.Ext.List
import           TreeScript.Misc.VCont

import           Control.Monad.Cont
import           Control.Monad.State.Strict
import           Data.Void

type ContFrame m = ContRes m -> m (VirtVoid m)

-- | Implements @MonadContStack@ with built-in continuation monad and strict state monad.
newtype ContStackT e u a = ContStackT{ unContStackT :: StateT [ContFrame (ContStackT e u)] (ContT e u) a } deriving (Functor, Applicative, Monad, MonadIO)

-- | Continuation monad and state monad containing a stack of continuations.
class (MonadVCont m) => MonadContStack m where
  -- | Push continuation onto the stack, run, then pop continuation.
  bumpCont :: ContFrame m -> m a -> m a
  topCont :: m (Maybe (ContFrame m))

instance (Monad u) => MonadVCont (ContStackT e u) where
  type ContRes (ContStackT e u) = e
  type VirtVoid (ContStackT e u) = Void
  mkCont f = ContStackT $ StateT $ \s ->
    let f' k' =
            (do
              let k                     = ContStackT . lift . k'
                  ContStackT (StateT x) = f k
              fst <$> x s
            )
    in  (, s) <$> mkCont f'
  mkLoop = forever

instance (Monad u) => MonadContStack (ContStackT e u) where
  bumpCont k action = ContStackT $ do
    ks <- get
    put $ k : ks
    res <- unContStackT action
    put ks
    pure res
  topCont = ContStackT $ headOpt <$> get

instance MonadTrans (ContStackT e) where
  lift = ContStackT . lift . lift

instance (MonadState s u) => MonadState s (ContStackT e u) where
  get   = lift get
  put   = lift . put
  state = lift . state

runContStackT :: (Monad u) => (a -> u e) -> ContStackT e u a -> u e
runContStackT f = (`runContT` f) . (`evalStateT` []) . unContStackT
