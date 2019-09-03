{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | "Virtual" continuation monad: it can embed actual continuations or evaluate linarly.
-- This version also must have 'id' as the final continuation, and only allows exiting let/cc with the continuation.
module TreeScript.Misc.VCont
  ( MonadVCont(..)
  )
where

import           Control.Monad.Cont
import           Data.Void

class (Monad m) => MonadVCont m where
  type ContRes m :: *
  type VirtVoid m :: *
  mkCont :: ((ContRes m -> m (VirtVoid m)) -> m (VirtVoid m)) -> m (ContRes m)
  -- | Creates a loop which repeats until exited with a continuation.
  mkLoop :: m () -> m (VirtVoid m)

instance (Monad u) => MonadVCont (ContT e u) where
  type ContRes (ContT e u) = e
  type VirtVoid (ContT e u) = Void
  mkCont f = ContT $ \k -> do
    let ContT next = f $ \res -> ContT $ \_ -> pure res
    res <- next $ pure . absurd
    k res
  mkLoop = forever
