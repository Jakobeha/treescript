{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Restricted continuation monad - must be run with 'id' and only allows exiting let/cc with the continuation.
module TreeScript.Misc.RCont
  ( VoidCont
  , RContT(..)
  , MonadRCont(..)
  , exitCont
  )
where

import           Control.Monad.Cont
import           Data.Void

-- | Void which represents a continuation.
newtype VoidCont = VoidCont Void

newtype RContT u e a = RContT{ unRCont :: ContT e u a } deriving (Functor, Applicative, Monad)

class (Monad m) => MonadRCont e m | m -> e where
  mkCont :: ((e -> m VoidCont) -> m VoidCont) -> m e

instance (Monad u) => MonadRCont e (RContT u e) where
  mkCont f = RContT $ mkCont' f'
    where f' k' = unRCont $ f k where k = RContT . k'

-- | Same as 'callCC', except this version forces the function to exit with the continuation (or fail)
mkCont'
  :: (Monad u)
  => ((a -> ContT a u VoidCont) -> ContT a u VoidCont)
  -> ContT f u a
-- Instantiate e with a
mkCont' f = ContT $ \k -> do
  let ContT next = f $ \res -> ContT $ \_ -> pure res
  res <- next $ \(VoidCont vd) -> pure $ absurd vd
  k res

exitCont :: (Functor m) => m VoidCont -> m a
exitCont = fmap $ \(VoidCont vd) -> absurd vd
