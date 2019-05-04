{-# LANGUAGE DefaultSignatures #-}

-- | Has a type param we don't really care about.
module TreeScript.Misc.Phantom
  ( Phantom (..)
  ) where

class Phantom f where
  pcast :: f a -> f b
  -- | Lazy implementation.
  default pcast :: (Functor f) => f a -> f b
  pcast = fmap (\_ -> error "pcast: not a phantom")