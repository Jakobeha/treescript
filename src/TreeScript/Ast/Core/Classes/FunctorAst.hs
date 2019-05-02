{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | High-arity functor for @Core@ ASTs.
module TreeScript.Ast.Core.Classes.FunctorAst
  ( MapA (..)
  , FunctorAst (..)
  ) where

data MapA a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8
  = MapA
  { mapAIdecls :: a1 -> b1
  , mapARdecls :: a2 -> b2
  , mapAFdecls :: a3 -> b3
  , mapAAliases :: a4 -> b4
  , mapABid :: a5 -> b5
  , mapAPropEnv :: a6 -> b6
  , mapAType :: a7 -> b7
  , mapAAnn :: a8 -> b8
  }

-- | Functor with the arity of all @Core@ ASTs.
class FunctorAst a where
  mapA :: MapA a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 -> a a1 a2 a3 a4 a5 a6 a7 a8 -> a b1 b2 b3 b4 b5 b6 b7 b8
