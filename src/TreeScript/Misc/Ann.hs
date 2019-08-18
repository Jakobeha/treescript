{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Allows trees to be "annotated", with arbitrary data assigned to each node.
module TreeScript.Misc.Ann
  ( SrcAnn(..)
  , Annd(..)
  , Annotatable(..)
  , mapAnnd
  , remAnns
  , (=@=)
  )
where

import           TreeScript.Misc.Loc
import qualified Data.Text                     as T

import           GHC.Generics

data SrcAnn
  = SrcAnn
  { srcAnnRange :: Range
  , srcAnnText :: T.Text
  } deriving (Eq, Ord, Read, Show)

-- | A node with an annotation.
data Annd a an
  = Annd
  { anndAnn :: an
  , annd :: a
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A tree which allows arbitrary data to be assigned to each node in an "annotation". For example, an AST can be "annotated" with source locations of each token. When these source locations aren't needed, they can be removed.
class (Functor a, Foldable a, Traversable a) => Annotatable a where
  -- | Gets the root's annotation.
  getAnn :: a an -> an
  default getAnn :: (Generic1 a, Annotatable' (Rep1 a)) => a an -> an
  getAnn = getAnn' . from1

instance Semigroup SrcAnn where
  SrcAnn xrng xtxt <> SrcAnn yrng ytxt =
    SrcAnn { srcAnnRange = xrng <> yrng, srcAnnText = xtxt <> ytxt }

instance Annotatable (Annd a) where
  getAnn = anndAnn

class (Functor a, Foldable a, Traversable a) => Annotatable' a where
  getAnn' :: a an -> an

instance Annotatable' V1 where
  getAnn' = undefined

instance (Annotatable' f, Annotatable' g) => Annotatable' (f :+: g) where
  getAnn' (L1 x) = getAnn' x
  getAnn' (R1 y) = getAnn' y

instance (Annotatable' f, Functor g, Foldable g, Traversable g) => Annotatable' (f :*: g) where
  getAnn' (x :*: _) = getAnn' x

instance (Annotatable' f) => Annotatable' (M1 i t f) where
  getAnn' = getAnn' . unM1

instance Annotatable' Par1 where
  getAnn' = unPar1

instance (Annotatable f) => Annotatable' (Rec1 f) where
  getAnn' = getAnn . unRec1

-- | Transform the annotated value.
mapAnnd :: (a -> b) -> Annd a an -> Annd b an
mapAnnd f (Annd ann x) = Annd ann $ f x

-- | Remove all annotations.
remAnns :: (Annotatable f) => f an -> f ()
remAnns x = () <$ x

-- | Checks for equality ignoring annotations.
(=@=) :: (Annotatable f, Eq (f ())) => f an1 -> f an2 -> Bool
x =@= y = remAnns x == remAnns y
