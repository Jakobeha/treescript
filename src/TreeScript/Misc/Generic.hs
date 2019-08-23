{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Utilities for generic programming.
module TreeScript.Misc.Generic
  ( GProduct2(..)
  , GProductList(..)
  )
where

import           GHC.Generics

-- | Product with at least 2 elements
class GProduct2 a where
  type Head a :: * -> *
  type Tail a :: * -> *

  gproductHead :: a b -> Head a b
  gproductTail :: a b -> Tail a b
  gproductCons :: Head a b -> Tail a b -> a b

-- | Linear sequence representation of product.
class GProductList a where
  type Linear a :: * -> *

  glinearize :: a b -> Linear a b
  gunlinearize :: Linear a b -> a b

instance GProduct2 (M1 i c p :*: b) where
  type Head (M1 i c p :*: b) = M1 i c p
  type Tail (M1 i c p :*: b) = b

  gproductHead (x :*: _) = x
  gproductTail (_ :*: xs) = xs
  gproductCons = (:*:)

instance (GProduct2 (M1 i c p :*: a2)) => GProduct2 ((M1 i c p :*: a2) :*: b) where
  type Head ((M1 i c p :*: a2) :*: b) = M1 i c p
  type Tail ((M1 i c p :*: a2) :*: b) = a2 :*: b

  gproductHead ((x :*: _) :*: _) = x
  gproductTail ((_ :*: xxs) :*: xs) = xxs :*: xs
  gproductCons x (xxs :*: xs) = (x :*: xxs) :*: xs

instance (GProduct2 ((a11 :*: a12) :*: a2)) => GProduct2 (((a11 :*: a12) :*: a2) :*: b) where
  type Head (((a11 :*: a12) :*: a2) :*: b) = Head ((a11 :*: a12) :*: a2)
  type Tail (((a11 :*: a12) :*: a2) :*: b) = Tail ((a11 :*: a12) :*: a2) :*: b

  gproductHead (x :*: _) = gproductHead x
  gproductTail (x :*: xs) = gproductTail x :*: xs
  gproductCons x (xxs :*: xs) = gproductCons x xxs :*: xs

instance GProductList (M1 i c p) where
  type Linear (M1 i c p) = M1 i c p

  glinearize x = x
  gunlinearize x = x

instance (GProductList b) => GProductList (M1 i c p :*: b) where
  type Linear (M1 i c p :*: b) = M1 i c p :*: Linear b

  glinearize (x :*: xs) = x :*: glinearize xs
  gunlinearize (x :*: xs) = x :*: gunlinearize xs

instance (GProductList (a2 :*: b), GProductList b) => GProductList ((M1 i c p :*: a2) :*: b) where
  type Linear ((M1 i c p :*: a2) :*: b) = M1 i c p :*: Linear (a2 :*: b)

  glinearize ((xx :*: xxs) :*: xs) = xx :*: glinearize (xxs :*: xs)
  gunlinearize (xx :*: ys) = (xx :*: xxs) :*: xs
    where (xxs :*: xs) = gunlinearize ys

instance (GProductList (a11 :*: (a12 :*: a2)), GProductList (Linear (a11 :*: (a12 :*: a2)) :*: b), GProductList b) => GProductList (((a11 :*: a12) :*: a2) :*: b) where
  type Linear (((a11 :*: a12) :*: a2) :*: b) = Linear (Linear (a11 :*: (a12 :*: a2)) :*: b)

  glinearize (((xxx :*: xxxs) :*: xxs) :*: xs) =
    glinearize (glinearize (xxx :*: (xxxs :*: xxs)) :*: xs)
  gunlinearize ys = ((xxx :*: xxxs) :*: xxs) :*: xs
   where
    yys :*:                   xs = gunlinearize ys
    (   xxx :*: (xxxs :*: xxs))  = gunlinearize yys
