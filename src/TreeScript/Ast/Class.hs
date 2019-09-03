{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Ability to map node annotations.
module TreeScript.Ast.Class
  ( module TreeScript.Ast.Class
  )
where

import           TreeScript.Ast.Ann            as A
import           TreeScript.Ast.Type

import           Data.Proxy
import           Data.Kind
import           Generics.Kind                 as G

type LoT1' (a :: StxType -> *) = a ':&&: 'LoT0

class Node (a :: (StxType -> *) -> *) where
  mapAnn :: forall (r1 :: StxType -> *) (r2 :: StxType -> *). (forall t. r1 t -> r2 t) -> a r1 -> a r2
  default mapAnn :: forall (r1 :: StxType -> *) (r2 :: StxType -> *). (GenericK a, GNode (RepK a), Reqs (RepK a) r1 r2) => (forall t. r1 t -> r2 t) -> a r1 -> a r2
  mapAnn v = toK . gmapAnn v . fromK

-- Below code derived from Functor1 and KFunctor (https://gitlab.com/trupill/kind-generics/blob/master/kind-generics-deriving/src/Generics/Kind/Derive/FunctorOne.hs and https://gitlab.com/trupill/kind-generics/blob/master/kind-generics-deriving/src/Generics/Kind/Derive/KFunctor.hs)

class GNode (f :: LoT ((StxType -> *) -> *) -> *) where
  type Reqs f (r1 :: StxType -> *) (r2 :: StxType -> *) :: Constraint
  gmapAnn
    :: forall (r1 :: StxType -> *) (r2 :: StxType -> *)
    . (Reqs f r1 r2)
    => (forall t . r1 t -> r2 t)
    -> f (LoT1' r1)
    -> f (LoT1' r2)

instance GNode U1 where
  type Reqs U1 r1 r2 = ()
  gmapAnn _ U1 = U1

instance GNode f => GNode (M1 i c f) where
  type Reqs (M1 i c f) r1 r2 = Reqs f r1 r2
  gmapAnn f (M1 x) = M1 (gmapAnn f x)

instance (GNode f, GNode g) => GNode (f :+: g) where
  type Reqs (f :+: g) r1 r2 = (Reqs f r1 r2, Reqs g r1 r2)
  gmapAnn f (L1 x) = L1 (gmapAnn f x)
  gmapAnn f (R1 x) = R1 (gmapAnn f x)

instance (GNode f, GNode g) => GNode (f :*: g) where
  type Reqs (f :*: g) r1 r2 = (Reqs f r1 r2, Reqs g r1 r2)
  gmapAnn f (x :*: y) = gmapAnn f x :*: gmapAnn f y

instance (GNode f) => GNode (c :=>: f) where
  type Reqs (c :=>: f) r1 r2 = (Interpret c (LoT1' r2), Reqs f r1 r2)
  gmapAnn f (SuchThat x) = SuchThat (gmapAnn f x)

class GNodeArg (a :: Atom ((StxType -> *) -> *) (*)) where
  gamapAnn :: forall (r1 :: StxType -> *) (r2 :: StxType -> *). Proxy a -> (forall t. r1 t -> r2 t) -> Interpret a (LoT1' r1) -> Interpret a (LoT1' r2)

instance GNodeArg a => GNode (Field a) where
  type Reqs (Field a) r1 r2 = ()
  gmapAnn
    :: forall (r1 :: StxType -> *) (r2 :: StxType -> *)
     . (Reqs (Field a) r1 r2)
    => (forall t . r1 t -> r2 t)
    -> Field a (LoT1' r1)
    -> Field a (LoT1' r2)
  gmapAnn f (Field x) = Field
    ((gamapAnn :: Proxy a
       -> (forall t . r1 t -> r2 t)
       -> Interpret a (LoT1' r1)
       -> Interpret a (LoT1' r2)
     )
      Proxy
      f
      x
    )

-- Constant
instance GNodeArg ('Kon a) where
  gamapAnn Proxy _ x = x

-- Type variable itself (notice: the @t@ is the same as in @forall t. r1 t -> r2 t@ above)
instance GNodeArg (Var0 'G.:@: 'Kon (t :: StxType)) where
  gamapAnn Proxy f = f

-- Child node
instance forall a. (Node a) => GNodeArg (a :$: Var0) where
  gamapAnn Proxy = mapAnn

-- List of children (can add other specific functors if necessary, we don't need to be more generic)
instance forall a. (Node a) => GNodeArg ([] :$: (a :$: Var0)) where
  gamapAnn Proxy f = map $ mapAnn f

tailAnn :: (Node a) => a (r A.:@: rs) -> a rs
tailAnn = mapAnn $ \(_ A.:@: anns) -> anns
