{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Node annotations.
-- Nodes support multiple annotations. Use @Node (A1 MyAnn)@ (equivalent to @Node (As '[MyAnn])@) instead of @Node MyAnn@.
-- To create an annotation:
-- - Define the annotation's datatype @MyAnn@
-- - Define a class providing your annotation's functionality, or reuse 'AnnHas'
-- - Implement an overlapping instance for @(MyAnn :@: rs)@, e.g. @instance {-# OVERLAPPING #-} AnnHas MyInfo (MyAnn :@: rs) where ...@
-- - If defining a new class, implement @instance {-# OVERLAPPABLE -#} (MyClass rs) => MyClass (r :@: rs) where ...@, and if the class supports all annotations, @instance MyClass NilAnn@
module TreeScript.Ast.Ann
  ( module TreeScript.Ast.Ann
  )
where

import           TreeScript.Ast.Type
import           TreeScript.Misc

data NilAnn (t :: StxType) = NilAnn

-- | Cons annotation.
data (a :@: b) (t :: StxType) = a t :@: b t

-- | Converts type-level list to annotation list.
type family As (rs :: [StxType -> *]) :: StxType -> * where
  As '[] = NilAnn
  As (x ': xs) = x :@: As xs

type A1 a = a :@: NilAnn

newtype SrcAnn (t :: StxType) = SrcAnn{ srcAnn :: SrcInfo }

class AnyAnn (r :: StxType -> *)
instance AnyAnn r

class NullableAnn (r :: StxType -> *) where
  nullAnn :: r t

class AnnHas a (r :: StxType -> *) | r -> a where
  fromAnn :: r t -> a

class AnnMaybeHas a (r :: StxType -> *) where
  maybeFromAnn :: r t -> Maybe a

instance NullableAnn NilAnn where
  nullAnn = NilAnn

instance (NullableAnn r, NullableAnn rs) => NullableAnn (r :@: rs) where
  nullAnn = nullAnn :@: nullAnn

instance {-# OVERLAPPING #-} AnnHas SrcInfo (SrcAnn :@: rs) where
  fromAnn (x :@: _) = srcAnn x

instance {-# OVERLAPPING #-} AnnMaybeHas SrcInfo (SrcAnn :@: rs) where
  maybeFromAnn (x :@: _) = Just $ srcAnn x

instance {-# OVERLAPPABLE #-} (AnnHas a rs) => AnnHas a (r :@: rs) where
  fromAnn (_ :@: xs) = fromAnn xs

instance AnnMaybeHas a NilAnn where
  maybeFromAnn NilAnn = Nothing

instance {-# OVERLAPPABLE #-} (AnnMaybeHas a rs) => AnnMaybeHas a (r :@: rs) where
  maybeFromAnn (_ :@: xs) = maybeFromAnn xs

mkA1 :: r t -> A1 r t
mkA1 x = x :@: NilAnn
