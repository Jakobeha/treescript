{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Parsed nodes.
module TreeScript.Ast.Node
  ( module TreeScript.Ast.Node
  )
where

import qualified Data.Text                     as T
import           GHC.Generics

-- | @x@.
data Identifier r
  = Identifier
  { identififerText :: T.Text
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

data SpliceText r
  = SpliceTextNil T.Text
  | SpliceTextCons T.Text r (SpliceText r)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

data Quote
  = QuoteSingle
  | QuoteDouble
  deriving (Eq, Ord, Read, Show, Generic)

-- | @"foobar\baz\"@.
data Quoted r
  = Quoted
  { quotedQuote :: Quote
  , quotedText :: SpliceText r
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | A simple block of data.
data Prim r
  = PrimInteger Int
  | PrimFloat Float
  | PrimQuoted (Quoted r)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @lhs = rhs@.
data Assign r
  = Assign
  { assignLhs :: r
  , assignRhs :: r
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @let lhs = rhs@.
newtype Let r
  = Let
  { letAssign :: Assign r
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @{ lhs = rhs, ... }@.
data Struct r
  = Struct
  { structProps :: [Assign r]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @rhs.x@.
data Access r
  = Access
  { accessExpr :: r
  , accessProp :: Identifier r
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @[rhs, ...]@.
data Array r
  = Array
  { arrayElems :: [r]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @rhs[rhs]@.
data Index r
  = Index
  { indexExpr :: r
  , indexLoc :: r
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @(x, ...) =>\n\tstmt...@
data Closure r
  = Closure
  { closureArgs :: [Identifier r]
  , closureBody :: [r]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

-- | @rhs(rhs, ...)@.
data Call r
  = Call
  { callFun :: r
  , callArgs :: [r]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

data Node r
  = NodePrim (Prim r)
  | NodeIdentifier (Identifier r)
  | NodeAssign (Assign r)
  | NodeLet (Let r)
  | NodeStruct (Struct r)
  | NodeAccess (Access r)
  | NodeArray (Array r)
  | NodeIndex (Index r)
  | NodeClosure (Closure r)
  | NodeCall (Call r)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)
