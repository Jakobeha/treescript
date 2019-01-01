{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Core@ AST.
module Descript.Ast.Core.Types
  ( module Descript.Ast.Core.Types
  ) where

import Descript.Misc

import qualified Data.Text as T
import GHC.Generics

-- | Declares a type of record but doesn't specifify property values.
data RecordDeclCompact
  = RecordDeclCompact
  { recordDeclCompactHead :: T.Text
  , recordDeclCompactNumProps :: Int
  }

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: T.Text
  , recordDeclProps :: [T.Text]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordHead :: T.Text
  , recordProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindContent :: Int
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in Descript. @abs@ is the abstraction which the value can encode, if any.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Whether a value or value-related type is input or output.
data ValueType
  = ValueTypeInput
  | ValueTypeOutput
  deriving (Eq)

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: Value an
  , reducerOutput :: Value an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full Descript program.
data Program an
  = Program
  { programAnn :: an
  , programRecordDecls :: [RecordDecl an]
  , programReducers :: [Reducer an]
  }

instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn xDecls xReds <> Program yAnn yDecls yReds
    = Program
    { programAnn = xAnn <> yAnn
    , programRecordDecls = xDecls <> yDecls
    , programReducers = xReds <> yReds
    }

instance (Monoid an) => Monoid (Program an) where
  mempty
    = Program
    { programAnn = mempty
    , programRecordDecls = mempty
    , programReducers = mempty
    }

instance Printable (RecordDecl an) where
  pprint (RecordDecl _ head' props)
    = head' <> "[" <> T.intercalate "; " props <> "]"

instance Printable (Primitive an) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

instance Printable (Record an) where
  pprint (Record _ head' props)
    = head' <> "[" <> T.intercalate "; " (map pprint props) <> "]"

instance Printable (Bind an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance Printable (Value an) where
  pprint (ValuePrimitive prim) = pprint prim
  pprint (ValueRecord record) = pprint record
  pprint (ValueBind bind) = pprint bind

instance Printable (Reducer an) where
  pprint (Reducer _ input output)
    = pprint input <> ": " <> pprint output <> ";"

instance Printable (Program an) where
  pprint (Program _ decls reds)
    = T.unlines $ map pprint decls ++ map pprint reds

builtinDecls :: [RecordDeclCompact]
builtinDecls =
  [ RecordDeclCompact "Unit" 0
  , RecordDeclCompact "True" 0
  , RecordDeclCompact "False" 0
  , RecordDeclCompact "Nil" 0
  , RecordDeclCompact "Cons" 2
  ]

compactRecordDecl :: RecordDecl an -> RecordDeclCompact
compactRecordDecl (RecordDecl _ head' props)
  = RecordDeclCompact
  { recordDeclCompactHead = head'
  , recordDeclCompactNumProps = length props
  }
