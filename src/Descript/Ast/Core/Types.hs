-- | Types for the @Core@ AST.
module Descript.Ast.Core.Types
  ( module Descript.Ast.Core.Types
  ) where

import qualified Data.Text as T

-- | Declares a type of record.
data RecordDecl
  = RecordDecl
  { recordDeclHead :: T.Text
  , recordDeclProps :: [T.Text]
  }

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive
  = PrimInteger Int
  | PrimFloat Float
  | PrimString T.Text

-- | Contains a head and properties. A parent in the AST.
data Record
  = Record
  { recordHead :: T.Text
  , recordProps :: [Value]
  }

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
newtype Bind = Bind Int

-- | Type of data in Descript. @abs@ is the abstraction which the value can encode, if any.
data Value
  = ValuePrimitive Primitive
  | ValueRecord Record
  | ValueBind Bind

-- | Transforms a value into a different value. Like a "function".
data Reducer
  = Reducer
  { reducerInput :: Value
  , reducerOutput :: Value
  }

-- | A full Descript program.
data Program
  = Program
  { programRecordDecls :: [RecordDecl]
  , programReducers :: [Reducer]
  }

instance Semigroup Program where
  Program xDecls xReds <> Program yDecls yReds
    = Program
    { programRecordDecls = xDecls <> yDecls
    , programReducers = xReds <> yReds
    }

instance Monoid Program where
  mempty
    = Program
    { programRecordDecls = mempty
    , programReducers = mempty
    }
