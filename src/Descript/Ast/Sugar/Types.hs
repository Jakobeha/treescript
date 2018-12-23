-- | Types for the @Sugar@ AST.
module Descript.Ast.Sugar.Types
  ( module Descript.Ast.Sugar.Types
  ) where

import Descript.Misc

import qualified Data.Text as T

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: T.Text
  , recordDeclProps :: [T.Text]
  }

-- | The type of a primitive.
data PrimitiveType an
  = Raw an T.Text -- ^ Represented as a backend literal.
  | Splice an (PrimitiveType an) -- ^ @Splice<...>@.

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = Primitive
  { primitiveAnn :: an
  , primitiveCode :: T.Text
  , primitiveType :: PrimitiveType an
  }

-- | Contains a key and value. A record property.
data Property abs an
  = Property
  { propertyAnn :: an
  , propertyKey :: T.Text
  , propertyValue :: Value abs an
  }

-- | Contains a head and properties. A parent in the AST.
data Record abs an
  = Record
  { recordAnn :: an
  , recordHead :: T.Text
  , recordProps :: [Property abs an]
  }

-- | An input abstraction. Matches a value against a predicate, or matches any value (if the predicate is 'None').
data Matcher an = Matcher an (Maybe (OutValue an))

-- | An element in a path. Makes the path refer to a property.
data PathElem an = PathElem an T.Text

-- | An output abstraction. Refers to the whole or part of the value which was matched.
data Path an = Path an [PathElem an]

-- | Type of data in Descript. @abs@ is the abstraction which the value can encode, if any.
data Value abs an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record abs an)
  | ValueAbstraction (abs an)

-- | A value with no abstractions.
type RegValue = Value Void1

-- | A value with matcher abstractions.
type InValue = Value Matcher

-- | A value with path abstractions
type OutValue = Value Path

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: InValue an
  , reducerOutput :: OutValue an
  }

-- | A full Descript program.
data Program an
  = Program
  { programAnn :: an
  , programRecordDecls :: [RecordDecl an]
  , programReducers :: [Reducer an]
  }
