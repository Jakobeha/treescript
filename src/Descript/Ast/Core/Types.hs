-- | Types for the @Core@ AST.
module Descript.Ast.Core.Types
  ( module Descript.Ast.Core.Types
  ) where

import qualified Data.Text as T
import Data.Void

-- | Declares a type of record.
data RecordDecl
  = RecordDecl
  { recordDeclHead :: T.Text
  , recordDeclProps :: [T.Text]
  }

-- | The type of a primitive.
data PrimitiveType
  = Raw T.Text -- ^ Represented as a backend literal.
  | Splice PrimitiveType -- ^ @Splice<...>@.

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive
  = Primitive
  { primitiveCode :: T.Text
  , primitiveType :: PrimitiveType
  }

-- | Contains a key and value. A record property.
data Property abs
  = Property
  { propertyKey :: T.Text
  , propertyValue :: Value abs
  }

-- | Contains a head and properties. A parent in the AST.
data Record abs
  = Record
  { recordHead :: T.Text
  , recordProps :: [Property abs]
  }

-- | An input abstraction. Matches a value against a predicate, or matches any value (if the predicate is 'None').
newtype Matcher = Matcher (Maybe OutValue)

-- | An element in a path. Makes the path refer to a property.
newtype PathElem = PathElem T.Text

-- | An output abstraction. Refers to the whole or part of the value which was matched.
newtype Path = Path [PathElem]

-- | Type of data in Descript. @abs@ is the abstraction which the value can encode, if any.
data Value abs
  = ValuePrimitive Primitive
  | ValueRecord (Record abs)
  | ValueAbstraction abs

-- | A value with no abstractions.
type RegValue = Value Void

-- | A value with matcher abstractions.
type InValue = Value Matcher

-- | A value with path abstractions
type OutValue = Value Path

-- | Transforms a value into a different value. Like a "function".
data Reducer
  = Reducer
  { reducerInput :: InValue
  , reducerOutput :: OutValue
  }

-- | A full Descript program.
data Program
  = Program
  { programRecordDecls :: [RecordDecl]
  , programReducers :: [Reducer]
  }
