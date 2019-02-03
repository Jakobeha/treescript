{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types to represent the generated C code.
module TreeScript.Ast.Translate.Types
  ( module TreeScript.Ast.Translate.Types
  ) where

import TreeScript.Misc

import Data.MessagePack
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive
  = PrimInteger Int
  | PrimFloat Float
  | PrimString T.Text
  deriving (Eq, Ord, Read, Show, Generic)

-- | Contains a head and properties. A parent in the AST.
data Record
  = Record
  { recordHead :: T.Text
  , recordProps :: [Value]
  } deriving (Eq, Ord, Read, Show, Generic, MessagePack)

-- | Type of data in TreeScript.
data Value
  = ValueSplice Int
  | ValuePrimitive Primitive
  | ValueRecord Record
  deriving (Eq, Ord, Read, Show, Generic)

-- | Describes what values an input value matches.
data Consume
  = ConsumeSplice Int
  | ConsumePrimitive Primitive
  | ConsumeRecord T.Text
  deriving (Eq, Ord, Read, Show, Generic)

-- | The input or output of a reducer.
data ReducerClause
  = ReducerClause
  { reducerClauseConsumes :: [Consume]
  , reducerClauseProduce :: Value
  , reducerClauseGroups :: [Group]
  } deriving (Eq, Ord, Read, Show, Generic, MessagePack)

-- | Transforms a value into a different value. Like a "function".
data Reducer
  = Reducer
  { reducerInput :: ReducerClause
  , reducerOutput :: ReducerClause
  } deriving (Eq, Ord, Read, Show, Generic, MessagePack)

-- | Performs some transformations on values.
data Statement
  = StatementReducer Reducer
  | StatementGroup Group
  deriving (Eq, Ord, Read, Show, Generic)

-- | Defines a group of statements, which can be referenced by other statements.
data Group
  = Group
  { groupRepeats :: Bool
  , groupStatements :: [[Statement]]
  } deriving (Eq, Ord, Read, Show, Generic, MessagePack)

-- | A full TreeScript program.
data Program
  = Program
  { programNumPropsByHead :: M.Map T.Text Int
  , programLibraries :: [T.Text]
  , programMainGroup :: Group
  } deriving (Eq, Ord, Read, Show, Generic, MessagePack)

-- Variants need custom @MessagePack@ implementations because the Rust interpreter uses a different format for them.

instance MessagePack Primitive where
  toObject (PrimInteger int) = ObjectArray [ObjectInt 0, ObjectArray [toObject int]]
  toObject (PrimFloat float) = ObjectArray [ObjectInt 1, ObjectArray [toObject float]]
  toObject (PrimString str) = ObjectArray [ObjectInt 2, ObjectArray [toObject str]]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = PrimInteger <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = PrimFloat <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 2, ObjectArray [xEncoded]]) = PrimString <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for Primitive"

instance MessagePack Value where
  toObject (ValueSplice idx) = ObjectArray [ObjectInt 0, ObjectArray [toObject idx]]
  toObject (ValuePrimitive prim) = ObjectArray [ObjectInt 1, ObjectArray [toObject prim]]
  toObject (ValueRecord record) = ObjectArray [ObjectInt 2, toObject record]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = ValueSplice <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = ValuePrimitive <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 2, xEncoded]) = ValueRecord <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for Value"

instance MessagePack Consume where
  toObject (ConsumeSplice idx) = ObjectArray [ObjectInt 0, ObjectArray [toObject idx]]
  toObject (ConsumePrimitive prim) = ObjectArray [ObjectInt 1, ObjectArray [toObject prim]]
  toObject (ConsumeRecord head') = ObjectArray [ObjectInt 2, ObjectArray [toObject head']]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = ConsumeSplice <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = ConsumePrimitive <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 2, ObjectArray [xEncoded]]) = ConsumeRecord <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for Consume"

instance MessagePack Statement where
  toObject (StatementReducer red) = ObjectArray [ObjectInt 0, ObjectArray [toObject red]]
  toObject (StatementGroup group) = ObjectArray [ObjectInt 1, ObjectArray [toObject group]]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = StatementReducer <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = StatementGroup <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for Statement"

instance Printable Program where
  pprint = T.pack . show
