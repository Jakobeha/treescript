{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types to represent the generated C code.
module TreeScript.Ast.Translate.Types
  ( module TreeScript.Ast.Translate.Types
  ) where

import TreeScript.Misc

import Data.MessagePack
import qualified Data.Text as T
import GHC.Generics

-- | Describes a library and provides its code.
data Lib
  = Lib
  { libName :: T.Text
  , libDirName :: T.Text
  } deriving (Eq, Ord, Read, Show, Generic, MessagePack)

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
  | ConsumeFunction T.Text [Value]
  deriving (Eq, Ord, Read, Show, Generic)

-- | References a group.
data GroupRef
  = GroupRef
  { groupRefIsProp :: Bool
  , groupRefIdx :: Int
  , groupRefGroupProps :: [GroupRef]
  , groupRefValueProps :: [Value]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | Transforms a value into a different value. Like a "function".
data Reducer
  = Reducer
  { reducerInput :: [Consume]
  , reducerOutput :: Value
  , reducerNexts :: [GroupRef]
  , reducerGuards :: [Statement]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | Performs some transformations on values.
data Statement
  = StatementReducer Reducer
  | StatementGroup GroupRef
  deriving (Eq, Ord, Read, Show, Generic)

-- | Defines a group of statements, which can be referenced by other statements.
data GroupDef
  = GroupDef
  { groupDefGroupProps :: [Int]
  , groupDefValueProps :: [Int]
  , groupDefStatements :: [[Statement]]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | A full TreeScript program.
data Program
  = Program
  { programLibraries :: [Lib]
  , programMainStatements :: [Statement]
  , programGroups :: [GroupDef]
  } deriving (Eq, Ord, Read, Show, Generic)

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
  toObject (ConsumeFunction name args) = ObjectArray [ObjectInt 3, ObjectArray [toObject name, toObject args]]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = ConsumeSplice <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = ConsumePrimitive <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 2, ObjectArray [xEncoded]]) = ConsumeRecord <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 3, ObjectArray [xEncoded, yEncoded]])
    = ConsumeFunction <$> fromObject xEncoded <*> fromObject yEncoded
  fromObject _ = fail "invalid encoding for Consume"

instance MessagePack GroupRef where
  toObject (GroupRef isProp idx gprops vprops)
    = ObjectArray [toObject isProp, toObject idx, toObject gprops, toObject vprops]
  fromObject (ObjectArray [isProp, idx, gprops, vprops])
      = GroupRef
    <$> fromObject isProp
    <*> fromObject idx
    <*> fromObject gprops
    <*> fromObject vprops
  fromObject _ = fail "invalid encoding for GroupRef"

instance MessagePack Reducer where
  toObject (Reducer input output nexts guards)
    = ObjectArray [toObject input, toObject output, toObject nexts, toObject guards]
  fromObject (ObjectArray [input, output, nexts, guards])
      = Reducer
    <$> fromObject input
    <*> fromObject output
    <*> fromObject nexts
    <*> fromObject guards
  fromObject _ = fail "invalid encoding for Reducer"

instance MessagePack Statement where
  toObject (StatementReducer red) = ObjectArray [ObjectInt 0, ObjectArray [toObject red]]
  toObject (StatementGroup group) = ObjectArray [ObjectInt 1, ObjectArray [toObject group]]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = StatementReducer <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = StatementGroup <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for Statement"

instance MessagePack GroupDef where
  toObject (GroupDef gprops vprops stmts)
    = ObjectArray [toObject gprops, toObject vprops, toObject stmts]
  fromObject (ObjectArray [gprops, vprops, stmts])
      = GroupDef
    <$> fromObject gprops
    <*> fromObject vprops
    <*> fromObject stmts
  fromObject _ = fail "invalid encoding for GroupDef"

instance MessagePack Program where
  toObject (Program libs mainStmts groups)
    = ObjectArray [toObject libs, toObject mainStmts, toObject groups]
  fromObject (ObjectArray [libs, mainStmts, groups])
      = Program
    <$> fromObject libs
    <*> fromObject mainStmts
    <*> fromObject groups
  fromObject _ = fail "invalid encoding for Program"

instance Printable Program where
  pprint = T.pack . show
