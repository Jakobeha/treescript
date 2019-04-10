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
  deriving (Eq, Ord, Read, Show, Generic)

-- | The type and identifier of the group
data GroupLoc
  = GroupLocGlobal Int
  | GroupLocLocal Int
  | GroupLocFunction T.Text
  deriving (Eq, Ord, Read, Show, Generic)

-- | References a group.
data GroupRef
  = GroupRef
  { groupRefLoc :: GroupLoc
  , groupRefProps :: [GroupRef]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | Matches an input value against an output value. Like a "let" statement.
data Guard
  = Guard
  { guardInput :: [Consume]
  , guardOutput :: Value
  , guardNexts :: [GroupRef]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | Transforms a value into a different value. Like a "match" statement.
data Reducer
  = Reducer
  { reducerMain :: Guard
  , reducerGuards :: [Guard]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | Defines a group of statements, which can be referenced by other statements.
data GroupDef
  = GroupDef
  { groupDefProps :: [Int]
  , groupDefReducers :: [Reducer]
  } deriving (Eq, Ord, Read, Show, Generic)

-- | A full TreeScript program.
data Program
  = Program
  { programLibraries :: [Lib]
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
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = ConsumeSplice <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = ConsumePrimitive <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 2, ObjectArray [xEncoded]]) = ConsumeRecord <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for Consume"

instance MessagePack GroupLoc where
  toObject (GroupLocGlobal idx) = ObjectArray [ObjectInt 0, ObjectArray [toObject idx]]
  toObject (GroupLocLocal idx) = ObjectArray [ObjectInt 1, ObjectArray [toObject idx]]
  toObject (GroupLocFunction txt) = ObjectArray [ObjectInt 2, ObjectArray [toObject txt]]
  fromObject (ObjectArray [ObjectInt 0, ObjectArray [xEncoded]]) = GroupLocGlobal <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 1, ObjectArray [xEncoded]]) = GroupLocLocal <$> fromObject xEncoded
  fromObject (ObjectArray [ObjectInt 2, ObjectArray [xEncoded]]) = GroupLocFunction <$> fromObject xEncoded
  fromObject _ = fail "invalid encoding for GroupLoc"

instance MessagePack GroupRef where
  toObject (GroupRef loc props)
    = ObjectArray [toObject loc, toObject props]
  fromObject (ObjectArray [loc, props])
      = GroupRef
    <$> fromObject loc
    <*> fromObject props
  fromObject _ = fail "invalid encoding for GroupRef"

instance MessagePack Guard where
  toObject (Guard input output nexts)
    = ObjectArray [toObject input, toObject output, toObject nexts]
  fromObject (ObjectArray [input, output, nexts])
      = Guard
    <$> fromObject input
    <*> fromObject output
    <*> fromObject nexts
  fromObject _ = fail "invalid encoding for Guard"

instance MessagePack Reducer where
  toObject (Reducer main guards)
    = ObjectArray [toObject main, toObject guards]
  fromObject (ObjectArray [main, guards])
      = Reducer
    <$> fromObject main
    <*> fromObject guards
  fromObject _ = fail "invalid encoding for Reducer"

instance MessagePack GroupDef where
  toObject (GroupDef props reds)
    = ObjectArray [toObject props, toObject reds]
  fromObject (ObjectArray [props, reds])
      = GroupDef
    <$> fromObject props
    <*> fromObject reds
  fromObject _ = fail "invalid encoding for GroupDef"

instance MessagePack Program where
  toObject (Program libs groups)
    = ObjectArray [toObject libs, toObject groups]
  fromObject (ObjectArray [libs, groups])
      = Program
    <$> fromObject libs
    <*> fromObject groups
  fromObject _ = fail "invalid encoding for Program"

instance Printable Program where
  pprint = T.pack . show
