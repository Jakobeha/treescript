{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Core@ AST.
module TreeScript.Ast.Core.Types
  ( module TreeScript.Ast.Core.Types
  ) where

import TreeScript.Misc

import qualified Data.Text as T
import GHC.Generics

-- | The type of a record.
data RecordHead
  = RecordHead
  { recordHeadIsFunc :: Bool
  , recordHeadName :: T.Text
  } deriving (Eq, Ord, Read, Show)

-- | Declares a type of record but doesn't specifify property values.
data RecordDeclCompact
  = RecordDeclCompact
  { recordDeclCompactHead :: RecordHead
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
  , recordHead :: RecordHead
  , recordProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefIsProp :: Bool
  , groupRefIdx :: Int
  , groupRefSubgroups :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerSubGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of statements, which can be referenced by other statements.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefGroupProps :: [Bind an]
  , groupDefReducers :: [Reducer an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full TreeScript program.
data Program an
  = Program
  { programAnn :: an
  , programRecordDecls :: [RecordDecl an]
  , programGroups :: [GroupDef an]
  }

instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn xDecls xStmts xGroups <> Program yAnn yDecls yStmts yGroups
    = Program
    { programAnn = xAnn <> yAnn
    , programRecordDecls = xDecls <> yDecls
    , programGroups = xGroups <> yGroups
    }

instance (Monoid an) => Monoid (Program an) where
  mempty
    = Program
    { programAnn = mempty
    , programRecordDecls = mempty
    , programGroups = mempty
    }

instance Printable RecordHead where
  pprint (RecordHead isFunc name)
    = printIsFunc <> name
    where printIsFunc
            | isFunc = "#"
            | otherwise = T.empty

instance Printable (RecordDecl an) where
  pprint (RecordDecl _ head' props)
    = head' <> "[" <> T.intercalate "; " props <> "]."

instance Printable (Primitive an) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

instance Printable (Record an) where
  pprint (Record _ head' props)
    = pprint head' <> "[" <> T.intercalate "; " (map pprint props) <> "]"

instance Printable (Bind an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance Printable (Value an) where
  pprint (ValuePrimitive prim) = pprint prim
  pprint (ValueRecord record) = pprint record
  pprint (ValueBind bind) = pprint bind

instance Printable (GroupRef an) where
  pprint (GroupRef _ isProp head' gprops vprops)
     = "&"
    <> printIsProp isProp
    <> pprint head'
    <> printGroupProps pprint gprops
    <> printProps pprint vprops
    where printIsProp False = "+"
          printIsProp True = "-"

instance Printable (Guard an) where
  pprint (Guard _ input output nexts)
     = pprint input
    <> " <- "
    <> pprint output
    <> foldMap printNext nexts
    where printNext next' = " " <> pprint next'

instance Printable (Reducer an) where
  pprint (Reducer _ (Guard _ input output nexts) guards)
     = pprint input
    <> " -> "
    <> pprint output
    <> foldMap printNext nexts
    <> foldMap printGuard guards
    <> ";"
    where printNext next' = " " <> pprint next'
          printGuard guard = ",\n  " <> pprint guard

instance Printable (Program an) where
  pprint (Program _ decls groups)
    = T.unlines $ map pprint decls ++ [T.empty] ++ zipWith printGroupDef [0..] groups

printProps :: (a -> T.Text) -> [a] -> T.Text
printProps f props = "[" <> T.intercalate "; " (map f props) <> "]"

printGroupDef :: Int -> GroupDef an -> T.Text
printGroupDef head' (GroupDef _ gprops reds)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
           = "&"
          <> pprint head'
          <> printProps printGroupDefProp gprops
          <> "."
        printGroupDefProp (Bind _ idx) = "&-" <> pprint idx

mkBuiltinDecl :: Bool -> T.Text -> Int -> RecordDeclCompact
mkBuiltinDecl isFunc head' numProps
  = RecordDeclCompact
  { recordDeclCompactHead
      = RecordHead
      { recordHeadIsFunc = isFunc
      , recordHeadName = head'
      }
  , recordDeclCompactNumProps = numProps
  }

-- | Specifies that a record declaration can take any number of properties.
varNumProps :: Int
varNumProps = (-1)

builtinDecls :: [RecordDeclCompact]
builtinDecls =
  [ mkBuiltinDecl False "T" varNumProps
  , mkBuiltinDecl False "Unit" 0
  , mkBuiltinDecl False "True" 0
  , mkBuiltinDecl False "False" 0
  , mkBuiltinDecl False "Nil" 0
  , mkBuiltinDecl False "None" 0
  , mkBuiltinDecl False "Some" 1
  , mkBuiltinDecl False "Cons" 2
  , mkBuiltinDecl False "Hole" 1
  ]

compactRecordDecl :: RecordDecl an -> RecordDeclCompact
compactRecordDecl (RecordDecl _ head' props)
  = RecordDeclCompact
  { recordDeclCompactHead
      = RecordHead
      { recordHeadIsFunc = False
      , recordHeadName = head'
      }
  , recordDeclCompactNumProps = length props
  }

hole :: an -> an -> Int -> Value an
hole ann idxAnn idx
  = ValueRecord Record
  { recordAnn = ann
  , recordHead
      = RecordHead
      { recordHeadIsFunc = False
      , recordHeadName = "Hole"
      }
  , recordProps = [ValuePrimitive $ PrimInteger idxAnn idx]
  }
