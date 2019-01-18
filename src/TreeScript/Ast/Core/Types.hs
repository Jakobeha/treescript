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
  , bindContent :: Int
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript. @abs@ is the abstraction which the value can encode, if any.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefIdx :: Int
  , groupRefProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The input or output of a reducer.
data ReducerClause an
  = ReducerClause
  { reducerClauseAnn :: an
  , reducerClauseValue :: Value an
  , reducerClauseGroups :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: ReducerClause an
  , reducerOutput :: ReducerClause an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Performs some transformations on values.
data Statement an
  = StatementGroup (GroupRef an)
  | StatementReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of statements, which can be referenced by other statements.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefProps :: [Bind an]
  , groupDefRepeats :: Bool
  , groupDefStatements :: [Statement an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full TreeScript program.
data Program an
  = Program
  { programAnn :: an
  , programRecordDecls :: [RecordDecl an]
  , programMainStatements :: [Statement an]
  , programGroups :: [GroupDef an]
  }

instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn xDecls xStmts xGroups <> Program yAnn yDecls yStmts yGroups
    = Program
    { programAnn = xAnn <> yAnn
    , programRecordDecls = xDecls <> yDecls
    , programMainStatements = xStmts <> yStmts
    , programGroups = xGroups <> yGroups
    }

instance (Monoid an) => Monoid (Program an) where
  mempty
    = Program
    { programAnn = mempty
    , programRecordDecls = mempty
    , programMainStatements = mempty
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
  pprint (GroupRef _ head' props)
    = "&" <> pprint head' <> "[" <> T.intercalate "; " (map pprint props) <> "]"

instance Printable (ReducerClause an) where
  pprint (ReducerClause _ val groups)
    = T.intercalate " " $ pprint val : map pprint groups

instance Printable (Reducer an) where
  pprint (Reducer _ input output)
    = pprint input <> ": " <> pprint output

instance Printable (Statement an) where
  pprint (StatementGroup group) = pprint group <> ";"
  pprint (StatementReducer red) = pprint red <> ";"

instance Printable (Program an) where
  pprint (Program _ decls stmts groups)
    = T.unlines $ map pprint decls ++ [T.empty] ++ map pprint stmts ++ [T.empty] ++ zipWith printGroupDef [0..] groups

printGroupDef :: Int -> GroupDef an -> T.Text
printGroupDef head' (GroupDef _ props repeats reds)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
           = "&"
          <> pprint head'
          <> "["
          <> T.intercalate "; " (map pprint props)
          <> "]"
          <> ".\n"
          <> printRepeats
        printRepeats
          | repeats = "==="
          | otherwise = "---"

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

builtinDecls :: [RecordDeclCompact]
builtinDecls =
  [ mkBuiltinDecl False "E" 1
  , mkBuiltinDecl False "Unit" 0
  , mkBuiltinDecl False "True" 0
  , mkBuiltinDecl False "False" 0
  , mkBuiltinDecl False "Nil" 0
  , mkBuiltinDecl False "Cons" 2
  , mkBuiltinDecl False "Hole" 1
  , mkBuiltinDecl True "Flush" 1
  ]

-- | The head of a special record, whose contents can contain unassigned binds in covariant or contravariant positions.
flushRecordHead :: RecordHead
flushRecordHead
  = RecordHead
  { recordHeadIsFunc = True
  , recordHeadName = "Flush"
  }

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
