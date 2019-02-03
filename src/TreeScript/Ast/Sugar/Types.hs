{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Sugar@ AST.
module TreeScript.Ast.Sugar.Types
  ( module TreeScript.Ast.Sugar.Types
  ) where

import TreeScript.Misc

import qualified Data.Text as T
import GHC.Generics

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclRecord :: Record an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An entire code block (in the future might also handle strings) which might contain splices.
data SpliceText an
  = SpliceTextNil an T.Text
  | SpliceTextCons an T.Text (Value an) (SpliceText an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A code block which might contain splices.
data SpliceCode an
  = SpliceCode
  { spliceCodeAnn :: an
  , spliceCodeLangugage :: Symbol an
  , spliceCodeText :: SpliceText an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An identifier, such as sa record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A group property.
data GroupProperty an
  = GroupProperty
  { groupPropertyAnn :: an
  , groupPropertyKey :: Symbol an
  , groupPropertyValue :: Value an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A record declaration property, group declaration property, record property, or group property.
data GenProperty an
  = GenPropertyDecl (Symbol an) -- ^ Record or group declaration property.
  | GenPropertyRecord (Value an) -- ^ Record (value) property.
  | GenPropertyGroup (GroupProperty an) -- ^ Group property.
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a group (properties will be symbols), or references it (properties will be group properties).
data Group an
  = Group
  { groupAnn :: an
  , groupHead :: Symbol an
  , groupProps :: [GenProperty an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a group - reducers below will be part of the group.
data GroupDecl an
  = GroupDecl
  { groupDeclAnn :: an
  , groupDeclGroup :: Group an
  , groupDeclRepeat :: Bool
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordIsFunc :: Bool
  , recordHead :: Symbol an
  , recordProps :: [GenProperty an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns a string identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be 'Nothing' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindSymbol :: Maybe (Symbol an) -- ^ The bound symbol, or "nil" for no binding.
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript, or a group.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  | ValueSpliceCode (SpliceCode an)
  | ValueGroup (Group an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An input or output of a reducer
data ReducerClause an
  = ReducerClause
  { reducerClauseAnn :: an
  , reducerClauseValue :: Value an
  , reducerClauseGroups :: [Group an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: ReducerClause an
  , reducerOutput :: ReducerClause an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Performs some transformations on values.
data Statement an
  = StatementGroup (Value an)
  | StatementReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Not nested in anything other than the program.
data TopLevel an
  = TopLevelRecordDecl (RecordDecl an)
  | TopLevelStatement (Statement an)
  | TopLevelGroupDecl (GroupDecl an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full TreeScript program.
data Program an
  = Program
  { programAnn :: an
  , programTopLevels :: [TopLevel an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

instance TreePrintable RecordDecl where
  treePrint par _ (RecordDecl _ record)
    = par record <> "."

instance TreePrintable SpliceText where
  treePrint par leaf spliceText = "'" <> printRest spliceText
    where printRest (SpliceTextNil _ txt) = leaf txt <> "'"
          printRest (SpliceTextCons _ txt val rst)
            = leaf txt <> printSpliced val <> printRest rst
          printSpliced val@(ValueBind _) = par val
          printSpliced val = "\\(" <> par val <> ")"

instance TreePrintable SpliceCode where
  treePrint par _ (SpliceCode _ lang txt) = par lang <> par txt

instance TreePrintable Primitive where
  treePrint _ leaf (PrimInteger _ int) = leaf int
  treePrint _ leaf (PrimFloat _ float) = leaf float
  treePrint _ leaf (PrimString _ string) = leaf string

instance TreePrintable Symbol where
  treePrint _ _ (Symbol _ lit) = fromLiteral lit

instance TreePrintable GroupProperty where
  treePrint par _ (GroupProperty _ key value) = par key <> ": " <> par value

instance TreePrintable GenProperty where
  treePrint par _ (GenPropertyDecl key) = par key
  treePrint par _ (GenPropertyRecord prop) = par prop
  treePrint par _ (GenPropertyGroup prop) = par prop

instance TreePrintable Group where
  treePrint par _ (Group _ head' props)
    = "&" <> par head' <> "[" <> mintercalate "; " (map par props) <> "]"

instance TreePrintable GroupDecl where
  treePrint par _ (GroupDecl _ group repeats)
    = par group <> ".\n" <> printRepeats
    where printRepeats
            | repeats = "==="
            | otherwise = "---"

instance TreePrintable Record where
  treePrint par _ (Record _ isFun head' props)
    = printIsFun <> par head' <> "[" <> mintercalate "; " (map par props) <> "]"
    where printIsFun
            | isFun = "#"
            | otherwise = mempty

instance TreePrintable Bind where
  treePrint par _ (Bind _ sym)
    = "\\" <> foldMap par sym

instance TreePrintable Value where
  treePrint par _ (ValuePrimitive prim) = par prim
  treePrint par _ (ValueRecord record) = par record
  treePrint par _ (ValueBind bind) = par bind
  treePrint par _ (ValueSpliceCode code) = par code
  treePrint par _ (ValueGroup group) = par group

instance TreePrintable ReducerClause where
  treePrint par _ (ReducerClause _ val groups)
    = mintercalate " " $ par val : map par groups

instance TreePrintable Reducer where
  treePrint par _ (Reducer _ input output)
    = par input <> ": " <> par output

instance TreePrintable Statement where
  treePrint par _ (StatementGroup group) = par group <> ";"
  treePrint par _ (StatementReducer red) = par red <> ";"

instance TreePrintable TopLevel where
  treePrint par _ (TopLevelRecordDecl decl) = par decl
  treePrint par _ (TopLevelStatement stmt) = par stmt
  treePrint par _ (TopLevelGroupDecl decl) = par decl

instance TreePrintable Program where
  treePrint par _ (Program _ topLevels) = mintercalate "\n" $ map par topLevels
