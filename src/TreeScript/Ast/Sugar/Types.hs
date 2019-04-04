{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO Groups out of values, group statements with guards

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

data Splice an
  = SpliceBind (BindTarget an)
  | SpliceHole (HoleIdx an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An entire code block (in the future might also handle strings) which might contain splices.
data SpliceText an
  = SpliceTextNil an T.Text
  | SpliceTextCons an T.Text Bool (Splice an) (SpliceText an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A code block which might contain splices.
data SpliceCode an
  = SpliceCode
  { spliceCodeAnn :: an
  , spliceCodeLangugage :: Symbol an
  , spliceCodeText :: SpliceText an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A simple block of data.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An identifier, such as a record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A subgroup property.
data SubGroupProperty an
  = SubGroupProperty
  { subGroupPropertyAnn :: an
  , subGroupPropertySymbol :: Symbol an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Property of a record or group.
data GenProperty an
  = GenPropertyDecl (Symbol an) -- ^ Record  declaration property.
  | GenPropertySubGroup (SubGroupProperty an) -- ^ Subgroup declaration property.
  | GenPropertyRecord (Value an) -- ^ Record or subgroup property.
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a group (subgroups will be symbols), or references it (subgroups will be groups).
data Group an
  = Group
  { groupAnn :: an
  , groupIsProp :: Bool
  , groupHead :: Symbol an
  , groupSubgroups :: [GenProperty an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a group - reducers below will be part of the group.
data GroupDecl an
  = GroupDecl
  { groupDeclAnn :: an
  , groupDeclGroup :: Group an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordIsFunc :: Bool
  , recordHead :: Symbol an
  , recordProps :: [GenProperty an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Where the bind is read or written.
data BindTarget an
  = BindTargetNone an
  | BindTargetSome (Symbol an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns a string identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be 'Nothing' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindTarget :: BindTarget an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Index of a hole
data HoleIdx an
  = HoleIdx
  { holeIdxAnn :: an
  , holeIdxVal :: Int
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | @\<idx>@ - syntax sugar for @Hole[<idx>]@.
data Hole an
  = Hole
  { holeAnn :: an
  , holeIdx :: HoleIdx an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript, or a group.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  | ValueSpliceCode (SpliceCode an)
  | ValueHole (Hole an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches an input value against an output value.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [Group an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Not nested in anything other than the program.
data TopLevel an
  = TopLevelRecordDecl (RecordDecl an)
  | TopLevelReducer (Reducer an)
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

instance TreePrintable Splice where
  treePrint par _ (SpliceBind bind) = par bind
  treePrint par _ (SpliceHole hole) = par hole

instance TreePrintable SpliceText where
  treePrint par leaf spliceText = "'" <> printRest spliceText
    where printRest (SpliceTextNil _ txt) = leaf txt <> "'"
          printRest (SpliceTextCons _ txt isElps val rst)
            = leaf txt <> "\\" <> printEllipsis isElps <> printSpliced val <> printRest rst
          printEllipsis False = ""
          printEllipsis True = "..."
          printSpliced (SpliceBind tgt) = par tgt
          printSpliced (SpliceHole (HoleIdx _ idx)) = leaf idx

instance TreePrintable SpliceCode where
  treePrint par _ (SpliceCode _ lang txt) = par lang <> par txt

instance TreePrintable Primitive where
  treePrint _ leaf (PrimInteger _ int) = leaf int
  treePrint _ leaf (PrimFloat _ float) = leaf float
  treePrint _ leaf (PrimString _ string) = leaf string

instance TreePrintable Symbol where
  treePrint _ _ (Symbol _ lit) = fromLiteral lit

instance TreePrintable SubGroupProperty where
  treePrint par _ (SubGroupProperty _ sym) = "&" <> par sym

instance TreePrintable GenProperty where
  treePrint par _ (GenPropertyDecl key) = par key
  treePrint par _ (GenPropertySubGroup prop) = par prop
  treePrint par _ (GenPropertyRecord prop) = par prop

instance TreePrintable Group where
  treePrint par _ (Group _ _ head' sgs)
    = "&" <> par head' <> printSubgroups sgs
    where printSubgroups ps = "[" <> mintercalate "; " (map par ps) <> "]"

instance TreePrintable GroupDecl where
  treePrint par _ (GroupDecl _ group)
    = par group <> "."

instance TreePrintable Record where
  treePrint par _ (Record _ isFun head' props)
    = printIsFun <> par head' <> "[" <> mintercalate "; " (map par props) <> "]"
    where printIsFun
            | isFun = "#"
            | otherwise = mempty

instance TreePrintable BindTarget where
  treePrint _ _ (BindTargetNone _) = "_"
  treePrint par _ (BindTargetSome sym) = par sym

instance TreePrintable Bind where
  treePrint par _ (Bind _ tgt)
    = "\\" <> par tgt

instance TreePrintable HoleIdx where
  treePrint _ leaf (HoleIdx _ idx) = leaf idx

instance TreePrintable Hole where
  treePrint par _ (Hole _ idx)
    = "\\" <> par idx

instance TreePrintable Value where
  treePrint par _ (ValuePrimitive prim) = par prim
  treePrint par _ (ValueRecord record) = par record
  treePrint par _ (ValueBind bind) = par bind
  treePrint par _ (ValueSpliceCode code) = par code
  treePrint par _ (ValueHole hole) = par hole

instance TreePrintable Guard where
  treePrint par _ (Guard _ input output nexts)
    = par input <> " <- " <> par output <> foldMap printNext nexts
    where printNext next' = " " <> par next'

instance TreePrintable Reducer where
  treePrint par _ (Reducer _ (Guard _ input output nexts) guards)
     = par input
    <> " -> "
    <> par output
    <> foldMap printNext nexts
    <> foldMap printGuard guards
    <> ";"
    where printNext next' = " " <> par next'
          printGuard guard = ",\n  " <> par guard

instance TreePrintable TopLevel where
  treePrint par _ (TopLevelRecordDecl decl) = par decl
  treePrint par _ (TopLevelReducer red) = par red <> ";"
  treePrint par _ (TopLevelGroupDecl decl) = par decl

instance TreePrintable Program where
  treePrint par _ (Program _ topLevels) = mintercalate "\n" $ map par topLevels
