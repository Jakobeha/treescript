{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Balance@ AST.
module TreeScript.Ast.Balance.Types
  ( module TreeScript.Ast.Balance.Types
  , module TreeScript.Ast.Lex.Types
  )
where

import           TreeScript.Ast.Lex.Types (Quote, SymbolCase)
import           TreeScript.Misc

import qualified Data.Text                     as T
import           GHC.Generics

data SpliceText an
  = SpliceTextNil an T.Text
  | SpliceTextCons an T.Text Bool (Node an) (SpliceText an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A code block which might contain splices.
data Quoted an
  = Quoted
  { quotedAnn :: an
  , quotedQuote :: Quote
  , quotedText :: SpliceText an
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
  , symbolCase ::
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Part of a type which could be a union.
data TypePart an
  = TypePartSymbol an (Symbol an)
  | TypePartTransparent an (Record an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A type.
data Type an
  = Type
  { typeAnn :: an
  , typeParts :: [TypePart an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a type alias.
data TypeAlias an
  = TypeAlias
  { typeAliasAnn :: an
  , typeAliasAlias :: Symbol an
  , typeAliasType :: Type an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A subgroup property.
data SubGroupProperty an
  = SubGroupProperty
  { subGroupPropertyAnn :: an
  , subGroupPropertySymbol :: Symbol an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Property of a record or group.
data GenProperty an
  = GenPropertyDecl (Type an) -- ^ Record  declaration property.
  | GenPropertySubGroup (SubGroupProperty an) -- ^ Subgroup declaration property.
  | GenPropertyRecord (Value an) -- ^ Record property.
  | GenPropertyGroup (Group an) -- ^ Subgroup reference property.
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Where this group is, its type.
data GroupLoc an
  = GroupLocGlobal an
  | GroupLocLocal an
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a group (subgroups will be symbols), or references it (subgroups will be groups).
data Group an
  = Group
  { groupAnn :: an
  , groupLoc :: GroupLoc an
  , groupHead :: Symbol an
  , groupProps :: [GenProperty an]
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

data Next an
  = NextEval an
  | NextGroup (Group an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches an input value against an output value.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [Next an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Whether this is a regular reducer or type cast.
data ReducerType an
  = ReducerTypeReg an
  | ReducerTypeCast an
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerType :: ReducerType an
  , reducerMain :: Guard an
  , reducerGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Not nested in anything other than the program.
data TopLevel an
  = TopLevelImportDecl (ImportDecl an)
  | TopLevelRecordDecl (RecordDecl an)
  | TopLevelGroupDecl (GroupDecl an)
  | TopLevelTypeAlias (TypeAlias an)
  | TopLevelReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full TreeScript program.
data Program an
  = Program
  { programAnn :: an
  , programTopLevels :: [TopLevel an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

instance TreePrintable ImportDecl where
  treePrint par _ (ImportDecl _ lit mdl qual) =
    "#" <> par lit <> " " <> par mdl <> foldMap printQual qual
    where printQual = (" -> " <>) . par

instance TreePrintable RecordDecl where
  treePrint par _ (RecordDecl _ record) = par record <> "."

instance TreePrintable Splice where
  treePrint par _ (SpliceBind bind) = par bind
  treePrint par _ (SpliceHole hole) = par hole

instance TreePrintable SpliceText where
  treePrint par leaf spliceText = "'" <> printRest spliceText
   where
    printRest (SpliceTextNil _ txt) = leaf txt <> "'"
    printRest (SpliceTextCons _ txt isElps val rst) =
      leaf txt
        <> "\\"
        <> printEllipsis isElps
        <> printSpliced val
        <> printRest rst
    printEllipsis False = ""
    printEllipsis True  = "..."
    printSpliced (SpliceBind tgt            ) = par tgt
    printSpliced (SpliceHole (HoleIdx _ idx)) = leaf idx

instance TreePrintable SpliceCode where
  treePrint par _ (SpliceCode _ lang txt) = par lang <> par txt

instance TreePrintable Primitive where
  treePrint _ leaf (PrimInteger _ int   ) = leaf int
  treePrint _ leaf (PrimFloat   _ float ) = leaf float
  treePrint _ leaf (PrimString  _ string) = leaf string

instance TreePrintable Symbol where
  treePrint _ _ (Symbol _ lit) = fromLiteral lit

instance TreePrintable TypePart where
  treePrint par _ (TypePartSymbol      _ sym) = "@" <> par sym
  treePrint par _ (TypePartTransparent _ x  ) = "@" <> par x

instance TreePrintable Type where
  treePrint par _ (Type _ parts) = mintercalate "|" $ map par parts

instance TreePrintable TypeAlias where
  treePrint par _ (TypeAlias _ ali typ) =
    "@" <> par ali <> " <- " <> par typ <> ";"

instance TreePrintable SubGroupProperty where
  treePrint par _ (SubGroupProperty _ sym) = "&" <> par sym

instance TreePrintable GenProperty where
  treePrint par _ (GenPropertyDecl     prop) = par prop
  treePrint par _ (GenPropertySubGroup prop) = par prop
  treePrint par _ (GenPropertyRecord   prop) = par prop
  treePrint par _ (GenPropertyGroup    prop) = par prop

instance TreePrintable GroupLoc where
  treePrint _ _ (GroupLocGlobal _) = "&"
  treePrint _ _ (GroupLocLocal  _) = "&"

instance TreePrintable Group where
  treePrint par _ (Group _ loc head' props) =
    par loc <> par head' <> printProps (map par props)

instance TreePrintable GroupDecl where
  treePrint par _ (GroupDecl _ group) = par group <> "."

instance TreePrintable Record where
  treePrint par _ (Record _ head' props) =
    par head' <> printProps (map par props)

instance TreePrintable BindTarget where
  treePrint _   _ (BindTargetNone _  ) = "_"
  treePrint par _ (BindTargetSome sym) = par sym

instance TreePrintable Bind where
  treePrint par _ (Bind _ tgt) = "\\" <> par tgt

instance TreePrintable HoleIdx where
  treePrint _ leaf (HoleIdx _ idx) = leaf idx

instance TreePrintable Hole where
  treePrint par _ (Hole _ idx) = "\\" <> par idx

instance TreePrintable Value where
  treePrint par _ (ValuePrimitive  prim  ) = par prim
  treePrint par _ (ValueRecord     record) = par record
  treePrint par _ (ValueBind       bind  ) = par bind
  treePrint par _ (ValueSpliceCode code  ) = par code
  treePrint par _ (ValueHole       hole  ) = par hole

instance TreePrintable Next where
  treePrint _   _ NextEval        = "!"
  treePrint par _ (NextGroup grp) = par grp

instance TreePrintable Guard where
  treePrint par _ (Guard _ input output nexts) =
    par input <> " <- " <> par output <> foldMap printNext nexts
    where printNext next' = " " <> par next'

instance TreePrintable ReducerType where
  treePrint _ _ (ReducerTypeReg  _) = "->"
  treePrint _ _ (ReducerTypeCast _) = "=>"

instance TreePrintable Reducer where
  treePrint par _ (Reducer _ typ (Guard _ input output nexts) guards) =
    par input
      <> " "
      <> par typ
      <> " "
      <> par output
      <> foldMap printNext  nexts
      <> foldMap printGuard guards
      <> ";"
   where
    printNext next' = " " <> par next'
    printGuard guard = ",\n  " <> par guard

instance TreePrintable TopLevel where
  treePrint par _ (TopLevelImportDecl decl) = par decl
  treePrint par _ (TopLevelRecordDecl decl) = par decl
  treePrint par _ (TopLevelGroupDecl  decl) = par decl
  treePrint par _ (TopLevelTypeAlias  ali ) = par ali
  treePrint par _ (TopLevelReducer    red ) = par red

instance TreePrintable Program where
  treePrint par _ (Program _ topLevels) = mintercalate "\n" $ map par topLevels

nullSymbol :: Symbol Range
nullSymbol = Symbol r0 ""

printProps :: (PrintOut o) => [o] -> o
printProps ps = "[" <> mintercalate ", " ps <> "]"
