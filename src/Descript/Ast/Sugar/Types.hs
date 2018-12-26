{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Sugar@ AST.
module Descript.Ast.Sugar.Types
  ( module Descript.Ast.Sugar.Types
  ) where

import Descript.Misc

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
  | PrimCode (SpliceCode an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An identifier, such as sa record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A record property.
data Property an
  = Property
  { propertyAnn :: an
  , propertyKey :: Symbol an
  , propertyValue :: Value an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Either a record property or property declaration (record declaration property).
data GenProperty an
  = GenPropertyDecl (Symbol an)
  | GenProperty (Property an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordHead :: Symbol an
  , recordProps :: [GenProperty an]
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An input abstraction. Matches a value against a predicate, or matches any value (if the predicate is 'None').
data Matcher an
  = Matcher
  { matcherAnn :: an
  , matcherBind :: Maybe (Symbol an) -- ^ The symbol which refers to the matched value in an output path or code block.
  , matcherPred :: Maybe (Value an) -- ^ The predicate the value is matched against.
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An output abstraction. Refers to the whole or part of the value which was matched.
data Path an
  = Path
  { pathAnn :: an
  , pathBind :: Symbol an -- ^ The symbol of the matcher which the path refers to.
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in Descript. This is a liberal grammar, so any value can encode any abstraction.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueMatcher (Matcher an)
  | ValuePath (Path an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: Value an
  , reducerOutput :: Value an
  } deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Not nested in anything other than the program.
data TopLevel an
  = TopLevelRecordDecl (RecordDecl an)
  | TopLevelReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Printable, ReducePrintable, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full Descript program.
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
            = leaf txt <> "\\(" <> par val <> ")" <> printRest rst

instance TreePrintable SpliceCode where
  treePrint par _ (SpliceCode _ lang txt) = par lang <> par txt

instance TreePrintable Primitive where
  treePrint _ leaf (PrimInteger _ int) = leaf int
  treePrint _ leaf (PrimFloat _ float) = leaf float
  treePrint _ leaf (PrimString _ string) = leaf string
  treePrint par _ (PrimCode code) = par code

instance TreePrintable Symbol where
  treePrint _ _ (Symbol _ lit) = fromLiteral lit

instance TreePrintable Property where
  treePrint par _ (Property _ key value)
    = par key <> ": " <> par value

instance TreePrintable GenProperty where
  treePrint par _ (GenPropertyDecl key) = par key
  treePrint par _ (GenProperty prop) = par prop

instance TreePrintable Record where
  treePrint par _ (Record _ head' props)
    = par head' <> "[" <> mintercalate "; " (map par props) <> "]"

instance TreePrintable Matcher where
  treePrint par _ (Matcher _ bind pred)
    = "<" <> printBind bind <> printPred pred
    where printBind Nothing = mempty
          printBind (Just bind') = par bind'
          printPred Nothing = mempty
          printPred (Just pred') = "?" <> par pred'

instance TreePrintable Path where
  treePrint par _ (Path _ bind) = ">" <> par bind

instance TreePrintable Value where
  treePrint par _ (ValuePrimitive prim) = par prim
  treePrint par _ (ValueRecord record) = par record
  treePrint par _ (ValueMatcher matcher) = par matcher
  treePrint par _ (ValuePath path) = par path

instance TreePrintable Reducer where
  treePrint par _ (Reducer _ input output)
    = par input <> ": " <> par output <> ";"

instance TreePrintable TopLevel where
  treePrint par _ (TopLevelRecordDecl decl) = par decl
  treePrint par _ (TopLevelReducer red) = par red

instance TreePrintable Program where
  treePrint par _ (Program _ topLevels) = mintercalate "\n" $ map par topLevels
