{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types for the @Core@ AST.
module TreeScript.Ast.Core.Types
  ( module TreeScript.Ast.Core.Types
  ) where

import TreeScript.Misc

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

-- | Name defines the next step
data SubPhase
  = Local -- ^ Index local binds
  | Module -- ^ Resolve modules
  | Combine -- ^ Combine with modules
  | Final -- ^ Done, now can be translated

data family Symbol (p :: SubPhpse) :: * -> *

data family GroupDef (p :: SubPhase) :: * -> *

class SubPhase a where
  type GroupDef a :: * -> *
  type Symbol a :: * -> *

-- | What reducers get from their parent group, or output values / groups from their reducer.
data LocalEnv
  = LocalEnv
  { localEnvBinds :: S.Set Int
  , localEnvGroups :: S.Set Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares a type of record or function but doesn't specifify property values.
data DeclCompact
  = DeclCompact
  { declCompactHead :: Symbol ()
  , declCompactNumProps :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: S.Set DeclCompact
  , declSetFunctions :: S.Set DeclCompact
  } deriving (Eq, Ord, Read, Show)

-- | Just a path, but in text form, with no extension and @""@ for builtin
type ModulePath = T.Text

-- | What the module is - how it works.
data ModuleType
  = ModuleTypeBuiltin -- ^ Builtin
  | ModuleTypeFile Bool Bool Bool -- ^ A source or compiled TreeScript, possibly wrapping a language or library
  | ModuleTypeDir -- ^ A directory - reexports other modules, may have symlinks.
  deriving (Eq, Ord, Read, Show)

data ImportDecl an
  = ImportDecl
  { importDeclAnn :: an
  , importDeclPath :: ModulePath
  , importDeclType :: ModuleType
  , importDeclQual :: T.Text -- Either empty or @<qual>_@
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | An identifier, such as a record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbolModule :: ModulePath
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: Symbol an
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
  , recordHead :: Symbol an
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

-- | The type and identifier of a group.
data GroupLoc an
  = GroupLocGlobal an (Symbol an)
  | GroupLocLocal an Int
  | GroupLocFunction an (Symbol an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc an
  , groupRefValueProps :: [Value an]
  , groupRefGroupProps :: [GroupRef an]
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
  , groupDefValueProps :: [Bind an]
  , groupDefGroupProps :: [Bind an]
  , groupDefReducers :: [Reducer an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full TreeScript program.
data Program an
  = Program
  { programAnn :: an
  , programPath :: ModulePath
  , programImportDecls :: [ImportDecl an]
  , programRecordDecls :: [RecordDecl an]
  , programGroups :: [GroupDef an]
  }

instance Semigroup DeclSet where
  DeclSet xRecs xFuns <> DeclSet yRecs yFuns
    = DeclSet
    { declSetRecords = xRecs <> yRecs
    , declSetFunctions = xFuns <> yFuns
    }

instance Monoid DeclSet where
  mempty
    = DeclSet
    { declSetRecords = mempty
    , declSetFunctions = mempty
    }

-- | Takes path of right program.
instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn _ xIdecls xMdecls xGroups <> Program yAnn yPath yIdecls yMdecls yGroups
    = Program
    { programAnn = xAnn <> yAnn
    , programPath = yPath
    , programImportDecls = xIdecls <> yIdecls
    , programRecordDecls = xMdecls <> yMdecls
    , programGroups = xGroups <> yGroups
    }

instance Printable ModuleType where
  pprint ModuleTypeBuiltin = "builtin"
  pprint (ModuleTypeFile cmp lang lib)
    = printCmp cmp <> printLang lang <> printLib lib
    where printCmp False = "tscr"
          printCmp True = "tprg"
          printLang False = ""
          printLang True = "+tlng"
          printLib False = ""
          printLib True = "+tlib"
  pprint ModuleTypeDir = "/"

instance Printable (ImportDecl an) where
  pprint (ImportDecl _ path typ qual)
    = "#import " <> pprint path <> " (" <> pprint typ <> ") " <> printQual
    where printQual
            | T.null qual = ""
            | otherwise = " => " <> T.dropEnd 1 qual

instance Printable (RecordDecl an) where
  pprint (RecordDecl _ head' props)
    = pprint head' <> printProps (map pprint props)

instance Printable (Primitive an) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

instance Printable (Symbol an) where
  pprint (Symbol _ md txt) = T.takeEnd modulePathPrintLength md <> "_" <> txt

instance Printable (Record an) where
  pprint (Record _ head' props)
    = pprint head' <> "[" <> T.intercalate ", " (map pprint props) <> "]"

instance Printable (Bind an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance Printable (Value an) where
  pprint (ValuePrimitive prim) = pprint prim
  pprint (ValueRecord record) = pprint record
  pprint (ValueBind bind) = pprint bind

instance Printable (GroupLoc an) where
  pprint (GroupLocGlobal _ sym) = "&" <> pprint sym
  pprint (GroupLocLocal _ idx) = "&" <> pprint idx
  pprint (GroupLocFunction _ sym) = "#" <> pprint sym

instance Printable (GroupRef an) where
  pprint (GroupRef _ loc vprops gprops)
    = pprint loc <> printProps (map pprint vprops ++ map pprint gprops)

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
  pprint (Program _ name idecls rdecls groups)
     = T.unlines
     $ ["#module " <> pprint name]
    ++ [T.empty]
    ++ map pprint idecls
    ++ [T.empty]
    ++ map pprint rdecls
    ++ [T.empty]
    ++ zipWith printGroupDef [0..] groups

-- | How much of the module path will be printed.
modulePathPrintLength :: Int
modulePathPrintLength = 5

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

printGroupDef :: Int -> GroupDef an -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
           = "&"
          <> pprint head'
          <> printProps (map pprint vprops ++ map printGroupDefProp gprops)
          <> "."
        printGroupDefProp (Bind _ idx) = "&" <> pprint idx

-- | Specifies that a record declaration can take any number of properties.
varNumProps :: Int
varNumProps = (-1)

localEnvInsertBinds :: S.Set Int -> LocalEnv -> LocalEnv
localEnvInsertBinds binds env
  = LocalEnv
  { localEnvBinds = binds <> localEnvBinds env
  , localEnvGroups = localEnvGroups env
  }

mkBuiltinSymbol :: T.Text -> Symbol ()
mkBuiltinSymbol txt
  = Symbol
  { symbolAnn = ()
  , symbolModule = ""
  , symbol = txt
  }

mkBuiltinDecl :: T.Text -> Int -> DeclCompact
mkBuiltinDecl head' numProps
  = DeclCompact
  { declCompactHead = mkBuiltinSymbol head'
  , declCompactNumProps = numProps
  }

compactRecordDecl :: RecordDecl an -> DeclCompact
compactRecordDecl (RecordDecl _ head' props)
  = DeclCompact
  { declCompactHead = remAnns head'
  , declCompactNumProps = length props
  }

builtinDecls :: DeclSet
builtinDecls
  = DeclSet
  { declSetRecords
      = S.fromList
      [ mkBuiltinDecl "T" varNumProps
      , mkBuiltinDecl "Unit" 0
      , mkBuiltinDecl "True" 0
      , mkBuiltinDecl "False" 0
      , mkBuiltinDecl "Nil" 0
      , mkBuiltinDecl "None" 0
      , mkBuiltinDecl "Some" 1
      , mkBuiltinDecl "Cons" 2
      , mkBuiltinDecl "Hole" 1
      ]
  , declSetFunctions = S.empty
  }

declSetToMap :: S.Set DeclCompact -> M.Map (Symbol ()) Int
declSetToMap = M.fromAscList . map declToTuple . S.toAscList
  where declToTuple (DeclCompact head' numProps) = (head', numProps)

hole :: an -> an -> an -> Int -> Value an
hole ann headAnn idxAnn idx
  = ValueRecord Record
  { recordAnn = ann
  , recordHead = Symbol
      { symbolAnn = headAnn
      , symbolModule = ""
      , symbol = "Hole"
      }
  , recordProps = [ValuePrimitive $ PrimInteger idxAnn idx]
  }

desugarError :: Range -> T.Text -> Error
desugarError rng msg
  = addRangeToErr rng $ Error
  { errorStage = StageDesugar
  , errorRange = Nothing
  , errorMsg = msg
  }
