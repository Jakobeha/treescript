{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Core@ AST.
module TreeScript.Ast.Core.Types
  ( module TreeScript.Ast.Core.Types
  ) where

import TreeScript.Ast.Core.Serialize
import TreeScript.Misc

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics

-- | Declares a type of record but doesn't specifify property values.
data RecordDeclCompact
  = RecordDeclCompact
  { recordDeclCompactHead :: T.Text
  , recordDeclCompactNumProps :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares a type of group but doesn't specifify property values.
data GroupDeclCompact
  = GroupDeclCompact
  { groupDeclCompactHead :: T.Text
  , groupDeclCompactNumValueProps :: Int
  , groupDeclCompactNumGroupProps :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares a type of function but doesn't specifify property values.
data FunctionDeclCompact
  = FunctionDeclCompact
  { functionDeclCompactHead :: T.Text
  , functionDeclCompactNumProps :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: M.Map T.Text Int
  , declSetGroups :: M.Map T.Text (Int, Int)
  , declSetFunctions :: M.Map T.Text Int
  } deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | Determines which module a symbol is located, so 2 symbols with the same name don't conflict.
type ModulePath = T.Text

data ImportDecl an
  = ImportDecl
  { importDeclAnn :: an
  , importDeclPath :: ModulePath
  , importDeclQual :: T.Text
  , importDeclExps :: DeclSet
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: T.Text
  , recordDeclProps :: [T.Text]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of symbol (used in resolution).
data SymbolType
  = SymbolTypeRecord
  | SymbolTypeGroup
  | SymbolTypeFunction
  deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | An identifier, such as a record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbolModule :: ModulePath
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordHead :: Symbol an
  , recordProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The type and identifier of a group.
data GroupLoc an
  = GroupLocGlobal an (Symbol an)
  | GroupLocLocal an Int
  | GroupLocFunction an (Symbol an)
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc an
  , groupRefValueProps :: [Value an]
  , groupRefGroupProps :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerSubGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef e1 e2 an
  = GroupDef
  { groupDefAnn :: an
  , groupDefValueProps :: [(e1, Bind an)]
  , groupDefGroupProps :: [(e1, Bind an)]
  , groupDefReducers :: [Reducer an]
  , groupDefPropEnv :: e2
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A foreign program this script uses.
data Library
  = LibraryCmdBinary B.ByteString
  | LibraryJavaScript T.Text
  deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | TODO Make import and record decls a removable type param.

-- | A full TreeScript program.
data Program e1 e2 an
  = Program
  { programAnn :: an
  , programPath :: ModulePath
  , programImportDecls :: [ImportDecl an]
  , programRecordDecls :: [RecordDecl an]
  , programExports :: DeclSet
  , programGroups :: M.Map (Symbol ()) (GroupDef e1 e2 an)
  , programLibraries :: M.Map ModulePath Library
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

instance Semigroup DeclSet where
  DeclSet xRecs xGrps xFuns <> DeclSet yRecs yGrps yFuns
    = DeclSet
    { declSetRecords = xRecs <> yRecs
    , declSetGroups = xGrps <> yGrps
    , declSetFunctions = xFuns <> yFuns
    }

instance Monoid DeclSet where
  mempty
    = DeclSet
    { declSetRecords = mempty
    , declSetGroups = mempty
    , declSetFunctions = mempty
    }

-- | Takes path and exports of left, unless empty (to satisfy @Monoid@).
-- TODO Fix all right program paths.
instance (Semigroup an) => Semigroup (Program e1 e2 an) where
  Program xAnn xPath xIdecls xRdecls xExps xGrps xLibs <> Program yAnn yPath _ _ _ yGrps yLibs
    = Program
    { programAnn = xAnn <> yAnn
    , programPath = path
    , programImportDecls = xIdecls
    , programRecordDecls = xRdecls
    , programExports = xExps
    , programGroups = xGrps <> yGrps
    , programLibraries = xLibs <> yLibs
    }
    where path
            | xPath == "" = yPath
            | otherwise = xPath

-- | No path.
instance (Monoid an) => Monoid (Program e1 e2 an) where
  mempty
    = Program
    { programAnn = mempty
    , programPath = ""
    , programImportDecls = mempty
    , programRecordDecls = mempty
    , programExports = mempty
    , programGroups = mempty
    , programLibraries = mempty
    }

instance Printable DeclSet where
  pprint (DeclSet repxs gexps fexps)
     = foldMap (",\n  " <>) (M.keys repxs)
    <> foldMap (",\n  &" <>) (M.keys gexps)
    <> foldMap (",\n  #" <>) (M.keys fexps)

instance Printable (ImportDecl an) where
  pprint (ImportDecl _ path qual exps)
    = "#import " <> pprint path <> printQual <> pprint exps <> ";"
    where printQual
            | T.null qual = ""
            | otherwise = " => " <> T.dropEnd 1 qual

instance Printable (RecordDecl an) where
  pprint (RecordDecl _ head' props)
    = head' <> printProps props

instance Printable (Symbol an) where
  pprint (Symbol _ md txt)
    | md == "" = txt
    | otherwise = md <> "_" <> txt

instance Printable (Primitive an) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

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

instance Printable Library where
  pprint (LibraryCmdBinary _) = "<command-line binary>"
  pprint (LibraryJavaScript txt) = pprint txt

instance (e1 ~ T.Text) => Printable (Program e1 e2 an) where
  pprint (Program _ mpath idecls rdecls _ grps libs)
     = T.unlines
     $ ["#module " <> mpath <> ";"]
    ++ map pprint idecls
    ++ [T.empty]
    ++ map pprint rdecls
    ++ [T.empty]
    ++ map (uncurry printGroupDef) (M.toList grps)
    ++ map (uncurry printLibrary) (M.toList libs)

-- TODO: Print env
printGroupDef :: Symbol () -> GroupDef T.Text e2 an -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds _)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
          = "&"
          <> pprint head'
          <> printProps (map (printGroupDefProp "\\") vprops ++ map (printGroupDefProp "&") gprops)
          <> "."
        printGroupDefProp pre (txt, (Bind _ idx)) = pre <> txt <> "=" <> pprint idx

printLibrary :: ModulePath -> Library -> T.Text
printLibrary path lib = "--- " <> path <> "\n" <> pprint lib

-- | How much of the module path will be printed.
modulePathPrintLength :: Int
modulePathPrintLength = 5

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

mkDeclSet :: [RecordDeclCompact] -> [GroupDeclCompact] -> [FunctionDeclCompact] -> DeclSet
mkDeclSet rdecls gdecls fdecls
  = DeclSet
  { declSetRecords = M.fromList $ map (\(RecordDeclCompact head' nps) -> (head', nps)) rdecls
  , declSetGroups = M.fromList $ map (\(GroupDeclCompact head' nvps ngps) -> (head', (nvps, ngps))) gdecls
  , declSetFunctions = M.fromList $ map (\(FunctionDeclCompact head' nps) -> (head', nps)) fdecls
  }

declSetContains :: SymbolType -> T.Text -> DeclSet -> Bool
declSetContains SymbolTypeRecord local = M.member local . declSetRecords
declSetContains SymbolTypeGroup local = M.member local . declSetGroups
declSetContains SymbolTypeFunction local = M.member local . declSetFunctions

-- | Specifies that a record declaration can take any number of properties.
varNumProps :: Int
varNumProps = (-1)

mkBuiltinSymbol :: T.Text -> Symbol ()
mkBuiltinSymbol txt
  = Symbol
  { symbolAnn = ()
  , symbolModule = ""
  , symbol = txt
  }

compactRecordDecl :: RecordDecl an -> RecordDeclCompact
compactRecordDecl (RecordDecl _ head' props)
  = RecordDeclCompact
  { recordDeclCompactHead = head'
  , recordDeclCompactNumProps = length props
  }

builtinDecls :: DeclSet
builtinDecls
  = mkDeclSet
  [ RecordDeclCompact "T" varNumProps
  , RecordDeclCompact "Unit" 0
  , RecordDeclCompact "True" 0
  , RecordDeclCompact "False" 0
  , RecordDeclCompact "Nil" 0
  , RecordDeclCompact "None" 0
  , RecordDeclCompact "Some" 1
  , RecordDeclCompact "Cons" 2
  , RecordDeclCompact "Hole" 1
  ] [] []

hole :: an -> an -> Int -> Value an
hole ann idxAnn idx
  = ValueRecord Record
  { recordAnn = ann
  , recordHead = Symbol
      { symbolAnn = ann
      , symbolModule = ""
      , symbol = "Hole"
      }
  , recordProps = [ValuePrimitive $ PrimInteger idxAnn idx]
  }

desugarError_ :: T.Text -> Error
desugarError_ msg
  = Error
  { errorStage = StageDesugar
  , errorRange = Nothing
  , errorMsg = msg
  }

desugarError :: Range -> T.Text -> Error
desugarError rng = addRangeToErr rng . desugarError_
