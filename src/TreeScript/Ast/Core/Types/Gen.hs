{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Types for the @Core@ AST, includes @Final@ (bare) phase.
module TreeScript.Ast.Core.Types.Gen
  ( module TreeScript.Ast.Core.Types.Gen
  ) where

import TreeScript.Ast.Core.Classes
import TreeScript.Misc

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

data Term a where
  TTypePart :: Term TypePart
  TType :: Term Type
  TPrim :: Term Primitive
  TSymbol :: Term Symbol
  TRecord :: Term Record
  TBind :: Term Bind
  TValue :: Term Value
  TGroupLoc :: Term GroupLoc
  TGroupRef :: Term GroupRef
  TGuard :: Term Guard
  TReducer :: Term Reducer
  TGroupDef :: Term GroupDef
  TProgram :: Term Program

-- | Final AST, stripped of all extra information.
type PFA a an = a () () () () () () CastRef an
type PF a = PFA a ()

-- | A non-record type part.
data PrimType
  = PrimTypeAny
  | PrimTypeInteger
  | PrimTypeFloat
  | PrimTypeString
  deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | Whether the record is a regular or special type.
data RecordKind
  = RecordKindTuple
  | RecordKindCons
  | RecordKindRegular T.Text
  deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | Part of a value type (which is a union).
data TypePart a1 a2 a3 a4 a5 a6 t an
  = TypePartPrim an PrimType
  | TypePartRecord an (Symbol a1 a2 a3 a4 a5 a6 t an)
  | TypePartTuple an [Type a1 a2 a3 a4 a5 a6 t an]
  | TypePartList an (Type a1 a2 a3 a4 a5 a6 t an)
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A value type.
data Type a1 a2 a3 a4 a5 a6 t an
  = Type
  { typeAnn :: an
  , typeParts :: [TypePart a1 a2 a3 a4 a5 a6 t an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a type cast: a single reducer applied to a value like a function.
data CastRef
  = CastRef
  { castRefIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic) -- SOON add serial

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: M.Map T.Text (Maybe [PF Type])
  , declSetFunctions :: M.Map T.Text ([PF Type], PF Type)
  , declSetGroups :: M.Map T.Text (Int, Int)
  , declSetAliases :: M.Map T.Text (PF Type)
  } deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | Determines which module a symbol is located, so 2 symbols with the same name don't conflict.
type ModulePath = T.Text

-- | Generated a module was imported.
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
  , recordDeclProps :: [PFA Type an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a type of group but doesn't specifify property values.
data GroupDecl
  = GroupDecl
  { groupDeclHead :: T.Text
  , groupDeclNumValueProps :: Int
  , groupDeclNumGroupProps :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares a function.
data FunctionDecl an
  = FunctionDecl
  { functionDeclAnn :: an
  , functionDeclHead :: T.Text
  , functionDeclProps :: [PFA Type an]
  , functionDeclOutput :: PFA Type an
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a type alias.
data TypeAlias an
  = TypeAlias
  { typeAliasAnn :: an
  , typeAliasAlias :: T.Text
  , typeAliasType :: PFA Type an
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive a1 a2 a3 a4 a5 a6 t an
  = PrimInteger an t Int
  | PrimFloat an t Float
  | PrimString an t T.Text
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of symbol (used in resolution).
data SymbolType a where
  SymbolTypeRecord :: SymbolType (Maybe [PF Type])
  SymbolTypeGroup :: SymbolType (Int, Int)
  SymbolTypeFunction :: SymbolType ([PF Type], PF Type)
  SymbolTypeAlias :: SymbolType (PF Type)

-- | An identifier, such as a record head or property key.
data Symbol a1 a2 a3 a4 a5 a6 t an
  = Symbol
  { symbolAnn :: an
  , symbolModule :: ModulePath
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record a1 a2 a3 a4 a5 a6 t an
  = Record
  { recordAnn :: an
  , recordType :: t
  , recordHead :: Symbol a1 a2 a3 a4 a5 a6 t an
  , recordProps :: [Value a1 a2 a3 a4 a5 a6 t an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind a1 a2 a3 a4 a5 a6 t an
  = Bind
  { bindAnn :: an
  , bindType :: t
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript.
data Value a1 a2 a3 a4 a5 a6 t an
  = ValuePrimitive (Primitive a1 a2 a3 a4 a5 a6 t an)
  | ValueRecord (Record a1 a2 a3 a4 a5 a6 t an)
  | ValueBind (Bind a1 a2 a3 a4 a5 a6 t an)
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The type and identifier of a group.
data GroupLoc a1 a2 a3 a4 a5 a6 t an
  = GroupLocGlobal an (Symbol a1 a2 a3 a4 a5 a6 t an)
  | GroupLocLocal an Int
  | GroupLocFunction an (Symbol a1 a2 a3 a4 a5 a6 t an)
  deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef a1 a2 a3 a4 a5 a6 t an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc a1 a2 a3 a4 a5 a6 t an
  , groupRefValueProps :: [Value a1 a2 a3 a4 a5 a6 t an]
  , groupRefGroupProps :: [GroupRef a1 a2 a3 a4 a5 a6 t an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard a1 a2 a3 a4 a5 a6 t an
  = Guard
  { guardAnn :: an
  , guardInput :: Value a1 a2 a3 a4 a5 a6 t an
  , guardOutput :: Value a1 a2 a3 a4 a5 a6 t an
  , guardNexts :: [GroupRef a1 a2 a3 a4 a5 a6 t an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer a1 a2 a3 a4 a5 a6 t an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard a1 a2 a3 a4 a5 a6 t an
  , reducerSubGuards :: [Guard a1 a2 a3 a4 a5 a6 t an]
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef a1 a2 a3 a4 a5 a6 t an
  = GroupDef
  { groupDefAnn :: an
  , groupDefValueProps :: [(a5, Int)]
  , groupDefGroupProps :: [(a5, Int)]
  , groupDefReducers :: [Reducer a1 a2 a3 a4 a5 a6 t an]
  , groupDefPropEnv :: a6
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A foreign program this script uses.
data Library
  = LibraryCmdBinary B.ByteString
  | LibraryJavaScript T.Text
  deriving (Eq, Ord, Read, Show, Generic, Serial)

-- | A full TreeScript program.
data Program a1 a2 a3 a4 a5 a6 t an
  = Program
  { programAnn :: an
  , programPath :: ModulePath
  , programImportDecls :: a1
  , programRecordDecls :: a2
  , programFunctionDecls :: a3
  , programTypeAliases :: a4
  , programExports :: DeclSet
  , programCastReducers :: [Reducer a1 a2 a3 a4 a5 a6 t an]
  , programGroups :: M.Map (PF Symbol) (GroupDef a1 a2 a3 a4 a5 a6 t an)
  , programLibraries :: M.Map ModulePath Library
  } deriving (Eq, Ord, Read, Show, Generic, Serial, Functor, Foldable, Traversable, Generic1, Annotatable)

class PrintableArr a where
  printArr :: a -> [T.Text]

class PrintableBid a where
  printBid :: a -> T.Text

instance PrintableArr () where
  printArr () = []

instance (Printable a) => PrintableArr [a] where
  printArr = (++ [T.empty]) . map pprint

instance PrintableBid () where
  printBid () = ""

instance PrintableBid T.Text where
  printBid txt = txt <> "="

instance Semigroup DeclSet where
  DeclSet xRecs xFuns xGrps xAlis <> DeclSet yRecs yFuns yGrps yAlis
    = DeclSet
    { declSetRecords = xRecs <> yRecs
    , declSetFunctions = xFuns <> yFuns
    , declSetGroups = xGrps <> yGrps
    , declSetAliases = xAlis <> yAlis
    }

instance Monoid DeclSet where
  mempty
    = DeclSet
    { declSetRecords = mempty
    , declSetFunctions = mempty
    , declSetGroups = mempty
    , declSetAliases = mempty
    }

-- | Takes path and exports of left, unless empty (to satisfy @Monoid@).
-- TODO Fix all right program paths.
instance (Semigroup an) => Semigroup (Program a1 a2 a3 a4 a5 a6 t an) where
  Program xAnn xPath xIdecls xRdecls xFdecls xAlis xExps xCastReds xGrps xLibs <> Program yAnn yPath _ _ _ _ _ yCastReds yGrps yLibs
    = Program
    { programAnn = xAnn <> yAnn
    , programPath = path
    , programImportDecls = xIdecls
    , programRecordDecls = xRdecls
    , programFunctionDecls = xFdecls
    , programTypeAliases = xAlis
    , programExports = xExps
    , programCastReducers = xCastReds <> yCastReds
    , programGroups = xGrps <> yGrps
    , programLibraries = xLibs <> yLibs
    }
    where path
            | xPath == "" = yPath
            | otherwise = xPath

-- | Has undefineds where it doesn't satisfy @Monoid@ rules.
instance (Monoid an) => Monoid (Program a1 a2 a3 a4 a5 a6 t an) where
  mempty
    = Program
    { programAnn = mempty
    , programPath = undefined
    , programImportDecls = undefined
    , programRecordDecls = undefined
    , programFunctionDecls = undefined
    , programTypeAliases = undefined
    , programExports = mempty
    , programCastReducers = mempty
    , programGroups = mempty
    , programLibraries = mempty
    }

instance FunctorAst TypePart where
  mapA f (TypePartPrim ann prm) = TypePartPrim (mapAAnn f ann) prm
  mapA f (TypePartRecord ann sym) = TypePartRecord (mapAAnn f ann) (mapA f sym)
  mapA f (TypePartTuple ann elms) = TypePartTuple (mapAAnn f ann) (map (mapA f) elms)
  mapA f (TypePartList ann elm) = TypePartList (mapAAnn f ann) (mapA f elm)

instance FunctorAst Type where
  mapA f (Type ann parts) = Type (mapAAnn f ann) (map (mapA f) parts)

instance FunctorAst Primitive where
  mapA f (PrimInteger ann typ x)
    = PrimInteger
    ( mapAAnn f ann )
    ( mapAType f typ )
    ( x )
  mapA f (PrimFloat ann typ x)
    = PrimFloat
    ( mapAAnn f ann )
    ( mapAType f typ )
    ( x )
  mapA f (PrimString ann typ x)
    = PrimString
    ( mapAAnn f ann )
    ( mapAType f typ )
    ( x )

instance FunctorAst Symbol where
  mapA f (Symbol ann mdl txt)
    = Symbol
    ( mapAAnn f ann )
    ( mdl )
    ( txt )

instance FunctorAst Record where
  mapA f (Record ann typ head' props)
    = Record
    ( mapAAnn f ann )
    ( mapAType f typ )
    ( mapA f head' )
    ( map (mapA f) props )

instance FunctorAst Bind where
  mapA f (Bind ann typ idx)
    = Bind
    ( mapAAnn f ann )
    ( mapAType f typ )
    ( idx )

instance FunctorAst Value where
  mapA f (ValuePrimitive x)
    = ValuePrimitive $ mapA f x
  mapA f (ValueRecord x)
    = ValueRecord $ mapA f x
  mapA f (ValueBind x)
    = ValueBind $ mapA f x

instance FunctorAst GroupLoc where
  mapA f (GroupLocGlobal ann sym)
    = GroupLocGlobal
    ( mapAAnn f ann )
    ( mapA f sym )
  mapA f (GroupLocLocal ann idx)
    = GroupLocLocal
    ( mapAAnn f ann )
    ( idx )
  mapA f (GroupLocFunction ann sym)
    = GroupLocFunction
    ( mapAAnn f ann )
    ( mapA f sym )

instance FunctorAst GroupRef where
  mapA f (GroupRef ann loc vprops gprops)
    = GroupRef
    ( mapAAnn f ann )
    ( mapA f loc )
    ( map (mapA f) vprops )
    ( map (mapA f) gprops )

instance FunctorAst Guard where
  mapA f (Guard ann inp out nexts)
    = Guard
    ( mapAAnn f ann )
    ( mapA f inp )
    ( mapA f out )
    ( map (mapA f) nexts )

instance FunctorAst Reducer where
  mapA f (Reducer ann main guards)
    = Reducer
    ( mapAAnn f ann )
    ( mapA f main )
    ( map (mapA f) guards )

instance FunctorAst GroupDef where
  mapA f (GroupDef ann vprops gprops reds env)
    = GroupDef
    ( mapAAnn f ann )
    ( map mapProp vprops )
    ( map mapProp gprops )
    ( map (mapA f) reds )
    ( mapAPropEnv f env )
    where mapProp (bid, idx) = (mapABid f bid, idx)

instance FunctorAst Program where
  mapA f (Program ann path idecls rdecls fdecls alis exps castReds grps libs)
    = Program
    ( mapAAnn f ann )
    ( path )
    ( mapAIdecls f idecls )
    ( mapARdecls f rdecls )
    ( mapAFdecls f fdecls )
    ( mapAAliases f alis )
    ( exps )
    ( mapA f <$> castReds )
    ( mapA f <$> grps )
    ( libs )

instance Printable PrimType where
  pprint PrimTypeAny = "any"
  pprint PrimTypeInteger = "int"
  pprint PrimTypeFloat = "float"
  pprint PrimTypeString = "string"

instance Printable (TypePart a1 a2 a3 a4 a5 a6 t an) where
  pprint (TypePartPrim _ x) = "@" <> pprint x
  pprint (TypePartRecord _ name) = "@" <> pprint name
  pprint (TypePartTuple _ props) = "@t" <> printProps (map pprint props)
  pprint (TypePartList _ prop) = "@list[" <> pprint prop <> "]"

instance Printable (Type a1 a2 a3 a4 a5 a6 t an) where
  pprint (Type _ parts)
    = T.intercalate "|" (map pprint parts)

instance Printable DeclSet where
  pprint (DeclSet alis rexps gexps fexps)
     = foldMap (",\n  @" <>) (M.keys alis)
    <> foldMap (",\n  " <>) (M.keys rexps)
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
    = head' <> printProps (map pprint props) <> "."

instance Printable (FunctionDecl an) where
  pprint (FunctionDecl _ head' props ret)
    = "#" <> head' <> printProps (map pprint props) <> " -> " <> pprint ret <> "."

instance Printable (TypeAlias an) where
  pprint (TypeAlias _ ali typ) = "@" <> pprint ali <> " -> " <> pprint typ <> ";"

instance Printable CastRef where
  pprint (CastRef mdl idx) = mdl <> ":" <> pprint idx

instance (Printable t) => Printable (Primitive a1 a2 a3 a4 a5 a6 t an) where
  pprint (PrimInteger _ typ int) = pprint typ <> pprint int
  pprint (PrimFloat _ typ float) = pprint typ <> pprint float
  pprint (PrimString _ typ str) = pprint typ <> pprint str

instance Printable (Symbol a1 a2 a3 a4 a5 a6 t an) where
  pprint (Symbol _ md txt)
    | md == "" = txt
    | otherwise = md <> "_" <> txt

instance (Printable t) => Printable (Record a1 a2 a3 a4 a5 a6 t an) where
  pprint (Record _ typ head' props)
     = pprint typ
    <> pprint head'
    <> "["
    <> T.intercalate ", " (map pprint props) <> "]"

instance (Printable t) => Printable (Bind a1 a2 a3 a4 a5 a6 t an) where
  pprint (Bind _ typ idx) = pprint typ <> "\\" <> pprint idx

instance (Printable t) => Printable (Value a1 a2 a3 a4 a5 a6 t an) where
  pprint (ValuePrimitive prim) = pprint prim
  pprint (ValueRecord record) = pprint record
  pprint (ValueBind bind) = pprint bind

instance Printable (GroupLoc a1 a2 a3 a4 a5 a6 t an) where
  pprint (GroupLocGlobal _ sym) = "&" <> pprint sym
  pprint (GroupLocLocal _ idx) = "&" <> pprint idx
  pprint (GroupLocFunction _ sym) = "#" <> pprint sym

instance (Printable t) => Printable (GroupRef a1 a2 a3 a4 a5 a6 t an) where
  pprint (GroupRef _ loc vprops gprops)
     = pprint loc
    <> printProps (map pprint vprops ++ map pprint gprops)

instance (Printable t) => Printable (Guard a1 a2 a3 a4 a5 a6 t an) where
  pprint (Guard _ input output nexts)
     = pprint input
    <> " <- "
    <> pprint output
    <> foldMap printNext nexts
    where printNext next' = " " <> pprint next'

instance (Printable t) => Printable (Reducer a1 a2 a3 a4 a5 a6 t an) where
  pprint = printReducer "->"

instance Printable Library where
  pprint (LibraryCmdBinary _) = "<command-line binary>"
  pprint (LibraryJavaScript txt) = pprint txt

instance (PrintableArr a1, PrintableArr a2, PrintableArr a3, PrintableArr a4, PrintableBid a5, Printable t) => Printable (Program a1 a2 a3 a4 a5 a6 t an) where
  pprint (Program _ mpath idecls rdecls fdecls alis _ castReds grps libs)
     = T.unlines
     $ ["#module " <> mpath <> ";"]
    ++ printArr idecls
    ++ printArr rdecls
    ++ printArr fdecls
    ++ printArr alis
    ++ map (printReducer "=>") castReds
    ++ map (uncurry printGroupDef) (M.toList grps)
    ++ map (uncurry printLibrary) (M.toList libs)

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

printReducer :: (Printable t) => T.Text -> Reducer a1 a2 a3 a4 a5 a6 t an -> T.Text
printReducer typ (Reducer _ (Guard _ input output nexts) guards)
     = pprint input
    <> " "
    <> pprint typ
    <> " "
    <> pprint output
    <> foldMap printNext nexts
    <> foldMap printGuard guards
    <> ";"
    where printNext next' = " " <> pprint next'
          printGuard guard = ",\n  " <> pprint guard

-- TODO: Print env
printGroupDef :: (PrintableBid a5, Printable t) => PF Symbol -> GroupDef a1 a2 a3 a4 a5 a6 t an -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds _)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
          = "&"
          <> pprint head'
          <> printProps (map (printGroupDefProp "\\") vprops ++ map (printGroupDefProp "&") gprops)
          <> "."
        printGroupDefProp pre (bid, idx) = pre <> printBid bid <> pprint idx

printLibrary :: ModulePath -> Library -> T.Text
printLibrary path lib = "--- " <> path <> "\n" <> pprint lib

-- | How much of the module path will be printed.
modulePathPrintLength :: Int
modulePathPrintLength = 5

-- | Type with 1 part
mkSType :: PFA TypePart an -> PFA Type an
mkSType part = Type (getAnn part) [part]

anyType :: an -> PFA Type an
anyType ann = mkSType $ TypePartPrim ann PrimTypeAny

mkDeclSet :: [RecordDecl ()] -> [T.Text] -> [GroupDecl] -> [FunctionDecl ()] -> [TypeAlias ()] -> DeclSet
mkDeclSet ordecls trdecls gdecls fdecls alis
  = DeclSet
  { declSetRecords
      = M.fromList
      $ map (\(RecordDecl () head' nps) -> (head', Just nps)) ordecls
     ++ map (, Nothing) trdecls
  , declSetGroups = M.fromList $ map (\(GroupDecl head' nvps ngps) -> (head', (nvps, ngps))) gdecls
  , declSetFunctions = M.fromList $ map (\(FunctionDecl () head' nps ret) -> (head', (nps, ret))) fdecls
  , declSetAliases = M.fromList $ map (\(TypeAlias () ali typ) -> (ali, typ)) alis
  }

declSetLookup :: SymbolType a -> T.Text -> DeclSet -> Maybe a
declSetLookup SymbolTypeRecord local = (M.!? local) . declSetRecords
declSetLookup SymbolTypeGroup local = (M.!? local) . declSetGroups
declSetLookup SymbolTypeFunction local = (M.!? local) . declSetFunctions
declSetLookup SymbolTypeAlias local = (M.!? local) . declSetAliases

-- | Specifies that a record declaration can take any number of properties.
varNumProps :: Int
varNumProps = (-1)

mkBuiltinSymbol :: T.Text -> PF Symbol
mkBuiltinSymbol txt
  = Symbol
  { symbolAnn = ()
  , symbolModule = ""
  , symbol = txt
  }

builtinDecls :: DeclSet
builtinDecls
  = mkDeclSet
  [ RecordDecl () "Unit" []
  , RecordDecl () "True" []
  , RecordDecl () "False" []
  , RecordDecl () "None" []
  , RecordDecl () "Nil" []
  , RecordDecl () "Hole" [Type () [TypePartPrim () PrimTypeInteger]]
  ]
  ( S.toList transparentDecls )
  []
  []
  [ TypeAlias () "num" $ Type () [TypePartPrim () PrimTypeInteger, TypePartPrim () PrimTypeFloat]
  , TypeAlias () "prim" $ Type () [TypePartPrim () PrimTypeInteger, TypePartPrim () PrimTypeFloat, TypePartPrim () PrimTypeString]
  , TypeAlias () "bool" $ Type () [TypePartRecord () $ mkBuiltinSymbol "True", TypePartRecord () $ mkBuiltinSymbol "False"]
  , TypeAlias () "atom" $ Type () [TypePartPrim () PrimTypeInteger, TypePartPrim () PrimTypeFloat, TypePartPrim () PrimTypeString, TypePartRecord () $ mkBuiltinSymbol "True", TypePartRecord () $ mkBuiltinSymbol "False"]
  ]

transparentDecls :: S.Set T.Text
transparentDecls = S.fromList ["T", "Cons"]

-- | Whether the record is regular or a special kind.
recordKind :: T.Text -> RecordKind
recordKind head'
  | head' == "T" = RecordKindTuple
  | head' == "Cons" = RecordKindCons
  | otherwise = RecordKindRegular head'

desugarError_ :: T.Text -> Error
desugarError_ msg
  = Error
  { errorStage = StageDesugar
  , errorRange = Nothing
  , errorMsg = msg
  }

desugarError :: Range -> T.Text -> Error
desugarError rng = addRangeToErr rng . desugarError_
