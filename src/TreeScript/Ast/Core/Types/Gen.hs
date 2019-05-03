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
import Data.Binary
import qualified Data.Map.Strict as M
import Data.MessagePack
import Data.Proxy
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
  TCastRef :: Term CastRef
  TNext :: Term Next
  TGuard :: Term Guard
  TReducer :: Term Reducer
  TGroupDef :: Term GroupDef
  TProgram :: Term Program

-- | A non-record type part.
data PrimType
  = PrimTypeAny
  | PrimTypeInteger
  | PrimTypeFloat
  | PrimTypeString
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Whether the record is a regular or special type.
data RecordKind
  = RecordKindTuple
  | RecordKindCons
  | RecordKindRegular T.Text
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Part of a value type (which is a union).
data TypePart t
  = TypePartPrim Range PrimType
  | TypePartRecord Range (Symbol t)
  | TypePartTuple Range [Type t]
  | TypePartList Range (Type t)
  deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable)

-- | A value type.
data Type t
  = Type
  { typeAnn :: Range
  , typeParts :: [TypePart t]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable)

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: M.Map T.Text (Maybe [Type ()])
  , declSetFunctions :: M.Map T.Text ([Type ()], Type ())
  , declSetGroups :: M.Map T.Text (Int, Int)
  , declSetAliases :: M.Map T.Text (Type ())
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Determines which module a symbol is located, so 2 symbols with the same name don't conflict.
type ModulePath = T.Text

-- | Created for each imported module.
data ImportDecl
  = ImportDecl
  { importDeclAnn :: Range
  , importDeclPath :: ModulePath
  , importDeclQual :: T.Text
  , importDeclExps :: DeclSet
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Declares a type of record.
data RecordDecl
  = RecordDecl
  { recordDeclAnn :: Range
  , recordDeclHead :: T.Text
  , recordDeclProps :: [Type ()]
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Declares a type of group but doesn't specifify property values.
data GroupDecl
  = GroupDecl
  { groupDeclHead :: T.Text
  , groupDeclNumValueProps :: Int
  , groupDeclNumGroupProps :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Declares a function.
data FunctionDecl
  = FunctionDecl
  { functionDeclAnn :: Range
  , functionDeclHead :: T.Text
  , functionDeclProps :: [Type ()]
  , functionDeclOutput :: Type ()
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Defines a type alias.
data TypeAlias
  = TypeAlias
  { typeAliasAnn :: Range
  , typeAliasAlias :: T.Text
  , typeAliasType :: Type ()
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive t
  = PrimInteger Range Int
  | PrimFloat Range Float
  | PrimString Range T.Text
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Type of symbol (used in resolution).
data SymbolType a where
  SymbolTypeRecord :: SymbolType (Maybe [Type ()])
  SymbolTypeGroup :: SymbolType (Int, Int)
  SymbolTypeFunction :: SymbolType ([Type ()], Type ())
  SymbolTypeAlias :: SymbolType (Type ())

-- | An identifier, such as a record head or property key.
data Symbol t
  = Symbol
  { symbolAnn :: Range
  , symbolModule :: ModulePath
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Contains a head and properties. A parent in the AST.
data Record t
  = Record
  { recordAnn :: Range
  , recordHead :: Symbol t
  , recordProps :: [Value t]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind t
  = Bind
  { bindAnn :: Range
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable)

-- | Type of data in TreeScript.
data Value t
  = ValuePrimitive t (Primitive t)
  | ValueRecord t (Record t)
  | ValueBind t (Bind t)
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | The type and identifier of a group.
data GroupLoc t
  = GroupLocGlobal Range (Symbol t)
  | GroupLocLocal Range Int
  | GroupLocFunction Range (Symbol t)
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef t
  = GroupRef
  { groupRefAnn :: Range
  , groupRefLoc :: GroupLoc t
  , groupRefValueProps :: [Value t]
  , groupRefGroupProps :: [GroupRef t]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Casts a value.
data CastRef t
  = CastRef
  { castRefIdx :: Int -- ^ Index of the reducer in the module.
  , castRefPath :: [Int] -- ^ Index path in the value.
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Transforms a reducers.
data Next t
  = NextCast (CastRef t)
  | NextGroup (GroupRef t)
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard t
  = Guard
  { guardAnn :: Range
  , guardInput :: Value t
  , guardOutput :: Value t
  , guardNexts :: [Next t]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer t
  = Reducer
  { reducerAnn :: Range
  , reducerMain :: Guard t
  , reducerSubGuards :: [Guard t]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | Group def property.
data GroupDefProp
  = GroupDefProp
  { groupDefPropAnn :: Range
  , groupDefPropText :: T.Text
  , groupDefPropIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef t
  = GroupDef
  { groupDefAnn :: Range
  , groupDefValueProps :: [GroupDefProp]
  , groupDefGroupProps :: [GroupDefProp]
  , groupDefReducers :: [Reducer t]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable)

-- | A foreign program this script uses.
data Library
  = LibraryCmdBinary B.ByteString
  | LibraryJavaScript T.Text
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial)

-- | A TreeScript program or module, containing all the source data, used by the compiler.
data Program t
  = Program
  { programAnn :: Range
  , programPath :: ModulePath
  , programImportDecls :: [ImportDecl]
  , programRecordDecls :: [RecordDecl]
  , programFunctionDecls :: [FunctionDecl]
  , programTypeAliases :: [TypeAlias]
  , programExports :: DeclSet
  , programCastReducers :: [Reducer t]
  , programGroups :: M.Map (Symbol ()) (GroupDef t)
  , programLibraries :: M.Map ModulePath Library
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable)

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
instance Semigroup (Program t) where
  Program xAnn xPath xIdecls xRdecls xFdecls xAlis xExps xCastReds xGrps xLibs <> Program yAnn _ _ _ _ _ _ yCastReds yGrps yLibs
    = Program
    { programAnn = xAnn <> yAnn
    , programPath = xPath
    , programImportDecls = xIdecls
    , programRecordDecls = xRdecls
    , programFunctionDecls = xFdecls
    , programTypeAliases = xAlis
    , programExports = xExps
    , programCastReducers = xCastReds <> yCastReds
    , programGroups = xGrps <> yGrps
    , programLibraries = xLibs <> yLibs
    }

-- | Has undefineds where it doesn't satisfy @Monoid@ rules.
instance Monoid (Program t) where
  mempty
    = Program
    { programAnn = singletonRange loc1
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

instance InterpSerial ImportDecl where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance InterpSerial RecordDecl where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance InterpSerial FunctionDecl where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance InterpSerial TypeAlias where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance InterpSerial DeclSet where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance InterpSerial (Bind t) where
  toMsgp = toMsgp . bindIdx
  skipMsgp Proxy = False

instance InterpSerial GroupDefProp where
  toMsgp = toMsgp . groupDefPropIdx
  skipMsgp Proxy = False

instance (InterpSerial t) => InterpSerial (Program t) where
  toMsgp (Program _ path _ _ _ _ _ xCastReds xGrps xLibs)
    = ObjectArray [toMsgp path, toMsgp xCastReds, toMsgp xGrps, toMsgp xLibs]
  skipMsgp Proxy = False

instance Printable PrimType where
  pprint PrimTypeAny = "any"
  pprint PrimTypeInteger = "int"
  pprint PrimTypeFloat = "float"
  pprint PrimTypeString = "string"

instance Printable (TypePart t) where
  pprint (TypePartPrim _ x) = "@" <> pprint x
  pprint (TypePartRecord _ name) = "@" <> pprint name
  pprint (TypePartTuple _ props) = "@t" <> printProps (map pprint props)
  pprint (TypePartList _ prop) = "@list[" <> pprint prop <> "]"

instance Printable (Type t) where
  pprint (Type _ parts)
    = T.intercalate "|" (map pprint parts)

instance Printable DeclSet where
  pprint (DeclSet alis rexps gexps fexps)
     = foldMap (",\n  @" <>) (M.keys alis)
    <> foldMap (",\n  " <>) (M.keys rexps)
    <> foldMap (",\n  &" <>) (M.keys gexps)
    <> foldMap (",\n  #" <>) (M.keys fexps)

instance Printable ImportDecl where
  pprint (ImportDecl _ path qual exps)
    = "#import " <> pprint path <> printQual <> pprint exps <> ";"
    where printQual
            | T.null qual = ""
            | otherwise = " => " <> T.dropEnd 1 qual

instance Printable RecordDecl where
  pprint (RecordDecl _ head' props)
    = head' <> printProps (map pprint props) <> "."

instance Printable FunctionDecl where
  pprint (FunctionDecl _ head' props ret)
    = "#" <> head' <> printProps (map pprint props) <> " -> " <> pprint ret <> "."

instance Printable TypeAlias where
  pprint (TypeAlias _ ali typ) = "@" <> pprint ali <> " -> " <> pprint typ <> ";"

instance Printable (Primitive t) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

instance Printable (Symbol t) where
  pprint (Symbol _ md txt)
    | md == "" = txt
    | otherwise = md <> "_" <> txt

instance (Printable t) => Printable (Record t) where
  pprint (Record _ head' props)
     = pprint head'
    <> "["
    <> T.intercalate ", " (map pprint props) <> "]"

instance Printable (Bind t) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance (Printable t) => Printable (Value t) where
  pprint (ValuePrimitive typ prim) = pprint prim <> printType typ
  pprint (ValueRecord typ record) = pprint record <> printType typ
  pprint (ValueBind typ bind) = pprint bind <> printType typ

instance Printable (GroupLoc t) where
  pprint (GroupLocGlobal _ sym) = "&" <> pprint sym
  pprint (GroupLocLocal _ idx) = "&" <> pprint idx
  pprint (GroupLocFunction _ sym) = "#" <> pprint sym

instance (Printable t) => Printable (GroupRef t) where
  pprint (GroupRef _ loc vprops gprops)
     = pprint loc
    <> printProps (map pprint vprops ++ map pprint gprops)

instance Printable (CastRef t) where
  pprint (CastRef idx path)
     = "&["
    <> T.intercalate "," (map pprint path)
    <> ":"
    <> pprint idx
    <> "]"

instance (Printable t) => Printable (Next t) where
  pprint (NextCast cast) = pprint cast
  pprint (NextGroup grp) = pprint grp

instance (Printable t) => Printable (Guard t) where
  pprint (Guard _ input output nexts)
     = pprint input
    <> " <- "
    <> pprint output
    <> foldMap printNext nexts
    where printNext next' = " " <> pprint next'

instance (Printable t) => Printable (Reducer t) where
  pprint = printReducer "->"

instance Printable Library where
  pprint (LibraryCmdBinary _) = "<command-line binary>"
  pprint (LibraryJavaScript txt) = pprint txt

instance (Printable t) => Printable (Program t) where
  pprint (Program _ mpath idecls rdecls fdecls alis _ castReds grps libs)
     = T.unlines
     $ ["#module " <> mpath <> ";"]
    ++ map pprint idecls
    ++ map pprint rdecls
    ++ map pprint fdecls
    ++ map pprint alis
    ++ map (printReducer "=>") castReds
    ++ map (uncurry printGroupDef) (M.toList grps)
    ++ map (uncurry printLibrary) (M.toList libs)

printType :: (Printable t) => t -> T.Text
printType typ
  | reg == "()" = ""
  | otherwise = ":" <> reg
  where reg = pprint typ

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

printReducer :: (Printable t) => T.Text -> Reducer t -> T.Text
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

printGroupDefProp :: T.Text -> GroupDefProp -> T.Text
printGroupDefProp pre (GroupDefProp _ bid idx) = pre <> bid <> pprint idx

printGroupDef :: (Printable t) => Symbol () -> GroupDef t -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
          = "&"
          <> pprint head'
          <> printProps (map (printGroupDefProp "\\") vprops ++ map (printGroupDefProp "&") gprops)
          <> "."

printLibrary :: ModulePath -> Library -> T.Text
printLibrary path lib = "--- " <> path <> "\n" <> pprint lib

-- | How much of the module path will be printed.
modulePathPrintLength :: Int
modulePathPrintLength = 5

typePartAnn :: TypePart t -> Range
typePartAnn _ = error "SOON"

-- | Type with 1 part
mkSType :: TypePart t -> Type t
mkSType part = Type (typePartAnn part) [part]

anyType :: Range -> Type t
anyType ann = mkSType $ TypePartPrim ann PrimTypeAny

mkDeclSet :: [RecordDecl] -> [T.Text] -> [GroupDecl] -> [FunctionDecl] -> [TypeAlias] -> DeclSet
mkDeclSet ordecls trdecls gdecls fdecls alis
  = DeclSet
  { declSetRecords
      = M.fromList
      $ map (\(RecordDecl _ head' nps) -> (head', Just nps)) ordecls
     ++ map (, Nothing) trdecls
  , declSetGroups = M.fromList $ map (\(GroupDecl head' nvps ngps) -> (head', (nvps, ngps))) gdecls
  , declSetFunctions = M.fromList $ map (\(FunctionDecl _ head' nps ret) -> (head', (nps, ret))) fdecls
  , declSetAliases = M.fromList $ map (\(TypeAlias _ ali typ) -> (ali, typ)) alis
  }

declSetLookup :: SymbolType a -> T.Text -> DeclSet -> Maybe a
declSetLookup SymbolTypeRecord local = (M.!? local) . declSetRecords
declSetLookup SymbolTypeGroup local = (M.!? local) . declSetGroups
declSetLookup SymbolTypeFunction local = (M.!? local) . declSetFunctions
declSetLookup SymbolTypeAlias local = (M.!? local) . declSetAliases

-- | Specifies that a record declaration can take any number of properties.
varNumProps :: Int
varNumProps = (-1)

mkBuiltinSymbol :: T.Text -> Symbol t
mkBuiltinSymbol txt
  = Symbol
  { symbolAnn = r0
  , symbolModule = ""
  , symbol = txt
  }

builtinDecls :: DeclSet
builtinDecls
  = mkDeclSet
  [ RecordDecl r0 "Unit" []
  , RecordDecl r0 "True" []
  , RecordDecl r0 "False" []
  , RecordDecl r0 "None" []
  , RecordDecl r0 "Nil" []
  , RecordDecl r0 "Hole" [Type r0 [TypePartPrim r0 PrimTypeInteger]]
  ]
  ( S.toList transparentDecls )
  []
  []
  [ TypeAlias r0 "num" $ Type r0 [TypePartPrim r0 PrimTypeInteger, TypePartPrim r0 PrimTypeFloat]
  , TypeAlias r0 "prim" $ Type r0 [TypePartPrim r0 PrimTypeInteger, TypePartPrim r0 PrimTypeFloat, TypePartPrim r0 PrimTypeString]
  , TypeAlias r0 "bool" $ Type r0 [TypePartRecord r0 $ mkBuiltinSymbol "True", TypePartRecord r0 $ mkBuiltinSymbol "False"]
  , TypeAlias r0 "atom" $ Type r0 [TypePartPrim r0 PrimTypeInteger, TypePartPrim r0 PrimTypeFloat, TypePartPrim r0 PrimTypeString, TypePartRecord r0 $ mkBuiltinSymbol "True", TypePartRecord r0 $ mkBuiltinSymbol "False"]
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
