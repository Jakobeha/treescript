{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types for the @Core@ AST, includes @Final@ (bare) phase.
module TreeScript.Ast.Core.Types.Gen
  ( module TreeScript.Ast.Core.Types.Gen
  )
where

import           TreeScript.Ast.Core.Types.InterpSerial
import           TreeScript.Misc

import qualified Data.ByteString.Lazy          as B
import           Data.Binary
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.MessagePack
import           Data.Proxy
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           GHC.Exts
import           GHC.Generics

data Term a where
  TType ::Term UType
  TPrim ::Term Primitive
  TSymbol ::Term Symbol
  TRecord ::Term Record
  TBind ::Term Bind
  TValue ::Term Value
  TGroupLoc ::Term GroupLoc
  TGroupRef ::Term GroupRef
  TCast ::Term Cast
  TNext ::Term Next
  TGuard ::Term Guard
  TReducer ::Term Reducer
  TGroupDef ::Term GroupDef
  TProgram ::Term Program

-- | The type of a primitive.
data PrimType
  = PrimTypeInteger
  | PrimTypeFloat
  | PrimTypeString
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial)

-- | Whether the record is a regular or special type.
data RecordKind
  = RecordKindTuple
  | RecordKindNil
  | RecordKindCons
  | RecordKindOpaque
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Part of a union type.
data TypePart
  = TypePartPrim PrimType
  | TypePartRecord (Symbol ())
  | TypePartTuple [XType]
  | TypePartList XType
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial)

-- | Set of @TypeParts@, with separate sets for each type of part (easier to merge lists/tuples).
data TypePartSet
  = TypePartSet
  { typePartSetPrims :: S.Set PrimType
  , typePartSetRecords :: S.Set (Symbol ())
  , typePartSetTuples :: M.Map Int [XType]
  , typePartSetLists :: Maybe XType
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Type without annotations.
-- Types are bounded lattices consisting of either a union of disjoint atoms or @any@.
data XType
  = XTypeAny
  | XType TypePartSet
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial)

-- | Type of a single value - values never have union types.
data SType
  = STypeAny
  | SType1 TypePart
  deriving (Eq, Ord, Read, Show)

-- | Type defined by the user, with annotations.
data UType an
  = UType
  { utypeAnn :: an
  , utype :: XType
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Whether a record has fixed or variable props
data PropsType
  = PropsTypeVarLen
  | PropsTypeFixed [XType]
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: M.Map T.Text PropsType
  , declSetFunctions :: M.Map T.Text [XType]
  , declSetGroups :: M.Map T.Text (Int, Int)
  , declSetAliases :: M.Map T.Text XType
  , declSetCasts :: S.Set CastSurface
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Determines which module a symbol is located, so 2 symbols with the same name don't conflict.
type ModulePath = T.Text

-- | Created for each imported module.
data ImportDecl an
  = ImportDecl
  { importDeclAnn :: an
  , importDeclPath :: ModulePath
  , importDeclQual :: T.Text
  , importDeclExps :: DeclSet
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: T.Text
  , recordDeclProps :: [UType an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a type of group but doesn't specifify property values.
data GroupDecl an
  = GroupDecl
  { groupDeclAnn :: an
  , groupDeclHead :: T.Text
  , groupDeclNumValueProps :: Int
  , groupDeclNumGroupProps :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a function.
data FunctionDecl an
  = FunctionDecl
  { functionDeclAnn :: an
  , functionDeclHead :: T.Text
  , functionDeclProps :: [UType an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a type alias.
data TypeAlias an
  = TypeAlias
  { typeAliasAnn :: an
  , typeAliasAlias :: T.Text
  , typeAliasType :: UType an
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of symbol (used in resolution).
data SymbolType a where
  SymbolTypeRecord ::SymbolType PropsType
  SymbolTypeGroup ::SymbolType (Int, Int)
  SymbolTypeFunction ::SymbolType [XType]
  SymbolTypeAlias ::SymbolType XType

-- | An identifier, such as a record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbolModule :: ModulePath
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordHead :: Symbol an
  , recordProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | UType of data in TreeScript.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The type and identifier of a group.
data GroupLoc an
  = GroupLocGlobal an (Symbol an)
  | GroupLocLocal an Int
  | GroupLocFunction an (Symbol an)
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc an
  , groupRefValueProps :: [Value an]
  , groupRefGroupProps :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Casts a value.
data Cast an
  = Cast
  { castAnn :: an
  , castPath :: [Int] -- ^ Index path in the value.
  , castType :: TypePartSet -- ^ Expected type.
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a reducers.
data Next an
  = NextCast (Cast an)
  | NextGroup (GroupRef an)
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [Next an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The input and output type of a cast reducer.
data CastSurface
  = CastSurface
  { castSurfaceInput :: TypePart
  , castSurfaceOutput :: TypePart
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerSubGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Group def property.
data GroupDefProp an
  = GroupDefProp
  { groupDefPropAnn :: an
  , groupDefPropText :: T.Text
  , groupDefPropIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefValueProps :: [GroupDefProp an]
  , groupDefGroupProps :: [GroupDefProp an]
  , groupDefReducers :: [Reducer an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A foreign program this script uses.
data Library
  = LibraryCmdBinary B.ByteString
  | LibraryJavaScript T.Text
  deriving (Eq, Ord, Read, Show, Generic, Binary, InterpSerial)

-- | A TreeScript program or module, containing all the source data, used by the compiler.
data Program an
  = Program
  { programAnn :: an
  , programPath :: ModulePath
  , programImportDecls :: [ImportDecl an]
  , programRecordDecls :: [RecordDecl an]
  , programFunctionDecls :: [FunctionDecl an]
  , programTypeAliases :: [TypeAlias an]
  , programExports :: DeclSet
  , programCastReducers :: M.Map CastSurface (Reducer an)
  , programGroups :: M.Map (Symbol ()) (GroupDef an)
  , programLibraries :: M.Map ModulePath Library
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

instance IsList TypePartSet where
  type Item TypePartSet = TypePart

  fromList parts = TypePartSet
    { typePartSetPrims   = S.fromList $ mapMaybe
                             (\case
                               TypePartPrim prm -> Just prm
                               _                -> Nothing
                             )
                             parts
    , typePartSetRecords = S.fromList $ mapMaybe
                             (\case
                               TypePartRecord sym -> Just sym
                               _                  -> Nothing
                             )
                             parts
    , typePartSetTuples  = M.fromListWith (<>) $ mapMaybe
                             (\case
                               TypePartTuple props -> Just (length props, props)
                               _                   -> Nothing
                             )
                             parts
    , typePartSetLists   = foldMap
                             (\case
                               TypePartList elm -> Just elm
                               _                -> Nothing
                             )
                             parts
    }

  toList (TypePartSet prms recs tups lsts) =
    map TypePartPrim (S.toList prms)
      ++ map TypePartRecord (S.toList recs)
      ++ map TypePartTuple  (M.elems tups)
      ++ map TypePartList   (maybeToList lsts)

instance Semigroup TypePartSet where
  TypePartSet xPrims xRecs xTups xLsts <> TypePartSet yPrims yRecs yTups yLsts
    = TypePartSet { typePartSetPrims   = xPrims <> yPrims
                  , typePartSetRecords = xRecs <> yRecs
                  , typePartSetTuples  = M.unionWith (<>) xTups yTups
                  , typePartSetLists   = unionLists xLsts yLsts
                  }
   where
    unionLists (Just xLsts') (Just yLsts') = Just $ xLsts' <> yLsts'
    unionLists (Just xLsts') Nothing       = Just xLsts'
    unionLists Nothing       (Just yLsts') = Just yLsts'
    unionLists Nothing       Nothing       = Nothing

instance Monoid TypePartSet where
  mempty = TypePartSet { typePartSetPrims   = mempty
                       , typePartSetRecords = mempty
                       , typePartSetTuples  = mempty
                       , typePartSetLists   = mempty
                       }

-- | @mappend == union@.
instance Semigroup XType where
  XTypeAny <> _        = XTypeAny
  _        <> XTypeAny = XTypeAny
-- TODO: Merge same length lists and tuples.
  XType xs <> XType ys = XType $ xs <> ys

-- | @mappend == union@.
instance Monoid XType where
  mempty = xBottom

instance Semigroup DeclSet where
  DeclSet xRecs xFuns xGrps xAlis xCasts <> DeclSet yRecs yFuns yGrps yAlis yCasts
    = DeclSet { declSetRecords   = xRecs <> yRecs
              , declSetFunctions = xFuns <> yFuns
              , declSetGroups    = xGrps <> yGrps
              , declSetAliases   = xAlis <> yAlis
              , declSetCasts     = xCasts <> yCasts
              }

instance Monoid DeclSet where
  mempty = DeclSet { declSetRecords   = mempty
                   , declSetFunctions = mempty
                   , declSetGroups    = mempty
                   , declSetAliases   = mempty
                   , declSetCasts     = mempty
                   }

-- | Takes path and exports of left, unless empty (to satisfy @Monoid@).
instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn xPath xIdecls xRdecls xFdecls xAlis xExps xCastReds xGrps xLibs <> Program yAnn _ _ _ _ _ _ yCastReds yGrps yLibs
    = Program { programAnn           = xAnn <> yAnn
              , programPath          = xPath
              , programImportDecls   = xIdecls
              , programRecordDecls   = xRdecls
              , programFunctionDecls = xFdecls
              , programTypeAliases   = xAlis
              , programExports       = xExps
              , programCastReducers  = xCastReds <> yCastReds
              , programGroups        = xGrps <> yGrps
              , programLibraries     = xLibs <> yLibs
              }

-- | Has undefineds where it doesn't satisfy @Monoid@ rules.
instance (Monoid an) => Monoid (Program an) where
  mempty = Program { programAnn           = mempty
                   , programPath          = undefined
                   , programImportDecls   = undefined
                   , programRecordDecls   = undefined
                   , programFunctionDecls = undefined
                   , programTypeAliases   = undefined
                   , programExports       = mempty
                   , programCastReducers  = mempty
                   , programGroups        = mempty
                   , programLibraries     = mempty
                   }

instance InterpSerial TypePartSet where
  toMsgp = toMsgp . S.fromList . toList
  skipMsgp Proxy = False

instance (InterpSerial an) => InterpSerial (ImportDecl an) where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance (InterpSerial an) => InterpSerial (RecordDecl an) where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance (InterpSerial an) => InterpSerial (FunctionDecl an) where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance (InterpSerial an) => InterpSerial (TypeAlias an) where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance InterpSerial DeclSet where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance (InterpSerial an) => InterpSerial (Bind an) where
  toMsgp = toMsgp . bindIdx
  skipMsgp Proxy = False

instance (InterpSerial an) => InterpSerial (GroupDefProp an) where
  toMsgp = toMsgp . groupDefPropIdx
  skipMsgp Proxy = False

instance (InterpSerial an) => InterpSerial (Program an) where
  toMsgp (Program _ path _ _ _ _ _ xCastReds xGrps xLibs) =
    ObjectArray [toMsgp path, toMsgp xCastReds, toMsgp xGrps, toMsgp xLibs]
  skipMsgp Proxy = False

instance Printable PrimType where
  pprint PrimTypeInteger = "int"
  pprint PrimTypeFloat   = "float"
  pprint PrimTypeString  = "string"

instance Printable TypePart where
  pprint (TypePartPrim   x    ) = "@" <> pprint x
  pprint (TypePartRecord name ) = "@" <> pprint name
  pprint (TypePartTuple  props) = "@t" <> printProps (map pprint props)
  pprint (TypePartList   prop ) = "@list[" <> pprint prop <> "]"

instance Printable XType where
  pprint XTypeAny = "@any"
  pprint (XType parts) | null parts' = "@bottom"
                       | otherwise   = T.intercalate "|" $ map pprint parts'
    where parts' = toList parts

instance Printable (UType an) where
  pprint (UType _ typ) = pprint typ

instance Printable DeclSet where
  pprint (DeclSet rexps gexps fexps alis casts) =
    foldMap (",\n  " <>) (M.keys rexps)
      <> foldMap (",\n  &" <>)           (M.keys gexps)
      <> foldMap (",\n  #" <>)           (M.keys fexps)
      <> foldMap (",\n  @" <>)           (M.keys alis)
      <> foldMap ((",\n  " <>) . pprint) (S.toList casts)

instance Printable (ImportDecl an) where
  pprint (ImportDecl _ path qual exps) =
    "#import " <> path <> printQual <> pprint exps <> ";"
   where
    printQual | T.null qual = ""
              | otherwise   = " => " <> qual

instance Printable (RecordDecl an) where
  pprint (RecordDecl _ head' props) =
    head' <> printProps (map pprint props) <> "."

instance Printable (FunctionDecl an) where
  pprint (FunctionDecl _ head' props) =
    "#" <> head' <> printProps (map pprint props) <> "."

instance Printable (TypeAlias an) where
  pprint (TypeAlias _ ali typ) =
    "@" <> pprint ali <> " -> " <> pprint typ <> ";"

instance Printable (Primitive an) where
  pprint (PrimInteger _ int  ) = pprint int
  pprint (PrimFloat   _ float) = pprint float
  pprint (PrimString  _ str  ) = pprint str

instance Printable (Symbol an) where
  pprint (Symbol _ md txt) | md == ""  = txt
                           | otherwise = md <> "_" <> txt

instance Printable (Record an) where
  pprint (Record _ head' props) =
    pprint head' <> "[" <> T.intercalate ", " (map pprint props) <> "]"

instance Printable (Bind an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance Printable (Value an) where
  pprint (ValuePrimitive prim  ) = pprint prim
  pprint (ValueRecord    record) = pprint record
  pprint (ValueBind      bind  ) = pprint bind

instance Printable (GroupLoc an) where
  pprint (GroupLocGlobal   _ sym) = "&" <> pprint sym
  pprint (GroupLocLocal    _ idx) = "&" <> pprint idx
  pprint (GroupLocFunction _ sym) = "#" <> pprint sym

instance Printable (GroupRef an) where
  pprint (GroupRef _ loc vprops gprops) =
    pprint loc <> printProps (map pprint vprops ++ map pprint gprops)

instance Printable (Cast an) where
  pprint (Cast _ path tparts) =
    "&["
      <> T.intercalate "," (map pprint path)
      <> ":"
      <> pprint (XType tparts)
      <> "]"

instance Printable (Next an) where
  pprint (NextCast  cast) = pprint cast
  pprint (NextGroup grp ) = pprint grp

instance Printable (Guard an) where
  pprint (Guard _ input output nexts) =
    pprint input <> " <- " <> pprint output <> foldMap printNext nexts
    where printNext next' = " " <> pprint next'

instance Printable CastSurface where
  pprint (CastSurface inp out) = pprint inp <> " => " <> pprint out

instance Printable (Reducer an) where
  pprint = printReducer "->"

instance Printable Library where
  pprint (LibraryCmdBinary  _  ) = "<command-line binary>"
  pprint (LibraryJavaScript txt) = pprint txt

instance Printable (Program an) where
  pprint (Program _ mpath idecls rdecls fdecls alis _ castReds grps libs) =
    T.unlines
      $  ["#module " <> mpath <> ";"]
      ++ map pprint                  idecls
      ++ map pprint                  rdecls
      ++ map pprint                  fdecls
      ++ map pprint                  alis
      ++ map (printReducer "=>")     (M.elems castReds)
      ++ map (uncurry printGroupDef) (M.toList grps)
      ++ map (uncurry printLibrary)  (M.toList libs)

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

printReducer :: T.Text -> Reducer t -> T.Text
printReducer typ (Reducer _ (Guard _ input output nexts) guards) =
  pprint input
    <> " "
    <> typ
    <> " "
    <> pprint output
    <> foldMap printNext  nexts
    <> foldMap printGuard guards
    <> ";"
 where
  printNext next' = " " <> pprint next'
  printGuard guard = ",\n  " <> pprint guard

printGroupDefProp :: T.Text -> GroupDefProp an -> T.Text
printGroupDefProp pre (GroupDefProp _ bid idx) = pre <> bid <> pprint idx

printGroupDef :: Symbol () -> GroupDef t -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds) =
  T.unlines $ printDecl : map pprint reds
 where
  printDecl =
    "&"
      <> pprint head'
      <> printProps
           (  map (printGroupDefProp "\\") vprops
           ++ map (printGroupDefProp "&")  gprops
           )
      <> "."

printLibrary :: ModulePath -> Library -> T.Text
printLibrary path lib = "--- " <> path <> "\n" <> pprint lib

-- | How much of the module path will be printed.
modulePathPrintLength :: Int
modulePathPrintLength = 5

xBottom :: XType
xBottom = XType mempty

xType1 :: TypePart -> XType
xType1 part = XType [part]

-- TODO typePartSetToList typePartSetFromList

typePartSetDisjoint :: TypePartSet -> TypePartSet -> Bool
typePartSetDisjoint (TypePartSet xPrms xRecs xTups xLsts) (TypePartSet yPrms yRecs yTups yLsts)
  = S.disjoint xPrms yPrms
    && S.disjoint xRecs yRecs
    && and
         (concat $ M.elems $ M.intersectionWith (zipWith typesDisjoint)
                                                xTups
                                                yTups
         )
    && and (typesDisjoint <$> xLsts <*> yLsts)

typesDisjoint :: XType -> XType -> Bool
typesDisjoint XTypeAny       _              = False
typesDisjoint _              XTypeAny       = False
typesDisjoint (XType xParts) (XType yParts) = typePartSetDisjoint xParts yParts

s2xType :: SType -> XType
s2xType STypeAny      = XTypeAny
s2xType (SType1 part) = XType [part]

-- | Whether the record is opaque or what transparent type it is.
recordKind :: Symbol an -> RecordKind
recordKind (Symbol _ mdl loc) | mdl == "" && loc == "T"    = RecordKindTuple
                              | mdl == "" && loc == "Nil"  = RecordKindNil
                              | mdl == "" && loc == "Cons" = RecordKindCons
                              | otherwise                  = RecordKindOpaque

-- | 'Nothing' if the cast type is @any@.
mkCast :: an -> [Int] -> XType -> Maybe (Cast an)
mkCast _   _    XTypeAny      = Nothing
mkCast ann path (XType parts) = Just $ Cast ann path parts

allPossibleCasts :: XType -> XType -> [CastSurface]
-- Can't cast to/from any
allPossibleCasts XTypeAny _        = []
allPossibleCasts _        XTypeAny = []
allPossibleCasts (XType xs) (XType ys) =
  CastSurface <$> toList xs <*> toList ys

mkDeclSet
  :: [RecordDecl an]
  -> [T.Text]
  -> [GroupDecl an]
  -> [FunctionDecl an]
  -> [TypeAlias an]
  -> S.Set CastSurface
  -> DeclSet
mkDeclSet frdecls vrheads gdecls fdecls alis casts = DeclSet
  { declSetRecords   = M.fromList
                       $  map
                            (\(RecordDecl _ head' ps) ->
                              (head', PropsTypeFixed $ map utype ps)
                            )
                            frdecls
                       ++ map (, PropsTypeVarLen) vrheads
  , declSetGroups    = M.fromList $ map
                         (\(GroupDecl _ head' nvps ngps) -> (head', (nvps, ngps)))
                         gdecls
  , declSetFunctions = M.fromList $ map
                         (\(FunctionDecl _ head' ps) -> (head', map utype ps))
                         fdecls
  , declSetAliases   = M.fromList
                         $ map (\(TypeAlias _ ali typ) -> (ali, utype typ)) alis
  , declSetCasts     = casts
  }

declSetLookup :: SymbolType a -> T.Text -> DeclSet -> Maybe a
declSetLookup SymbolTypeRecord   local = (M.!? local) . declSetRecords
declSetLookup SymbolTypeGroup    local = (M.!? local) . declSetGroups
declSetLookup SymbolTypeFunction local = (M.!? local) . declSetFunctions
declSetLookup SymbolTypeAlias    local = (M.!? local) . declSetAliases

mkBuiltinSymbol :: T.Text -> Symbol ()
mkBuiltinSymbol txt =
  Symbol { symbolAnn = (), symbolModule = "", symbol = txt }

builtinDecls :: DeclSet
builtinDecls = mkDeclSet
  [ RecordDecl () "Unit"  []
  , RecordDecl () "True"  []
  , RecordDecl () "False" []
  , RecordDecl () "None"  []
  , RecordDecl () "Hole" [UType () $ xType1 $ TypePartPrim PrimTypeInteger]
  , RecordDecl () "Nil"   []
  , RecordDecl ()
               "Cons"
               [UType () XTypeAny, UType () $ xType1 $ TypePartList XTypeAny]
  ]
  (S.toList transparentDecls)
  []
  []
  [ TypeAlias () "num" $ UType () $ XType
    [TypePartPrim PrimTypeInteger, TypePartPrim PrimTypeFloat]
  , TypeAlias () "prim" $ UType () $ XType
    [ TypePartPrim PrimTypeInteger
    , TypePartPrim PrimTypeFloat
    , TypePartPrim PrimTypeString
    ]
  , TypeAlias () "bool" $ UType () $ XType
    [ TypePartRecord $ mkBuiltinSymbol "True"
    , TypePartRecord $ mkBuiltinSymbol "False"
    ]
  , TypeAlias () "atom" $ UType () $ XType
    [ TypePartPrim PrimTypeInteger
    , TypePartPrim PrimTypeFloat
    , TypePartPrim PrimTypeString
    , TypePartRecord $ mkBuiltinSymbol "True"
    , TypePartRecord $ mkBuiltinSymbol "False"
    ]
  ]
  []

transparentDecls :: S.Set T.Text
transparentDecls = S.fromList ["T", "Nil", "Cons"]
