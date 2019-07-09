{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Types.Program
  ( module TreeScript.Ast.Core.Types.Program
  )
where

import           TreeScript.Ast.Core.Types.Value
import           TreeScript.Ast.Core.Types.Stx
import           TreeScript.Misc

import           Data.Binary
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           GHC.Generics

data ProgramTerm a where
  TProgramValue ::ValueTerm a -> ProgramTerm a
  TGroupLoc ::ProgramTerm GroupLoc
  TGroupRef ::ProgramTerm GroupRef
  TCast ::ProgramTerm Cast
  TNext ::ProgramTerm Next
  TGuard ::ProgramTerm Guard
  TReducer ::ProgramTerm Reducer
  TGroupDef ::ProgramTerm GroupDef
  TProgram ::ProgramTerm Program

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: M.Map T.Text PropsType
  , declSetGroups :: M.Map T.Text (Int, Int)
  , declSetAliases :: M.Map T.Text MType
  , declSetCasts :: S.Set CastSurface
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

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

-- | Defines a type alias.
data TypeAlias an
  = TypeAlias
  { typeAliasAnn :: an
  , typeAliasAlias :: T.Text
  , typeAliasType :: UType an
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of symbol (used in resolution).
data SymbolType a where
  SymbolTypeRecord ::SymbolType PropsType
  SymbolTypeGroup ::SymbolType (Int, Int)
  SymbolTypeAlias ::SymbolType MType

-- | The type and identifier of a group.
data GroupLoc an
  = GroupLocGlobal an (Symbol an)
  | GroupLocLocal an Int
  deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc an
  , groupRefValueProps :: [Value an]
  , groupRefGroupProps :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Casts a value.
data Cast an
  = Cast
  { castAnn :: an
  , castPath :: [Int] -- ^ Index path in the value.
  , castType :: S.Set SType -- ^ Expected type.
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a reducers.
data Next an
  = NextEval an
  | NextCast (Cast an)
  | NextGroup (GroupRef an)
  deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [Next an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The input and output type of a cast reducer.
data CastSurface
  = CastSurface
  { castSurfaceInput :: SType
  , castSurfaceOutput :: SType
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerSubGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

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
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A TreeScript program or module, containing all the source data, used by the compiler.
data Program an
  = Program
  { programAnn :: an
  , programPath :: ModulePath
  , programImportDecls :: [ImportDecl an]
  , programRecordDecls :: [RecordDecl an]
  , programTypeAliases :: [TypeAlias an]
  , programExports :: DeclSet
  , programInits :: [LStxBlob]
  , programCastReducers :: M.Map CastSurface (Reducer an)
  , programGroups :: M.Map (Symbol ()) (GroupDef an)
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

instance Semigroup DeclSet where
  DeclSet xRecs xGrps xAlis xCasts <> DeclSet yRecs yGrps yAlis yCasts =
    DeclSet { declSetRecords = xRecs <> yRecs
            , declSetGroups  = xGrps <> yGrps
            , declSetAliases = xAlis <> yAlis
            , declSetCasts   = xCasts <> yCasts
            }

instance Monoid DeclSet where
  mempty = DeclSet { declSetRecords = mempty
                   , declSetGroups  = mempty
                   , declSetAliases = mempty
                   , declSetCasts   = mempty
                   }

-- | Takes path and exports of left, unless empty (to satisfy @Monoid@).
instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn xPath xIdecls xRdecls xAlis xExps xInits xCastReds xGrps <> Program yAnn _ _ _ _ _ yInits yCastReds yGrps
    = Program { programAnn          = xAnn <> yAnn
              , programPath         = xPath
              , programImportDecls  = xIdecls
              , programRecordDecls  = xRdecls
              , programTypeAliases  = xAlis
              , programExports      = xExps
              , programInits        = xInits <> yInits
              , programCastReducers = xCastReds <> yCastReds
              , programGroups       = xGrps <> yGrps
              }

-- | Has undefineds where it doesn't satisfy @Monoid@ rules.
instance (Monoid an) => Monoid (Program an) where
  mempty = Program { programAnn          = mempty
                   , programPath         = undefined
                   , programImportDecls  = undefined
                   , programRecordDecls  = undefined
                   , programTypeAliases  = undefined
                   , programExports      = mempty
                   , programInits        = mempty
                   , programCastReducers = mempty
                   , programGroups       = mempty
                   }

instance Printable DeclSet where
  pprint (DeclSet rexps gexps alis casts) =
    foldMap (",\n  " <>) (M.keys rexps)
      <> foldMap (",\n  &" <>)           (M.keys gexps)
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

instance Printable (TypeAlias an) where
  pprint (TypeAlias _ ali typ) =
    "@" <> pprint ali <> " -> " <> pprint typ <> ";"

instance Printable (GroupLoc an) where
  pprint (GroupLocGlobal _ sym) = "&" <> pprint sym
  pprint (GroupLocLocal  _ idx) = "&" <> pprint idx

instance Printable (GroupRef an) where
  pprint (GroupRef _ loc vprops gprops) =
    pprint loc <> printProps (map pprint vprops ++ map pprint gprops)

instance Printable (Cast an) where
  pprint (Cast _ path tparts) =
    "&["
      <> T.intercalate "," (map pprint path)
      <> ":"
      <> pprint (MType tparts)
      <> "]"

instance Printable (Next an) where
  pprint (NextEval  _   ) = "!"
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

instance Printable (Program an) where
  pprint (Program _ mpath idecls rdecls alis _ inits castReds grps) =
    T.unlines
      $  ["#module " <> mpath <> ";"]
      ++ map pprint                  idecls
      ++ map pprint                  rdecls
      ++ map pprint                  alis
      ++ map ((<> "!;") . pprint)    inits
      ++ map (printReducer "=>")     (M.elems castReds)
      ++ map (uncurry printGroupDef) (M.toList grps)

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

-- | 'Nothing' if the cast type is @any@.
mkCast :: an -> [Int] -> MType -> Maybe (Cast an)
mkCast _   _    MTypeAny      = Nothing
mkCast ann path (MType parts) = Just $ Cast ann path parts

allPossibleCasts :: MType -> MType -> [CastSurface]
-- Can't cast to/from any
allPossibleCasts MTypeAny _        = []
allPossibleCasts _        MTypeAny = []
allPossibleCasts (MType xs) (MType ys) =
  CastSurface <$> S.toList xs <*> S.toList ys

mkDeclSet
  :: [RecordDecl an]
  -> [T.Text]
  -> [GroupDecl an]
  -> [TypeAlias an]
  -> S.Set CastSurface
  -> DeclSet
mkDeclSet frdecls vrheads gdecls alis casts = DeclSet
  { declSetRecords = M.fromList
                     $  map
                          (\(RecordDecl _ head' ps) ->
                            (head', PropsTypeFixed $ map utype ps)
                          )
                          frdecls
                     ++ map (, PropsTypeVarLen) vrheads
  , declSetGroups  = M.fromList $ map
                       (\(GroupDecl _ head' nvps ngps) -> (head', (nvps, ngps)))
                       gdecls
  , declSetAliases = M.fromList
                       $ map (\(TypeAlias _ ali typ) -> (ali, utype typ)) alis
  , declSetCasts   = casts
  }

declSetLookup :: SymbolType a -> T.Text -> DeclSet -> Maybe a
declSetLookup SymbolTypeRecord local = (M.!? local) . declSetRecords
declSetLookup SymbolTypeGroup  local = (M.!? local) . declSetGroups
declSetLookup SymbolTypeAlias  local = (M.!? local) . declSetAliases

builtinDecls :: DeclSet
builtinDecls = mkDeclSet
  [ RecordDecl () "Unit"  []
  , RecordDecl () "True"  []
  , RecordDecl () "False" []
  , RecordDecl () "None"  []
  , RecordDecl () "Hole" [UType () $ mType1 $ STypePrim PrimTypeInteger]
  , RecordDecl () "Nil"   []
  ]
  (S.toList transparentDecls)
  []
  [ TypeAlias () "num" $ UType () $ MType
    [STypePrim PrimTypeInteger, STypePrim PrimTypeFloat]
  , TypeAlias () "prim" $ UType () $ MType
    [ STypePrim PrimTypeInteger
    , STypePrim PrimTypeFloat
    , STypePrim PrimTypeString
    ]
  , TypeAlias () "bool" $ UType () $ MType
    [ STypeRecord $ mkBuiltinSymbol "True"
    , STypeRecord $ mkBuiltinSymbol "False"
    ]
  , TypeAlias () "atom" $ UType () $ MType
    [ STypePrim PrimTypeInteger
    , STypePrim PrimTypeFloat
    , STypePrim PrimTypeString
    , STypeRecord $ mkBuiltinSymbol "True"
    , STypeRecord $ mkBuiltinSymbol "False"
    ]
  ]
  []

transparentDecls :: S.Set T.Text
transparentDecls = S.fromList ["T", "L"]
