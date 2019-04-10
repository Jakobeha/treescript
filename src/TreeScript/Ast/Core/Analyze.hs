{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( mapValue
  , mapValuesInGuard
  , mapValuesInReducer
  , mapGroupsInGroupRef
  , mapGroupsInGuard
  , mapGroupsInReducer
  , foldValue
  , foldValuesInGuard
  , foldValuesInReducer
  , traverseValue
  , traverseValuesInGuard
  , traverseValuesInReducer
  , allProgramReducers
  , maxNumBindsInProgram
  , bindsInValue
  , bindsInReducerInput
  , bindsInGuardInput
  , langSpecDecls
  , allImportedDecls
  , getAllProgramDecls
  , allProgramFunctionNames
  , getAllProgramUsedLibraries
  , allGroupDefReducers
  , allGroupRefReducers
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import Data.List hiding (group)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Applies to each child value, then combines all results.
mapValue :: (Value an -> Value an) -> Value an -> Value an
mapValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
mapValue f (ValueRecord (Record ann head' props))
  = f $ ValueRecord $ Record ann head' $ map (mapValue f) props
mapValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
mapValuesInGuard :: (Value an -> Value an) -> Guard an -> Guard an
mapValuesInGuard f (Guard ann input output nexts)
  = Guard
  { guardAnn = ann
  , guardInput = mapValue f input
  , guardOutput = mapValue f output
  , guardNexts = nexts
  }

-- | Applies to each value, then combines all results.
mapValuesInReducer :: (Value an -> Value an) -> Reducer an -> Reducer an
mapValuesInReducer f (Reducer ann main guards)
  = Reducer
  { reducerAnn = ann
  , reducerMain = mapValuesInGuard f main
  , reducerSubGuards = map (mapValuesInGuard f) guards
  }

-- | Applies to each group, then combines all results. Properties before parent groups.
mapGroupsInGroupRef :: (GroupRef an -> GroupRef an) -> GroupRef an -> GroupRef an
mapGroupsInGroupRef f (GroupRef ann loc props)
  = f $ GroupRef
  { groupRefAnn = ann
  , groupRefLoc = loc
  , groupRefProps = map (mapGroupsInGroupRef f) props
  }

-- | Applies to each group, then combines all results. Properties before parent groups.
mapGroupsInGuard :: (GroupRef an -> GroupRef an) -> Guard an -> Guard an
mapGroupsInGuard f (Guard ann input output nexts)
  = Guard
  { guardAnn = ann
  , guardInput = input
  , guardOutput = output
  , guardNexts = map (mapGroupsInGroupRef f) nexts
  }

-- | Applies to each group, then combines all results. Properties before parent groups.
mapGroupsInReducer :: (GroupRef an -> GroupRef an) -> Reducer an -> Reducer an
mapGroupsInReducer f (Reducer ann main guards)
  = Reducer
  { reducerAnn = ann
  , reducerMain = mapGroupsInGuard f main
  , reducerSubGuards = map (mapGroupsInGuard f) guards
  }

-- | Applies to each child value, then combines all results.
foldValue :: (Semigroup r) => (Value an -> r) -> Value an -> r
foldValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
foldValue f (ValueRecord record)
  = foldl' foldInValue (f (ValueRecord record)) (recordProps record)
  where foldInValue res val = foldValue f val <> res
foldValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
foldValuesInGuard :: (Monoid r) => (Value an -> r) -> Guard an -> r
foldValuesInGuard f (Guard _ input output _)
   = foldValue f input <> foldValue f output

-- | Applies to each value, then combines all results.
foldValuesInReducer :: (Monoid r) => (Value an -> r) -> Reducer an -> r
foldValuesInReducer f (Reducer _ main guards)
   = foldValuesInGuard f main
  <> foldMap (foldValuesInGuard f) guards

-- | Applies to each child group, then combines all results. Properties left of parent groups.
foldGroupsInGroupRef :: (Monoid r) => (GroupRef an -> r) -> GroupRef an -> r
foldGroupsInGroupRef f grp
   = foldMap (foldGroupsInGroupRef f) (groupRefProps grp)
  <> f grp

-- | Applies to each child group, then combines all results. Properties left of parent groups.
foldGroupsInGuard :: (Monoid r) => (GroupRef an -> r) -> Guard an -> r
foldGroupsInGuard f = foldMap (foldGroupsInGroupRef f) . guardNexts

-- | Applies to each child group, then combines all results. Properties left of parent groups.
foldGroupsInReducer :: (Monoid r) => (GroupRef an -> r) -> Reducer an -> r
foldGroupsInReducer f (Reducer _ main guards)
  = foldGroupsInGuard f main <> foldMap (foldGroupsInGuard f) guards

-- | Applies to each child value, then combines all results.
traverseValue :: (Monad w) => (Value an -> w (Value an)) -> Value an -> w (Value an)
traverseValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
traverseValue f (ValueRecord (Record ann head' props)) = f =<< traverseInValue
  where traverseInValue
          = ValueRecord . Record ann head' <$> traverse (traverseValue f) props
traverseValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
traverseValuesInGuard :: (Monad w) => (Value an -> w (Value an)) -> Guard an -> w (Guard an)
traverseValuesInGuard f (Guard ann input output nexts)
    = Guard ann
  <$> traverseValue f input
  <*> traverseValue f output
  <*> pure nexts

-- | Applies to each value, then combines all results.
traverseValuesInReducer :: (Monad w) => (Value an -> w (Value an)) -> Reducer an -> w (Reducer an)
traverseValuesInReducer f (Reducer ann main guards)
    = Reducer ann
  <$> traverseValuesInGuard f main
  <*> traverse (traverseValuesInGuard f) guards

-- | Reducers in all groups.
allProgramReducers :: Program an -> [Reducer an]
allProgramReducers = concatMap groupDefReducers . programGroups

substGroupProp1 :: [(Int, GroupRef an)] -> GroupRef an -> GroupRef an
substGroupProp1 substs x
  = case groupRefLoc x of
      GroupLocLocal _ idx
        -> case find (\(old, _) -> idx == old) substs of
             Nothing -> x
             Just (_, new)
               -> GroupRef
                { groupRefAnn = getAnn new
                , groupRefLoc = groupRefLoc new
                , groupRefProps = groupRefProps x ++ groupRefProps new
                }
      _ -> x

numBindsInValue1 :: Value an -> Int
numBindsInValue1 (ValuePrimitive _) = 0
numBindsInValue1 (ValueRecord _) = 0
numBindsInValue1 (ValueBind (Bind _ idx)) = idx

maxNumBindsInValue :: Value an -> Int
maxNumBindsInValue = getMax0 . foldValue (Max0 . numBindsInValue1)

maxNumBindsInGuard :: Guard an -> Int
maxNumBindsInGuard (Guard _ input output _)
  = max (maxNumBindsInValue input) (maxNumBindsInValue output)

maxNumBindsInReducer :: Reducer an -> Int
maxNumBindsInReducer (Reducer _ main guards)
  = maximum $ map maxNumBindsInGuard $ main : guards

maxNumBindsInReducers :: [Reducer an] -> Int
maxNumBindsInReducers stmts = maximum $ 0 : map maxNumBindsInReducer stmts

-- | The maximum number of binds used by the main reducers in the program - the maximum index in any used bind.
maxNumBindsInProgram :: Program an -> Int
maxNumBindsInProgram = maxNumBindsInReducers . allProgramReducers

bindsInValue1 :: Value an -> S.Set Int
bindsInValue1 (ValuePrimitive _) = S.empty
bindsInValue1 (ValueRecord _) = S.empty
bindsInValue1 (ValueBind bind) = S.singleton $ bindIdx bind

bindsInValue :: Value an -> S.Set Int
bindsInValue = foldValue bindsInValue1

bindsInGuardInput :: Guard an -> S.Set Int
bindsInGuardInput = bindsInValue . guardInput

bindsInReducerInput :: Reducer an -> S.Set Int
bindsInReducerInput (Reducer _ main guards)
   = bindsInGuardInput main <> foldMap bindsInGuardInput guards

declSpecToCompactDecl :: T.Text -> DeclSpec -> DeclCompact
declSpecToCompactDecl name (DeclSpec nodeName numArgs)
  = DeclCompact
  { declCompactHead = name <> "_" <> nodeName
  , declCompactNumProps = numArgs
  }

langSpecDecls :: LangSpec -> DeclSet
langSpecDecls spec
  = DeclSet
  { declSetRecords = S.fromList $ map (declSpecToCompactDecl langName) $ langSpecNodes spec
  , declSetFunctions = S.empty
  }
  where langName = langSpecName spec

librarySpecDecls :: LibrarySpec -> DeclSet
librarySpecDecls spec
  = DeclSet
  { declSetRecords = S.fromList $ map (declSpecToCompactDecl libraryName) $ librarySpecRecords spec
  , declSetFunctions = S.fromList $ map (declSpecToCompactDecl libraryName) $ librarySpecFunctions spec
  }
  where libraryName = librarySpecName spec

-- | All record declarations imported by a program in the given environment.
allImportedDecls :: SessionEnv -> DeclSet
allImportedDecls env
   = builtinDecls
  <> foldMap (langSpecDecls . languageSpec) (sessionEnvLanguages env)
  <> foldMap (librarySpecDecls . librarySpec) (sessionEnvLibraries env)

-- | All declarations accessible from the program, declared and imported.
getAllProgramDecls :: Program an -> SessionRes DeclSet
getAllProgramDecls prog = do
  env <- getSessionEnv
  let declaredDecls
        = DeclSet
        { declSetRecords = S.fromList $ map compactRecordDecl $ programRecordDecls prog
        , declSetFunctions = S.empty
        }
      importedDecls = allImportedDecls env
  pure $ declaredDecls <> importedDecls

groupFunctionName1 :: GroupLoc an -> [Annd T.Text an]
groupFunctionName1 (GroupLocGlobal _ _) = []
groupFunctionName1 (GroupLocLocal _ _) = []
groupFunctionName1 (GroupLocFunction ann head') = [Annd ann head']

-- | All function names in the program.
allProgramFunctionNames :: Program an -> [Annd T.Text an]
allProgramFunctionNames = concatMap (foldGroupsInReducer $ groupFunctionName1 . groupRefLoc) . allProgramReducers

functionUsedLibName1 :: T.Text -> Maybe T.Text
functionUsedLibName1 name
  = case T.splitOn "_" name of
      (libName : _ : _) -> Just libName
      _ -> Nothing

allProgramUsedLibNames :: Program an -> [T.Text]
allProgramUsedLibNames
  = nub . mapMaybe (functionUsedLibName1 . annd) . allProgramFunctionNames

-- | Gets all libraries used by the program.
getAllProgramUsedLibraries :: Program an -> SessionRes [Library]
getAllProgramUsedLibraries
  = traverse (libraryWithName StageExtracting) . allProgramUsedLibNames

-- | The reducers in the group and super-groups, substituting exported binds, and their mode.
allGroupDefReducers :: [GroupRef an] -> GroupDef an -> [Reducer an]
allGroupDefReducers props (GroupDef _ propIdxs reds)
    = map (mapGroupsInReducer (substGroupProp1 propSubsts)) reds
  where propSubsts = zip (map bindIdx propIdxs) props

-- | The reducers in the referenced group, substituting exported binds, and their mode.
allGroupRefReducers :: V.Vector (GroupDef an) -> GroupRef an -> [Reducer an]
allGroupRefReducers groups (GroupRef _ (GroupLocGlobal _ idx) props)
  = allGroupDefReducers props $ groups V.! idx
allGroupRefReducers _ (GroupRef _ _ _) = error "can't get all group ref statements from unsubstituted group prop"
