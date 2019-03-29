{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( mapValue
  , mapValuesInGroupRef
  , mapValuesInReducer
  , mapValuesInStatement
  , mapGroupsInGroupRef
  , mapGroupsInReducer
  , mapGroupsInStatement
  , foldValue
  , foldValuesInGroupRef
  , foldValuesInReducer
  , foldValuesInStatement
  , traverseValue
  , traverseValuesInGroupRef
  , traverseValuesInReducer
  , traverseValuesInStatement
  , groupDefStatementList
  , allProgramStatements
  , maxNumBindsInProgram
  , bindsInValue
  , bindsInGroupRef
  , bindsInReducerInput
  , bindsInGuardInput
  , langSpecDecls
  , allImportedDecls
  , getAllProgramDecls
  , allProgramRecordHeads
  , getAllProgramUsedLibraries
  , allGroupDefStatements
  , allGroupRefStatements
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import Data.List hiding (group)
import Data.Maybe
import qualified Data.Array as A
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
mapValuesInGroupRef :: (Value an -> Value an) -> GroupRef an -> GroupRef an
mapValuesInGroupRef f (GroupRef ann isProp idx gprops vprops)
  = GroupRef ann isProp idx
  ( map (mapValuesInGroupRef f) gprops )
  $ map (mapValue f) vprops

-- | Applies to each value, then combines all results.
mapValuesInReducer :: (Value an -> Value an) -> Reducer an -> Reducer an
mapValuesInReducer f (Reducer ann input output nexts guards)
  = Reducer
  { reducerAnn = ann
  , reducerInput = mapValue f input
  , reducerOutput = mapValue f output
  , reducerNexts = map (mapValuesInGroupRef f) nexts
  , reducerGuards = map (mapValuesInStatement f) guards
  }

-- | Applies to each value, then combines all results.
mapValuesInStatement :: (Value an -> Value an) -> Statement an -> Statement an
mapValuesInStatement f (StatementGroup group) = StatementGroup $ mapValuesInGroupRef f group
mapValuesInStatement f (StatementReducer red) = StatementReducer $ mapValuesInReducer f red

-- | Applies to each value, then combines all results.
mapGroupsInGroupRef :: (GroupRef an -> GroupRef an) -> GroupRef an -> GroupRef an
mapGroupsInGroupRef f (GroupRef ann isProp idx gprops vprops)
  = GroupRef
  { groupRefAnn = ann
  , groupRefIsProp = isProp
  , groupRefIdx = idx
  , groupRefGroupProps = map (mapGroupsInGroupRef f) gprops
  , groupRefValueProps = vprops
  }

-- | Applies to each value, then combines all results.
mapGroupsInReducer :: (GroupRef an -> GroupRef an) -> Reducer an -> Reducer an
mapGroupsInReducer f (Reducer ann input output nexts guards)
  = Reducer
  { reducerAnn = ann
  , reducerInput = input
  , reducerOutput = output
  , reducerNexts = map (mapGroupsInGroupRef f) nexts
  , reducerGuards = map (mapGroupsInStatement f) guards
  }

-- | Applies to each group, then combines all results.
mapGroupsInStatement :: (GroupRef an -> GroupRef an) -> Statement an -> Statement an
mapGroupsInStatement f (StatementGroup group) = StatementGroup $ mapGroupsInGroupRef f group
mapGroupsInStatement f (StatementReducer red) = StatementReducer $ mapGroupsInReducer f red

-- | Applies to each child value, then combines all results.
foldValue :: (Semigroup r) => (Value an -> r) -> Bool -> Value an -> r
foldValue f _ (ValuePrimitive prim) = f $ ValuePrimitive prim
foldValue f flush (ValueRecord record)
  | recordHead record == flushRecordHead && not flush
  = f $ ValueRecord record
  | otherwise
  = foldl' foldInValue (f (ValueRecord record)) (recordProps record)
  where foldInValue res val = res <> foldValue f flush val
foldValue f _ (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
foldValuesInGroupRef :: (Monoid r) => (Value an -> r) -> Bool -> GroupRef an -> r
foldValuesInGroupRef f flush (GroupRef _ _ _ gprops vprops)
   = foldMap (foldValuesInGroupRef f flush) gprops
  <> foldMap (foldValue f flush) vprops

-- | Applies to each value, then combines all results.
foldValuesInReducer :: (Monoid r) => (Value an -> r) -> Bool -> Reducer an -> r
foldValuesInReducer f flush (Reducer _ input output nexts guards)
   = foldValue f flush input
  <> foldValue f flush output
  <> foldMap (foldValuesInGroupRef f flush) nexts
  <> foldMap (foldValuesInStatement f flush) guards

-- | Applies to each value, then combines all results.
foldValuesInStatement :: (Monoid r) => (Value an -> r) -> Bool -> Statement an -> r
foldValuesInStatement f flush (StatementGroup group) = foldValuesInGroupRef f flush group
foldValuesInStatement f flush (StatementReducer red) = foldValuesInReducer f flush red

-- | Applies to each child value, then combines all results.
traverseValue :: (Monad w) => (Value an -> w (Value an)) -> Value an -> w (Value an)
traverseValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
traverseValue f (ValueRecord (Record ann head' props)) = f =<< traverseInValue
  where traverseInValue
          = ValueRecord . Record ann head' <$> traverse (traverseValue f) props
traverseValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
traverseValuesInGroupRef :: (Monad w) => (Value an -> w (Value an)) -> GroupRef an -> w (GroupRef an)
traverseValuesInGroupRef f (GroupRef ann isProp idx gprops vprops)
    = GroupRef ann isProp idx
  <$> traverse (traverseValuesInGroupRef f) gprops
  <*> traverse (traverseValue f) vprops

-- | Applies to each value, then combines all results.
traverseValuesInReducer :: (Monad w) => (Value an -> w (Value an)) -> Reducer an -> w (Reducer an)
traverseValuesInReducer f (Reducer ann input output nexts guards)
    = Reducer ann
  <$> traverseValue f input
  <*> traverseValue f output
  <*> traverse (traverseValuesInGroupRef f) nexts
  <*> traverse (traverseValuesInStatement f) guards

-- | Applies to each value, then combines all results.
traverseValuesInStatement :: (Monad w) => (Value an -> w (Value an)) -> Statement an -> w (Statement an)
traverseValuesInStatement f (StatementGroup group) = StatementGroup <$> traverseValuesInGroupRef f group
traverseValuesInStatement f (StatementReducer red) = StatementReducer <$> traverseValuesInReducer f red

-- | All the @GroupDef@'s statements for every reduce type.
groupDefStatementList :: GroupDef an -> [Statement an]
groupDefStatementList = concat . A.elems . groupDefStatements

-- | The main statements and statements in all groups.
allProgramStatements :: Program an -> [Statement an]
allProgramStatements prog
   = programMainStatements prog
  ++ concatMap groupDefStatementList (programGroups prog)

substValue1 :: [(Value an, Value an)] -> Value an -> Value an
substValue1 substs x
  = case find (\(old, _) -> x =@= old) substs of
      Nothing -> x
      Just (_, new) -> new

substGroupProp1 :: [(Int, GroupRef an)] -> GroupRef an -> GroupRef an
substGroupProp1 substs x
  | groupRefIsProp x
  = case find (\(old, _) -> groupRefIdx x == old) substs of
      Nothing -> x
      Just (_, new)
        -> GroupRef
         { groupRefAnn = getAnn new
         , groupRefIsProp = groupRefIsProp new
         , groupRefIdx = groupRefIdx new
         , groupRefGroupProps = groupRefGroupProps x ++ groupRefGroupProps new
         , groupRefValueProps = groupRefValueProps x ++ groupRefValueProps new
         }
  | otherwise = x

numBindsInValue1 :: Value an -> Int
numBindsInValue1 (ValuePrimitive _) = 0
numBindsInValue1 (ValueRecord _) = 0
numBindsInValue1 (ValueBind (Bind _ idx)) = idx

maxNumBindsInValue :: Value an -> Int
maxNumBindsInValue = getMax0 . foldValue (Max0 . numBindsInValue1) True

maxNumBindsInReducer :: Reducer an -> Int
maxNumBindsInReducer (Reducer _ input output _ _)
  = max (maxNumBindsInValue input) (maxNumBindsInValue output)

maxNumBindsInStatement :: Statement an -> Int
maxNumBindsInStatement (StatementGroup _) = 0 -- Already counted in group def
maxNumBindsInStatement (StatementReducer red) = maxNumBindsInReducer red

maxNumBindsInStatements :: [Statement an] -> Int
maxNumBindsInStatements stmts = maximum $ 0 : map maxNumBindsInStatement stmts

-- | The maximum number of binds used by the main reducers in the program - the maximum index in any used bind.
maxNumBindsInProgram :: Program an -> Int
maxNumBindsInProgram = maxNumBindsInStatements . allProgramStatements

bindsInValue1 :: Value an -> S.Set Int
bindsInValue1 (ValuePrimitive _) = S.empty
bindsInValue1 (ValueRecord _) = S.empty
bindsInValue1 (ValueBind bind) = S.singleton $ bindIdx bind

bindsInValue :: Value an -> S.Set Int
bindsInValue = foldValue bindsInValue1 True

bindsInGroupRef :: GroupRef an -> S.Set Int
bindsInGroupRef = foldValuesInGroupRef bindsInValue1 True

bindsInReducerInput :: V.Vector (GroupDef an) -> Reducer an -> S.Set Int
bindsInReducerInput groups (Reducer _ input _ _ guards)
   = bindsInValue input <> foldMap (bindsInGuardInput groups) guards

bindsInGuardInput :: V.Vector (GroupDef an) -> Statement an -> S.Set Int
bindsInGuardInput groupDefs (StatementGroup groupRef)
  = foldMap (bindsInGuardInput groupDefs)
  $ concat
  $ A.elems
  $ allGroupRefStatements groupDefs groupRef
bindsInGuardInput groups (StatementReducer red)
  = bindsInReducerInput groups red

langSpecDecls :: LangSpec -> [RecordDeclCompact]
langSpecDecls spec
  = map nodeSpecToCompactDecl $ langSpecNodes spec
  where langName = langSpecName spec
        nodeSpecToCompactDecl (AstNodeSpec nodeName numArgs)
          = RecordDeclCompact
          { recordDeclCompactHead
              = RecordHead
              { recordHeadIsFunc = False
              , recordHeadName = langName <> "_" <> nodeName
              }
          , recordDeclCompactNumProps = numArgs
          }

librarySpecDecls :: LibrarySpec -> [RecordDeclCompact]
librarySpecDecls spec
  = map functionSpecToCompactDecl $ librarySpecFunctions spec
  where libraryName = librarySpecName spec
        functionSpecToCompactDecl (FunctionSpec funcName numArgs)
          = RecordDeclCompact
          { recordDeclCompactHead
              = RecordHead
              { recordHeadIsFunc = True
              , recordHeadName = libraryName <> "_" <> funcName
              }
          , recordDeclCompactNumProps = numArgs
          }

-- | All record declarations imported by a program in the given environment.
allImportedDecls :: SessionEnv -> [RecordDeclCompact]
allImportedDecls env
   = builtinDecls
  ++ concatMap (langSpecDecls . languageSpec) (sessionEnvLanguages env)
  ++ concatMap (librarySpecDecls . librarySpec) (sessionEnvLibraries env)

-- | All declarations accessible from the program, declared and imported.
getAllProgramDecls :: Program an -> SessionRes [RecordDeclCompact]
getAllProgramDecls prog = do
  env <- getSessionEnv
  let declaredDecls = map compactRecordDecl $ programRecordDecls prog
      importedDecls = allImportedDecls env
  pure $ declaredDecls ++ importedDecls

valueRecordHeads1 :: Value an -> [RecordHead]
valueRecordHeads1 (ValuePrimitive _) = []
valueRecordHeads1 (ValueRecord record) = [recordHead record]
valueRecordHeads1 (ValueBind _) = []

-- | Heads of all records in the program.
allProgramRecordHeads :: Program an -> [RecordHead]
allProgramRecordHeads
  = foldMap (foldValuesInStatement valueRecordHeads1 True) . allProgramStatements

recordHeadUsedLibName1 :: RecordHead -> Maybe T.Text
recordHeadUsedLibName1 (RecordHead isFunc name)
  | isFunc
  = case T.splitOn "_" name of
      (libName : _ : _) -> Just libName
      _ -> Nothing
  | otherwise = Nothing

allProgramUsedLibNames :: Program an -> [T.Text]
allProgramUsedLibNames
  = nub . mapMaybe recordHeadUsedLibName1 . allProgramRecordHeads

-- | Gets all libraries used by the program.
getAllProgramUsedLibraries :: Program an -> SessionRes [Library]
getAllProgramUsedLibraries
  = traverse (libraryWithName StageExtracting) . allProgramUsedLibNames

-- | The statements in the group and super-groups, substituting exported binds, and their mode.
allGroupDefStatements :: [GroupRef an] -> [Value an] -> GroupDef an -> A.Array ReduceType [Statement an]
allGroupDefStatements gprops vprops (GroupDef _ gpropIdxs vpropIdxs stmts)
    = map
    ( mapGroupsInStatement (substGroupProp1 gpropSubsts)
    . mapValuesInStatement (substValue1 vpropSubsts) )
  <$> stmts
  where gpropSubsts = zip (map bindIdx gpropIdxs) gprops
        vpropSubsts = zip (map ValueBind vpropIdxs) vprops

-- | The statements in the referenced group, substituting exported binds, and their mode.
allGroupRefStatements :: V.Vector (GroupDef an) -> GroupRef an -> A.Array ReduceType [Statement an]
allGroupRefStatements groups (GroupRef _ False idx gprops vprops)
  = allGroupDefStatements gprops vprops $ groups V.! idx
allGroupRefStatements _ (GroupRef _ True _ _ _) = error "can't get all group ref statements from unsubstituted group prop"
