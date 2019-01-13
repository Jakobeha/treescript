{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( mapValue
  , mapValuesInGroupRef
  , mapValuesInClause
  , mapValuesInReducer
  , mapValuesInStatement
  , foldValue
  , foldValuesInGroupRef
  , foldValuesInClause
  , foldValuesInReducer
  , foldValuesInStatement
  , traverseValue
  , traverseValuesInGroupRef
  , traverseValuesInClause
  , traverseValuesInReducer
  , traverseValuesInStatement
  , maxNumBindsInProgram
  , bindsInClause
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
import qualified Data.List.NonEmpty as N
import Data.Semigroup.Foldable
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
mapValuesInGroupRef f (GroupRef ann idx props)
  = GroupRef ann idx $ map (mapValue f) props

-- | Applies to each value, then combines all results.
mapValuesInClause :: (Value an -> Value an) -> ReducerClause an -> ReducerClause an
mapValuesInClause f (ReducerClause ann val groups)
  = ReducerClause ann (mapValue f val) $ map (mapValuesInGroupRef f) groups

-- | Applies to each value, then combines all results.
mapValuesInReducer :: (Value an -> Value an) -> Reducer an -> Reducer an
mapValuesInReducer f (Reducer ann input output)
  = Reducer ann (mapValuesInClause f input) (mapValuesInClause f output)

-- | Applies to each value, then combines all results.
mapValuesInStatement :: (Value an -> Value an) -> Statement an -> Statement an
mapValuesInStatement f (StatementGroup (GroupStmt ann group mode))
  = StatementGroup GroupStmt
  { groupStmtAnn = ann
  , groupStmtRef = mapValuesInGroupRef f group
  , groupStmtMode = mode
  }
mapValuesInStatement f (StatementReducer red) = StatementReducer $ mapValuesInReducer f red

-- | Applies to each child value, then combines all results.
foldValue :: (Semigroup r) => (Value an -> r) -> Value an -> r
foldValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
foldValue f (ValueRecord record)
  = foldl' foldInValue (f (ValueRecord record)) (recordProps record)
  where foldInValue res val = res <> foldValue f val
foldValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
foldValuesInGroupRef :: (Monoid r) => (Value an -> r) -> GroupRef an -> r
foldValuesInGroupRef f = foldMap (foldValue f) . groupRefProps

-- | Applies to each value, then combines all results.
foldValuesInClause :: (Semigroup r) => (Value an -> r) -> ReducerClause an -> r
foldValuesInClause f (ReducerClause _ val []) = foldValue f val
foldValuesInClause f (ReducerClause _ val groups)
  = fold1 $ foldValue f val N.:| mapMaybe (foldValuesInGroupRef $ Just . f) groups

-- | Applies to each value, then combines all results.
foldValuesInReducer :: (Semigroup r) => (Value an -> r) -> Reducer an -> r
foldValuesInReducer f (Reducer _ input output)
  = foldValuesInClause f input <> foldValuesInClause f output

-- | Applies to each value, then combines all results.
foldValuesInStatement :: (Monoid r) => (Value an -> r) -> Statement an -> r
foldValuesInStatement f (StatementGroup groupStmt) = foldValuesInGroupRef f $ groupStmtRef groupStmt
foldValuesInStatement f (StatementReducer red) = foldValuesInReducer f red

-- | Applies to each child value, then combines all results.
traverseValue :: (Monad w) => (Value an -> w (Value an)) -> Value an -> w (Value an)
traverseValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
traverseValue f (ValueRecord (Record ann head' props)) = f =<< traverseInValue
  where traverseInValue
          = ValueRecord . Record ann head' <$> traverse (traverseValue f) props
traverseValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
traverseValuesInGroupRef :: (Monad w) => (Value an -> w (Value an)) -> GroupRef an -> w (GroupRef an)
traverseValuesInGroupRef f (GroupRef ann idx props)
  = GroupRef ann idx <$> traverse (traverseValue f) props

-- | Applies to each value, then combines all results.
traverseValuesInClause :: (Monad w) => (Value an -> w (Value an)) -> ReducerClause an -> w (ReducerClause an)
traverseValuesInClause f (ReducerClause ann val groups)
  = ReducerClause ann <$> traverseValue f val <*> traverse (traverseValuesInGroupRef f) groups

-- | Applies to each value, then combines all results.
traverseValuesInReducer :: (Monad w) => (Value an -> w (Value an)) -> Reducer an -> w (Reducer an)
traverseValuesInReducer f (Reducer ann input output)
  = Reducer ann <$> traverseValuesInClause f input <*> traverseValuesInClause f output

-- | Applies to each value, then combines all results.
traverseValuesInStatement :: (Monad w) => (Value an -> w (Value an)) -> Statement an -> w (Statement an)
traverseValuesInStatement f (StatementGroup (GroupStmt ann group mode))
  = StatementGroup . mkGroupStmt <$> traverseValuesInGroupRef f group
  where mkGroupStmt group' = GroupStmt ann group' mode
traverseValuesInStatement f (StatementReducer red)
  = StatementReducer <$> traverseValuesInReducer f red

substMany1 :: [(Value an, Value an)] -> Value an -> Value an
substMany1 substs x
  = case find (\(old, _) -> x =@= old) substs of
      Nothing -> x
      Just (_, new) -> new

numBindsInValue1 :: Value an -> Int
numBindsInValue1 (ValuePrimitive _) = 0
numBindsInValue1 (ValueRecord _) = 0
numBindsInValue1 (ValueBind (Bind _ idx)) = idx

maxNumBindsInValue :: Value an -> Int
maxNumBindsInValue = getMax0 . foldValue (Max0 . numBindsInValue1)

maxNumBindsInClause :: V.Vector (GroupDef an) -> ReducerClause an -> Int
maxNumBindsInClause groupDefs (ReducerClause _ value groupRefs)
  = maximum $ maxNumBindsInValue value : map (maxNumBindsInGroupRef groupDefs) groupRefs

maxNumBindsInGroupRef :: V.Vector (GroupDef an) -> GroupRef an -> Int
maxNumBindsInGroupRef groupDefs groupRef
  = maxNumBindsInStatements groupDefs $ allGroupRefStatements groupDefs groupRef

maxNumBindsInReducer :: V.Vector (GroupDef an) -> Reducer an -> Int
maxNumBindsInReducer groups (Reducer _ input output)
  = max (maxNumBindsInClause groups input) (maxNumBindsInClause groups output)

maxNumBindsInStatement :: V.Vector (GroupDef an) -> Statement an -> Int
maxNumBindsInStatement groupDefs (StatementGroup groupStmt) = maxNumBindsInGroupRef groupDefs $ groupStmtRef groupStmt
maxNumBindsInStatement groups (StatementReducer red) = maxNumBindsInReducer groups red

maxNumBindsInStatements :: V.Vector (GroupDef an) -> [Statement an] -> Int
maxNumBindsInStatements groups stmts = maximum $ 0 : map (maxNumBindsInStatement groups) stmts

-- | The maximum number of binds used by the main reducers in the program - the maximum index in any used bind.
maxNumBindsInProgram :: Program an -> Int
maxNumBindsInProgram prog
  = maxNumBindsInStatements groups stmts
  where stmts = programMainStatements prog
        groups = V.fromList $ programGroups prog

bindsInValue1 :: Value an -> S.Set Int
bindsInValue1 (ValuePrimitive _) = S.empty
bindsInValue1 (ValueRecord _) = S.empty
bindsInValue1 (ValueBind bind) = S.singleton $ bindContent bind

bindsInClause :: ReducerClause an -> S.Set Int
bindsInClause = foldValuesInClause bindsInValue1

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
allProgramRecordHeads prog
   = foldMap (foldValuesInStatement valueRecordHeads1)
   $ programMainStatements prog
  ++ concatMap groupDefStatements (programGroups prog)

recordHeadUsedLibName1 :: RecordHead -> Maybe T.Text
recordHeadUsedLibName1 (RecordHead isFunc name)
  | isFunc
  = case T.splitOn "_" name of
      [libName, _] -> Just libName
      _ -> error $ T.unpack $ "unexpected function name - " <> name
  | otherwise = Nothing

allProgramUsedLibNames :: Program an -> [T.Text]
allProgramUsedLibNames
  = nub . mapMaybe recordHeadUsedLibName1 . allProgramRecordHeads

-- | Gets all libraries used by the program.
getAllProgramUsedLibraries :: Program an -> SessionRes [Library]
getAllProgramUsedLibraries
  = traverse (libraryWithName StageExtracting) . allProgramUsedLibNames

-- | The statements in the group and super-groups, substituting exported binds.
allGroupDefStatements :: [Value an] -> GroupDef an -> [Statement an]
allGroupDefStatements props (GroupDef _ propIdxs stmts)
  = map (mapValuesInStatement $ substMany1 propSubsts) stmts
  where propSubsts = zip (map ValueBind propIdxs) props

-- | The statements in the referenced group, substituting exported binds.
allGroupRefStatements :: V.Vector (GroupDef an) -> GroupRef an -> [Statement an]
allGroupRefStatements groups (GroupRef _ idx props)
  = allGroupDefStatements props $ groups V.! idx
