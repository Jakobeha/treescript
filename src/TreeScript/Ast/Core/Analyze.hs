{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( mapValue
  , mapValuesInGroupRef
  , mapValuesInClause
  , mapValuesInReducer
  , foldValue
  , foldValuesInGroupRef
  , foldValuesInClause
  , foldValuesInReducer
  , traverseValue
  , traverseValuesInGroupRef
  , traverseValuesInClause
  , traverseValuesInReducer
  , maxNumBindsInReducers
  , langSpecDecls
  , allImportedDecls
  , getAllProgramDecls
  , allGroupDefReducers
  , allGroupRefReducers
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as N
import Data.Semigroup.Foldable
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

subst1 :: Value an -> Value an -> Value an -> Value an
subst1 old new x
  | x =@= old = new
  | otherwise = x

substMany1 :: [(Value an, Value an)] -> Value an -> Value an
substMany1 substs x = foldr (uncurry subst1) x substs

numBindsInValue1 :: Value an -> Int
numBindsInValue1 (ValuePrimitive _) = 0
numBindsInValue1 (ValueRecord _) = 0
numBindsInValue1 (ValueBind (Bind _ idx)) = idx

-- | The maximum number of binds used by the reducers, the maximum index in a bind inside any of the reducers.
maxNumBindsInReducers :: [Reducer an] -> Int
maxNumBindsInReducers = getMax0 . foldMap (foldValuesInReducer $ Max0 . numBindsInValue1)

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

-- | All record declarations imported by a program in the given environment.
allImportedDecls :: SessionEnv -> [RecordDeclCompact]
allImportedDecls env
  -- TODO Server functions
  = builtinDecls ++ concatMap (langSpecDecls . languageSpec) (sessionEnvLanguages env)

-- | All declarations accessible from the program, declared and imported.
getAllProgramDecls :: Program an -> SessionRes [RecordDeclCompact]
getAllProgramDecls prog = do
  env <- getSessionEnv
  let declaredDecls = map compactRecordDecl $ programRecordDecls prog
      importedDecls = allImportedDecls env
  pure $ declaredDecls ++ importedDecls

-- | The reducers in the group and super-groups, substituting exported binds.
allGroupDefReducers :: V.Vector (GroupDef an) -> [Value an] -> GroupDef an -> [Reducer an]
allGroupDefReducers groups props (GroupDef _ propIdxs supers immReds)
  = immReds' ++ concatMap (allGroupRefReducers groups) supers'
  where supers' = map (mapValuesInGroupRef $ substMany1 propSubsts) supers
        immReds' = map (mapValuesInReducer $ substMany1 propSubsts) immReds
        propSubsts = zip (map ValueBind propIdxs) props

-- | The reducers in the referenced group, substituting exported binds.
allGroupRefReducers :: V.Vector (GroupDef an) -> GroupRef an -> [Reducer an]
allGroupRefReducers groups (GroupRef _ idx props)
  = allGroupDefReducers groups props $ groups V.! idx
