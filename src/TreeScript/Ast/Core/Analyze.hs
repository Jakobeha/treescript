{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( mapValue
  , mapValuesInGroupRef
  , mapValuesInGuard
  , mapValuesInReducer
  , mapGroupsInGroupRef
  , mapGroupsInGuard
  , mapGroupsInReducer
  , foldValue
  , foldValuesInGroupRef
  , foldValuesInGuard
  , foldValuesInReducer
  , foldGroupsInGroupRef
  , foldGroupsInGuard
  , foldGroupsInReducer
  , traverseValue
  , traverseValuesInGroupRef
  , traverseValuesInGuard
  , traverseValuesInReducer
  , allProgramReducers
  , maxNumBindsInProgram
  , bindsInValue
  , allGroupDefReducers
  , allGroupRefReducers
  , remExtra
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc

import Data.List hiding (group)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- | Applies to each child value, then combines all results.
mapValue :: (Value an -> Value an) -> Value an -> Value an
mapValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
mapValue f (ValueRecord (Record ann head' props))
  = f $ ValueRecord $ Record ann head' $ map (mapValue f) props
mapValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
mapValuesInGroupRef :: (Value an -> Value an) -> GroupRef an -> GroupRef an
mapValuesInGroupRef f (GroupRef ann loc vprops gprops)
  = GroupRef
  { groupRefAnn = ann
  , groupRefLoc = loc
  , groupRefValueProps = map (mapValue f) vprops
  , groupRefGroupProps = map (mapValuesInGroupRef f) gprops}

-- | Applies to each value, then combines all results.
mapValuesInGuard :: (Value an -> Value an) -> Guard an -> Guard an
mapValuesInGuard f (Guard ann input output nexts)
  = Guard
  { guardAnn = ann
  , guardInput = mapValue f input
  , guardOutput = mapValue f output
  , guardNexts = map (mapValuesInGroupRef f) nexts
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
mapGroupsInGroupRef f (GroupRef ann loc vprops gprops)
  = f $ GroupRef
  { groupRefAnn = ann
  , groupRefLoc = loc
  , groupRefValueProps = vprops
  , groupRefGroupProps = map (mapGroupsInGroupRef f) gprops
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
foldValuesInGroupRef :: (Monoid r) => (Value an -> r) -> GroupRef an -> r
foldValuesInGroupRef f (GroupRef _ _ vprops gprops)
   = foldMap (foldValue f) vprops
  <> foldMap (foldValuesInGroupRef f) gprops

-- | Applies to each value, then combines all results.
foldValuesInGuard :: (Monoid r) => (Value an -> r) -> Guard an -> r
foldValuesInGuard f (Guard _ input output nexts)
   = foldValue f input
  <> foldValue f output
  <> foldMap (foldValuesInGroupRef f) nexts

-- | Applies to each value, then combines all results.
foldValuesInReducer :: (Monoid r) => (Value an -> r) -> Reducer an -> r
foldValuesInReducer f (Reducer _ main guards)
   = foldValuesInGuard f main
  <> foldMap (foldValuesInGuard f) guards

-- | Applies to each child group, then combines all results. Properties left of parent groups.
foldGroupsInGroupRef :: (Monoid r) => (GroupRef an -> r) -> GroupRef an -> r
foldGroupsInGroupRef f grp
   = foldMap (foldGroupsInGroupRef f) (groupRefGroupProps grp)
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
traverseValuesInGroupRef :: (Monad w) => (Value an -> w (Value an)) -> GroupRef an -> w (GroupRef an)
traverseValuesInGroupRef f (GroupRef ann loc vprops gprops)
    = GroupRef ann loc
  <$> traverse (traverseValue f) vprops
  <*> traverse (traverseValuesInGroupRef f) gprops

-- | Applies to each value, then combines all results.
traverseValuesInGuard :: (Monad w) => (Value an -> w (Value an)) -> Guard an -> w (Guard an)
traverseValuesInGuard f (Guard ann input output nexts)
    = Guard ann
  <$> traverseValue f input
  <*> traverseValue f output
  <*> traverse (traverseValuesInGroupRef f) nexts

-- | Applies to each value, then combines all results.
traverseValuesInReducer :: (Monad w) => (Value an -> w (Value an)) -> Reducer an -> w (Reducer an)
traverseValuesInReducer f (Reducer ann main guards)
    = Reducer ann
  <$> traverseValuesInGuard f main
  <*> traverse (traverseValuesInGuard f) guards

-- | Reducers in all groups.
allProgramReducers :: Program e1 e2 an -> [Reducer an]
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
                , groupRefValueProps = groupRefValueProps x ++ groupRefValueProps new
                , groupRefGroupProps = groupRefGroupProps x ++ groupRefGroupProps new
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
maxNumBindsInProgram :: Program e1 e2 an -> Int
maxNumBindsInProgram = maxNumBindsInReducers . allProgramReducers

bindsInValue1 :: Value an -> S.Set Int
bindsInValue1 (ValuePrimitive _) = S.empty
bindsInValue1 (ValueRecord _) = S.empty
bindsInValue1 (ValueBind bind) = S.singleton $ bindIdx bind

bindsInValue :: Value an -> S.Set Int
bindsInValue = foldValue bindsInValue1

-- | The reducers in the group and super-groups, substituting exported binds, and their mode.
allGroupDefReducers :: [GroupRef an] -> GroupDef e1 e2 an -> [Reducer an]
allGroupDefReducers gprops (GroupDef _ _ gpropIdxs reds _)
    = map (mapGroupsInReducer (substGroupProp1 gpropSubsts)) reds
  where gpropSubsts = zip (map (bindIdx . snd) gpropIdxs) gprops

-- | The reducers in the referenced group, substituting exported binds, and their mode.
allGroupRefReducers :: M.Map (Symbol ()) (GroupDef e1 e2 an) -> GroupRef an -> [Reducer an]
allGroupRefReducers groups (GroupRef _ (GroupLocGlobal _ name) _ gprops)
  = allGroupDefReducers gprops $ groups M.! remAnns name
allGroupRefReducers _ (GroupRef _ _ _ _) = error "can't get all group ref statements from unsubstituted group prop"

remGroupBindEnv :: GroupDef e1 e2 an -> GroupDef () () an
remGroupBindEnv (GroupDef ann vprops gprops reds _)
  = GroupDef ann (map remText vprops) (map remText gprops) reds ()
  where remText (_, x) = ((), x)

remProgBindEnv :: Program e1 e2 an -> Program () () an
remProgBindEnv (Program ann mpath idecls rdecls exps grps libs)
  = Program ann mpath idecls rdecls exps (remGroupBindEnv <$> grps) libs

remExtra :: Program e1 e2 an -> Program () () ()
remExtra = remProgBindEnv . (() <$)
