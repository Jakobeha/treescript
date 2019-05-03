{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( traverseAst
  , foldAst
  , mapAst
  , allProgramReducers
  , maxNumBindsInProgram
  , bindsInValue
  , allGroupDefReducers
  , allGroupRefReducers
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc

import Control.Monad
import Control.Monad.Writer.Strict
import Data.Functor.Identity
import Data.List hiding (group)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

traverseAstSelf :: (Monad w) => Term o -> Term i -> (i t -> w (i t)) -> o t -> w (o t)
traverseAstSelf TTypePart TTypePart f = f
traverseAstSelf TType TType f = f
traverseAstSelf TPrim TPrim f = f
traverseAstSelf TSymbol TSymbol f = f
traverseAstSelf TRecord TRecord f = f
traverseAstSelf TBind TBind f = f
traverseAstSelf TValue TValue f = f
traverseAstSelf TGroupLoc TGroupLoc f = f
traverseAstSelf TGroupRef TGroupRef f = f
traverseAstSelf TCastRef TCastRef f = f
traverseAstSelf TNext TNext f = f
traverseAstSelf TGuard TGuard f = f
traverseAstSelf TReducer TReducer f = f
traverseAstSelf TGroupDef TGroupDef f = f
traverseAstSelf TProgram TProgram f = f
traverseAstSelf _ _ _ = pure

traverseAstSub :: (Monad w) => Term o -> Term i -> (i t -> w (i t)) -> o t -> w (o t)
traverseAstSub TTypePart _ _ x = pure x
traverseAstSub TType ti f (Type ann parts) = Type ann <$> traverse (traverseAst TTypePart ti f) parts
traverseAstSub TPrim _ _ x = pure x
traverseAstSub TSymbol _ _ x = pure x
traverseAstSub TRecord ti f (Record ann head' props) = Record ann <$> traverseAst TSymbol ti f head' <*> traverse (traverseAst TValue ti f) props
traverseAstSub TBind _ _ x = pure x
traverseAstSub TValue ti f (ValuePrimitive t prim) = ValuePrimitive t <$> traverseAst TPrim ti f prim
traverseAstSub TValue ti f (ValueRecord t record) = ValueRecord t <$> traverseAst TRecord ti f record
traverseAstSub TValue ti f (ValueBind t bind) = ValueBind t <$> traverseAst TBind ti f bind
traverseAstSub TGroupLoc ti f (GroupLocGlobal ann sym) = GroupLocGlobal ann <$> traverseAst TSymbol ti f sym
traverseAstSub TGroupLoc _ _ (GroupLocLocal ann idx) = pure $ GroupLocLocal ann idx
traverseAstSub TGroupLoc ti f (GroupLocFunction ann sym) = GroupLocFunction ann <$> traverseAst TSymbol ti f sym
traverseAstSub TGroupRef ti f (GroupRef ann loc vprops gprops) = GroupRef ann <$> traverseAst TGroupLoc ti f loc <*> traverse (traverseAst TValue ti f) vprops <*> traverse (traverseAst TGroupRef ti f) gprops
traverseAstSub TCastRef _ _ x = pure x
traverseAstSub TNext ti f (NextCast cast) = NextCast <$> traverseAst TCastRef ti f cast
traverseAstSub TNext ti f (NextGroup grp) = NextGroup <$> traverseAst TGroupRef ti f grp
traverseAstSub TGuard ti f (Guard ann inp out nxts) = Guard ann <$> traverseAst TValue ti f inp <*> traverseAst TValue ti f out <*> traverse (traverseAst TNext ti f) nxts
traverseAstSub TReducer ti f (Reducer ann main guards) = Reducer ann <$> traverseAst TGuard ti f main <*> traverse (traverseAst TGuard ti f) guards
traverseAstSub TGroupDef ti f (GroupDef ann vprops gprops reds) = GroupDef ann <$> pure vprops <*> pure gprops <*> traverse (traverseAst TReducer ti f) reds
traverseAstSub TProgram ti f (Program ann pth idcls rdcls fdcls alis exps castReds grps libs) = Program ann pth idcls rdcls fdcls alis exps <$> traverse (traverseAst TReducer ti f) castReds <*> traverse (traverseAst TGroupDef ti f) grps <*> pure libs

-- | Traverses each child node (includes recursive), inner to outer.
traverseAst :: (Monad w) => Term o -> Term i -> (i t -> w (i t)) -> o t -> w (o t)
traverseAst to ti f = traverseAstSelf to ti f <=< traverseAstSub to ti f

-- | Folds each child node (includes recursive), inner to outer.
foldAst :: (Monoid r) => Term o -> Term i -> (i t -> r) -> o t -> r
foldAst to ti f = execWriter . traverseAst to ti (\x -> WriterT $ Identity (x, f x))

-- | Maps each child node (includes recursive), inner to outer.
mapAst :: Term o -> Term i -> (i t -> i t) -> o t -> o t
mapAst to ti f = runIdentity . traverseAst to ti (Identity . f)

-- | Reducers in all groups.
allProgramReducers :: Program t -> [Reducer t]
allProgramReducers = concatMap groupDefReducers . programGroups

substGroupProp1 :: [(Int, GroupRef t)] -> GroupRef t -> GroupRef t
substGroupProp1 substs x
  = case groupRefLoc x of
      GroupLocLocal _ idx
        -> case find (\(old, _) -> idx == old) substs of
             Nothing -> x
             Just (_, new)
               -> GroupRef
                { groupRefAnn = groupRefAnn new
                , groupRefLoc = groupRefLoc new
                , groupRefValueProps = groupRefValueProps x ++ groupRefValueProps new
                , groupRefGroupProps = groupRefGroupProps x ++ groupRefGroupProps new
                }
      _ -> x

numBindsInValue1 :: Value t -> Int
numBindsInValue1 (ValuePrimitive _ _) = 0
numBindsInValue1 (ValueRecord _ _) = 0
numBindsInValue1 (ValueBind _ (Bind _ idx)) = idx

maxNumBindsInValue :: Value t -> Int
maxNumBindsInValue = getMax0 . foldAst TValue TValue (Max0 . numBindsInValue1)

maxNumBindsInGuard :: Guard t -> Int
maxNumBindsInGuard (Guard _ input output _)
  = max (maxNumBindsInValue input) (maxNumBindsInValue output)

maxNumBindsInReducer :: Reducer t -> Int
maxNumBindsInReducer (Reducer _ main guards)
  = maximum $ map maxNumBindsInGuard $ main : guards

maxNumBindsInReducers :: [Reducer t] -> Int
maxNumBindsInReducers stmts = maximum $ 0 : map maxNumBindsInReducer stmts

-- | The maximum number of binds used by the main reducers in the program - the maximum index in any used bind.
maxNumBindsInProgram :: Program t -> Int
maxNumBindsInProgram = maxNumBindsInReducers . allProgramReducers

bindsInValue1 :: Value t -> S.Set Int
bindsInValue1 (ValuePrimitive _ _) = S.empty
bindsInValue1 (ValueRecord _ _) = S.empty
bindsInValue1 (ValueBind _ bind) = S.singleton $ bindIdx bind

bindsInValue :: Value t -> S.Set Int
bindsInValue = foldAst TValue TValue bindsInValue1

-- | The reducers in the group and super-groups, substituting exported binds, and their mode.
allGroupDefReducers :: [GroupRef t] -> GroupDef t -> [Reducer t]
allGroupDefReducers gprops (GroupDef _ _ gpropIdxs reds)
    = map (mapAst TReducer TGroupRef (substGroupProp1 gpropSubsts)) reds
  where gpropSubsts = zip (map groupDefPropIdx gpropIdxs) gprops

-- | The reducers in the referenced group, substituting exported binds, and their mode.
allGroupRefReducers :: M.Map (Symbol ()) (GroupDef t) -> GroupRef t -> [Reducer t]
allGroupRefReducers groups (GroupRef _ (GroupLocGlobal _ name) _ gprops)
  = allGroupDefReducers gprops $ groups M.! (() <$ name)
allGroupRefReducers _ (GroupRef _ _ _ _) = error "can't get all group ref statements from unsubstituted group prop"
