{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze
  ( traverseAst
  , foldAst
  , mapAst
  , coerceSmall
  , remExtra
  , allProgramReducers
  , maxNumBindsInProgram
  , bindsInValue
  , allGroupDefReducers
  , allGroupRefReducers
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Ast.Core.Classes
import TreeScript.Misc

import Control.Monad
import Control.Monad.Writer.Strict
import Data.Functor.Identity
import Data.List hiding (group)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

traverseAstSelf :: (Monad w) => Term o -> Term i -> (i a1 a2 a3 a4 a5 a6 t an -> w (i a1 a2 a3 a4 a5 a6 t an)) -> o a1 a2 a3 a4 a5 a6 t an -> w (o a1 a2 a3 a4 a5 a6 t an)
traverseAstSelf TTypePart TTypePart f = f
traverseAstSelf TType TType f = f
traverseAstSelf TPrim TPrim f = f
traverseAstSelf TSymbol TSymbol f = f
traverseAstSelf TRecord TRecord f = f
traverseAstSelf TBind TBind f = f
traverseAstSelf TValue TValue f = f
traverseAstSelf TGroupLoc TGroupLoc f = f
traverseAstSelf TGroupRef TGroupRef f = f
traverseAstSelf TGuard TGuard f = f
traverseAstSelf TReducer TReducer f = f
traverseAstSelf TGroupDef TGroupDef f = f
traverseAstSelf TProgram TProgram f = f
traverseAstSelf _ _ _ = pure

traverseAstSub :: (Monad w) => Term o -> Term i -> (i a1 a2 a3 a4 a5 a6 t an -> w (i a1 a2 a3 a4 a5 a6 t an)) -> o a1 a2 a3 a4 a5 a6 t an -> w (o a1 a2 a3 a4 a5 a6 t an)
traverseAstSub TTypePart _ _ x = pure x
traverseAstSub TType ti f (Type ann parts) = Type ann <$> traverse (traverseAst TTypePart ti f) parts
traverseAstSub TPrim _ _ x = pure x
traverseAstSub TSymbol _ _ x = pure x
traverseAstSub TRecord ti f (Record ann t head' props) = Record ann t <$> traverseAst TSymbol ti f head' <*> traverse (traverseAst TValue ti f) props
traverseAstSub TBind _ _ x = pure x
traverseAstSub TValue ti f (ValuePrimitive prim) = ValuePrimitive <$> traverseAst TPrim ti f prim
traverseAstSub TValue ti f (ValueRecord record) = ValueRecord <$> traverseAst TRecord ti f record
traverseAstSub TValue ti f (ValueBind bind) = ValueBind <$> traverseAst TBind ti f bind
traverseAstSub TGroupLoc ti f (GroupLocGlobal ann sym) = GroupLocGlobal ann <$> traverseAst TSymbol ti f sym
traverseAstSub TGroupLoc _ _ (GroupLocLocal ann idx) = pure $ GroupLocLocal ann idx
traverseAstSub TGroupLoc ti f (GroupLocFunction ann sym) = GroupLocFunction ann <$> traverseAst TSymbol ti f sym
traverseAstSub TGroupRef ti f (GroupRef ann loc vprops gprops) = GroupRef ann <$> traverseAst TGroupLoc ti f loc <*> traverse (traverseAst TValue ti f) vprops <*> traverse (traverseAst TGroupRef ti f) gprops
traverseAstSub TGuard ti f (Guard ann inp out nxts) = Guard ann <$> traverseAst TValue ti f inp <*> traverseAst TValue ti f out <*> traverse (traverseAst TGroupRef ti f) nxts
traverseAstSub TReducer ti f (Reducer ann main guards) = Reducer ann <$> traverseAst TGuard ti f main <*> traverse (traverseAst TGuard ti f) guards
traverseAstSub TGroupDef ti f (GroupDef ann vprops gprops reds env) = GroupDef ann <$> pure vprops <*> pure gprops <*> traverse (traverseAst TReducer ti f) reds <*> pure env
traverseAstSub TProgram ti f (Program ann pth idcls rdcls fdcls alis exps castReds grps libs) = Program ann pth idcls rdcls fdcls alis exps <$> traverse (traverseAst TReducer ti f) castReds <*> traverse (traverseAst TGroupDef ti f) grps <*> pure libs

-- | Traverses each child node (includes recursive), inner to outer.
traverseAst :: (Monad w) => Term o -> Term i -> (i a1 a2 a3 a4 a5 a6 t an -> w (i a1 a2 a3 a4 a5 a6 t an)) -> o a1 a2 a3 a4 a5 a6 t an -> w (o a1 a2 a3 a4 a5 a6 t an)
traverseAst to ti f = traverseAstSelf to ti f <=< traverseAstSub to ti f

-- | Folds each child node (includes recursive), inner to outer.
foldAst :: (Monoid r) => Term o -> Term i -> (i a1 a2 a3 a4 a5 a6 t an -> r) -> o a1 a2 a3 a4 a5 a6 t an -> r
foldAst to ti f = execWriter . traverseAst to ti (\x -> WriterT $ Identity (x, f x))

-- | Maps each child node (includes recursive), inner to outer.
mapAst :: Term o -> Term i -> (i a1 a2 a3 a4 a5 a6 t an -> i a1 a2 a3 a4 a5 a6 t an) -> o a1 a2 a3 a4 a5 a6 t an -> o a1 a2 a3 a4 a5 a6 t an
mapAst to ti f = runIdentity . traverseAst to ti (Identity . f)

coerceSmall :: (FunctorAst a) => a a1 a2 a3 a4 a5 a6 t an -> a b1 b2 b3 b4 b5 b6 t2 an
coerceSmall = mapA $ MapA undefined undefined undefined undefined undefined undefined undefined id

remExtra :: (FunctorAst a) => PR a -> PF a
remExtra = mapA $ MapA rem' rem' rem' rem' rem' rem' id rem'
  where rem' _ = ()

-- | Reducers in all groups.
allProgramReducers :: Program a1 a2 a3 a4 a5 a6 t an -> [Reducer a1 a2 a3 a4 a5 a6 t an]
allProgramReducers = concatMap groupDefReducers . programGroups

substGroupProp1 :: [(Int, GroupRef a1 a2 a3 a4 a5 a6 t an)] -> GroupRef a1 a2 a3 a4 a5 a6 t an -> GroupRef a1 a2 a3 a4 a5 a6 t an
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

numBindsInValue1 :: Value a1 a2 a3 a4 a5 a6 t an -> Int
numBindsInValue1 (ValuePrimitive _) = 0
numBindsInValue1 (ValueRecord _) = 0
numBindsInValue1 (ValueBind (Bind _ _ idx)) = idx

maxNumBindsInValue :: Value a1 a2 a3 a4 a5 a6 t an -> Int
maxNumBindsInValue = getMax0 . foldAst TValue TValue (Max0 . numBindsInValue1)

maxNumBindsInGuard :: Guard a1 a2 a3 a4 a5 a6 t an -> Int
maxNumBindsInGuard (Guard _ input output _)
  = max (maxNumBindsInValue input) (maxNumBindsInValue output)

maxNumBindsInReducer :: Reducer a1 a2 a3 a4 a5 a6 t an -> Int
maxNumBindsInReducer (Reducer _ main guards)
  = maximum $ map maxNumBindsInGuard $ main : guards

maxNumBindsInReducers :: [Reducer a1 a2 a3 a4 a5 a6 t an] -> Int
maxNumBindsInReducers stmts = maximum $ 0 : map maxNumBindsInReducer stmts

-- | The maximum number of binds used by the main reducers in the program - the maximum index in any used bind.
maxNumBindsInProgram :: Program a1 a2 a3 a4 a5 a6 t an -> Int
maxNumBindsInProgram = maxNumBindsInReducers . allProgramReducers

bindsInValue1 :: Value a1 a2 a3 a4 a5 a6 t an -> S.Set Int
bindsInValue1 (ValuePrimitive _) = S.empty
bindsInValue1 (ValueRecord _) = S.empty
bindsInValue1 (ValueBind bind) = S.singleton $ bindIdx bind

bindsInValue :: Value a1 a2 a3 a4 a5 a6 t an -> S.Set Int
bindsInValue = foldAst TValue TValue bindsInValue1

-- | The reducers in the group and super-groups, substituting exported binds, and their mode.
allGroupDefReducers :: [GroupRef a1 a2 a3 a4 a5 a6 t an] -> GroupDef a1 a2 a3 a4 a5 a6 t an -> [Reducer a1 a2 a3 a4 a5 a6 t an]
allGroupDefReducers gprops (GroupDef _ _ gpropIdxs reds _)
    = map (mapAst TReducer TGroupRef (substGroupProp1 gpropSubsts)) reds
  where gpropSubsts = zip (map snd gpropIdxs) gprops

-- | The reducers in the referenced group, substituting exported binds, and their mode.
allGroupRefReducers :: M.Map (PF Symbol) (GroupDef a1 a2 a3 a4 a5 a6 t an) -> GroupRef a1 a2 a3 a4 a5 a6 t an -> [Reducer a1 a2 a3 a4 a5 a6 t an]
allGroupRefReducers groups (GroupRef _ (GroupLocGlobal _ name) _ gprops)
  = allGroupDefReducers gprops $ groups M.! coerceSmall (remAnns name)
allGroupRefReducers _ (GroupRef _ _ _ _) = error "can't get all group ref statements from unsubstituted group prop"
