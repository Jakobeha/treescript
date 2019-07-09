{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to manipulate @Core@ ASTs.
module TreeScript.Ast.Core.Analyze.Misc
  ( traverseStx
  , traverseStx_
  , foldStx
  , mapStx
  , traverseValue
  , traverseValue_
  , foldValue
  , mapValue
  , traverseProgram
  , traverseProgram_
  , foldProgram
  , mapProgram
  , moveProgPath
  , allProgramReducers
  , substStxSplices
  , substBinds
  , substSplices
  , maxNumBindsInReducer
  , maxNumBindsInProgram
  , bindsInValue
  , allGroupDefReducers
  , allGroupRefReducers
  , valAtPath
  )
where

import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import           Control.Monad
import           Control.Monad.Writer.Strict
import           Data.Functor.Identity
import           Data.List               hiding ( group )
import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M

traverseStxSelf :: (Monad w) => StxTerm o -> StxTerm i -> (i -> w i) -> o -> w o
traverseStxSelf TStxWord   TStxWord   f = f
traverseStxSelf TStxPunc   TStxPunc   f = f
traverseStxSelf TStxString TStxString f = f
traverseStxSelf TStxInt    TStxInt    f = f
traverseStxSelf TStxSplice TStxSplice f = f
traverseStxSelf TStxBlock  TStxBlock  f = f
traverseStxSelf TStx    TStx    f = f
traverseStxSelf TStxIdd   TStxIdd   f = f
traverseStxSelf TStxBlob   TStxBlob   f = f
traverseStxSelf _          _          _ = pure

traverseStxSub :: (Monad w) => StxTerm o -> StxTerm i -> (i -> w i) -> o -> w o
traverseStxSub TStxWord   _ _ x = pure x
traverseStxSub TStxPunc   _ _ x = pure x
traverseStxSub TStxString _ _ x = pure x
traverseStxSub TStxInt    _ _ x = pure x
traverseStxSub TStxSplice _ _ x = pure x
traverseStxSub TStxBlock ti f (c, StxBlob xs) =
  (c, ) . StxBlob <$> traverse (traverseStx TStxIdd ti f) xs
traverseStxSub TStx ti f (StxWord word) =
  StxWord <$> traverseStxSub TStxWord ti f word
traverseStxSub TStx ti f (StxPunc word) =
  StxPunc <$> traverseStxSub TStxPunc ti f word
traverseStxSub TStx ti f (StxString delim txt) =
  uncurry StxString <$> traverseStxSub TStxString ti f (delim, txt)
traverseStxSub TStx ti f (StxInt base n) =
  uncurry StxInt <$> traverseStxSub TStxInt ti f (base, n)
traverseStxSub TStx ti f (StxSplice elp idx) =
  uncurry StxSplice <$> traverseStxSub TStxSplice ti f (elp, idx)
traverseStxSub TStx ti f (StxBlock delim blob) =
  uncurry StxBlock <$> traverseStxSub TStxBlock ti f (delim, blob)
traverseStxSub TStxIdd ti f (Idd uid stx) =
  Idd uid <$> traverseStxSub TStx ti f stx
traverseStxSub TStxBlob ti f (StxBlob xs) =
  StxBlob <$> traverse (traverseStxSub TStxIdd ti f) xs

traverseValueSelf
  :: (Monad w) => ValueTerm o -> ValueTerm i -> (i an -> w (i an)) -> o an -> w (o an)
traverseValueSelf TType TType f = f
traverseValueSelf (TValueStx stx1) (TValueStx stx2) f =
  fmap TStxWrapper
    . traverseStxSelf stx1 stx2 (fmap unTStxWrapper . f . TStxWrapper)
    . unTStxWrapper
traverseValueSelf TPrim     TPrim     f = f
traverseValueSelf TSymbol   TSymbol   f = f
traverseValueSelf TRecord   TRecord   f = f
traverseValueSelf TBind     TBind     f = f
traverseValueSelf TValue    TValue    f = f
traverseValueSelf _         _         _ = pure

traverseValueSub
  :: (Monad w) => ValueTerm o -> ValueTerm i -> (i an -> w (i an)) -> o an -> w (o an)
traverseValueSub TType _ _ x = pure x
traverseValueSub (TValueStx stx1) (TValueStx stx2) f (TStxWrapper x) =
  TStxWrapper
    <$> traverseStxSub stx1 stx2 (fmap unTStxWrapper . f . TStxWrapper) x
traverseValueSub TPrim ti f (PrimStx an (LStxBlob lang blob)) =
  PrimStx an . LStxBlob lang . unTStxWrapper <$> traverseValueSub
    (TValueStx TStxBlob)
    ti
    f
    (TStxWrapper blob)
traverseValueSub (TValueStx _) _ _ x = pure x
traverseValueSub TPrim    _ _ x = pure x
traverseValueSub TSymbol  _ _ x = pure x
traverseValueSub TRecord ti f (Record ann head' props) =
  Record ann
    <$> traverseValue TSymbol ti f head'
    <*> traverse (traverseValue TValue ti f) props
traverseValueSub TBind _ _ x = pure x
traverseValueSub TValue ti f (ValuePrimitive prim) =
  ValuePrimitive <$> traverseValue TPrim ti f prim
traverseValueSub TValue ti f (ValueRecord record) =
  ValueRecord <$> traverseValue TRecord ti f record
traverseValueSub TValue ti f (ValueBind bind) =
  ValueBind <$> traverseValue TBind ti f bind

traverseProgramSelf
  :: (Monad w) => ProgramTerm o -> ProgramTerm i -> (i an -> w (i an)) -> o an -> w (o an)
traverseProgramSelf (TProgramValue val1) (TProgramValue val2) f =
  traverseValueSelf val1 val2 f
traverseProgramSelf TGroupLoc TGroupLoc f = f
traverseProgramSelf TGroupRef TGroupRef f = f
traverseProgramSelf TCast     TCast     f = f
traverseProgramSelf TNext     TNext     f = f
traverseProgramSelf TGuard    TGuard    f = f
traverseProgramSelf TReducer  TReducer  f = f
traverseProgramSelf TGroupDef TGroupDef f = f
traverseProgramSelf TProgram  TProgram  f = f
traverseProgramSelf _         _         _ = pure

traverseProgramSub
  :: (Monad w) => ProgramTerm o -> ProgramTerm i -> (i an -> w (i an)) -> o an -> w (o an)
traverseProgramSub (TProgramValue val1) (TProgramValue val2) f x =
  traverseValueSub val1 val2 f x
traverseProgramSub (TProgramValue _) _ _ x = pure x
traverseProgramSub TGroupLoc ti f (GroupLocGlobal ann sym) =
  GroupLocGlobal ann <$> traverseProgram (TProgramValue TSymbol) ti f sym
traverseProgramSub TGroupLoc _ _ (GroupLocLocal ann idx) =
  pure $ GroupLocLocal ann idx
traverseProgramSub TGroupRef ti f (GroupRef ann loc vprops gprops) =
  GroupRef ann
    <$> traverseProgram TGroupLoc ti f loc
    <*> traverse (traverseProgram (TProgramValue TValue) ti f)    vprops
    <*> traverse (traverseProgram TGroupRef ti f) gprops
traverseProgramSub TCast _ _ x              = pure x
traverseProgramSub TNext _ _ x@(NextEval _) = pure x
traverseProgramSub TNext ti f (NextCast cast) =
  NextCast <$> traverseProgram TCast ti f cast
traverseProgramSub TNext ti f (NextGroup grp) =
  NextGroup <$> traverseProgram TGroupRef ti f grp
traverseProgramSub TGuard ti f (Guard ann inp out nxts) =
  Guard ann
    <$> traverseProgram (TProgramValue TValue) ti f inp
    <*> traverseProgram (TProgramValue TValue) ti f out
    <*> traverse (traverseProgram TNext ti f) nxts
traverseProgramSub TReducer ti f (Reducer ann main guards) =
  Reducer ann
    <$> traverseProgram TGuard ti f main
    <*> traverse (traverseProgram TGuard ti f) guards
traverseProgramSub TGroupDef ti f (GroupDef ann vprops gprops reds) =
  GroupDef ann
    <$> pure vprops
    <*> pure gprops
    <*> traverse (traverseProgram TReducer ti f) reds
traverseProgramSub TProgram ti f (Program ann pth idcls rdcls alis exps inits' castReds grps)
  = Program ann pth idcls rdcls alis exps inits'
    <$> traverse (traverseProgram TReducer ti f)  castReds
    <*> traverse (traverseProgram TGroupDef ti f) grps

-- | Traverses each child node (includes recursive), inner to outer.
traverseStx :: (Monad w) => StxTerm o -> StxTerm i -> (i -> w i) -> o -> w o
traverseStx to ti f = traverseStxSelf to ti f <=< traverseStxSub to ti f

-- | Traverses each child node (includes recursive), inner to outer, just for effects.
traverseStx_ :: (Monad w) => StxTerm o -> StxTerm i -> (i -> w ()) -> o -> w ()
traverseStx_ to ti f = (() <$) . traverseStx to ti (\x -> x <$ f x)

-- | Folds each child node (includes recursive), inner to outer.
foldStx :: (Monoid r) => StxTerm o -> StxTerm i -> (i -> r) -> o -> r
foldStx to ti f =
  execWriter . traverseStx to ti (\x -> WriterT $ Identity (x, f x))

-- | Maps each child node (includes recursive), inner to outer.
mapStx :: StxTerm o -> StxTerm i -> (i -> i) -> o -> o
mapStx to ti f = runIdentity . traverseStx to ti (Identity . f)

-- | Traverses each child node (includes recursive), inner to outer.
traverseValue
  :: (Monad w) => ValueTerm o -> ValueTerm i -> (i an -> w (i an)) -> o an -> w (o an)
traverseValue to ti f = traverseValueSelf to ti f <=< traverseValueSub to ti f

-- | Traverses each child node (includes recursive), inner to outer, just for effects.
traverseValue_ :: (Monad w) => ValueTerm o -> ValueTerm i -> (i an -> w ()) -> o an -> w ()
traverseValue_ to ti f = (() <$) . traverseValue to ti (\x -> x <$ f x)

-- | Folds each child node (includes recursive), inner to outer.
foldValue :: (Monoid r) => ValueTerm o -> ValueTerm i -> (i an -> r) -> o an -> r
foldValue to ti f =
  execWriter . traverseValue to ti (\x -> WriterT $ Identity (x, f x))

-- | Maps each child node (includes recursive), inner to outer.
mapValue :: ValueTerm o -> ValueTerm i -> (i an -> i an) -> o an -> o an
mapValue to ti f = runIdentity . traverseValue to ti (Identity . f)

-- | Traverses each child node (includes recursive), inner to outer.
traverseProgram
  :: (Monad w) => ProgramTerm o -> ProgramTerm i -> (i an -> w (i an)) -> o an -> w (o an)
traverseProgram to ti f = traverseProgramSelf to ti f <=< traverseProgramSub to ti f

-- | Traverses each child node (includes recursive), inner to outer, just for effects.
traverseProgram_ :: (Monad w) => ProgramTerm o -> ProgramTerm i -> (i an -> w ()) -> o an -> w ()
traverseProgram_ to ti f = (() <$) . traverseProgram to ti (\x -> x <$ f x)

-- | Folds each child node (includes recursive), inner to outer.
foldProgram :: (Monoid r) => ProgramTerm o -> ProgramTerm i -> (i an -> r) -> o an -> r
foldProgram to ti f =
  execWriter . traverseProgram to ti (\x -> WriterT $ Identity (x, f x))

-- | Maps each child node (includes recursive), inner to outer.
mapProgram :: ProgramTerm o -> ProgramTerm i -> (i an -> i an) -> o an -> o an
mapProgram to ti f = runIdentity . traverseProgram to ti (Identity . f)

-- | Replaces all occurrences of the program's module path with @new@.
moveProgPath :: ModulePath -> Program an -> Program an
moveProgPath = mapProgram TProgram (TProgramValue TSymbol) . moveSymPath
 where
  moveSymPath new (Symbol rng pth loc) | pth == new = Symbol rng new loc
                                       | otherwise  = Symbol rng pth loc

-- | Reducers in all groups.
allProgramReducers :: Program an -> [Reducer an]
allProgramReducers = concatMap groupDefReducers . programGroups

substStxSplice :: [Int] -> (Bool, Int) -> (Bool, Int)
substStxSplice substs (elp, idx) | idx < length substs = (elp, substs !! idx)
                                 | otherwise           = (elp, idx)

substStxSplices :: [Int] -> StxBlob -> StxBlob
substStxSplices = mapStx TStxBlob TStxSplice . substStxSplice

substBinds1 :: [(Int, Value an)] -> Value an -> Value an
substBinds1 substs x@(ValueBind (Bind rng idx)) =
  case find (\(old, _) -> idx == old) substs of
    Nothing       -> x
    Just (_, new) -> rng <$ new
substBinds1 _ x = x

substBinds :: [(Int, Value an)] -> Value an -> Value an
substBinds = mapValue TValue TValue . substBinds1

substSplices :: [Value an] -> Value an -> Value an
substSplices = substBinds . zip [1 ..]

substGroupProp1 :: [(Int, GroupRef an)] -> GroupRef an -> GroupRef an
substGroupProp1 substs x = case groupRefLoc x of
  GroupLocLocal _ idx -> case find (\(old, _) -> idx == old) substs of
    Nothing       -> x
    Just (_, new) -> GroupRef
      { groupRefAnn        = groupRefAnn new
      , groupRefLoc        = groupRefLoc new
      , groupRefValueProps = groupRefValueProps x ++ groupRefValueProps new
      , groupRefGroupProps = groupRefGroupProps x ++ groupRefGroupProps new
      }
  _ -> x

numBindsInValue1 :: Value an -> Int
numBindsInValue1 (ValuePrimitive _           ) = 0
numBindsInValue1 (ValueRecord    _           ) = 0
numBindsInValue1 (ValueBind      (Bind _ idx)) = idx

maxNumBindsInValue :: Value an -> Int
maxNumBindsInValue = getMax0 . foldValue TValue TValue (Max0 . numBindsInValue1)

maxNumBindsInGuard :: Guard an -> Int
maxNumBindsInGuard (Guard _ input output _) =
  max (maxNumBindsInValue input) (maxNumBindsInValue output)

maxNumBindsInReducer :: Reducer an -> Int
maxNumBindsInReducer (Reducer _ main guards) =
  maximum $ map maxNumBindsInGuard $ main : guards

maxNumBindsInReducers :: [Reducer an] -> Int
maxNumBindsInReducers stmts = maximum $ 0 : map maxNumBindsInReducer stmts

-- | The maximum number of binds used by the main reducers in the program - the maximum index in any used bind.
maxNumBindsInProgram :: Program an -> Int
maxNumBindsInProgram = maxNumBindsInReducers . allProgramReducers

bindsInValue1 :: Value an -> S.Set Int
bindsInValue1 (ValuePrimitive _   ) = S.empty
bindsInValue1 (ValueRecord    _   ) = S.empty
bindsInValue1 (ValueBind      bind) = S.singleton $ bindIdx bind

bindsInValue :: Value an -> S.Set Int
bindsInValue = foldValue TValue TValue bindsInValue1

-- | The reducers in the group and super-groups, substituting exported binds, and their mode.
allGroupDefReducers :: [GroupRef an] -> GroupDef an -> [Reducer an]
allGroupDefReducers gprops (GroupDef _ _ gpropIdxs reds) = map
  (mapProgram TReducer TGroupRef (substGroupProp1 gpropSubsts))
  reds
  where gpropSubsts = zip (map groupDefPropIdx gpropIdxs) gprops

-- | The reducers in the referenced group, substituting exported binds, and their mode.
allGroupRefReducers
  :: M.Map (Symbol ()) (GroupDef an) -> GroupRef an -> [Reducer an]
allGroupRefReducers groups (GroupRef _ (GroupLocGlobal _ name) _ gprops) =
  allGroupDefReducers gprops $ groups M.! (() <$ name)
allGroupRefReducers _ (GroupRef _ _ _ _) =
  error "can't get all group ref statements from unsubstituted group prop"

valAtPath :: [Int] -> Value an -> Maybe (Value an)
valAtPath [] val = Just val
valAtPath (x : xs) (ValueRecord (Record _ _ props))
  | x < length props = valAtPath xs $ props !! x
  | otherwise        = Nothing
valAtPath _ _ = Nothing
