{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
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
import qualified Control.Monad.Fail            as F
import           Control.Monad.Writer.Strict
import           Data.Functor.Identity
import           Data.List               hiding ( group )
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Set                      as S

traverseStxSelf :: (Monad w) => StxTerm o -> StxTerm i -> (i -> w i) -> o -> w o
traverseStxSelf TStxWord   TStxWord   f = f
traverseStxSelf TStxPunc   TStxPunc   f = f
traverseStxSelf TStxString TStxString f = f
traverseStxSelf TStxInt    TStxInt    f = f
traverseStxSelf TStxSplice TStxSplice f = f
traverseStxSelf TStxBlock  TStxBlock  f = f
traverseStxSelf TStx       TStx       f = f
traverseStxSelf TStxIdd    TStxIdd    f = f
traverseStxSelf TStxBlob   TStxBlob   f = f
traverseStxSelf _          _          _ = pure

traverseStxSub
  :: (Monad w)
  => StxTerm o
  -> StxTerm i
  -> (i -> w i)
  -> (  forall o2 i2
      . StxTerm o2
     -> StxTerm i2
     -> (i2 -> w i2)
     -> o2
     -> w o2
     )
  -> o
  -> w o
traverseStxSub TStxWord   _ _ _ x = pure x
traverseStxSub TStxPunc   _ _ _ x = pure x
traverseStxSub TStxString _ _ _ x = pure x
traverseStxSub TStxInt    _ _ _ x = pure x
traverseStxSub TStxSplice _ _ _ x = pure x
traverseStxSub TStxBlock ti f travSub (c, StxBlob xs) =
  (c, ) . StxBlob <$> traverse (travSub TStxIdd ti f) xs
traverseStxSub TStx ti f travSub (StxWord word) =
  StxWord <$> travSub TStxWord ti f word
traverseStxSub TStx ti f travSub (StxPunc word) =
  StxPunc <$> travSub TStxPunc ti f word
traverseStxSub TStx ti f travSub (StxString delim txt) =
  uncurry StxString <$> travSub TStxString ti f (delim, txt)
traverseStxSub TStx ti f travSub (StxInt base n) =
  uncurry StxInt <$> travSub TStxInt ti f (base, n)
traverseStxSub TStx ti f travSub (StxSplice elp idx) =
  uncurry StxSplice <$> travSub TStxSplice ti f (elp, idx)
traverseStxSub TStx ti f travSub (StxBlock delim blob) = do
  blob' <- travSub TStxBlob ti f blob
  uncurry StxBlock <$> travSub TStxBlock ti f (delim, blob')
traverseStxSub TStxIdd ti f travSub (Idd uid pre post stx) =
  Idd uid pre post <$> travSub TStx ti f stx
traverseStxSub TStxBlob ti f travSub (StxBlob xs) =
  StxBlob <$> traverse (travSub TStxIdd ti f) xs

traverseValueSelf
  :: (Monad w)
  => ValueTerm o
  -> ValueTerm i
  -> (i an -> w (i an))
  -> o an
  -> w (o an)
traverseValueSelf TType TType f = f
traverseValueSelf (TValueStx stx1) (TValueStx stx2) f =
  fmap TStxWrapper
    . traverseStxSelf stx1 stx2 (fmap unTStxWrapper . f . TStxWrapper)
    . unTStxWrapper
traverseValueSelf TPrim   TPrim   f = f
traverseValueSelf TSymbol TSymbol f = f
traverseValueSelf TRecord TRecord f = f
traverseValueSelf TBind   TBind   f = f
traverseValueSelf TValue  TValue  f = f
traverseValueSelf _       _       _ = pure

traverseValueSub
  :: (Monad w)
  => ValueTerm o
  -> ValueTerm i
  -> (i an -> w (i an))
  -> (  forall o2 i2
      . ValueTerm o2
     -> ValueTerm i2
     -> (i2 an -> w (i2 an))
     -> o2 an
     -> w (o2 an)
     )
  -> o an
  -> w (o an)
traverseValueSub TType _ _ _ x = pure x
traverseValueSub (TValueStx stx1) (TValueStx stx2) f travSub (TStxWrapper x) =
  TStxWrapper
    <$> traverseStxSub
          stx1
          stx2
          (wrap f)
          (\to2 ti2 -> wrap . travSub (TValueStx to2) (TValueStx ti2) . unwrap)
          x
 where
  wrap
    :: forall a b w2 an2
     . (Monad w2)
    => (TStxWrapper a an2 -> w2 (TStxWrapper b an2))
    -> a
    -> w2 b
  wrap f' = fmap unTStxWrapper . f' . TStxWrapper
  unwrap f' = fmap TStxWrapper . f' . unTStxWrapper
traverseValueSub TPrim ti f _ (PrimStx an (LStxBlob lang blob)) =
  PrimStx an . LStxBlob lang . unTStxWrapper <$> traverseValue
    (TValueStx TStxBlob)
    ti
    f
    (TStxWrapper blob)
traverseValueSub (TValueStx _) _ _ _ x = pure x
traverseValueSub TPrim         _ _ _ x = pure x
traverseValueSub TSymbol       _ _ _ x = pure x
traverseValueSub TRecord ti f travSub (Record ann head' props) =
  Record ann
    <$> travSub TSymbol ti f head'
    <*> traverse (travSub TValue ti f) props
traverseValueSub TBind _ _ _ x = pure x
traverseValueSub TValue ti f travSub (ValuePrimitive prim) =
  ValuePrimitive <$> travSub TPrim ti f prim
traverseValueSub TValue ti f travSub (ValueRecord record) =
  ValueRecord <$> travSub TRecord ti f record
traverseValueSub TValue ti f travSub (ValueBind bind) =
  ValueBind <$> travSub TBind ti f bind

traverseProgramSelf
  :: (Monad w)
  => ProgramTerm o
  -> ProgramTerm i
  -> (i an -> w (i an))
  -> o an
  -> w (o an)
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
  :: (Monad w)
  => ProgramTerm o
  -> ProgramTerm i
  -> (i an -> w (i an))
  -> (  forall o2 i2
      . ProgramTerm o2
     -> ProgramTerm i2
     -> (i2 an -> w (i2 an))
     -> o2 an
     -> w (o2 an)
     )
  -> o an
  -> w (o an)
traverseProgramSub (TProgramValue val1) (TProgramValue val2) f travSub x =
  traverseValueSub
    val1
    val2
    f
    (\to2 ti2 -> travSub (TProgramValue to2) (TProgramValue ti2))
    x
traverseProgramSub (TProgramValue _) _ _ _ x = pure x
traverseProgramSub TGroupLoc ti f travSub (GroupLocGlobal ann sym) =
  GroupLocGlobal ann <$> travSub (TProgramValue TSymbol) ti f sym
traverseProgramSub TGroupLoc _ _ _ (GroupLocLocal ann idx) =
  pure $ GroupLocLocal ann idx
traverseProgramSub TGroupRef ti f travSub (GroupRef ann loc vprops gprops) =
  GroupRef ann
    <$> travSub TGroupLoc ti f loc
    <*> traverse (travSub (TProgramValue TValue) ti f) vprops
    <*> traverse (travSub TGroupRef ti f)              gprops
traverseProgramSub TCast _ _ _ x              = pure x
traverseProgramSub TNext _ _ _ x@(NextEval _) = pure x
traverseProgramSub TNext ti f travSub (NextCast cast) =
  NextCast <$> travSub TCast ti f cast
traverseProgramSub TNext ti f travSub (NextGroup grp) =
  NextGroup <$> travSub TGroupRef ti f grp
traverseProgramSub TGuard ti f travSub (Guard ann inp out nxts) =
  Guard ann
    <$> travSub (TProgramValue TValue) ti f inp
    <*> travSub (TProgramValue TValue) ti f out
    <*> traverse (travSub TNext ti f) nxts
traverseProgramSub TReducer ti f travSub (Reducer ann main guards) =
  Reducer ann
    <$> traverseProgram TGuard ti f main
    <*> traverse (travSub TGuard ti f) guards
traverseProgramSub TGroupDef ti f travSub (GroupDef ann vprops gprops reds) =
  GroupDef ann
    <$> pure vprops
    <*> pure gprops
    <*> traverse (travSub TReducer ti f) reds
traverseProgramSub TProgram ti f travSub (Program ann pth idcls rdcls alis exps inits' castReds grps)
  = Program ann pth idcls rdcls alis exps inits'
    <$> traverse (travSub TReducer ti f)  castReds
    <*> traverse (travSub TGroupDef ti f) grps

-- | Traverses each child node (includes recursive), inner to outer.
traverseStx :: (Monad w) => StxTerm o -> StxTerm i -> (i -> w i) -> o -> w o
traverseStx to ti f =
  traverseStxSelf to ti f <=< traverseStxSub to ti f traverseStx

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
  :: (Monad w)
  => ValueTerm o
  -> ValueTerm i
  -> (i an -> w (i an))
  -> o an
  -> w (o an)
traverseValue to ti f =
  traverseValueSelf to ti f <=< traverseValueSub to ti f traverseValue

-- | Traverses each child node (includes recursive), inner to outer, just for effects.
traverseValue_
  :: (Monad w) => ValueTerm o -> ValueTerm i -> (i an -> w ()) -> o an -> w ()
traverseValue_ to ti f = (() <$) . traverseValue to ti (\x -> x <$ f x)

-- | Folds each child node (includes recursive), inner to outer.
foldValue
  :: (Monoid r) => ValueTerm o -> ValueTerm i -> (i an -> r) -> o an -> r
foldValue to ti f =
  execWriter . traverseValue to ti (\x -> WriterT $ Identity (x, f x))

-- | Maps each child node (includes recursive), inner to outer.
mapValue :: ValueTerm o -> ValueTerm i -> (i an -> i an) -> o an -> o an
mapValue to ti f = runIdentity . traverseValue to ti (Identity . f)

-- | Traverses each child node (includes recursive), inner to outer.
traverseProgram
  :: (Monad w)
  => ProgramTerm o
  -> ProgramTerm i
  -> (i an -> w (i an))
  -> o an
  -> w (o an)
traverseProgram to ti f =
  traverseProgramSelf to ti f <=< traverseProgramSub to ti f traverseProgram

-- | Traverses each child node (includes recursive), inner to outer, just for effects.
traverseProgram_
  :: (Monad w)
  => ProgramTerm o
  -> ProgramTerm i
  -> (i an -> w ())
  -> o an
  -> w ()
traverseProgram_ to ti f = (() <$) . traverseProgram to ti (\x -> x <$ f x)

-- | Folds each child node (includes recursive), inner to outer.
foldProgram
  :: (Monoid r) => ProgramTerm o -> ProgramTerm i -> (i an -> r) -> o an -> r
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
substStxSplice substs (elp, idx)
  | idx > 0 && idx' < length substs = (elp, substs !! idx')
  | otherwise                       = (elp, idx)
  where idx' = idx - 1

substStxSplices :: [Int] -> StxBlob -> StxBlob
substStxSplices = mapStx TStxBlob TStxSplice . substStxSplice

substStxBinds1 :: (F.MonadFail m) => [(Int, Value an)] -> Idd Stx -> m StxBlob
substStxBinds1 substs x@(Idd _ _ _ (StxSplice _ idx)) =
  case find (\(old, _) -> idx == old) substs of
    Nothing       -> pure $ StxBlob [x]
    Just (_, new) -> case value2Stx new of
      Nothing ->
        F.fail $ T.unpack $ "can't put value in syntax: " <> pprint new
      Just new' -> pure new'
substStxBinds1 _ x = pure $ StxBlob [x]

substBinds1 :: (F.MonadFail m) => [(Int, Value an)] -> Value an -> m (Value an)
substBinds1 substs x@(ValueBind (Bind rng idx)) =
  case find (\(old, _) -> idx == old) substs of
    Nothing       -> pure x
    Just (_, new) -> pure $ rng <$ new
substBinds1 _ x = pure x

substBinds :: (F.MonadFail m) => [(Int, Value an)] -> Value an -> m (Value an)
substBinds substs =
  traverseValue
      TValue
      (TValueStx TStxBlob)
      ( fmap TStxWrapper
      . concatTraverseStxBlob (substStxBinds1 substs)
      . unTStxWrapper
      )
    <=< traverseValue TValue TValue (substBinds1 substs)

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
maxNumBindsInValue =
  getMax0 . foldValue TValue TValue (Max0 . numBindsInValue1)

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
