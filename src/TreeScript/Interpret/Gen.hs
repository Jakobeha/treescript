{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Interpret.Gen
  ( MonadInterpret(..)
  , mifail
  , misucceed
  , mieof
  , mipanic
  , madvanceGroup
  , mreduceLocal
  , mtransform
  , minterpret
  )
where

import           TreeScript.Ast
import           TreeScript.Misc

import           Control.Monad.Reader
import           Control.Monad.Extra
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

-- | Abstracts minterpret, compile, debug, and low-level static analysis:
-- Larger operations are composed of these functions, which correspond to "opcodes"
class (Monad m, Printable (IType m), Printable (IValue m), Printable (ICast m)) => MonadInterpret m where
  type IType m :: *
  type IValue m :: *
  type ICast m :: *
  type IGroup m :: *

  mlog :: T.Text -> T.Text -> m ()
  mloadPath :: ModulePath -> m ()
  mloadCreds :: M.Map CastSurface (Reducer Range) -> m ()
  mloadGroups :: M.Map (Symbol ()) (GroupDef Range) -> m ()
  mloadStart :: m ()
  mloadEnd :: m ()
  mstartLib :: Library -> m ()
  mstopLib :: Library -> m ()
  mcatchEOF :: m () -> m ()
  mcatchFail :: m () -> m ()
  mcatchSuccess :: m (IValue m) -> m (IValue m)
  mjmpEof :: m a
  mjmpFail :: m a
  mjmpSucceed :: IValue m -> m a
  mjmpPanic :: T.Text -> m a
  mreadInput :: m ()
  mwriteOutput :: IValue m -> m ()
  -- Also fills binds in vprops
  mpushGFrame :: [Value Range] -> [GroupRef Range] -> m ()
  mgframe2rframe :: [GroupDefProp Range] -> [GroupDefProp Range] -> m ()
  mdupRFrame :: m ()
  mpopRFrame :: m ()
  mtransformI :: IGroup m -> m ()
  -- | Stores length of input in register
  mpushFixedInput :: IValue m -> m ()
  -- | Does nothing is input isn't fixed
  mdropFixedInput :: m ()
  -- | Fails if not enough input left, or if fixed input was pushed and len is different
  mpeekInput :: Int -> m (IValue m)
  mdropInput :: Int -> m ()
  mval2Ival :: Value Range -> m (IValue m)
  mmapMPath :: IValue m -> [Int] -> (IValue m -> m (IValue m)) -> m (IValue m)
  mgetType :: IValue m -> m (IType m)
  misSubtype :: IType m -> S.Set SType -> m Bool
  mlookupCast :: IType m -> SType -> m (Maybe (ICast m))
  mreduceCast :: ICast m -> IValue m -> m (IValue m)
  mresolveGlobal :: Symbol Range -> m (IGroup m)
  madvanceLocalGroup :: IValue m -> Range -> Range -> Int -> [Value Range] -> [GroupRef Range] -> m (IValue m)
  mcheckLengthEq :: IValue m -> Int -> m ()
  mconsumePrim :: IValue m -> Primitive Range -> m (IValue m)
  mconsumeRecordHead :: IValue m -> Symbol Range -> Int -> m (IValue m)
  mconsumeTrue :: IValue m -> m (IValue m)
  mconsumeBind :: IValue m -> Int -> m (IValue m)
  mfillBinds :: IValue m -> m (IValue m)

-- * NOTE: In a compiler these all push/pop from the same stack, but the names help clarify

{-
STACK
- Output : n val addresses
- Input : 8 val addresses

- Binds : 16 val addresses
- In Binds : 16 val addresses
--- or ---
- Val props : n val addresses
- Group props : n local group idxs

REGISTERS
- Size of output
- # Value props
- # Group props
-}

mifail :: (MonadInterpret m) => m a
mifail = do
  mlog "Control" "* Fail"
  mjmpFail

misucceed :: (MonadInterpret m) => IValue m -> m a
misucceed res = do
  mlog "Control" $ "* Succeed: " <> pprint res
  mjmpSucceed res

mieof :: (MonadInterpret m) => m a
mieof = do
  mlog "Control" "* Eof"
  mjmpEof

mipanic :: (MonadInterpret m) => T.Text -> m a
mipanic msg = do
  mlog "Control" $ "** Panic: " <> msg
  mjmpPanic msg

mconsume1 :: (MonadInterpret m) => IValue m -> Value Range -> m (IValue m)
mconsume1 old (ValuePrimitive x) = do
  mlog "Consume" $ "  Consume Primitive: " <> pprint old <> " -> " <> pprint x
  mconsumePrim old x
mconsume1 old (ValueRecord inp@(Record _ head' props)) = do
  mlog "Consume" $ "  Consume Record: " <> pprint old <> " -> " <> pprint inp
  old' <- mconsumeRecordHead old head' $ length props
  mlog "Consume" "  Consume Props"
  foldM mconsume1 old' props
mconsume1 old (ValueBind (Bind _ 0)) = do
  mlog "Consume" $ "  Consume Bind 0: " <> pprint old
  mconsumeTrue old
mconsume1 old (ValueBind (Bind _ idx)) = do
  mlog "Consume" $ "  Consume Bind " <> pprint idx <> ": " <> pprint old
  mconsumeBind old idx

mconsume :: (MonadInterpret m) => IValue m -> [Value Range] -> m ()
mconsume old inpl = do
  mlog "Consume" $ "Consume: " <> pprint old <> " -> " <> pprint inpl
  rest <- foldM mconsume1 old inpl
  mcheckLengthEq rest 0

mproduce1 :: (MonadInterpret m) => Value Range -> m (IValue m)
mproduce1 out = do
  out' <- mval2Ival out
  mfillBinds out'

mcast :: (MonadInterpret m) => IValue m -> S.Set SType -> m (IValue m)
mcast new typ = do
  ntyp <- mgetType new
  mlog "Cast"
    $  "Casting "
    <> pprint ntyp
    <> " to "
    <> pprint (S.toList typ)
    <> "\n  in "
    <> pprint new
  sub <- misSubtype ntyp typ
  if sub
    then do
      mlog "Cast" "Success - is subtype"
      pure new
    else do
      ocast <- firstJustM (mlookupCast ntyp) $ S.toList typ
      case ocast of
        Nothing -> do
          -- TODO handle ilists
          mlog "Cast" "Cast failed"
          mifail
        Just cast -> do
          mlog "Cast" $ "Success - found " <> pprint cast
          mreduceCast cast new

madvanceCast :: (MonadInterpret m) => IValue m -> Cast Range -> m (IValue m)
madvanceCast new (Cast _ pth typ) = mmapMPath new pth (`mcast` typ)

madvanceGroup
  :: (MonadInterpret m) => IValue m -> GroupRef Range -> m (IValue m)
madvanceGroup new (GroupRef _ (GroupLocGlobal _ ghead) vprops gprops) = do
  grp <- mresolveGlobal ghead
  mtransformD grp vprops gprops new
madvanceGroup new (GroupRef rng (GroupLocLocal lrng idx) vprops gprops) =
  madvanceLocalGroup new rng lrng idx vprops gprops
madvanceGroup new (GroupRef _ (GroupLocFunction _ grp) vprops gprops) =
  undefined -- SOON

madvance' :: (MonadInterpret m) => IValue m -> Next Range -> m (IValue m)
madvance' new (NextCast  cast) = madvanceCast new cast
madvance' new (NextGroup grp ) = madvanceGroup new grp

madvance :: (MonadInterpret m) => IValue m -> Next Range -> m (IValue m)
madvance new next = do
  mlog "Advance" $ "Advance: " <> pprint new <> " " <> pprint next <> " ..."
  res <- madvance' new next
  mlog "Advance" $ "Succeeded advance: " <> pprint next
  pure res

mproduce :: (MonadInterpret m) => Value Range -> [Next Range] -> m (IValue m)
mproduce out nexts = do
  mlog "Produce" $ "Produce: " <> pprint out <> T.concat
    (map ((" " <>) . pprint) nexts)
  new <- mproduce1 out
  -- foldM is left-fold
  foldM madvance new nexts

mguard :: (MonadInterpret m) => Guard Range -> m ()
mguard grd@(Guard _ inp out nexts) = do
  mlog "Guard" $ "Guard: " <> pprint grd
  let inpl = unwrapIList inp
  new <- mproduce out nexts
  mcheckLengthEq new $ length inpl
  mconsume new inpl

mreduceLocal :: (MonadInterpret m) => Reducer Range -> IValue m -> m (IValue m)
mreduceLocal red@(Reducer _ (Guard _ inp out nexts) guards) old = do
  mlog "Reduce" $ "Reduce local: " <> pprint red
  let inpl = unwrapIList inp
  mconsume old inpl
  forM_ (reverse guards) mguard
  new <- mproduce out nexts
  mlog "Reduce" $ "Succeeded reduce local: " <> pprint red
  pure new

mreduce :: (MonadInterpret m) => Reducer Range -> m ()
mreduce red@(Reducer _ (Guard _ inp out nexts) guards) = do
  mlog "Reduce" $ "Reduce: " <> pprint red
  let inpl = unwrapIList inp
  old <- mpeekInput $ length inpl
  mconsume old inpl
  forM_ (reverse guards) mguard
  new <- mproduce out nexts
  mdropInput $ length inpl
  mpopRFrame -- For local reducer
  mpopRFrame -- For group
  mlog "Reduce" $ "Succeeded reduce: " <> pprint red
  misucceed new

mtransform :: (MonadInterpret m) => GroupDef Range -> m ()
mtransform (GroupDef _ vprops gprops reds) = do
  mlog "Transform" "Transform"
  mgframe2rframe vprops gprops
  forM_ reds $ \red -> do
    mdupRFrame
    mcatchFail $ mreduce red
    mlog "Transform" $ "Failed reduce: " <> pprint red
    mpopRFrame
  mpopRFrame
  mdropFixedInput
  mlog "Transform" "* Transform fail"
  mifail

mtransformD
  :: (MonadInterpret m)
  => IGroup m
  -> [Value Range]
  -> [GroupRef Range]
  -> IValue m
  -> m (IValue m)
mtransformD grp inVProps inGProps new = do
  mpushFixedInput new
  mpushGFrame inVProps inGProps
  mcatchSuccess $ do
    mtransformI grp -- pops GFrame
    mipanic "didn't succeed or fail"

mtransformMain :: (MonadInterpret m) => IGroup m -> m (IValue m)
mtransformMain grp = do
  mpushGFrame [] []
  mcatchSuccess $ do
    mcatchFail $ mtransformI grp
    mipanic "can't transform this value"

minterpret :: (MonadInterpret m) => Program Range -> m ()
minterpret (Program _ pth _ _ _ _ _ creds grps libs) = do
  mlog "Interpret" "Loading"
  mloadPath pth
  mloadCreds creds
  mloadGroups grps
  mloadStart
  mlog "Interpret" "Starting Libs"
  forM_ libs mstartLib
  grp <- mresolveGlobal Symbol { symbolAnn    = r0
                               , symbolModule = pth
                               , symbol       = "Main"
                               }
  mcatchEOF $ forever $ do
    mlog "Interpret" "Move Index"
    mreadInput
    new <- mtransformMain grp
    mwriteOutput new
  mlog "Interpret" "Stopping Libs"
  forM_ libs mstopLib
  mlog "Interpret" "Finishing"
  mloadEnd
