{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Interpret.Gen
  ( MonadInterpret(..)
  , madvanceGroup
  , mtransform
  , minterpret
  )
where

import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import qualified Data.Map.Strict               as M
import           Control.Monad.Reader

-- | Abstracts minterpret, compile, debug, and low-level static analysis:
-- Larger operations are composed of these functions, which correspond to "opcodes"
class (Monad m) => MonadInterpret m where
  type IValue m :: *
  type IGroup m :: *

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
  mifail :: m a
  misucceed :: IValue m -> m a
  mieof :: m a
  mipanic :: String -> m a
  mreadInput :: m ()
  mwriteOutput :: IValue m -> m ()
  mpushGFrame :: [Value Range] -> [GroupRef Range] -> m ()
  mgframe2rframe :: [GroupDefProp Range] -> [GroupDefProp Range] -> m ()
  mdupRFrame :: m ()
  mpopRFrame :: m ()
  mtransformI :: IGroup m -> m ()
  -- | Stores length of input in register
  mpushFixedInput :: IValue m -> m ()
  -- | Fails if not enough input left, or if fixed input was pushed and len is different
  mpeekInput :: Int -> m (IValue m)
  mdropInput :: Int -> m ()
  mval2Ival :: Value Range -> m (IValue m)
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

mconsume1 :: (MonadInterpret m) => IValue m -> Value Range -> m (IValue m)
mconsume1 old (ValuePrimitive x                     ) = mconsumePrim old x
mconsume1 old (ValueRecord    (Record _ head' props)) = do
  old' <- mconsumeRecordHead old head' $ length props
  foldM mconsume1 old' props
mconsume1 old (ValueBind (Bind _ 0  )) = mconsumeTrue old
mconsume1 old (ValueBind (Bind _ idx)) = mconsumeBind old idx

mconsume :: (MonadInterpret m) => IValue m -> [Value Range] -> m ()
mconsume old inpl = do
  rest <- foldM mconsume1 old inpl
  mcheckLengthEq rest 0

mproduce1 :: (MonadInterpret m) => Value Range -> m (IValue m)
mproduce1 out = do
  out' <- mval2Ival out
  mfillBinds out'

madvanceGroup
  :: (MonadInterpret m) => IValue m -> GroupRef Range -> m (IValue m)
madvanceGroup new (GroupRef _ (GroupLocGlobal _ ghead) vprops gprops) = do
  grp <- mresolveGlobal ghead
  mtransformD grp vprops gprops new
madvanceGroup new (GroupRef rng (GroupLocLocal lrng idx) vprops gprops) =
  madvanceLocalGroup new rng lrng idx vprops gprops
madvanceGroup new (GroupRef _ (GroupLocFunction _ grp) vprops gprops) =
  undefined -- SOON

madvance :: (MonadInterpret m) => IValue m -> Next Range -> m (IValue m)
madvance new (NextCast  (Cast _ pth typ)) = undefined -- SOON
madvance new (NextGroup grp             ) = madvanceGroup new grp

mproduce :: (MonadInterpret m) => Value Range -> [Next Range] -> m (IValue m)
mproduce out nexts = do
  new <- mproduce1 out
  -- foldM is left-fold
  foldM madvance new nexts

mguard' :: (MonadInterpret m) => Guard Range -> m ()
mguard' (Guard _ inp out nexts) = do
  let inpl = unwrapIList inp
  new <- mproduce out nexts
  mcheckLengthEq new $ length inpl
  mconsume new inpl

mreduce :: (MonadInterpret m) => Reducer Range -> m ()
mreduce (Reducer _ (Guard _ inp out nexts) guards) = do
  let inpl = unwrapIList inp
  old <- mpeekInput $ length inpl
  mconsume old inpl
  forM_ (reverse guards) mguard'
  new <- mproduce out nexts
  mdropInput $ length inpl
  mpopRFrame -- For local reducer
  mpopRFrame -- For group
  misucceed new

mtransform :: (MonadInterpret m) => GroupDef Range -> m ()
mtransform (GroupDef _ vprops gprops reds) = do
  mgframe2rframe vprops gprops
  forM_ reds $ \red -> do
    mdupRFrame
    mcatchFail $ mreduce red
    mpopRFrame
  mpopRFrame
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
    mipanic "can't mtransform this value"

minterpret :: (MonadInterpret m) => Program Range -> m ()
minterpret (Program _ pth _ _ _ _ _ creds grps libs) = do
  mloadPath pth
  mloadCreds creds
  mloadGroups grps
  mloadStart
  forM_ libs mstartLib
  grp <- mresolveGlobal Symbol { symbolAnn    = r0
                               , symbolModule = pth
                               , symbol       = "Main"
                               }
  mcatchEOF $ do
    mreadInput
    new <- mtransformMain grp
    mwriteOutput new
  forM_ libs mstopLib
  mloadEnd
