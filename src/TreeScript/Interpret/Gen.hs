{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Interpret.Gen
  ( GlobalPipe(..)
  , LocalPipe(..)
  , Pipe(..)
  , IText
  , ILanguage
  , IType
  , IValue
  , IValueList
  , IGlobalPipe
  , ILocalPipe
  , IPipe
  , ICast
  , IGroup
  , MonadInterpret(..)
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
import           TreeScript.Plugin

import           Control.Monad.Reader
import           Control.Monad.Extra
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified System.IO.Streams             as St

data GlobalPipe
  = GlobalPipe
  { globalPipeInput :: St.InputStream (Idd Stx)
  , globalPipeOutput :: St.OutputStream (Idd Stx)
  }

data LocalPipe
  = LocalPipe
  { localPipeInput :: Either (St.InputStream (Idd Stx)) (Value Range)
  , localPipeOutput :: St.OutputStream (Either (Idd Stx) (Value Range))
  , localPipeGetOutputRaw :: IO [Either (Idd Stx) (Value Range)]
  }

data Pipe
  = PipeGlobal GlobalPipe
  | PipeLocal LocalPipe

type IText m = IData m T.Text
type ILanguage m = IData m Language
type IType m = IData m SType
type IValue m = IData m (Value Range)
type IValueList m = IData m [Value Range]
type ICast m = IData m (Reducer Range)
type IGroup m = IData m (GroupDef Range)
type ILocalPipe m = IData m LocalPipe
type IGlobalPipe m = IData m GlobalPipe
type IPipe m = IData m Pipe

-- | Abstracts minterpret, compile, debug, and low-level static analysis:
-- Larger operations are composed of these functions, which correspond to "opcodes"
class (Functor (IData m), Monad m) => MonadInterpret m where
  data IData m :: * -> *

  mprintv :: IValue m -> m T.Text
  mprintv = mprint
  mprint :: (Printable a) => IData m a -> m T.Text
  mlog :: T.Text -> T.Text -> m ()
  mloadPath :: ModulePath -> m ()
  mloadCreds :: M.Map CastSurface (Reducer Range) -> m ()
  mloadGroups :: M.Map (Symbol ()) (GroupDef Range) -> m ()
  mloadStart :: m ()
  mloadEnd :: m ()
  mloadInits :: [LStxBlob] -> m ()
  mcatchEOF :: m () -> m ()
  mcatchFail :: m () -> m ()
  mcatchSuccess :: m () -> m ()
  mjmpEof :: m a
  mjmpFail :: m a
  mjmpSucceed :: m a
  mjmpPanic :: T.Text -> m a
  mgetGlobalPipe :: m (IGlobalPipe m)
  mmkLocalPipe :: IValue m -> m (ILocalPipe m)
  mgetLocalPipeOut :: ILocalPipe m -> m (IValue m)
  -- Also fills binds in vprops
  mpushGFrame :: [Value Range] -> [GroupRef Range] -> m ()
  mgframe2rframe :: [GroupDefProp Range] -> [GroupDefProp Range] -> m ()
  mdupRFrame :: m ()
  mpopRFrame :: m ()
  mtransformI :: IPipe m -> IGroup m -> m ()
  -- | Fails if not enough input left
  mpeekInput :: IPipe m -> Int -> m (IValue m)
  mdropInput :: IPipe m -> Int -> m ()
  mpushOutput :: IPipe m -> IValue m -> m ()
  mval2Ival :: Value Range -> m (IValue m)
  munwrapLang :: IValue m -> m (ILanguage m, IValue m)
  mval2Stx :: IValue m -> m (IText m)
  meval :: ILanguage m -> IText m -> m (IValue m)
  mmapMPath :: IValue m -> [Int] -> (IValue m -> m (IValue m)) -> m (IValue m)
  mgetType :: IValue m -> m (IType m)
  misSubtype :: IType m -> S.Set SType -> m Bool
  mlookupCast :: IType m -> SType -> m (Maybe (ICast m))
  mreduceCast :: ICast m -> IValue m -> m (IValue m)
  mresolveGlobal :: Symbol Range -> m (IGroup m)
  madvanceLocalGroup :: IValue m -> Range -> Range -> Int -> [Value Range] -> [GroupRef Range] -> m (IValue m)
  mcheckLengthEq :: IValue m -> Int -> m ()
  mvalList1 :: IValue m -> m (IValueList m)
  mconsumePrim :: IValueList m -> Primitive Range -> m (IValueList m)
  mconsumeRecordHead :: IValueList m -> Symbol Range -> Int -> m (IValueList m)
  mconsumeTrue :: IValueList m -> m (IValueList m)
  mconsumeBind :: IValueList m -> Int -> m (IValueList m)
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

misucceed :: (MonadInterpret m) => m ()
misucceed = do
  mlog "Control" "* Succeed"
  mjmpSucceed

mieof :: (MonadInterpret m) => m a
mieof = do
  mlog "Control" "* Eof"
  mjmpEof

mipanic :: (MonadInterpret m) => T.Text -> m a
mipanic msg = do
  mlog "Control" $ "** Panic: " <> msg
  mjmpPanic msg

mconsume1
  :: (MonadInterpret m) => IValueList m -> Value Range -> m (IValueList m)
mconsume1 old (ValuePrimitive inp) = do
  oldp <- mprint old
  mlog "Consume" $ "  Consume Primitive: " <> pprint inp <> " -> " <> oldp
  mconsumePrim old inp
mconsume1 old (ValueRecord inp@(Record _ head' props)) = do
  oldp <- mprint old
  mlog "Consume" $ "  Consume Record: " <> pprint inp <> " -> " <> oldp
  old' <- mconsumeRecordHead old head' $ length props
  mlog "Consume" "  Consume Props"
  foldM mconsume1 old' props
mconsume1 old (ValueBind (Bind _ 0)) = do
  oldp <- mprint old
  mlog "Consume" $ "  Consume Bind 0 -> " <> oldp
  mconsumeTrue old
mconsume1 old (ValueBind (Bind _ idx)) = do
  oldp <- mprint old
  mlog "Consume" $ "  Consume Bind " <> pprint idx <> " -> " <> oldp
  mconsumeBind old idx

mconsume :: (MonadInterpret m) => IValue m -> Value Range -> m ()
mconsume old inp = do
  oldp <- mprintv old
  mlog "Consume" $ "Consume: " <> oldp <> " -> " <> pprint inp
  oldl <- mvalList1 old
  () <$ foldM mconsume1 oldl [inp]

mproduce1 :: (MonadInterpret m) => Value Range -> m (IValue m)
mproduce1 out = do
  out' <- mval2Ival out
  mfillBinds out'

mcast :: (MonadInterpret m) => IValue m -> S.Set SType -> m (IValue m)
mcast new typ = do
  ntyp  <- mgetType new
  newp  <- mprintv new
  ntypp <- mprint ntyp
  mlog "Cast"
    $  "Casting "
    <> ntypp
    <> " to "
    <> pprint (S.toList typ)
    <> "\n  in "
    <> newp
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
          castp <- mprint cast
          mlog "Cast" $ "Success - found " <> castp
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

madvance' :: (MonadInterpret m) => IValue m -> Next Range -> m (IValue m)
madvance' new (NextEval _) = do
  (lang, val) <- munwrapLang new
  txt         <- mval2Stx val
  meval lang txt
madvance' new (NextCast  cast) = madvanceCast new cast
madvance' new (NextGroup grp ) = madvanceGroup new grp

madvance :: (MonadInterpret m) => IValue m -> Next Range -> m (IValue m)
madvance new next = do
  newp <- mprintv new
  mlog "Advance" $ "Advance: " <> newp <> " " <> pprint next <> " ..."
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
  new <- mproduce out nexts
  mcheckLengthEq new $ valBlobLength inp
  mconsume new inp

mreduceLocal :: (MonadInterpret m) => Reducer Range -> IValue m -> m (IValue m)
mreduceLocal red@(Reducer _ (Guard _ inp out nexts) guards) old = do
  mlog "Reduce" $ "Reduce local: " <> pprint red
  mconsume old inp
  forM_ (reverse guards) mguard
  new <- mproduce out nexts
  mlog "Reduce" $ "Succeeded reduce local: " <> pprint red
  pure new

mreduce :: (MonadInterpret m) => IPipe m -> Reducer Range -> m ()
mreduce pipe red@(Reducer _ (Guard _ inp out nexts) guards) = do
  mlog "Reduce" $ "Reduce: " <> pprint red
  old <- mpeekInput pipe $ valBlobLength inp
  mconsume old inp
  forM_ (reverse guards) mguard
  new <- mproduce out nexts
  mdropInput pipe $ valBlobLength inp
  mpopRFrame -- For local reducer
  mpopRFrame -- For group
  mlog "Reduce" $ "Succeeded reduce: " <> pprint red
  mpushOutput pipe new
  misucceed

mtransform :: (MonadInterpret m) => IPipe m -> GroupDef Range -> m ()
mtransform pipe (GroupDef _ vprops gprops reds) = do
  mlog "Transform" "Transform"
  mgframe2rframe vprops gprops
  forM_ reds $ \red -> do
    mdupRFrame
    mcatchFail $ mreduce pipe red
    mlog "Transform" $ "Failed reduce: " <> pprint red
    mpopRFrame
  mpopRFrame
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
  pipe <- mmkLocalPipe new
  mcatchEOF $ forever $ do
    mpushGFrame inVProps inGProps
    mcatchSuccess $ do
      mtransformI (PipeLocal <$> pipe) grp -- pops GFrame
      mipanic "didn't succeed or fail"
  mgetLocalPipeOut pipe

mtransformMain :: (MonadInterpret m) => IGroup m -> m ()
mtransformMain grp = do
  pipe <- mgetGlobalPipe
  mpushGFrame [] []
  mcatchSuccess $ do
    mcatchFail $ mtransformI (PipeGlobal <$> pipe) grp
    mipanic "can't transform this value"

minterpret :: (MonadInterpret m) => Program Range -> m ()
minterpret (Program _ pth _ _ _ _ its creds grps) = do
  mlog "Interpret" "Loading"
  mloadPath pth
  mloadCreds creds
  mloadGroups grps
  mloadStart
  mloadInits its
  grp <- mresolveGlobal Symbol { symbolAnn    = r0
                               , symbolModule = pth
                               , symbol       = "Main"
                               }
  mcatchEOF $ forever $ do
    mlog "Interpret" "Move Index"
    mtransformMain grp
  mlog "Interpret" "Finishing"
  mloadEnd
