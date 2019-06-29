{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Interpret.Eval
  ( EvalRead(..)
  , eval
  , evalFile
  , evalFileOutText
  )
where

import           TreeScript.Ast
import           TreeScript.Interpret.Gen
import           TreeScript.Misc
import           TreeScript.Plugin

import           Control.Monad.Cont
import           Control.Monad.Logger
import           Control.Monad.Loops
import           Control.Monad.RWS.Strict
import           Data.Foldable
import qualified Data.Map.Strict               as M
import qualified Data.Sequence                 as Se
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Void
import qualified System.IO.Streams             as St

type LenList a = [a]

-- | TODO Follow naming conventions
data EvalRead
  = EvalRead
  { evalReadInput :: ResultInputStream (Value Range)
  , evalReadOutput :: St.OutputStream (Value Range)
  }

data GFrame
  = GFrame [Value Range] [GroupRef Range]

data RFrame
  = RFrame
  { rframeBinds :: MV.IOVector (Maybe (Value Range))
  , rframeLocalGroups :: V.Vector (GroupRef Range)
  }

data Frame
  = FrameG GFrame
  | FrameR RFrame

data EvalState
  = EvalState
  { evalStatePath :: ModulePath
  , evalStateCreds :: M.Map CastSurface (Reducer Range)
  , evalStateGroups :: M.Map (Symbol ()) (GroupDef Range)
  , evalStatePanicMsg :: Maybe T.Text
  , evalStateEof :: () -> Eval Void
  , evalStateFailC :: () -> Eval Void
  , evalStateSuccessC :: LenList (Value Range) -> Eval Void
  , evalStateImmInput :: Se.Seq (Value Range)
  , evalStateImmInputFix :: Maybe Int
  , evalStateCachedAllInput :: Bool
  , evalStateFrames :: [Frame]
  }

newtype Eval a = Eval{ unEval :: RWST EvalRead () EvalState (ContT () SessionRes) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadInterpret Eval where
  type IValue Eval = LenList (Value Range)
  type IGroup Eval = GroupDef Range

  mlog = Eval . logDebugN
  mloadPath x = Eval $ modify $ \s -> s { evalStatePath = x }
  mloadCreds x = Eval $ modify $ \s -> s { evalStateCreds = x }
  mloadGroups x = Eval $ modify $ \s -> s { evalStateGroups = x }
  mloadStart = Eval $ pure ()
  mloadEnd   = Eval $ do
    out <- evalReadOutput <$> ask
    liftIO $ St.write Nothing out
  mstartLib lib = Eval $ pure () -- SOON
  mstopLib lib = Eval $ pure () -- SOON
  mcatchEOF expr = Eval $ callCC $ \k -> do
    modify $ \s -> s { evalStateEof = Eval . k }
    unEval expr
  mcatchFail expr = Eval $ callCC $ \k -> do
    modify $ \s -> s { evalStateFailC = Eval . k }
    unEval expr
  mcatchSuccess expr = Eval $ callCC $ \k -> do
    modify $ \s -> s { evalStateSuccessC = Eval . k }
    unEval expr
  mjmpFail = Eval $ do
    k <- evalStateFailC <$> get
    unEval $ absurd <$> k ()
  mjmpSucceed res = Eval $ do
    k <- evalStateSuccessC <$> get
    unEval $ absurd <$> k res
  mjmpEof = Eval $ do
    unEval $ mlog "Got eof"
    k <- evalStateEof <$> get
    unEval $ absurd <$> k ()
  mjmpPanic msg = Eval $ do
    modify $ \s -> s { evalStatePanicMsg = Just msg }
    unEval mieof
  mreadInput = Eval $ do
    inp <- evalReadInput <$> ask
    let keepReading st =
          length (evalStateImmInput st) < 8 && not (evalStateCachedAllInput st)
    whileM_ (keepReading <$> get) $ do
      next <- lift $ lift $ readResultStream inp
      case next of
        Nothing    -> modify $ \s -> s { evalStateCachedAllInput = True }
        Just next' -> modify
          $ \s -> s { evalStateImmInput = evalStateImmInput s Se.|> next' }
    immInp <- evalStateImmInput <$> get
    when (null immInp) $ unEval mieof
  mwriteOutput xl = Eval $ do
    out <- evalReadOutput <$> ask
    forM_ xl $ \x -> liftIO $ St.write (Just x) out
  mpushGFrame vprops gprops =
    Eval
      $ modify
      $ \s -> s
          { evalStateFrames = FrameG (GFrame vprops gprops) : evalStateFrames s
          }
  mgframe2rframe vprops gprops = Eval $ do
    FrameG f : fs <- evalStateFrames <$> get
    f'            <- unEval $ FrameR <$> mgframe2rframe' f vprops gprops
    modify $ \s -> s { evalStateFrames = f' : fs }
  mdupRFrame = Eval $ modify $ \s ->
    let (f : fs) = evalStateFrames s in s { evalStateFrames = f : f : fs }
  mpopRFrame =
    Eval $ modify $ \s -> s { evalStateFrames = tail $ evalStateFrames s }
  mtransformI = mtransform
-- | Stores length of input in register
  mpushFixedInput inp = Eval $ modify $ \s -> s
    { evalStateImmInputFix = Just $ length inp
    , evalStateImmInput    = evalStateImmInput s <> Se.fromList inp
    }
-- | Fails if not enough input left, or if fixed input was pushed and len is different
  mpeekInput len = Eval $ do
    st <- get
    when
        (  any (/= len) (evalStateImmInputFix st)
        || length (evalStateImmInput st)
        <  len
        )
      $ unEval mifail
    pure $ toList $ Se.take len $ evalStateImmInput st
  mdropInput len = Eval $ do
    st <- get
    when
        (  any (/= len) (evalStateImmInputFix st)
        || length (evalStateImmInput st)
        <  len
        )
      $ unEval mifail
    put $ st { evalStateImmInput    = Se.drop len $ evalStateImmInput st
             , evalStateImmInputFix = Nothing
             }
  mval2Ival x = pure $ unwrapIList x
  mresolveGlobal sym = Eval $ (M.! remAnns sym) . evalStateGroups <$> get
  madvanceLocalGroup new rng _ idx lvps lgps = do
    frame <- topRFrame
    let GroupRef _ loc rvps rgps = rframeLocalGroups frame V.! idx
    madvanceGroup new $ GroupRef rng loc (rvps ++ lvps) (rgps ++ lgps)
  mcheckLengthEq val len = when (length val /= len) mifail
  mconsumePrim []       _   = mifail
  mconsumePrim (x : xs) inp = do
    when (remAnns x /= ValuePrimitive (remAnns inp)) mifail
    pure xs
-- SOON Consume multiple when encounter an IList
  mconsumeRecordHead (ValueRecord (Record _ head' props) : xs) ihead ilen = do
    when (remAnns head' /= remAnns ihead || length props /= ilen) mifail
    pure $ props ++ xs
  mconsumeRecordHead _ _ _ = mifail
-- | Binds are never ILists
  mconsumeTrue []       = mifail
  mconsumeTrue (_ : xs) = pure xs
-- | Binds are never ILists
  mconsumeBind []       _   = mifail
  mconsumeBind (x : xs) idx = do
    binds <- rframeBinds <$> topRFrame
    prev  <- liftIO $ MV.read binds idx
    case prev of
      Nothing -> liftIO $ MV.write binds idx $ Just x
      Just prev' | remAnns prev' == remAnns x -> pure ()
                 | otherwise                  -> mifail
    pure xs
  mfillBinds old = do
    binds      <- rframeBinds <$> topRFrame
    idxSplices <-
      liftIO
      $   V.toList
      .   V.imapMaybe (\idx x -> (idx, ) <$> x)
      <$> V.freeze binds
    let new = map (substBinds idxSplices) old
    pure new

mgframe2rframe'
  :: GFrame -> [GroupDefProp Range] -> [GroupDefProp Range] -> Eval RFrame
mgframe2rframe' (GFrame vprops gprops) vpropIdxs gpropIdxs = Eval $ do
  binds <- liftIO $ MV.replicate 16 Nothing
  when (length vprops /= length vpropIdxs) $ unEval $ mipanic
    "# of value props is different"
  when (length gprops /= length gpropIdxs) $ unEval $ mipanic
    "# of group props is different"
  when (map groupDefPropIdx gpropIdxs /= [1 .. length gpropIdxs])
    $ unEval
    $ mipanic
        "expected group props to be in sequence (TODO replace with a simple int to enforce this)"
  zipWithM_ (\vprop idx -> liftIO $ MV.write binds idx $ Just vprop)
            vprops
            (map groupDefPropIdx vpropIdxs)
  let localGroups = V.fromList gprops
  pure RFrame { rframeBinds = binds, rframeLocalGroups = localGroups }

-- | This is here because RFrame isn't representable by the compiler
topRFrame :: Eval RFrame
topRFrame = Eval $ do
  FrameR top <- head . evalStateFrames <$> get
  pure top

initialState :: EvalState
initialState = EvalState { evalStatePath           = error "not set"
                         , evalStateCreds          = error "not set"
                         , evalStateGroups         = error "not set"
                         , evalStatePanicMsg       = Nothing
                         , evalStateEof            = error "not set"
                         , evalStateFailC          = error "not set"
                         , evalStateSuccessC       = error "not set"
                         , evalStateImmInput       = Se.empty
                         , evalStateImmInputFix    = Nothing
                         , evalStateCachedAllInput = False
                         , evalStateFrames         = []
                         }

runEval :: EvalRead -> Eval () -> SessionRes ()
runEval env (Eval x) = (`runContT` pure) $ () <$ runRWST x env initialState

eval :: EvalRead -> Program Range -> SessionRes ()
eval env = runEval env . minterpret

evalFile :: FilePath -> FilePath -> Program Range -> SessionRes ()
evalFile inp out prg = do
  olang <- forceLangForPath out
  sinp  <-
    liftIOAndCatch StageReadInput
    .   mapPureInputStream (St.map (r0 <$))
    =<< parseAstFile inp
  withFileAsOutput out $ \out' -> do
    sout <- printAstStream olang out'
    eval EvalRead { evalReadInput = sinp, evalReadOutput = sout } prg

evalFileOutText :: FilePath -> Language -> Program Range -> SessionRes T.Text
evalFileOutText inp olang prg = do
  sinp <-
    liftIOAndCatch StageReadInput
    .   mapPureInputStream (St.map (r0 <$))
    =<< parseAstFile inp
  (out', getRes) <- liftIOAndCatch StageReadInput St.listOutputStream
  sout           <- printAstStream olang out'
  eval EvalRead { evalReadInput = sinp, evalReadOutput = sout } prg
  T.concat . map T.decodeUtf8 <$> liftIOAndCatch StageWriteOutput getRes
