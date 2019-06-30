{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.Maybe
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
  { evalReadInput :: St.InputStream (Value Range)
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

data EvalCont
  = EvalContEof (() -> Eval Void)
  | EvalContFail (() -> Eval Void)
  | EvalContSuccess (LenList (Value Range) -> Eval Void)
data EvalState
  = EvalState
  { evalStatePath :: ModulePath
  , evalStateCreds :: M.Map CastSurface (Reducer Range)
  , evalStateGroups :: M.Map (Symbol ()) (GroupDef Range)
  , evalStatePanicMsg :: Maybe T.Text
  , evalStateCStack :: [EvalCont]
  , evalStateImmInput :: Se.Seq (Value Range)
  , evalStateImmInputFix :: [Int]
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
    modify $ \s ->
      s { evalStateCStack = EvalContEof (Eval . k) : evalStateCStack s }
    res <- unEval expr
    unEval $ popCsUntilInclusive $ \case
      (EvalContEof _) -> Just ()
      _               -> Nothing
    pure res
  mcatchFail expr = Eval $ callCC $ \k -> do
    modify $ \s ->
      s { evalStateCStack = EvalContFail (Eval . k) : evalStateCStack s }
    res <- unEval expr
    unEval $ popCsUntilInclusive $ \case
      (EvalContFail _) -> Just ()
      _                -> Nothing
    pure res
  mcatchSuccess expr = Eval $ callCC $ \k -> do
    modify $ \s ->
      s { evalStateCStack = EvalContSuccess (Eval . k) : evalStateCStack s }
    res <- unEval expr
    unEval $ popCsUntilInclusive $ \case
      (EvalContSuccess _) -> Just ()
      _                   -> Nothing
    pure res
  mjmpEof = Eval $ do
    panicMsg <- evalStatePanicMsg <$> get
    case panicMsg of
      Nothing        -> unEval $ mlog "Got eof"
      Just panicMsg' -> logWarnN $ "Panic: " <> panicMsg'
    k <- unEval $ popCsUntilInclusive $ \case
      (EvalContEof x) -> Just x
      _               -> Nothing
    unEval $ absurd <$> k ()
  mjmpFail = Eval $ do
    k <- unEval $ popCsUntilInclusive $ \case
      (EvalContFail x) -> Just x
      _                -> Nothing
    unEval $ absurd <$> k ()
  mjmpSucceed res = Eval $ do
    k <- unEval $ popCsUntilInclusive $ \case
      (EvalContSuccess x) -> Just x
      _                   -> Nothing
    unEval $ absurd <$> k res
  mjmpPanic msg = Eval $ do
    modify $ \s -> s { evalStatePanicMsg = Just msg }
    unEval mjmpEof
  mreadInput = Eval $ do
    inp <- evalReadInput <$> ask
    let keepReading st =
          length (evalStateImmInput st) < 8 && not (evalStateCachedAllInput st)
    whileM_ (keepReading <$> get) $ do
      next <- lift $ lift
        (liftIOAndCatch StageReadInput $ St.read inp :: SessionRes
            (Maybe (Value Range))
        )
      case next of
        Nothing    -> modify $ \s -> s { evalStateCachedAllInput = True }
        Just next' -> modify
          $ \s -> s { evalStateImmInput = evalStateImmInput s Se.|> next' }
    immInp <- evalStateImmInput <$> get
    unEval $ mlog $ "Immediate input: " <> pprint (toList immInp)
    when (null immInp) $ unEval mieof
  mwriteOutput xl = Eval $ do
    out <- evalReadOutput <$> ask
    forM_ xl $ \x -> liftIO $ St.write (Just x) out
  mpushGFrame vprops gprops =
    logFrames ("Push " <> pprint (GFrame vprops gprops)) $ Eval $ do
-- Lazy reuse of code which "happens" to be equivalent
      noFrames <- null . evalStateFrames <$> get
      vprops'  <- if noFrames then pure vprops else unEval $ mfillBinds vprops
      modify
        $ \s -> s
            { evalStateFrames = FrameG (GFrame vprops' gprops)
                                  : evalStateFrames s
            }
  mgframe2rframe vprops gprops = logFrames "GFrame -> RFrame" $ Eval $ do
    FrameG f : fs <- evalStateFrames <$> get
    f'            <- unEval $ FrameR <$> mgframe2rframe' f vprops gprops
    modify $ \s -> s { evalStateFrames = f' : fs }
  mdupRFrame = logFrames "Dup RFrame" $ Eval $ do
    FrameR f : fs <- evalStateFrames <$> get
    f'            <- unEval $ dupRFrame f
    modify $ \s -> s { evalStateFrames = FrameR f' : FrameR f : fs }
  mpopRFrame = logFrames "Pop" $ Eval $ modify $ \s ->
    s { evalStateFrames = tail $ evalStateFrames s }
  mtransformI = mtransform
-- | Stores length of input in register
  mpushFixedInput inp = Eval $ modify $ \s -> s
    { evalStateImmInputFix = length inp : evalStateImmInputFix s
    , evalStateImmInput    = Se.fromList inp <> evalStateImmInput s
    }
  mdropFixedInput = Eval $ do
    st <- get
    let immInputFix' = evalStateImmInputFix st
        immInput     = evalStateImmInput st
    unEval
      $  mlog
      $  "Dropping fixed input if "
      <> pprint (length immInputFix')
      <> " > 0 from "
      <> pprint (toList immInput)
    modify $ \s -> case evalStateImmInputFix s of
      []                -> s
      immInputFix : fxs -> s
        { evalStateImmInputFix = fxs
        , evalStateImmInput    = Se.drop immInputFix $ evalStateImmInput s
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
    let immInputFix = evalStateImmInputFix st
        immInput    = evalStateImmInput st
    when
        (  (not (null immInputFix) && head immInputFix /= len)
        || length immInput
        <  len
        )
      $ unEval mifail
    unEval $ mlog $ "Dropping " <> pprint len <> " from " <> pprint
      (toList immInput)
    put $ st { evalStateImmInputFix = drop 1 immInputFix
             , evalStateImmInput    = Se.drop len immInput
             }
  mval2Ival x = pure $ unwrapIList x
  mresolveGlobal sym = Eval $ (M.! remAnns sym) . evalStateGroups <$> get
  madvanceLocalGroup new rng _ idx lvps lgps = do
    frame <- topRFrame
    let GroupRef _ loc rvps rgps = rframeLocalGroups frame V.! (idx - 1)
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
    prev  <- liftIO $ MV.read binds (idx - 1)
    case prev of
      Nothing    -> pure ()
      Just prev' -> mlog $ "Prev: " <> pprint prev'
    case prev of
      Nothing -> liftIO $ MV.write binds (idx - 1) $ Just x
      Just prev' | remAnns prev' == remAnns x -> pure ()
                 | otherwise                  -> mifail
    pure xs
  mfillBinds old = do
    binds      <- rframeBinds <$> topRFrame
    idxSplices <-
      liftIO
      $   V.toList
      .   V.imapMaybe (\idx x -> (idx + 1, ) <$> x)
      <$> V.freeze binds
    let new = map (substBinds idxSplices) old
    pure new

instance Printable GFrame where
  pprint (GFrame vals grefs) =
    "GFrame{" <> pprint vals <> ", " <> pprint grefs <> "}"

printRFrame :: RFrame -> Eval T.Text
printRFrame (RFrame bnds grps) = do
  bnds' <- liftIO $ V.toList <$> V.freeze bnds
  let grps' = V.toList grps
      printBindSlot :: Int -> Maybe (Value Range) -> T.Text
      printBindSlot _   Nothing    = ""
      printBindSlot idx (Just bnd) = "\n  " <> pprint idx <> ": " <> pprint bnd
  pure
    $  "RFrame{\n binds: "
    <> T.concat (zipWith printBindSlot [1 ..] bnds')
    <> "\n groups: "
    <> pprint grps'
    <> "}"

printFrame :: Frame -> Eval T.Text
printFrame (FrameG gframe) = pure $ pprint gframe
printFrame (FrameR rframe) = printRFrame rframe

printFrames :: [Frame] -> Eval T.Text
printFrames frms = do
  frms' <- if length frms > 3
    then (++ ["..."]) <$> traverse printFrame (take 3 frms)
    else traverse printFrame frms
  pure
    $  pprint (length frms)
    <> " frames:\n=====\n"
    <> T.intercalate "\n=====\n" frms'

logFrames :: T.Text -> Eval a -> Eval a
logFrames desc action = do
  bfrms  <- Eval $ evalStateFrames <$> get
  pbfrms <- printFrames bfrms
  mlog $ "Before " <> desc <> ": " <> pbfrms
  res    <- action
  afrms  <- Eval $ evalStateFrames <$> get
  pafrms <- printFrames afrms
  mlog $ "After " <> desc <> ": " <> pafrms
  pure res

dupRFrame :: RFrame -> Eval RFrame
dupRFrame (RFrame bnds grps) = RFrame <$> liftIO (MV.clone bnds) <*> pure grps

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
  zipWithM_ (\vprop idx -> liftIO $ MV.write binds (idx - 1) $ Just vprop)
            vprops
            (map groupDefPropIdx vpropIdxs)
  let localGroups = V.fromList gprops
  pure RFrame { rframeBinds = binds, rframeLocalGroups = localGroups }

-- | This is here because RFrame isn't representable by the compiler
topRFrame :: Eval RFrame
topRFrame = Eval $ do
  FrameR top <- head . evalStateFrames <$> get
  pure top

-- | "inclusive" = pops 1 more and return the predicate returns @True@
popCsUntilInclusive :: (EvalCont -> Maybe a) -> Eval a
popCsUntilInclusive f = Eval $ do
  cs <- evalStateCStack <$> get
  let c : cs' = dropWhile (isNothing . f) cs
      c'      = fromJust $ f c
  modify $ \s -> s { evalStateCStack = cs' }
  pure c'

initialState :: EvalState
initialState = EvalState { evalStatePath           = error "not set"
                         , evalStateCreds          = error "not set"
                         , evalStateGroups         = error "not set"
                         , evalStatePanicMsg       = Nothing
                         , evalStateCStack         = []
                         , evalStateImmInput       = Se.empty
                         , evalStateImmInputFix    = []
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
  sinp  <- liftIOAndCatch StageReadInput . St.map (r0 <$) =<< parseStxFile inp
  withFileAsOutput out $ \out' -> do
    sout <- printAstStream olang out'
    eval EvalRead { evalReadInput = sinp, evalReadOutput = sout } prg

evalFileOutText :: FilePath -> Language -> Program Range -> SessionRes T.Text
evalFileOutText inp olang prg = do
  sinp <- liftIOAndCatch StageReadInput . St.map (r0 <$) =<< parseStxFile inp
  (out', getRes) <- liftIOAndCatch StageReadInput St.listOutputStream
  sout <- printAstStream olang out'
  eval EvalRead { evalReadInput = sinp, evalReadOutput = sout } prg
  T.concat . map T.decodeUtf8 <$> liftIOAndCatch StageWriteOutput getRes
