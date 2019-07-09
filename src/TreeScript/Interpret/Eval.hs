{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Interpret.Eval
  ( eval
  , evalFile
  , evalFileOutText
  )
where

import           TreeScript.Ast
import           TreeScript.Interpret.Gen
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Streams   as St
import           TreeScript.Plugin

import           Control.Monad.Cont
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict               as M
import           Data.Maybe
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Void
import           Language.JavaScript.Inline
                                         hiding ( eval )
import           System.FilePath
import qualified System.IO.Streams             as St

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
  | EvalContSuccess (() -> Eval Void)
data EvalState
  = EvalState
  { evalStatePath :: ModulePath
  , evalStateSession :: JSSession
  , evalStateCreds :: M.Map CastSurface (Reducer Range)
  , evalStateGroups :: M.Map (Symbol ()) (GroupDef Range)
  , evalStatePanicMsg :: Maybe T.Text
  , evalStateCStack :: [EvalCont]
  , evalStateFrames :: [Frame]
  }

newtype Eval a = Eval{ unEval :: RWST GlobalPipe () EvalState (ContT () SessionRes) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadResult)

instance MonadInterpret Eval where
  data IData Eval a = EData{ unEData :: a } deriving (Functor)

  mprint (EData x) = pure $ pprint x
  mlog typ msg
    | typ `Set.member` Set.fromList
      [ "Interpret"
      , "Reduce"
      , "Guard"
      , "Produce"
      , "Advance"
      , "Cast"
      , "Consume"
      , "Control"
      ]
    = Eval $ logDebugN msg
    | otherwise
    = pure ()
  mloadPath x = Eval $ modify $ \s -> s { evalStatePath = x }
  mloadCreds x = Eval $ modify $ \s -> s { evalStateCreds = x }
  mloadGroups x = Eval $ modify $ \s -> s { evalStateGroups = x }
  mloadStart = Eval $ do
    pth     <- evalStatePath <$> get
    session <- unEval $ mliftIO StageStartLib $ newJSSession defJSSessionOpts
      { nodeWorkDir = Just $ takeDirectory $ T.unpack pth
      }
    modify $ \s -> s { evalStateSession = session }
  mloadInits _ = pure () -- SOON
  mloadEnd = Eval $ do
    out <- globalPipeOutput <$> ask
    unEval $ mliftIO StageWriteOutput $ St.write Nothing out
    session <- evalStateSession <$> get
    unEval $ mliftIO StageShutdown $ closeJSSession session
  mcatchEOF expr' = Eval $ callCC $ \k -> do
    modify $ \s ->
      s { evalStateCStack = EvalContEof (Eval . k) : evalStateCStack s }
    res <- unEval expr'
    unEval $ popCsUntilInclusive $ \case
      (EvalContEof _) -> Just ()
      _               -> Nothing
    pure res
  mcatchFail expr' = Eval $ callCC $ \k -> do
    modify $ \s ->
      s { evalStateCStack = EvalContFail (Eval . k) : evalStateCStack s }
    res <- unEval expr'
    unEval $ popCsUntilInclusive $ \case
      (EvalContFail _) -> Just ()
      _                -> Nothing
    pure res
  mcatchSuccess expr' = Eval $ callCC $ \k -> do
    modify $ \s ->
      s { evalStateCStack = EvalContSuccess (Eval . k) : evalStateCStack s }
    res <- unEval expr'
    unEval $ popCsUntilInclusive $ \case
      (EvalContSuccess _) -> Just ()
      _                   -> Nothing
    pure res
  mjmpEof = Eval $ do
    panicMsg <- evalStatePanicMsg <$> get
    case panicMsg of
      Nothing        -> pure ()
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
  mjmpSucceed = Eval $ do
    k <- unEval $ popCsUntilInclusive $ \case
      (EvalContSuccess x) -> Just x
      _                   -> Nothing
    unEval $ absurd <$> k ()
  mjmpPanic msg = Eval $ do
    modify $ \s -> s { evalStatePanicMsg = Just msg }
    unEval mjmpEof
  mgetGlobalPipe = Eval $ EData <$> ask
  mmkLocalPipe (EData inVal) = do
    inp <- case value2Stx inVal of
      Nothing             -> pure $ Right inVal
      Just (StxBlob stxs) -> mliftIO StageReadInput $ Left <$> St.fromList stxs
    (out, getOutRaw) <- mliftIO StageWriteOutput St.listOutputStream
    pure $ EData LocalPipe { localPipeInput        = inp
                           , localPipeOutput       = out
                           , localPipeGetOutputRaw = getOutRaw
                           }
  mgetLocalPipeOut (EData pipe) = do
    outRaw <- mliftIO StageWriteOutput $ localPipeGetOutputRaw pipe
    case outRaw of
      [Right val] -> pure $ EData val
      _ ->
        case
            traverse
              (\case
                Left  stx -> Just stx
                Right _   -> Nothing
              )
              outRaw
          of
            Nothing   -> mipanic $ "Bad output format: " <> pprint outRaw
            Just stxs -> pure $ EData $ stx2Value r0 $ StxBlob stxs
  mpushGFrame vprops gprops =
    logFrames ("Push " <> pprint (GFrame vprops gprops)) $ Eval $ do
      noFrames <- null . evalStateFrames <$> get
      vprops'  <- if noFrames
        then pure vprops
        else unEval $ mapM (fmap unEData . mfillBinds . EData) vprops
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
  mtransformI pipe = mtransform pipe . unEData
-- | Fails if not enough input left, or if fixed input was pushed and len is different
  mpeekInput (EData (PipeGlobal gpipe)) len =
    EData <$> mpeekStxInput (globalPipeInput gpipe) len
  mpeekInput (EData (PipeLocal lpipe)) len = case localPipeInput lpipe of
    Left stxIn -> EData <$> mpeekStxInput stxIn len
    Right val | len == 1  -> pure $ EData val
              | otherwise -> mifail
  mdropInput (EData (PipeGlobal gpipe)) len =
    mdropStxInput (globalPipeInput gpipe) len
  mdropInput (EData (PipeLocal lpipe)) len = case localPipeInput lpipe of
    Left stxIn -> mdropStxInput stxIn len
    Right _ | len == 1  -> pure ()
            | otherwise -> fail $ "unexpected drop input " ++ show len
  mpushOutput (EData (PipeGlobal gpipe)) (EData out) = case value2Stx out of
    Nothing ->
      mipanic $ "can't write output because it isn't syntax: " <> pprint out
    Just (StxBlob stxs) ->
      mliftIO StageWriteOutput $ St.writeList stxs $ globalPipeOutput gpipe
  mpushOutput (EData (PipeLocal lpipe)) (EData out) = case value2Stx out of
    Nothing ->
      mliftIO StageWriteOutput $ St.write (Just $ Right out) $ localPipeOutput
        lpipe
    Just (StxBlob stxs) ->
      mliftIO StageWriteOutput $ St.writeList (map Left stxs) $ localPipeOutput
        lpipe
  mval2Ival = pure . EData
  munwrapLang (EData val) = case unwrapLang val of
    Nothing           -> mipanic $ "not a semantic language: " <> pprint val
    Just (lang, val') -> pure (EData lang, EData val')
  mval2Stx (EData val) = case value2Stx val of
    Nothing  -> mipanic $ "not syntax: " <> pprint val
    Just stx -> pure $ EData $ pprint stx
  meval (EData LanguageStx) _ =
    mipanic "can't evaluate raw syntax (need semantics)"
  meval (EData LanguageJavaScript) (EData inTxt) = do
    outTxt <- mrunJs [expr| $inTxt |]
    Eval $ lift $ lift $ EData . stx2Value r0 <$> parseStxText outTxt
  mmapMPath x           []       f = f x
  mmapMPath (EData val) (p : ps) f = case val of
    ValueRecord (Record ann head' props) | p < length props ->
      EData . ValueRecord . Record ann head' <$> zipWithM mapMProp [0 ..] props
    _ ->
      mipanic
        $  "Path expects record with at least "
        <> pprint (p + 1)
        <> " props, got: "
        <> pprint val
   where
    mapMProp idx prop | idx == p  = unEData <$> mmapMPath (EData prop) ps f
                      | otherwise = pure prop
  mgetType (EData val) = case valueType val of
    Nothing  -> mipanic $ "Value doesn't have type: " <> pprint val
    Just typ -> pure $ EData typ
  misSubtype (EData typ) = pure . Set.member typ
  mlookupCast (EData ityp) otyp = Eval $ do
    creds <- evalStateCreds <$> get
    pure $ EData <$> creds M.!? CastSurface ityp otyp
  mreduceCast = mreduceLocal . unEData
  mresolveGlobal sym =
    Eval $ EData . (M.! remAnns sym) . evalStateGroups <$> get
  madvanceLocalGroup new rng _ idx lvps lgps = do
    frame <- topRFrame
    let GroupRef _ loc rvps rgps = rframeLocalGroups frame V.! (idx - 1)
    madvanceGroup new $ GroupRef rng loc (rvps ++ lvps) (rgps ++ lgps)
  mcheckLengthEq (EData val) len = when (length val /= len) mifail
  mvalList1 = pure . EData . pure . unEData
  mconsumePrim (EData []      ) _   = mipanic "unexpected end of consume list"
  mconsumePrim (EData (x : xs)) inp = do
    when (remAnns x /= ValuePrimitive (remAnns inp)) mifail
    pure $ EData xs
  mconsumeRecordHead (EData (ValueRecord (Record _ head' props) : xs)) ihead ilen
    = do
      when (remAnns head' /= remAnns ihead || length props /= ilen) mifail
      pure $ EData $ props ++ xs
  mconsumeRecordHead _ _ _ = mifail
  mconsumeTrue (EData []      ) = mipanic "unexpected end of consume list"
  mconsumeTrue (EData (_ : xs)) = pure $ EData xs
  mconsumeBind (EData []      ) _   = mipanic "unexpected end of consume list"
  mconsumeBind (EData (x : xs)) idx = do
    binds <- rframeBinds <$> topRFrame
    prev  <- liftIO $ MV.read binds (idx - 1)
    case prev of
      Nothing    -> pure ()
      Just prev' -> mlog "Consume" $ "Prev: " <> pprint prev'
    case prev of
      Nothing -> liftIO $ MV.write binds (idx - 1) $ Just x
      Just prev' | remAnns prev' == remAnns x -> pure ()
                 | otherwise                  -> mifail
    pure $ EData xs
  mfillBinds (EData old) = do
    binds      <- rframeBinds <$> topRFrame
    idxSplices <-
      liftIO
      $   V.toList
      .   V.imapMaybe (\idx x -> (idx + 1, ) <$> x)
      <$> V.freeze binds
    let new = substBinds idxSplices old
    pure $ EData new

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
  mlog "Stack" $ "Before " <> desc <> ": " <> pbfrms
  res    <- action
  afrms  <- Eval $ evalStateFrames <$> get
  pafrms <- printFrames afrms
  mlog "Stack" $ "After " <> desc <> ": " <> pafrms
  pure res

mliftIO :: Stage -> IO a -> Eval a
mliftIO stage = Eval . lift . lift . liftIOAndCatch stage

mpeekStxInput :: St.InputStream (Idd Stx) -> Int -> Eval (Value Range)
mpeekStxInput inp len = do
  oinLst <- mliftIO StageReadInput $ St.peekn len inp
  case oinLst of
    Nothing    -> mifail
    Just inLst -> pure $ stx2Value r0 $ StxBlob inLst

mdropStxInput :: St.InputStream (Idd Stx) -> Int -> Eval ()
mdropStxInput inp len = do
  suc <- mliftIO StageReadInput $ St.dropn len inp
  unless suc $ fail "can't drop items"

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

mrunJs :: (JSSession -> IO a) -> Eval a
mrunJs fjs = Eval $ do
  session <- evalStateSession <$> get
  unEval $ mliftIO StageEvalStx $ fjs session

initialState :: EvalState
initialState = EvalState { evalStatePath     = error "not set"
                         , evalStateSession  = error "not set"
                         , evalStateCreds    = error "not set"
                         , evalStateGroups   = error "not set"
                         , evalStatePanicMsg = Nothing
                         , evalStateCStack   = []
                         , evalStateFrames   = []
                         }

runEval :: GlobalPipe -> Eval () -> SessionRes ()
runEval env (Eval x) = (`runContT` pure) $ () <$ runRWST x env initialState

eval :: GlobalPipe -> Program Range -> SessionRes ()
eval env = runEval env . minterpret

evalFile :: FilePath -> FilePath -> Program Range -> SessionRes ()
evalFile inp out prg = do
  sinp <- parseStxFile inp
  withFileAsOutput out $ \out' -> do
    sout <- printStxStream out'
    eval GlobalPipe { globalPipeInput = sinp, globalPipeOutput = sout } prg

evalFileOutText :: FilePath -> Program Range -> SessionRes T.Text
evalFileOutText inp prg = do
  sinp           <- parseStxFile inp
  (out', getRes) <- liftIOAndCatch StageReadInput St.listOutputStream
  sout           <- printStxStream out'
  eval GlobalPipe { globalPipeInput = sinp, globalPipeOutput = sout } prg
  T.concat . map T.decodeUtf8 <$> liftIOAndCatch StageWriteOutput getRes
