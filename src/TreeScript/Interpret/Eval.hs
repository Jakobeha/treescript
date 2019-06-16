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
import           TreeScript.Parse.Lang
import           TreeScript.Session

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Logger
import           Control.Monad.Loops
import           Control.Monad.RWS.Strict
import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Void
import           System.IO
import qualified System.IO.Streams             as S

type LenList a = [a]

-- | TODO Follow naming conventions
data EvalRead
  = EvalRead
  { input :: S.InputStream (Value Range)
  , output :: S.OutputStream (Value Range)
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
  { path :: ModulePath
  , creds :: M.Map CastSurface (Reducer Range)
  , groups :: M.Map (Symbol ()) (GroupDef Range)
  , panicMsg :: Maybe T.Text
  , eofC :: () -> Eval Void
  , failC :: () -> Eval Void
  , successC :: LenList (Value Range) -> Eval Void
  , immInput :: LenList (Value Range)
  , immInputFix :: Maybe Int
  , cachedAllInput :: Bool
  , frames :: [Frame]
  }

newtype Eval a = Eval{ unInterpret :: RWST EvalRead () EvalState (ContT () SessionRes) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadInterpret Eval where
  type IValue Eval = LenList (Value Range)
  type IGroup Eval = GroupDef Range

  mlog = Eval . logDebugN
  mloadPath x = Eval $ modify $ \s -> s { path = x }
  mloadCreds x = Eval $ modify $ \s -> s { creds = x }
  mloadGroups x = Eval $ modify $ \s -> s { groups = x }
  mloadStart = Eval $ pure ()
  mloadEnd   = Eval $ do
    out <- output <$> ask
    liftIO $ S.write Nothing out
  mstartLib lib = Eval $ pure () -- SOON
  mstopLib lib = Eval $ pure () -- SOON
  mcatchEOF expr = Eval $ callCC $ \k -> do
    modify $ \s -> s { eofC = Eval . k }
    unInterpret expr
  mcatchFail expr = Eval $ callCC $ \k -> do
    modify $ \s -> s { failC = Eval . k }
    unInterpret expr
  mcatchSuccess expr = Eval $ callCC $ \k -> do
    modify $ \s -> s { successC = Eval . k }
    unInterpret expr
  mjmpFail = Eval $ do
    k <- failC <$> get
    unInterpret $ absurd <$> k ()
  mjmpSucceed res = Eval $ do
    k <- successC <$> get
    unInterpret $ absurd <$> k res
  mjmpEof = Eval $ do
    k <- eofC <$> get
    unInterpret $ absurd <$> k ()
  mjmpPanic msg = Eval $ do
    modify $ \s -> s { panicMsg = Just msg }
    unInterpret mieof
  mreadInput = Eval $ do
    inp <- input <$> ask
    let keepReading st = length (immInput st) < 8 && not (cachedAllInput st)
    whileM_ (keepReading <$> get) $ do
      next <- liftIO $ S.read inp
      case next of
        Nothing    -> modify $ \s -> s { cachedAllInput = True }
        Just next' -> modify $ \s -> s { immInput = next' : immInput s }
    immInp <- immInput <$> get
    when (null immInp) $ unInterpret mieof
  mwriteOutput xl = Eval $ do
    out <- output <$> ask
    forM_ xl $ \x -> liftIO $ S.write (Just x) out
  mpushGFrame vprops gprops = Eval $ modify $ \s ->
    s { frames = FrameG (GFrame vprops gprops) : frames s }
  mgframe2rframe vprops gprops = Eval $ do
    FrameG f : fs <- frames <$> get
    f'            <- unInterpret $ FrameR <$> mgframe2rframe' f vprops gprops
    modify $ \s -> s { frames = f' : fs }
  mdupRFrame = Eval $ modify $ \s ->
    let (f : fs) = frames s in s { frames = f : f : fs }
  mpopRFrame  = Eval $ modify $ \s -> s { frames = tail $ frames s }
  mtransformI = mtransform
-- | Stores length of input in register
  mpushFixedInput inp = Eval $ modify $ \s ->
    s { immInputFix = Just $ length inp, immInput = inp ++ immInput s }
-- | Fails if not enough input left, or if fixed input was pushed and len is different
  mpeekInput len = Eval $ do
    st <- get
    when (any (/= len) (immInputFix st) || length (immInput st) < len)
      $ unInterpret mifail
    pure $ take len $ immInput st
  mdropInput len = Eval $ do
    st <- get
    when (any (/= len) (immInputFix st) || length (immInput st) < len)
      $ unInterpret mifail
    put $ st { immInput = drop len $ immInput st, immInputFix = Nothing }
  mval2Ival x = pure [x]
  mresolveGlobal sym = Eval $ (M.! remAnns sym) . groups <$> get
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
    pure xs
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
  when (length vprops /= length vpropIdxs) $ unInterpret $ mipanic
    "# of value props is different"
  when (length gprops /= length gpropIdxs) $ unInterpret $ mipanic
    "# of group props is different"
  when (map groupDefPropIdx gpropIdxs /= [1 .. length gpropIdxs])
    $ unInterpret
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
  FrameR top <- head . frames <$> get
  pure top

initialState :: EvalState
initialState = EvalState { path           = error "not set"
                         , creds          = error "not set"
                         , groups         = error "not set"
                         , panicMsg       = Nothing
                         , eofC           = error "not set"
                         , failC          = error "not set"
                         , successC       = error "not set"
                         , immInput       = []
                         , immInputFix    = Nothing
                         , cachedAllInput = False
                         , frames         = []
                         }

runEval :: EvalRead -> Eval () -> SessionRes ()
runEval env (Eval x) = (`runContT` pure) $ () <$ runRWST x env initialState

eval :: EvalRead -> Program Range -> SessionRes ()
eval env = runEval env . minterpret

withFileAsOutput
  :: (MonadMask m, MonadIO m)
  => FilePath
  -> (S.OutputStream B.ByteString -> m a)
  -> m a
withFileAsOutput pth f =
  bracket (liftIO $ openFile pth WriteMode) (liftIO . hClose) $ \hdl -> do
    out <- liftIO (S.handleToOutputStream hdl)
    f out


evalFile :: FilePath -> FilePath -> Program Range -> SessionRes ()
evalFile inp out prg = do
  sinp <- liftIOAndCatch StageReadInput . S.map (r0 <$) =<< parseLang inp
  ResultT $ withFileAsOutput out $ \out' -> runResultT $ do
    sout <- liftIOAndCatch StageEval $ S.contramap (T.encodeUtf8 . pprint) out'
    eval EvalRead { input = sinp, output = sout } prg

evalFileOutText :: FilePath -> Program Range -> SessionRes T.Text
evalFileOutText inp prg = do
  sinp <- liftIOAndCatch StageReadInput . S.map (r0 <$) =<< parseLang inp
  (out', getRes) <- liftIOAndCatch StageReadInput S.listOutputStream
  eval EvalRead { input = sinp, output = out' } prg
  T.unlines . map pprint <$> liftIOAndCatch StageReadInput getRes
