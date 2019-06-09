{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Interpret.Interpret
  ( InterpretRead(..)
  , interpret
  )
where

import           TreeScript.Ast.Core.Analyze
import           TreeScript.Ast.Core.Interpret.Gen
import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import           Control.Monad.Cont
import           Control.Monad.Loops
import           Control.Monad.RWS.Strict
import qualified System.IO.Streams             as S
import qualified Data.Map.Strict               as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Void

type LenList a = [a]

-- | TODO Follow naming conventions
data InterpretRead
  = InterpretRead
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

data InterpretState
  = InterpretState
  { path :: ModulePath
  , creds :: M.Map CastSurface (Reducer Range)
  , groups :: M.Map (Symbol ()) (GroupDef Range)
  , panicMsg :: Maybe String
  , eofC :: () -> Interpret Void
  , failC :: () -> Interpret Void
  , successC :: LenList (Value Range) -> Interpret Void
  , immInput :: LenList (Value Range)
  , immInputFix :: Maybe Int
  , cachedAllInput :: Bool
  , frames :: [Frame]
  }

newtype Interpret a = Interpret{ unInterpret :: RWST InterpretRead () InterpretState (ContT () IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadInterpret Interpret where
  type IValue Interpret = LenList (Value Range)
  type IGroup Interpret = GroupDef Range

  mloadPath x = Interpret $ modify $ \s -> s { path = x }
  mloadCreds x = Interpret $ modify $ \s -> s { creds = x }
  mloadGroups x = Interpret $ modify $ \s -> s { groups = x }
  mloadStart = Interpret $ pure ()
  mloadEnd   = Interpret $ do
    out <- output <$> ask
    liftIO $ S.write Nothing out
  mstartLib lib = Interpret $ pure () -- SOON
  mstopLib lib = Interpret $ pure () -- SOON
  mcatchEOF expr = Interpret $ callCC $ \k -> do
    modify $ \s -> s { eofC = Interpret . k }
    unInterpret expr
  mcatchFail expr = Interpret $ callCC $ \k -> do
    modify $ \s -> s { failC = Interpret . k }
    unInterpret expr
  mcatchSuccess expr = Interpret $ callCC $ \k -> do
    modify $ \s -> s { successC = Interpret . k }
    unInterpret expr
  mifail = Interpret $ do
    k <- failC <$> get
    unInterpret $ absurd <$> k ()
  misucceed res = Interpret $ do
    k <- successC <$> get
    unInterpret $ absurd <$> k res
  mieof = Interpret $ do
    k <- eofC <$> get
    unInterpret $ absurd <$> k ()
  mipanic msg = Interpret $ do
    modify $ \s -> s { panicMsg = Just msg }
    unInterpret mieof
  mreadInput = Interpret $ do
    inp <- input <$> ask
    let stopReading st = length (immInput st) < 8 || cachedAllInput st
    whileM_ (stopReading <$> get) $ do
      next <- liftIO $ S.read inp
      case next of
        Nothing    -> modify $ \s -> s { cachedAllInput = True }
        Just next' -> modify $ \s -> s { immInput = next' : immInput s }
  mwriteOutput xl = Interpret $ do
    out <- output <$> ask
    forM_ xl $ \x -> liftIO $ S.write (Just x) out
  mpushGFrame vprops gprops = Interpret $ modify $ \s ->
    s { frames = FrameG (GFrame vprops gprops) : frames s }
  mgframe2rframe vprops gprops = Interpret $ do
    FrameG f : fs <- frames <$> get
    f'            <- unInterpret $ FrameR <$> mgframe2rframe' f vprops gprops
    modify $ \s -> s { frames = f' : fs }
  mdupRFrame = Interpret $ modify $ \s ->
    let (f : fs) = frames s in s { frames = f : f : fs }
  mpopRFrame  = Interpret $ modify $ \s -> s { frames = tail $ frames s }
  mtransformI = mtransform
-- | Stores length of input in register
  mpushFixedInput inp = Interpret $ modify $ \s ->
    s { immInputFix = Just $ length inp, immInput = inp ++ immInput s }
-- | Fails if not enough input left, or if fixed input was pushed and len is different
  mpeekInput len = Interpret $ do
    st <- get
    when (any (/= len) (immInputFix st) || length (immInput st) < len)
      $ unInterpret mifail
    pure $ take len $ immInput st
  mdropInput len = Interpret $ do
    st <- get
    when (any (/= len) (immInputFix st) || length (immInput st) < len)
      $ unInterpret mifail
    put $ st { immInput = drop len $ immInput st, immInputFix = Nothing }
  mval2Ival x = pure [x]
  mresolveGlobal sym = Interpret $ (M.! remAnns sym) . groups <$> get
  madvanceLocalGroup new rng _ idx lvps lgps = Interpret $ do
    frame <- unInterpret topRFrame
    let GroupRef _ loc rvps rgps = rframeLocalGroups frame V.! idx
    unInterpret $ madvanceGroup new $ GroupRef rng
                                               loc
                                               (rvps ++ lvps)
                                               (rgps ++ lgps)
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
  :: GFrame -> [GroupDefProp Range] -> [GroupDefProp Range] -> Interpret RFrame
mgframe2rframe' (GFrame vprops gprops) vpropIdxs gpropIdxs = Interpret $ do
  binds <- MV.replicate 16 Nothing
  when (length vprops /= length vpropIdxs) $ unInterpret $ mipanic
    "# of value props is different"
  when (length gprops /= length gpropIdxs) $ unInterpret $ mipanic
    "# of group props is different"
  when (map groupDefPropIdx gpropIdxs /= [1 .. length gpropIdxs])
    $ unInterpret
    $ mipanic
        "expected group props to be in sequence (TODO replace with a simple int to enforce this)"
  zipWithM_ (\vprop idx -> MV.write binds idx $ Just vprop)
            vprops
            (map groupDefPropIdx vpropIdxs)
  let localGroups = V.fromList gprops
  pure RFrame { rframeBinds = binds, rframeLocalGroups = localGroups }

-- | This is here because RFrame isn't representable by the compiler
topRFrame :: Interpret RFrame
topRFrame = Interpret $ do
  FrameR top <- head . frames <$> get
  pure top

initialState :: InterpretState
initialState = InterpretState { path           = error "not set"
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

runInterpret :: InterpretRead -> Interpret () -> IO ()
runInterpret env (Interpret x) =
  (`runContT` pure) $ () <$ runRWST x env initialState

interpret :: InterpretRead -> Program Range -> IO ()
interpret env = runInterpret env . minterpret
