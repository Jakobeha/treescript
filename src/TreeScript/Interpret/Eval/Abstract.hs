{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Direct AST evaluation where side effects must be provided.
module TreeScript.Interpret.Eval.Abstract
  ( AbsEvalT
  , BuiltinVal(..)
  , EvalAnn
  , Val(..)
  , evalProgramWith
  )
where

import           TreeScript.Ast
import           TreeScript.Interpret.Abstract
import           TreeScript.Interpret.FreeVars
import           TreeScript.Print
import           TreeScript.Misc

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Data.List
import qualified Data.List.NonEmpty            as N
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.Void

-- | Abstract evaluation monad transformer. Side effects must be implemented in underlying monads.
newtype AbsEvalT u a = AbsEvalT{ unAbsEvalT :: ContStackT (Val (AbsEvalT u)) (StateT (EvalState (AbsEvalT u)) (EResultT u)) a } deriving (Functor, Applicative, Monad, MonadIO)

data BuiltinVal m
  = BuiltinValPrimitive LitData
  | BuiltinValClosure [T.Text] ([Val m] -> m (Val m))

newtype Ref = Ref{ unRef :: Int } deriving (Eq, Ord, Read, Show)

newtype Frame m = Frame
  { frameVars :: M.Map T.Text (Val m)
  }

data EvalState m
  = EvalState
  { evalStateLastLoc :: Maybe SrcInfo
  , evalStateStack :: Stack m
  , evalStateHeap :: Heap m
  , evalStateObjOrders :: M.Map T.Text [T.Text]
  }

class ValEq a where
  (===) :: (Monad u) => a -> a -> AbsEvalT u Bool

class (AnnMaybeHas SrcInfo r, AnnPrintable r) => EvalAnn r
instance (AnnMaybeHas SrcInfo r, AnnPrintable r) => EvalAnn r

deriving instance (Monad u) => MonadVCont (AbsEvalT u)
deriving instance (Monad u) => MonadContStack (AbsEvalT u)

instance (Monad u) => Interpret (AbsEvalT u) where
  type HasAnns (AbsEvalT u) = EvalAnn
  data Stack (AbsEvalT u) = Stack{ unStack :: N.NonEmpty (Frame (AbsEvalT u)) }
  data Heap (AbsEvalT u)
    = Heap
    { heapData :: M.Map Ref (RefData (AbsEvalT u))
    , heapNextFree :: Ref
    }
  data Val (AbsEvalT u)
    = ValPrimitive LitData
    | ValRef Ref
    deriving (Eq, Ord, Read, Show)
  data Deref (AbsEvalT u)
    = Deref
    { derefGet :: AbsEvalT u (RefData (AbsEvalT u))
    , derefSet :: RefData (AbsEvalT u) -> AbsEvalT u ()
    }
  data ClosureVal (AbsEvalT u) = ClosureVal
    { closureValBody :: AbsEvalT u (Val (AbsEvalT u))
    , closureValFormals :: [T.Text]
    , closureValFreeVars :: S.Set T.Text
    }

  runAnn x = AbsEvalT $ modify $ \s -> s { evalStateLastLoc = maybeFromAnn x }
  mkLiteral = pure . ValPrimitive
  mkReference dt = AbsEvalT $ do
    Heap hdata ref@(Ref refi) <- evalStateHeap <$> get
    let hdata' = M.insert ref dt hdata
        ref'   = Ref $ refi + 1
    modify $ \s -> s { evalStateHeap = Heap hdata' ref' }
    pure $ ValRef ref
  mkClos body' frmls' fvs' = pure ClosureVal { closureValBody     = body'
                                             , closureValFormals  = frmls'
                                             , closureValFreeVars = fvs'
                                             }
  defineVar lhs@(Identifier _ name) val' = modifyVars $ \bs -> if
    | M.member name bs -> mkFail' $ "variable already defined: " <> pprint lhs
    | otherwise        -> pure $ M.insert name val' bs
  setVar lhs@(Identifier _ name) val' = modifyVars $ \bs -> if
    | M.member name bs -> pure $ M.insert name val' bs
    | otherwise        -> mkFail' $ "unbound variable: " <> pprint lhs
  getVar lhs@(Identifier _ name) = getVar' name $ pprint lhs
  deref (ValPrimitive _  ) = mkFail' "can't deref primitive"
  deref (ValRef       ref) = pure $ deref' ref
  getProp drf targ = do
    (_, props, tidx) <- genAccess drf targ
    pure $ props !! tidx -- Should never be out of bounds
  getSubscript drf idx' = do
    (elms, n) <- genSubscript drf idx'
    pure $ elms !! n -- Already checked
  setProp drf targ new = do
    (tag, props, tidx) <- genAccess drf targ
    let props' = replaceIndex tidx new props -- Should never be out of bounds
    derefSet drf $ RefDataObject tag props'
  setSubscript drf idx' new = do
    (elms, n) <- genSubscript drf idx'
    let elms' = replaceIndex n new elms -- Already checked
    derefSet drf $ RefDataArray elms'
  getClos drf = do
    rd <- derefGet drf
    case rd of
      RefDataClosure clos -> pure clos
      _                   -> mkFail' "expected a closure"
  runCall (ClosureVal clos frmls fvids) args = AbsEvalT $ do
    let expNargs = length frmls
        actNargs = length args
    when (expNargs /= actNargs)
      $  unAbsEvalT
      $  mkFail'
      $  "wrong number of arguments: expected "
      <> pprint expNargs
      <> " got "
      <> pprint actNargs
    let args' = M.fromList $ zip frmls args
    unless (M.disjoint args' fvs)
      $ unAbsEvalT
      $ mkFail'
          "internal error (sanity check): formal shares the same name as a free variable"
    let f = Frame $ args' <> fvs
    Stack fs <- evalStateStack <$> get
    modify $ \s -> s { evalStateStack = Stack $ f N.<| fs }
    res <- unAbsEvalT clos
    modify $ \s -> s { evalStateStack = Stack fs }
    pure res
  runUnOp opr@(UnOperator _ typ) val = case doUnOp typ val of
    Nothing  -> mkFail' $ "argument is invalid type for " <> pprint opr
    Just res -> pure res
  runBinOp lhs opr@(BinOperator _ typ) rhs = case doBinOp typ lhs rhs of
    Nothing  -> mkFail' $ "arguments are invalid types for " <> pprint opr
    Just res -> pure res
  getObjectOrders = AbsEvalT $ evalStateObjOrders <$> get
  addDefaultObjectOrder tag orderHere = AbsEvalT $ modify $ \s ->
    s { evalStateObjOrders = M.insert tag orderHere $ evalStateObjOrders s }
  tryMatchCases []                      _    _ = pure ()
  tryMatchCases (Case _ cond body : cs) val' f = do
    res <- tryMatch1 cond val'
    when res $ () <$ f body
    tryMatchCases cs val' f
  freeVariables (FormalList _ frmls) =
    pure . runFreeVars (S.fromList $ map identifierText frmls)
  raiseErr   = mkFail'
  vabsurdVal = pure . absurd

instance ValEq (Val (AbsEvalT u)) where
  ValPrimitive x    === ValPrimitive y    = pure $ x == y
  ValRef       xref === ValRef       yref = do
    xdrf <- derefGet $ deref' xref
    ydrf <- derefGet $ deref' yref
    xdrf === ydrf
  _ === _ = pure False

instance ValEq (RefData (AbsEvalT u)) where
  RefDataArray xs === RefDataArray ys
    | length xs /= length ys = pure False
    | otherwise              = allM (\(x, y) -> x === y) $ zip xs ys
  RefDataObject xtag xs === RefDataObject ytag ys
    | xtag /= ytag = pure False
    | otherwise    = allM (\(x, y) -> x === y) $ zip xs ys
  RefDataClosure _ === RefDataClosure _ =
    mkFail' "internal error: can't compare closures"
  _ === _ = pure False

instance Printable Ref where
  pprint (Ref idx) = "@" <> pprint idx

getVar' :: (Monad u) => T.Text -> T.Text -> AbsEvalT u (Val (AbsEvalT u))
getVar' name prnt = AbsEvalT $ do
  Frame bs N.:| _ <- unStack . evalStateStack <$> get
  case bs M.!? name of
    Nothing   -> unAbsEvalT $ mkFail' $ "unbound variable: " <> prnt
    Just val' -> pure val'

modifyVars
  :: (Monad u)
  => (  M.Map T.Text (Val (AbsEvalT u))
     -> AbsEvalT u (M.Map T.Text (Val (AbsEvalT u)))
     )
  -> AbsEvalT u ()
modifyVars f = AbsEvalT $ do
  Frame bs N.:| frs <- unStack . evalStateStack <$> get
  bs'               <- unAbsEvalT $ f bs
  modify $ \s -> s { evalStateStack = Stack $ Frame bs' N.:| frs }

deref' :: (Monad u) => Ref -> Deref (AbsEvalT u)
deref' ref = Deref
  { derefGet = AbsEvalT $ do
                 heap <- heapData . evalStateHeap <$> get
                 case heap M.!? ref of
                   Nothing ->
                     unAbsEvalT $ mkFail' $ "invalid address: " <> pprint ref
                   Just drf -> pure drf
  , derefSet = AbsEvalT . modify . derefSet'
  }
 where
  derefSet' newVal s = s
    { evalStateHeap = heap { heapData = M.insert ref newVal $ heapData heap }
    }
    where heap = evalStateHeap s

genAccess
  :: (EvalAnn r, Monad u)
  => Deref (AbsEvalT u)
  -> Identifier r
  -> AbsEvalT u (T.Text, [Val (AbsEvalT u)], Int)
genAccess drf targ = do
  dt <- derefGet drf
  case dt of
    RefDataObject tag props -> do
      order <- getObjectOrder tag Nothing
      case elemIndex (identifierText targ) order of
        Nothing ->
          mkFail'
            $  "property not in tag '"
            <> pprint tag
            <> "': "
            <> pprint targ
        Just tidx -> pure (tag, props, tidx)
    _ -> mkFail' "can't access, not an object"

genSubscript
  :: (Monad u)
  => Deref (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> AbsEvalT u ([Val (AbsEvalT u)], Int)
genSubscript drf idx' = do
  dt <- derefGet drf
  case dt of
    RefDataArray elms -> case idx' of
      ValPrimitive (LitDataInt n)
        | n < length elms
        -> pure (elms, n)
        | otherwise
        -> mkFail'
          $  "index out of bounds ("
          <> pprint (length elms)
          <> "): "
          <> pprint n
      _ -> mkFail' "index not an integer"
    _ -> mkFail' "can't subscript, not an array"

doUnOp :: (Monad u) => UnOpType -> Val (AbsEvalT u) -> Maybe (Val (AbsEvalT u))
doUnOp UnOpTypeNot = doBoolUnOp not
doUnOp UnOpTypeNeg = doNumUnOp negate

doBoolUnOp
  :: (Monad u) => (Bool -> Bool) -> Val (AbsEvalT u) -> Maybe (Val (AbsEvalT u))
doBoolUnOp f (ValPrimitive (LitDataBool x)) =
  Just $ ValPrimitive $ LitDataBool $ f x
doBoolUnOp _ _ = Nothing

doNumUnOp
  :: (Monad u)
  => (forall a . (Num a) => a -> a)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doNumUnOp f (ValPrimitive (LitDataInt x)) =
  Just $ ValPrimitive $ LitDataInt $ f x
doNumUnOp f (ValPrimitive (LitDataFloat x)) =
  Just $ ValPrimitive $ LitDataFloat $ f x
doNumUnOp _ _ = Nothing

doBinOp
  :: (Monad u)
  => BinOpType
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doBinOp BinOpTypeAnd   x y = doBoolBinOp (&&) x y
doBinOp BinOpTypeOr    x y = doBoolBinOp (||) x y
doBinOp BinOpTypePlus  x y = doNumBinOp (+) x y <|> doStringBinOp (<>) x y
doBinOp BinOpTypeMinus x y = doNumBinOp (-) x y
doBinOp BinOpTypeTimes x y = doNumBinOp (*) x y
doBinOp BinOpTypeDiv   x y = doIntBinOp quot x y <|> doFloatBinOp (/) x y
doBinOp BinOpTypeExp   x y = doIntBinOp (^) x y <|> doFloatBinOp (**) x y
doBinOp BinOpTypeLT    x y = doOrdBinOp (<) x y
doBinOp BinOpTypeLE    x y = doOrdBinOp (<=) x y
doBinOp BinOpTypeEQ    x y = doOrdBinOp (==) x y
doBinOp BinOpTypeNE    x y = doOrdBinOp (/=) x y
doBinOp BinOpTypeGE    x y = doOrdBinOp (>=) x y
doBinOp BinOpTypeGT    x y = doOrdBinOp (>) x y

doBoolBinOp
  :: (Monad u)
  => (Bool -> Bool -> Bool)
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doBoolBinOp f (ValPrimitive (LitDataBool x)) (ValPrimitive (LitDataBool y)) =
  Just $ ValPrimitive $ LitDataBool $ f x y
doBoolBinOp _ _ _ = Nothing

doIntBinOp
  :: (Monad u)
  => (Int -> Int -> Int)
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doIntBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataInt $ f x y
doIntBinOp _ _ _ = Nothing

-- | Will convert integers to floats.
doFloatBinOp
  :: (Monad u)
  => (Float -> Float -> Float)
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doFloatBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataFloat $ f (fromIntegral x) (fromIntegral y)
doFloatBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataFloat y))
  = Just $ ValPrimitive $ LitDataFloat $ f x y
doFloatBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataFloat y)) =
  Just $ ValPrimitive $ LitDataFloat $ f (fromIntegral x) y
doFloatBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataFloat $ f x (fromIntegral y)
doFloatBinOp _ _ _ = Nothing

doNumBinOp
  :: (Monad u)
  => (forall a . (Num a) => a -> a -> a)
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doNumBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataInt $ f x y
doNumBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataFloat y)) =
  Just $ ValPrimitive $ LitDataFloat $ f x y
doNumBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataFloat y)) =
  Just $ ValPrimitive $ LitDataFloat $ f (fromIntegral x) y
doNumBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataFloat $ f x (fromIntegral y)
doNumBinOp _ _ _ = Nothing

doOrdBinOp
  :: (Monad u)
  => (forall a . (Ord a) => a -> a -> Bool)
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doOrdBinOp f (ValPrimitive (LitDataBool x)) (ValPrimitive (LitDataBool y)) =
  Just $ ValPrimitive $ LitDataBool $ f x y
doOrdBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataBool $ f x y
doOrdBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataFloat y)) =
  Just $ ValPrimitive $ LitDataBool $ f x y
doOrdBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataFloat y)) =
  Just $ ValPrimitive $ LitDataBool $ f (fromIntegral x) y
doOrdBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataBool $ f x (fromIntegral y)
doOrdBinOp _ _ _ = Nothing

doStringBinOp
  :: (Monad u)
  => (T.Text -> T.Text -> T.Text)
  -> Val (AbsEvalT u)
  -> Val (AbsEvalT u)
  -> Maybe (Val (AbsEvalT u))
doStringBinOp f (ValPrimitive (LitDataString x)) (ValPrimitive (LitDataString y))
  = Just $ ValPrimitive $ LitDataString $ f x y
doStringBinOp f (ValPrimitive (LitDataString x)) (ValPrimitive (LitDataInt y))
  = Just $ ValPrimitive $ LitDataString $ f x (pprint y)
doStringBinOp f (ValPrimitive (LitDataString x)) (ValPrimitive (LitDataFloat y))
  = Just $ ValPrimitive $ LitDataString $ f x (pprint y)
doStringBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataString y))
  = Just $ ValPrimitive $ LitDataString $ f (pprint x) y
doStringBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataString $ f (pprint x) (pprint y)
doStringBinOp f (ValPrimitive (LitDataInt x)) (ValPrimitive (LitDataFloat y)) =
  Just $ ValPrimitive $ LitDataString $ f (pprint x) (pprint y)
doStringBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataString y))
  = Just $ ValPrimitive $ LitDataString $ f (pprint x) y
doStringBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataFloat y))
  = Just $ ValPrimitive $ LitDataString $ f (pprint x) (pprint y)
doStringBinOp f (ValPrimitive (LitDataFloat x)) (ValPrimitive (LitDataInt y)) =
  Just $ ValPrimitive $ LitDataString $ f (pprint x) (pprint y)
doStringBinOp _ _ _ = Nothing

tryMatch1
  :: (EvalAnn r, Monad u) => Pattern r -> Val (AbsEvalT u) -> AbsEvalT u Bool
tryMatch1 (PatternE pexpr) val' = do
  pexpr' <- runPExpr pexpr
  pexpr' === val'
tryMatch1 (PatternTag _                    ) (ValPrimitive _  ) = pure False
tryMatch1 (PatternTag (TagIdentifier _ tag)) (ValRef       ref) = do
  drf <- derefGet $ deref' ref
  case drf of
    RefDataObject atag _ -> pure $ tag == atag
    _                    -> pure False
tryMatch1 (PatternBlank (Blank _)) _ = pure True

mkFail' :: (Monad u) => T.Text -> AbsEvalT u a
mkFail' msg = AbsEvalT $ do
  lastLoc <- evalStateLastLoc <$> get
  lift $ mkFail Error { errorRange = srcInfoRange <$> lastLoc, errorMsg = msg }

tryBuiltinToPrimData :: BuiltinVal (AbsEvalT u) -> Maybe LitData
tryBuiltinToPrimData (BuiltinValPrimitive prim) = Just prim
tryBuiltinToPrimData _                          = Nothing

tryBuiltinToRefData
  :: (Monad u) => BuiltinVal (AbsEvalT u) -> Maybe (RefData (AbsEvalT u))
tryBuiltinToRefData (BuiltinValClosure frmls f) = Just $ RefDataClosure
  ClosureVal
    { closureValBody     = do
                             args' <- mapM (\frml -> getVar' frml $ "#" <> frml)
                                           frmls
                             f args'
    , closureValFormals  = frmls
    , closureValFreeVars = S.empty
    }
tryBuiltinToRefData _ = Nothing

initState
  :: (Monad u)
  => M.Map T.Text (BuiltinVal (AbsEvalT u))
  -> EvalState (AbsEvalT u)
initState blts = EvalState
  { evalStateLastLoc   = Nothing
  , evalStateStack     = Stack $ Frame bltVals N.:| []
  , evalStateHeap      = Heap
                           { heapData     = M.fromDistinctAscList $ map
                                              (\(ref, (_, rd)) -> (ref, rd))
                                              refBlts
                           , heapNextFree = Ref $ length refBlts
                           }
  , evalStateObjOrders = M.empty
  }
 where
  bltVals = M.map ValPrimitive primBlts <> M.fromDistinctAscList
    (map (\(ref, (name, _)) -> (name, ValRef ref)) refBlts)
  primBlts = M.mapMaybe tryBuiltinToPrimData blts
  refBlts =
    zip (map Ref [0 ..]) $ M.toAscList $ M.mapMaybe tryBuiltinToRefData blts

-- | Evaluate a program with custom builtins. If you don't provide the default builtins they won't be usable.
evalProgramWith
  :: (EvalAnn r, Monad u)
  => M.Map T.Text (BuiltinVal (AbsEvalT u))
  -> Program r
  -> EResultT u ()
evalProgramWith blts =
  (() <$)
    . (`evalStateT` initState blts)
    . runContStackT (\() -> pure $ error "dummy")
    . unAbsEvalT
    . runProgram
