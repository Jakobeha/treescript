{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- | Direct AST evaluation.
module TreeScript.Interpret.Eval
  ()
where

import           TreeScript.Ast
import           TreeScript.Interpret.Abstract
import           TreeScript.Print
import           TreeScript.Misc

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T

data RefType
  = RefTypeArray
  | RefTypeObject
  | RefTypeClosure
  deriving (Eq, Ord, Read, Show)

type EPos = Int

newtype Frame = Frame
  { frameVars :: M.Map T.Text (Val Eval)
  }

data EvalState
  = EvalState
  { evalStateLastLoc :: SrcInfo
  , evalStateStack :: Stack Eval
  , evalStateHeap :: Heap Eval
  , evalStateObjOrders :: M.Map T.Text [T.Text]
  }

newtype Eval a = Eval{ unEval :: EResultT (State EvalState) a } deriving (Functor, Applicative, Monad)

-- | Evaluation monad.
instance Interpret Eval where
  type HasAnns Eval = AnnHas SrcInfo
  data Stack Eval = Stack{ unStack :: [Frame] }
  data Heap Eval = Heap{ unHeap :: M.Map (Ref Eval) (RefData Eval) }
  data Val Eval
    = ValPrimitive LitData
    | ValRef RefType (Ref Eval)
    deriving (Eq, Ord, Read, Show)
  data Ref Eval = Ref{ unRef :: Int } deriving (Eq, Ord, Read, Show)
  data ClosVal Eval = ClosVal{ unClosVal :: Eval (Val Eval) }

  runAnn x = Eval $ modify $ \s -> s { evalStateLastLoc = getSrcLoc x }
  defineVar (Identifier _ name) vr = modifyVars $ \bs -> if
    | M.member name bs -> mkFail $ "variable already defined: " <> pprint
    | otherwise        -> pure $ (name, vr) : bs
  setVar lhs@(Identifier _ name) vr = modifyVars $ \bs -> if
    | M.member name bs -> M.insert name vr bs
    | otherwise        -> mkFail $ "unbound variable: " <> pprint lhs
  getVar lhs@(Identifier _ name) vr = do
    Frame bs : _ <- evalStateStack <$> get
    case name M.!? bs of
      Nothing -> mkFail $ "unbound variable: " <> pprint lhs
      Just vr -> pure vr
  getObjectOrder (TagIdentifier _ txt) orderHere = do
    objOrders <- evalStateObjOrders <$> get
    case txt M.!? objOrders of
      Nothing -> do
        modify
          $ \s -> s { evalStateObjOrders = M.insert txt orderHere objOrders }
        pure orderHere
      Just estOrder -> pure estOrder
  reorderProps order props
    | orderLen /= propsLen
    = mkFail'
      $  "object with same tag previously defined with "
      <> pprint orderLen
      <> " props, but this object has "
      <> pprint propsLen
    | otherwise
    = mapM lookupProp order
   where
    lookupProp expProp = case lookup expProp propsTup of
      Nothing ->
        mkFail'
          $ "object is missing property: '"
          <> expProp
          <> "' (an object with the same tag was previously defined with the property)"
      Just val -> pure val
    propsTup =
      map (\(ObjectProp _ (Identifier _ name) rhs) -> (name, rhs)) props
    orderLen = length order
    propsLen = length props
  tryMatchCases []                    _  _ = pure ()
  tryMatchCases (Case _ cond body : cs) vr f = do
    res <- tryMatch1 cond vr
    when res $ f body
    tryMatchCases cs vr f
  mkFail' = mkFail

modifyVars f = do
  Frame bs : frs <- evalStateStack <$> get
  bs'            <- f bs
  modify $ \s -> s { evalStateStack = Frame bs' : frs }
