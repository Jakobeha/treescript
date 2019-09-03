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

-- | An abstract interpretation to get free variables.
module TreeScript.Interpret.FreeVars
  ( FreeVars
  , runFreeVars
  )
where

import           TreeScript.Ast
import           TreeScript.Interpret.Abstract
import           TreeScript.Misc

import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

-- | Abstract evaluation monad transformer. Side effects must be implemented in underlying monads.
newtype FreeVars a = FreeVars{ unFreeVars :: StateT (S.Set T.Text) (Writer (S.Set T.Text)) a } deriving (Functor, Applicative, Monad)

instance MonadVCont FreeVars where
  type ContRes FreeVars = Val FreeVars
  type VirtVoid FreeVars = ()
  mkCont f = (Val () <$) $ f $ \_ -> pure ()
  mkLoop action = action

instance MonadContStack FreeVars where
  bumpCont _ action = action
  topCont = pure $ Just $ \_ -> pure ()

instance Interpret FreeVars where
  type HasAnns FreeVars = AnyAnn
  data Stack FreeVars = Stack ()
  data Heap FreeVars = Heap ()
  data Val FreeVars = Val ()
  data Deref FreeVars = Deref ()
  data ClosureVal FreeVars = ClosureVal ()

  runAnn _ = pure ()
  mkLiteral _ = pure $ Val ()
  mkReference _ = pure $ Val ()
  mkClos body _ _ = do
    Val () <- body
    pure $ ClosureVal ()
  defineVar x (Val ()) = foundVarDefine x
  setVar x (Val ()) = foundVarUse x
  getVar = (Val () <$) . foundVarUse
  deref (Val ()) = pure $ Deref ()
  getProp (Deref ()) _ = pure $ Val ()
  getSubscript (Deref ()) (Val ()) = pure $ Val ()
  setProp (Deref ()) _ (Val ()) = pure ()
  setSubscript (Deref ()) (Val ()) (Val ()) = pure ()
  getClos (Deref ()) = pure $ ClosureVal ()
  runCall (ClosureVal ()) _ = pure $ Val ()
  runUnOp (UnOperator _ _) (Val ()) = pure $ Val ()
  runBinOp (Val ()) (BinOperator _ _) (Val ()) = pure $ Val ()
  getObjectOrders = pure $ M.empty
  addDefaultObjectOrder _ _ = pure ()
  tryMatchCases []                   _    _ = pure ()
  tryMatchCases (Case _ _ body : cs) val' f = do
    f body
    tryMatchCases cs val' f
  freeVariables _ _ = pure S.empty
  raiseErr _ = pure ()
  vabsurdVal () = pure $ Val ()

foundVarDefine :: Identifier r -> FreeVars ()
foundVarDefine (Identifier _ name) = FreeVars $ modify $ S.insert name

foundVarUse :: Identifier r -> FreeVars ()
foundVarUse (Identifier _ name) = FreeVars $ do
  shadowing <- get
  unless (S.member name shadowing) $ tell $ S.singleton name

-- | Find the free variables in a closure.
runFreeVars :: S.Set T.Text -> Block r -> S.Set T.Text
runFreeVars args = execWriter . (`evalStateT` args) . unFreeVars . runBlock
