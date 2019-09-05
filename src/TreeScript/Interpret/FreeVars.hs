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
newtype FreeVars r a = FreeVars{ unFreeVars :: StateT (S.Set T.Text) (Writer (Dual (M.Map T.Text (Identifier r)))) a } deriving (Functor, Applicative, Monad)

instance MonadVCont (FreeVars r) where
  type ContRes (FreeVars r) = Val (FreeVars r)
  type VirtVoid (FreeVars r) = ()
  mkCont f = (Val () <$) $ f $ \_ -> pure ()
  mkLoop action = action

instance MonadContStack (FreeVars r) where
  bumpCont _ action = action
  topCont = pure $ Just $ \_ -> pure ()

instance Interpret (FreeVars r) where
  type HasAnns (FreeVars r) = ((~) r)
  data Stack (FreeVars r) = Stack ()
  data Heap (FreeVars r) = Heap ()
  data Val (FreeVars r) = Val ()
  data Deref (FreeVars r) = Deref ()
  data ClosureVal (FreeVars r) = ClosureVal ()

  runAnn _ = pure ()
  mkLiteral _ = pure $ Val ()
  mkReference _ = pure $ Val ()
  mkClos' body _ _ = do
    Val () <- body
    pure $ ClosureVal ()
  preDefineFun (Identifier _ _) = pure ()
  defineFun x (ClosureVal ()) = foundVarDefine x
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
  freeVariables _ _ = pure []
  raiseErr _ = pure ()
  vabsurdVal () = pure $ Val ()

foundVarDefine :: Identifier r -> FreeVars r ()
foundVarDefine (Identifier _ name) = FreeVars $ modify $ S.insert name

foundVarUse :: Identifier r -> FreeVars r ()
foundVarUse use@(Identifier _ name) = FreeVars $ do
  shadowing <- get
  unless (S.member name shadowing) $ tell $ Dual $ M.singleton name use

-- | Find the free variables in a closure.
runFreeVars :: S.Set T.Text -> Block r -> [Identifier r]
runFreeVars args =
  M.elems . getDual . execWriter . (`evalStateT` args) . unFreeVars . runBlock
