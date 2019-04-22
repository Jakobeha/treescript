{-# LANGUAGE OverloadedStrings #-}

-- | Ensures an AST is well-formed, catching errors before runtime.
module TreeScript.Ast.Core.Validate
  ( validate
  ) where

import TreeScript.Ast.Core.Analyze
import TreeScript.Ast.Core.Env
import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.List hiding (group)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T

type UnboundErrs = WriterT [Error] (StateT LocalEnv Identity) ()

duplicateDeclErrs :: S.Set T.Text -> [RecordDecl Range] -> [Error]
duplicateDeclErrs imported
  = catMaybes . snd . mapAccumL duplicateDeclErr imported
  where duplicateDeclErr prevs (RecordDecl rng head' _)
          | S.member head' prevs = (nexts, Just $ desugarError rng $ "duplicate record declaration: " <> head')
          | otherwise = (nexts, Nothing)
          where nexts = S.insert head' prevs

unboundErrsValue :: S.Set Int -> Value Range -> [Error]
unboundErrsValue _ (ValuePrimitive _) = []
unboundErrsValue binds (ValueRecord record)
  = foldMap (unboundErrsValue binds) $ recordProps record
unboundErrsValue binds (ValueBind (Bind rng idx))
  | idx == 0 = [desugarError rng "unlabeled bind in output"]
  | S.member idx binds = []
  | otherwise = [desugarError rng "unmatched bind in output"]

unboundErrsGroupLoc :: S.Set Int -> GroupLoc Range -> [Error]
unboundErrsGroupLoc groups (GroupLocLocal locRng idx)
  | not (S.member idx groups) = [desugarError locRng $ "undeclared local group"]
unboundErrsGroupLoc _ _ = []

unboundErrsGroupRef :: LocalEnv -> GroupRef Range -> [Error]
unboundErrsGroupRef env@(LocalEnv binds groups) (GroupRef _ loc vprops gprops)
   = unboundErrsGroupLoc groups loc
  ++ foldMap (unboundErrsValue binds) vprops
  ++ foldMap (unboundErrsGroupRef env) gprops

addGuardBindsToEnv :: Guard Range -> UnboundErrs
addGuardBindsToEnv = modify . localEnvInsertBinds . bindsInValue . guardInput

unboundErrsGuard :: Guard Range -> UnboundErrs
unboundErrsGuard (Guard _ _ output nexts) = do
  env@(LocalEnv binds _) <- get
  tell $ unboundErrsValue binds output ++ foldMap (unboundErrsGroupRef env) nexts

unboundErrsReducer :: LocalEnv -> Reducer Range -> [Error]
unboundErrsReducer env (Reducer _ main guards) = (`evalState` env) $ execWriterT $ do
  addGuardBindsToEnv main
  forM_ (reverse guards) $ \guard' -> do
    unboundErrsGuard guard'
    addGuardBindsToEnv guard'
  unboundErrsGuard main

unboundErrs :: GroupDef e1 e2 Range -> [Error]
unboundErrs (GroupDef _ vprops gprops reds _)
  = concatMap (unboundErrsReducer env) reds
  where env = LocalEnv
          { localEnvBinds = convertProps $ map snd vprops
          , localEnvGroups = convertProps $ map snd gprops
          }
        convertProps = S.fromList . map bindIdx

-- SOON: Error on module path with bad characters

-- TODO: Undeclared function errors

-- TODO: Validate imports

validationErrs :: ImportEnv -> Program e1 e2 Range -> [Error]
validationErrs imps (Program _ mpath _ decls _ groups _)
   = duplicateDeclErrs (M.keysSet importedRecordDecls) decls
  ++ concatMap unboundErrs groups
  where importedRecordDecls = declSetRecords $ importEnvImportedLocals imps

-- | Adds syntax errors which didn't affect parsing but would cause problems during compilation.
validate :: SessionRes ((Program e1 e2 Range, ImportEnv), Program () () ()) -> SessionRes (Program e1 e2 Range, Program () () ())
validate res = do
  ((x, imps), imods) <- res
  tellErrors $ sort $ validationErrs imps x
  pure (x, imods)
