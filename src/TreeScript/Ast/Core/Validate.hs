{-# LANGUAGE OverloadedStrings #-}

-- | Ensures an AST is well-formed, catching errors before runtime.
module TreeScript.Ast.Core.Validate
  ( validate'
  , validate
  ) where

import TreeScript.Ast.Core.Analyze
import TreeScript.Ast.Core.Types
import TreeScript.Misc

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.Char
import Data.Functor.Identity
import Data.List hiding (group)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T

type UnboundErrs = WriterT [Error] (StateT LocalEnv Identity) ()

duplicateDeclErrs :: S.Set T.Text -> [RecordDecl] -> [Error]
duplicateDeclErrs imported
  = catMaybes . snd . mapAccumL duplicateDeclErr imported
  where duplicateDeclErr prevs (RecordDecl rng head' _)
          | S.member head' prevs = (nexts, Just $ desugarError rng $ "duplicate record declaration: " <> head')
          | otherwise = (nexts, Nothing)
          where nexts = S.insert head' prevs

unboundErrsValue :: S.Set Int -> Value () -> [Error]
unboundErrsValue _ (ValuePrimitive () _) = []
unboundErrsValue binds (ValueRecord () record)
  = foldMap (unboundErrsValue binds) $ recordProps record
unboundErrsValue binds (ValueBind () (Bind rng idx))
  | idx == 0 = [desugarError rng "unlabeled bind in output"]
  | S.member idx binds = []
  | otherwise = [desugarError rng "unmatched bind in output"]

unboundErrsGroupLoc :: S.Set Int -> GroupLoc () -> [Error]
unboundErrsGroupLoc groups (GroupLocLocal locRng idx)
  | not (S.member idx groups) = [desugarError locRng $ "undeclared local group"]
unboundErrsGroupLoc _ _ = []

unboundErrsGroupRef :: LocalEnv -> GroupRef () -> [Error]
unboundErrsGroupRef env@(LocalEnv binds groups) (GroupRef _ loc vprops gprops)
   = unboundErrsGroupLoc groups loc
  ++ foldMap (unboundErrsValue binds) vprops
  ++ foldMap (unboundErrsGroupRef env) gprops

unboundErrsNext :: LocalEnv -> Next () -> [Error]
unboundErrsNext _ (NextCast _) = []
unboundErrsNext env (NextGroup grp) = unboundErrsGroupRef env grp

addGuardBindsToEnv :: Guard () -> UnboundErrs
addGuardBindsToEnv = modify . localEnvInsertBinds . bindsInValue . guardInput

unboundErrsGuard :: Guard () -> UnboundErrs
unboundErrsGuard (Guard _ _ output nexts) = do
  env@(LocalEnv binds _) <- get
  tell $ unboundErrsValue binds output ++ foldMap (unboundErrsNext env) nexts

unboundErrsReducer :: LocalEnv -> Reducer () -> [Error]
unboundErrsReducer env (Reducer _ main guards) = (`evalState` env) $ execWriterT $ do
  addGuardBindsToEnv main
  forM_ (reverse guards) $ \guard' -> do
    unboundErrsGuard guard'
    addGuardBindsToEnv guard'
  unboundErrsGuard main

unboundErrs :: GroupDef () -> [Error]
unboundErrs (GroupDef _ vprops gprops reds)
  = concatMap (unboundErrsReducer env) reds
  where env = LocalEnv
          { localEnvBinds = S.fromList $ map groupDefPropIdx vprops
          , localEnvGroups = S.fromList $ map groupDefPropIdx gprops
          }

hiddenModulePathErrs :: ModulePath -> [Error]
hiddenModulePathErrs mdl
  | mdl == "" || not (isUpper $ T.head mdl) || T.any isHiddenChar mdl
  = [desugarError_ $ "module path can't be referenced - must be CamelCase and all letters and numbers: " <> mdl]
  | otherwise = []
  where isHiddenChar x = not (isAlphaNum x) && x /= '_'

-- TODO: Undeclared function errors

-- TODO: Validate imports

-- | Reports syntax errors which didn't affect parsing but would cause problems during compilation.
validate' :: GlobalEnv -> Program () -> [Error]
validate' imps (Program _ mpath _ rdecls _ _ _ castReds groups _)
   = sort
   $ hiddenModulePathErrs mpath
  ++ duplicateDeclErrs (M.keysSet importedRecordDecls) rdecls
  ++ concatMap (unboundErrsReducer emptyLocalEnv) castReds
  ++ concatMap unboundErrs groups
  where importedRecordDecls = declSetRecords $ globalEnvImportedLocals imps

-- | Wrapper for 'validate''
validate :: Program () -> GlobalSessionRes (Program ())
validate x = do
  genv <- getGlobalEnv
  tellErrors $ validate' genv x
  pure x