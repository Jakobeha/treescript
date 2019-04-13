{-# LANGUAGE OverloadedStrings #-}

-- | Ensures an AST is well-formed, catching errors before runtime.
module TreeScript.Ast.Core.Validate
  ( validate
  ) where

import TreeScript.Ast.Core.Analyze
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

invalidRecordErrs :: M.Map T.Text Int -> [Reducer Range] -> [Error]
invalidRecordErrs decls reds
  = concatMap (foldValuesInReducer invalidRecordErrorsValue1) reds
  where invalidRecordErrorsValue1 (ValuePrimitive _) = []
        invalidRecordErrorsValue1 (ValueRecord (Record rng head' props))
          = case decls M.!? head' of
              Nothing -> [desugarError rng $ "undeclared record: " <> head']
              Just numDeclProps
                 | numDeclProps /= numProps && numDeclProps /= varNumProps
                -> [ desugarError rng
                 $ "record has wrong number of properties: expected "
                <> pprint numDeclProps
                <> ", got "
                <> pprint numProps ]
                 | otherwise -> []
                where numProps = length props
        invalidRecordErrorsValue1 (ValueBind _) = []

invalidFunctionErrs :: S.Set T.Text -> [Reducer Range] -> [Error]
invalidFunctionErrs decls reds
  = concatMap (foldGroupsInReducer invalidFunctionErrorsGroup1) reds
  where invalidFunctionErrorsGroup1 (GroupRef _ (GroupLocGlobal _ _) _ _) = []
        invalidFunctionErrorsGroup1 (GroupRef _ (GroupLocLocal _ _) _ _) = []
        invalidFunctionErrorsGroup1 (GroupRef _ (GroupLocFunction nameRng name) vprops gprops)
          | not (null vprops) || not (null gprops)
          = error "function can't have properties (shouldn't be allowed by syntax)"
          | not (S.member name decls) = [desugarError nameRng $ "undeclared function: " <> name]
          | otherwise = []

unboundErrsValue :: S.Set Int -> Value Range -> [Error]
unboundErrsValue _ (ValuePrimitive _) = []
unboundErrsValue binds (ValueRecord record)
  = foldMap (unboundErrsValue binds) $ recordProps record
unboundErrsValue binds (ValueBind (Bind rng idx))
  | idx == 0 = [desugarError rng "unlabeled bind in output"]
  | S.member idx binds = []
  | otherwise = [desugarError rng "unmatched bind in output"]

unboundErrsGroupRef :: LocalEnv -> GroupRef Range -> [Error]
unboundErrsGroupRef env@(LocalEnv binds groups) (GroupRef _ (GroupLocGlobal locRng idx) vprops gprops)
   = selfErrs
  ++ foldMap (unboundErrsValue binds) vprops
  ++ foldMap (unboundErrsGroupRef env) gprops
  where selfErrs
          | S.member idx groups = [desugarError locRng $ "undeclared local group"]
          | otherwise = []
unboundErrsGroupRef _ (GroupRef _ (GroupLocLocal _ _) _ _) = []
unboundErrsGroupRef _ (GroupRef _ (GroupLocFunction _ _) _ _) = []

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

unboundErrs :: GroupDef Range -> [Error]
unboundErrs (GroupDef _ vprops gprops reds)
  = concatMap (unboundErrsReducer env) reds
  where env = LocalEnv
          { localEnvBinds = convertProps vprops
          , localEnvGroups = convertProps gprops
          }
        convertProps = S.fromList . map bindIdx

-- TODO: Undeclared function errors

validationErrs :: SessionEnv -> Program Range -> [Error]
validationErrs env (Program _ decls groups)
   = duplicateDeclErrs (S.map declCompactHead $ declSetRecords importedDecls) decls
  ++ invalidRecordErrs (declSetToMap $ declSetRecords allDecls) allReds
  ++ invalidFunctionErrs (S.map declCompactHead $ declSetFunctions allDecls) allReds
  ++ concatMap unboundErrs groups
  where importedDecls = allImportedDecls env
        localDecls
          = DeclSet
          { declSetRecords = S.fromList $ map compactRecordDecl decls
          , declSetFunctions = S.empty
          }
        allDecls = localDecls <> importedDecls
        allReds = concatMap groupDefReducers groups

-- | Adds syntax errors which didn't affect parsing but would cause problems during compilation.
validate :: SessionRes (Program Range) -> SessionRes (Program Range)
validate res = do
  env <- getSessionEnv
  x <- res
  tellErrors $ sort $ validationErrs env x
  pure x
