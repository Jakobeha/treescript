{-# LANGUAGE OverloadedStrings #-}

-- | Ensures an AST is well-formed, catching errors before runtime.
module TreeScript.Ast.Core.Analyze.Validate
  ( validate_
  , validate
  )
where

import           TreeScript.Ast.Core.Analyze.Misc
import           TreeScript.Ast.Core.Analyze.Type
import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Char
import           Data.List               hiding ( group )
import qualified Data.Map.Strict               as M
import           Data.Maybe
import qualified Data.Set                      as S
import qualified Data.Text                     as T

type UnboundValidate = StateT LocalEnv GlobalSessionRes

vDuplicateDecls :: [RecordDecl Range] -> GlobalSessionRes ()
vDuplicateDecls decls = do
  imported <-
    M.keysSet . declSetRecords . globalEnvImportedLocals <$> getGlobalEnv
  tellErrors $ catMaybes $ snd $ mapAccumL duplicateDeclErr imported decls
 where
  duplicateDeclErr prevs (RecordDecl rng head' _)
    | S.member head' prevs
    = ( nexts
      , Just $ validateError rng $ "duplicate record declaration: " <> head'
      )
    | otherwise
    = (nexts, Nothing)
    where nexts = S.insert head' prevs

vDisjointProps :: Record Range -> GlobalSessionRes ()
vDisjointProps (Record rng head' props) = do
  genv <- getGlobalEnv
  let ptyps = globalEnvLookup SymbolTypeRecord head' genv
      casts = globalEnvAllCasts genv
  case ptyps of
    -- Lookup failed - raised error during parsing.
    Nothing                      -> pure ()
    -- Properties are @any@, so never disjoint.
    Just PropsTypeVarLen         -> pure ()
    Just (PropsTypeFixed ptyps') -> do
      -- Validate num props.
      let nptyps = length ptyps'
          nprops = length props
      when (nptyps /= nprops)
        $  tellError
        $  typeError rng
        $  "wrong number of props for "
        <> pprint head'
        <> ": expected "
        <> pprint nptyps
        <> ", got "
        <> pprint nprops
      -- Validate prop types
      forM_ (zip ptyps' props) $ \(eptyp, prop) -> do
        let aptyp  = s2xType $ surfaceType prop
            pcasts = S.fromList $ allPossibleCasts aptyp eptyp
        -- | SOON make casts contain ones from modules (add to exports)
        when (typesDisjoint eptyp aptyp && S.disjoint pcasts casts)
          $  tellError
          $  typeError rng
          $  "types are disjoint and there aren't any casts: expected "
          <> pprint eptyp
          <> ", actual "
          <> pprint aptyp

unboundErrsValue :: S.Set Int -> Value Range -> [Error]
unboundErrsValue _ (ValuePrimitive _) = []
unboundErrsValue binds (ValueRecord record) =
  foldMap (unboundErrsValue binds) $ recordProps record
unboundErrsValue binds (ValueBind (Bind rng idx))
  | idx == 0           = [validateError rng "unlabeled bind in output"]
  | S.member idx binds = []
  | otherwise          = [validateError rng "unmatched bind in output"]

unboundErrsGroupLoc :: S.Set Int -> GroupLoc Range -> [Error]
unboundErrsGroupLoc groups (GroupLocLocal locRng idx)
  | not (S.member idx groups)
  = [validateError locRng $ "undeclared local group"]
unboundErrsGroupLoc _ _ = []

unboundErrsGroupRef :: LocalEnv -> GroupRef Range -> [Error]
unboundErrsGroupRef env@(LocalEnv binds groups) (GroupRef _ loc vprops gprops)
  = unboundErrsGroupLoc groups loc
    ++ foldMap (unboundErrsValue binds)  vprops
    ++ foldMap (unboundErrsGroupRef env) gprops

unboundErrsGuard :: LocalEnv -> Next Range -> [Error]
unboundErrsGuard _   (NextCast  _  ) = []
unboundErrsGuard env (NextGroup grp) = unboundErrsGroupRef env grp

addGuardBindsToEnv :: Guard Range -> UnboundValidate ()
addGuardBindsToEnv = modify . localEnvInsertBinds . bindsInValue . guardInput

vUnboundGuard :: Guard Range -> UnboundValidate ()
vUnboundGuard (Guard _ _ output nexts) = do
  env@(LocalEnv binds _) <- get
  tellErrors
    $  unboundErrsValue binds output
    ++ foldMap (unboundErrsGuard env) nexts

vUnboundReducer :: LocalEnv -> Reducer Range -> GlobalSessionRes ()
vUnboundReducer env (Reducer _ main guards) = (`evalStateT` env) $ do
  addGuardBindsToEnv main
  forM_ (reverse guards) $ \guard' -> do
    vUnboundGuard guard'
    addGuardBindsToEnv guard'
  vUnboundGuard main

vUnbound :: GroupDef Range -> GlobalSessionRes ()
vUnbound (GroupDef _ vprops gprops reds) = mapM_ (vUnboundReducer env) reds
 where
  env = LocalEnv { localEnvBinds  = S.fromList $ map groupDefPropIdx vprops
                 , localEnvGroups = S.fromList $ map groupDefPropIdx gprops
                 }

vHiddenModulePath :: ModulePath -> GlobalSessionRes ()
vHiddenModulePath mdl
  | mdl == "" || not (isUpper $ T.head mdl) || T.any isHiddenChar mdl
  = tellError
    $ validateError_
    $ "module path can't be referenced - must be CamelCase and all letters and numbers: "
    <> mdl
  | otherwise
  = pure ()
  where isHiddenChar x = not (isAlphaNum x) && x /= '_'

-- TODO: Undeclared function errors

-- TODO: Validate imports

-- | Reports syntax errors which didn't affect parsing but would cause problems during compilation.
validate_ :: Program Range -> GlobalSessionRes ()
validate_ prog@(Program _ mpath _ rdecls _ _ _ castReds groups _) = do
  vHiddenModulePath mpath
  vDuplicateDecls rdecls
  traverseAst_ TProgram TRecord vDisjointProps prog
  mapM_ (vUnboundReducer emptyLocalEnv) castReds
  mapM_ vUnbound                        groups

-- | Wrapper for 'validate_' which returns the input.
validate :: Program Range -> GlobalSessionRes (Program Range)
validate x = x <$ validate_ x
