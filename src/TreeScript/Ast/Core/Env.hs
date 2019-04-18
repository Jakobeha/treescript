{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Environments and monads used to parse and validate the @Core@ AST.
module TreeScript.Ast.Core.Env
  ( module TreeScript.Ast.Core.Env
  ) where

import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Sugar as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.FilePath
import System.Directory

data ImportEnv
  = ImportEnv
  { importEnvLocalDecls :: (ModulePath, DeclSet)
  , importEnvImportedDecls :: M.Map T.Text [(ModulePath, DeclSet)] -- ^ Includes module decl.
  }

-- | What reducers get from their parent group, or output values / groups from their reducer.
data LocalEnv
  = LocalEnv
  { localEnvBinds :: S.Set Int
  , localEnvGroups :: S.Set Int
  } deriving (Eq, Ord, Read, Show)

data BindEnv
  = BindEnv
  { bindEnvBinds :: M.Map T.Text Int
  , bindEnvNextFree :: Int
  } deriving (Eq, Ord, Read, Show)

data GVEnv a
  = GVEnv
  { gvEnvValue :: a
  , gvEnvGroup :: a
  } deriving (Eq, Ord, Read, Show, Functor)

type GVBindEnv = GVEnv BindEnv

type ImportSessionRes a = ReaderT ImportEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (ReaderT ImportEnv (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

type GVBindSessionRes a = StateT (GVEnv BindEnv) (ReaderT ImportEnv (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

mkImportEnv :: ModulePath -> DeclSet -> [ImportDecl Range] -> ImportEnv
mkImportEnv mpath mexps idecls
  = ImportEnv
  { importEnvLocalDecls = (mpath, mexps)
  , importEnvImportedDecls = M.fromListWith (<>) $ map convertDecl idecls
  }
  where convertDecl (ImportDecl _ path qual (Module exps _)) = (qual, [(path, exps)])

importEnvAllDecls :: ImportEnv -> M.Map T.Text [(ModulePath, DeclSet)]
importEnvAllDecls (ImportEnv locs imps) = M.insertWith (<>) "" [locs] imps

importEnvImportedLocals :: ImportEnv -> DeclSet
importEnvImportedLocals = foldMap snd . fold . (M.!? "") . importEnvImportedDecls

localEnvInsertBinds :: S.Set Int -> LocalEnv -> LocalEnv
localEnvInsertBinds binds env
  = LocalEnv
  { localEnvBinds = binds <> localEnvBinds env
  , localEnvGroups = localEnvGroups env
  }

emptyBindEnv :: BindEnv
emptyBindEnv
  = BindEnv
  { bindEnvBinds = M.empty
  , bindEnvNextFree = 1
  }

emptyGVBindEnv :: GVBindEnv
emptyGVBindEnv
  = GVEnv
  { gvEnvValue = emptyBindEnv
  , gvEnvGroup = emptyBindEnv
  }

bindEnvLookup :: T.Text -> BindEnv -> (Int, BindEnv)
bindEnvLookup bind env@(BindEnv binds nextFree)
  = case binds M.!? bind of
      Nothing ->
        ( nextFree,
          BindEnv
          { bindEnvBinds = M.insert bind nextFree binds
          , bindEnvNextFree = nextFree + 1
          }
        )
      Just idx -> (idx, env)

-- | Makes the path absolute (relative to source path).
resolvePath :: T.Text -> S.ModulePath Range -> SessionRes (S.ModulePath Range)
resolvePath myPath (S.ModulePath rng path) = do
  let liftIO'
        = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't resolve relative path " <> path))
        . liftIOAndCatch StageDesugar
      dirPath = takeDirectory $ T.unpack myPath
      path' = T.unpack path
  liftIO' $ S.ModulePath rng . T.pack <$> canonicalizePath (dirPath </> path')
