{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Environments and monads used to parse and validate the @Core@ AST.
module TreeScript.Ast.Core.Env
  ( module TreeScript.Ast.Core.Env
  ) where

import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Sugar as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Catch
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
  -- | Includes builtins.
  , importEnvImportedDecls :: M.Map T.Text [(ModulePath, DeclSet)]
  , importEnvImportDecls :: [ImportDecl Range]
  }

newtype ImportT m a = ImportT (StateT ImportEnv m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadResult)

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

type ImportSessionRes a = ImportT (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (ImportT (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

type GVBindSessionRes a = StateT (GVEnv BindEnv) (ImportT (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

class (Monad m) => MonadImport m where
  getImportEnv :: m ImportEnv
  importTInsertDecl :: ImportDecl Range -> m ()

instance (Monad m) => MonadImport (ImportT m) where
  getImportEnv = ImportT get
  importTInsertDecl = ImportT . modify . importEnvInsertDecl

instance (MonadReader r m) => MonadReader r (ImportT m) where
  ask = ImportT ask
  reader = ImportT . reader
  local f (ImportT x) = ImportT $ local f x

instance (MonadImport m) => MonadImport (StateT s m) where
  getImportEnv = lift getImportEnv
  importTInsertDecl = lift . importTInsertDecl

importEnvInsertDecl :: ImportDecl Range -> ImportEnv -> ImportEnv
importEnvInsertDecl decl@(ImportDecl _ path qual (Module exps _)) (ImportEnv locs imps idecls)
  = ImportEnv
  { importEnvLocalDecls = locs
  , importEnvImportedDecls = M.insertWith (<>) qual [(path, exps)] imps
  , importEnvImportDecls = decl : idecls
  }

mkImportEnv :: ModulePath -> DeclSet -> [ImportDecl Range] -> ImportEnv
mkImportEnv mpath mexps
  = foldr importEnvInsertDecl localEnv
  where localEnv
          = ImportEnv
          { importEnvLocalDecls = (mpath, mexps)
          , importEnvImportedDecls = M.fromList [("", [("", builtinDecls)])]
          , importEnvImportDecls = []
          }

importEnvAllDecls :: ImportEnv -> M.Map T.Text [(ModulePath, DeclSet)]
importEnvAllDecls (ImportEnv locs imps _) = M.insertWith (<>) "" [locs] imps

importEnvImportedLocals :: ImportEnv -> DeclSet
importEnvImportedLocals = foldMap snd . fold . (M.!? "") . importEnvImportedDecls

evalImportT :: (Monad m) => ImportT m a -> ImportEnv -> m a
evalImportT (ImportT x) = evalStateT x

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
