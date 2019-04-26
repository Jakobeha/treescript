{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types for the @Local@ phase.
module TreeScript.Ast.Core.Types.Local
  ( module TreeScript.Ast.Core.Types.Local
  ) where

import TreeScript.Ast.Core.Types.Gen
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

-- | AST except with local binds.
type PLProgram = Program [ImportDecl Range] [RecordDecl Range] T.Text GVBindEnv Range
type PLGroupDef = GroupDef T.Text GVBindEnv Range

data ImportEnv
  = ImportEnv
  { importEnvRoot :: FilePath
  , importEnvModulePath :: ModulePath
  , importEnvLocalDecls :: (ModulePath, DeclSet)
  -- | Includes builtins.
  , importEnvImportedModules :: S.Set ModulePath
  -- | Includes builtins.
  , importEnvImportedDecls :: M.Map T.Text [(ModulePath, DeclSet)]
  , importEnvImportDecls :: [ImportDecl Range]
  }

newtype ImportT m a = ImportT (StateT ImportEnv (WriterT PFProgram m) a) deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadResult)

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
  addImportedModule :: Range -> T.Text -> PFProgram -> m ()

instance (Monad m) => MonadImport (ImportT m) where
  getImportEnv = ImportT get
  addImportedModule rng qual mdl = do
    let decl = ImportDecl rng (programPath mdl) qual (programExports mdl)
    ImportT $ lift $ tell mdl
    ImportT $ modify $ importEnvInsertDecl decl

instance (MonadImport m) => MonadImport (StateT s m) where
  getImportEnv = lift getImportEnv
  addImportedModule rng qual = lift . addImportedModule rng qual

instance (MonadReader r m) => MonadReader r (ImportT m) where
  ask = ImportT ask
  reader = ImportT . reader
  local f (ImportT x) = ImportT $ local f x

instance MonadTrans ImportT where
  lift = ImportT . lift . lift

importEnvInsertDecl :: ImportDecl Range -> ImportEnv -> ImportEnv
importEnvInsertDecl decl@(ImportDecl _ ipath qual exps) (ImportEnv root mpath locs imods imps idecls)
  = ImportEnv
  { importEnvRoot = root
  , importEnvModulePath = mpath
  , importEnvLocalDecls = locs
  , importEnvImportedModules = S.insert ipath imods
  , importEnvImportedDecls = M.insertWith (<>) qual [(ipath, exps)] imps
  , importEnvImportDecls = decl : idecls
  }

importEnvAllDecls :: ImportEnv -> M.Map T.Text [(ModulePath, DeclSet)]
importEnvAllDecls (ImportEnv _ _ locs _ imps _) = M.insertWith (<>) "" [locs] imps

importEnvImportedLocals :: ImportEnv -> DeclSet
importEnvImportedLocals = foldMap snd . fold . (M.!? "") . importEnvImportedDecls

runImportT :: (Monad m) => FilePath -> ModulePath -> DeclSet -> ImportT m a -> m (a, PFProgram)
runImportT root mpath mexps (ImportT x) = runWriterT $ (`evalStateT` initEnv) x
  where initEnv
          = ImportEnv
          { importEnvRoot = root
          , importEnvModulePath = mpath
          , importEnvLocalDecls = (mpath, mexps)
          , importEnvImportedModules = S.singleton ""
          , importEnvImportedDecls = M.fromList [("", [("", builtinDecls)])]
          , importEnvImportDecls = []
          }

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

mkLocalSymbol :: (Monad m) => an -> T.Text -> ImportT m (Symbol an)
mkLocalSymbol ann lcl = do
  mpath <- importEnvModulePath <$> getImportEnv
  pure Symbol
    { symbolAnn = ann
    , symbolModule = mpath
    , symbol = lcl
    }
