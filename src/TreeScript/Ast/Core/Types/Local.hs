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
type PL a = a [ImportDecl Range] [RecordDecl Range] [FunctionDecl Range] [TypeAlias Range] T.Text GVBindEnv () Range

data GlobalEnv
  = GlobalEnv
  { globalEnvRoot :: FilePath
  , globalEnvModulePath :: ModulePath
  , globalEnvLocalDecls :: DeclSet
  -- | Includes builtins.
  , globalEnvImportedModules :: S.Set ModulePath
  -- | Includes builtins.
  , globalEnvImportedDecls :: M.Map T.Text [(ModulePath, DeclSet)]
  , globalEnvImportDecls :: [ImportDecl Range]
  }

newtype GlobalT m a = GlobalT (StateT GlobalEnv (WriterT (PF Program) m) a) deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadResult)

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

type ImportSessionRes a = GlobalT (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (GlobalT (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

type GVBindSessionRes a = StateT (GVEnv BindEnv) (GlobalT (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

class (Monad m) => MonadGlobal m where
  getGlobalEnv :: m GlobalEnv
  addImportedModule :: Range -> T.Text -> PF Program -> m ()
  putLocals :: DeclSet -> m ()

instance (Monad m) => MonadGlobal (GlobalT m) where
  getGlobalEnv = GlobalT get
  addImportedModule rng qual mdl = do
    let decl = ImportDecl rng (programPath mdl) qual (programExports mdl)
    GlobalT $ lift $ tell mdl
    GlobalT $ modify $ globalEnvInsertDecl decl
  putLocals exps
    = GlobalT $ modify
    $ \genv -> genv{ globalEnvLocalDecls = exps }

instance (MonadGlobal m) => MonadGlobal (StateT s m) where
  getGlobalEnv = lift getGlobalEnv
  addImportedModule rng qual = lift . addImportedModule rng qual
  putLocals = lift . putLocals

instance (MonadReader r m) => MonadReader r (GlobalT m) where
  ask = GlobalT ask
  reader = GlobalT . reader
  local f (GlobalT x) = GlobalT $ local f x

instance MonadTrans GlobalT where
  lift = GlobalT . lift . lift

globalEnvInsertDecl :: ImportDecl Range -> GlobalEnv -> GlobalEnv
globalEnvInsertDecl decl@(ImportDecl _ ipath qual exps) (GlobalEnv root mpath locs imods imps idecls)
  = GlobalEnv
  { globalEnvRoot = root
  , globalEnvModulePath = mpath
  , globalEnvLocalDecls = locs
  , globalEnvImportedModules = S.insert ipath imods
  , globalEnvImportedDecls = M.insertWith (<>) qual [(ipath, exps)] imps
  , globalEnvImportDecls = decl : idecls
  }

globalEnvAllDecls :: GlobalEnv -> M.Map T.Text [(ModulePath, DeclSet)]
globalEnvAllDecls (GlobalEnv _ mpath locs _ imps _)
  = M.insertWith (<>) "" [(mpath, locs)] imps

globalEnvImportedLocals :: GlobalEnv -> DeclSet
globalEnvImportedLocals = foldMap snd . fold . (M.!? "") . globalEnvImportedDecls

runGlobalT :: (Monad m) => FilePath -> ModulePath -> GlobalT m a -> m (a, PF Program)
runGlobalT root mpath (GlobalT x) = runWriterT $ (`evalStateT` initEnv) x
  where initEnv
          = GlobalEnv
          { globalEnvRoot = root
          , globalEnvModulePath = mpath
          , globalEnvLocalDecls = mempty
          , globalEnvImportedModules = S.singleton ""
          , globalEnvImportedDecls = M.fromList [("", [("", builtinDecls)])]
          , globalEnvImportDecls = []
          }

emptyLocalEnv :: LocalEnv
emptyLocalEnv
  = LocalEnv
  { localEnvBinds = S.empty
  , localEnvGroups = S.empty
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

mkLocalSymbol :: (Monad m) => T.Text -> GlobalT m (PF Symbol)
mkLocalSymbol lcl = do
  mpath <- globalEnvModulePath <$> getGlobalEnv
  pure Symbol
    { symbolAnn = ()
    , symbolModule = mpath
    , symbol = lcl
    }

hole :: Range -> Range -> Int -> PL Value
hole ann idxAnn idx
  = ValueRecord Record
  { recordAnn = ann
  , recordType = ()
  , recordHead = Symbol
      { symbolAnn = ann
      , symbolModule = ""
      , symbol = "Hole"
      }
  , recordProps = [ValuePrimitive $ PrimInteger idxAnn () idx]
  }
