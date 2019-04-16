{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Intermediate types used to parse the @Core@ AST.
module TreeScript.Ast.Core.Intermediate
  ( module TreeScript.Ast.Core.Intermediate
  ) where

import qualified TreeScript.Ast.Core.Types as C
import qualified TreeScript.Ast.Sugar.Types as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import System.FilePath
import System.Directory

-- = Bind identifier/index environment

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

type ImportSessionRes a = ReaderT [C.ImportDecl Range] (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (ReaderT [C.ImportDecl Range] (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

type GVBindSessionRes a = StateT (GVEnv BindEnv) (ReaderT [C.ImportDecl Range] (ResultT (ReaderT SessionEnv (LoggingT IO)))) a

-- = Intermediate AST

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefHead :: T.Text
  , groupDefValueProps :: [(T.Text, C.Bind an)]
  , groupDefGroupProps :: [(T.Text, C.Bind an)]
  , groupDefReducers :: [C.Reducer an]
  , groupDefPropEnv :: GVBindEnv
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

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
