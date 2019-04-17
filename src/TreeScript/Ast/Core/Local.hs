{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Local
  (

  ) where

import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Sugar.Types as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

-- = Bind identifier/index environment

type GroupEnv = M.Map T.Text Int

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

type GroupSessionRes a = ReaderT GroupEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type GVBindSessionRes a = StateT (GVEnv BindEnv) (ResultT (ReaderT SessionEnv (LoggingT IO))) a

-- | What reducers get from their parent group, or output values / groups from their reducer.
data LocalEnv
  = LocalEnv
  { localEnvBinds :: S.Set Int
  , localEnvGroups :: S.Set Int
  } deriving (Eq, Ord, Read, Show)

-- = Intermediate AST

-- | The type and identifier of a group reference.
data instance GroupLoc 'Local an
  = GroupLocGlobal (S.Symbol an)
  | GroupLocLocal (Bind 'Local an)
  | GroupLocFunction (S.Symbol an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data instance GroupDef 'Local an
  = GroupDef
  { groupDefAnn :: an
  , groupDefHead :: T.Text
  , groupDefValueProps :: [(T.Text, Bind 'Local an)]
  , groupDefGroupProps :: [(T.Text, Bind 'Local an)]
  , groupDefReducers :: [Reducer 'Local an]
  , groupDefPropEnv :: GVBindEnv
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

hole :: an -> an -> Int -> Value 'Local an
hole ann idxAnn idx
  = ValueRecord Record
  { recordAnn = ann
  , recordHead = "Hole"
  , recordProps = [ValuePrimitive $ PrimInteger idxAnn idx]
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
