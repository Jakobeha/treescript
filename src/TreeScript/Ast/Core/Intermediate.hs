{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

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

-- = Intermediate AST

-- | The type and identifier of a group reference.
data GroupLoc an
  = GroupLocGlobal (S.Symbol an)
  | GroupLocLocal (C.Bind an)
  | GroupLocFunction (S.Symbol an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group - group gets applied when the reducer or guard matches, and if it fails, the entire reducer / guard fails.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc an
  , groupRefValueProps :: [C.Value an]
  , groupRefGroupProps :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches an input value against an output value. Like a "let" statement.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: C.Value an
  , guardOutput :: C.Value an
  , guardNexts :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a "match" case.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefHead :: T.Text
  , groupDefValueProps :: [(T.Text, C.Bind an)]
  , groupDefGroupProps :: [(T.Text, C.Bind an)]
  , groupDefReducers :: [Reducer an]
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
