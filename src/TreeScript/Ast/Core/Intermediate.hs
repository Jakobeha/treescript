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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

-- = Bind identifier/index environment

type GroupEnv = M.Map T.Text (Int, V.Vector (C.Bind Range), M.Map T.Text Int)

data BindEnv
  = BindEnv
  { bindEnvBinds :: M.Map T.Text Int
  , bindEnvNextFree :: Int
  } deriving (Eq, Ord, Read, Show)

type GroupSessionRes a = ReaderT GroupEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type FreeSessionRes a = StateT Int (ResultT (ReaderT SessionEnv (LoggingT IO))) a

data Variance
  = VarianceContravariant
  | VarianceCovariant (S.Set Int)

-- = Intermediate AST

-- | Property in a group reference - assigns a value to a bind in the group's reducers.
data GroupProperty an
  = GroupProperty
  { groupPropertyAnn :: an
  , groupPropertyKey :: S.Symbol an -- ^ The bind which gets assigned.
  , groupPropertyValue :: C.Value an -- ^ The value which gets assigned to the bind.
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefHead :: S.Symbol an
  , groupRefProps :: [GroupProperty an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The input or output of a reducer.
data ReducerClause an
  = ReducerClause
  { reducerClauseAnn :: an
  , reducerClauseValue :: C.Value an
  , reducerClauseGroups :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: ReducerClause an
  , reducerOutput :: ReducerClause an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Performs some transformations on values.
data Statement an
  = StatementGroup (GroupRef an)
  | StatementReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefHead :: T.Text
  , groupDefProps :: [(T.Text, C.Bind an)]
  , groupDefRepeats :: Bool
  , groupDefStatements :: [Statement an]
  , groupDefBindEnv :: BindEnv
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

emptyBindEnv :: BindEnv
emptyBindEnv
  = BindEnv
  { bindEnvBinds = M.empty
  , bindEnvNextFree = 1
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
