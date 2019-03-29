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
import GHC.Generics

-- = Bind identifier/index environment

type GroupEnv = M.Map T.Text Int

data BindEnv
  = BindEnv
  { bindEnvBinds :: M.Map T.Text Int
  , bindEnvNextFree :: Int
  } deriving (Eq, Ord, Read, Show)

data GroupValEnv a
  = GroupValEnv
  { groupValEnvGroup :: a
  , groupValEnvValue :: a
  } deriving (Eq, Ord, Read, Show, Functor)

type GroupSessionRes a = ReaderT GroupEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type BindSessionRes a = StateT BindEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type PropSessionRes a = StateT (GroupValEnv BindEnv) (ResultT (ReaderT SessionEnv (LoggingT IO))) a

type FreeSessionRes a = StateT (GroupValEnv Int) (ResultT (ReaderT SessionEnv (LoggingT IO))) a

data Variance
  = VarianceContravariant
  | VarianceCovariant (S.Set Int)

-- = Intermediate AST

-- | The head of a group reference, and whether it's a property - property heads become indices before value heads.
data GroupHead an
  = GroupHeadGlobal (S.Symbol an)
  | GroupHeadProp (C.Bind an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefHead :: GroupHead an
  , groupRefGroupProps :: [GroupRef an]
  , groupRefValueProps :: [C.Value an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A group as a statement.
data GroupStmt an
  = GroupStmt
  { groupStmtAnn :: an
  , groupStmtReduceType :: C.ReduceType
  , groupStmtRef :: GroupRef an
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Guards a reducer.
data Guard an
  = GuardGroup (GroupRef an)
  | GuardReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a "function".
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerInput :: C.Value an
  , reducerOutput :: C.Value an
  , reducerNexts :: [GroupRef an]
  , reducerGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Performs some transformations on values.
data Statement an
  = StatementGroup (GroupStmt an)
  | StatementReducer (Reducer an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of reducers, which can be referenced by other reducers.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefHead :: T.Text
  , groupDefGroupProps :: [(T.Text, C.Bind an)]
  , groupDefValueProps :: [(T.Text, C.Bind an)]
  , groupDefStatements :: [Statement an]
  , groupDefPropEnv :: GroupValEnv BindEnv
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

emptyBindEnv :: BindEnv
emptyBindEnv
  = BindEnv
  { bindEnvBinds = M.empty
  , bindEnvNextFree = 1
  }

emptyPropEnv :: GroupValEnv BindEnv
emptyPropEnv
  = GroupValEnv
  { groupValEnvGroup = emptyBindEnv
  , groupValEnvValue = emptyBindEnv
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

zipGroupValEnvWith :: (a -> b -> c) -> GroupValEnv a -> GroupValEnv b -> GroupValEnv c
zipGroupValEnvWith f (GroupValEnv xg xv) (GroupValEnv yg yv)
  = GroupValEnv
  { groupValEnvGroup = f xg yg
  , groupValEnvValue = f xv yv
  }
