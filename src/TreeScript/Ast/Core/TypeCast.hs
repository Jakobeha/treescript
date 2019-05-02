{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TreeScript.Ast.Core.TypeCast
  ( castCheckTypes
  ) where

import TreeScript.Ast.Core.Classes
import TreeScript.Ast.Core.Types
import TreeScript.Misc
import TreeScript.Plugin

import Data.MessagePack
import qualified Data.Text as T

dummy :: CastRef
dummy = CastRef "SOON" (-1)

castCheckTypes :: PL Program -> SessionRes (PR Program)
castCheckTypes = pure . mapA (MapA id id id id (\() -> dummy) id)

-- SOON
instance Serial CastRef where
  toMsgp _ = ObjectNil
  fromMsgp ObjectNil = pure dummy
