module TreeScript.Ast.Core.TypeCast
  ( castCheckTypes
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Plugin

castCheckTypes :: Program () -> SessionRes (Program ())
castCheckTypes = pure