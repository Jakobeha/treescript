-- Types for the @Raw@ phase (like final but with extra information).
module TreeScript.Ast.Core.Types.Raw
  ( module TreeScript.Ast.Core.Types.Raw
  ) where

import TreeScript.Ast.Core.Types.Local
import TreeScript.Ast.Core.Types.Gen
import TreeScript.Misc

import qualified Data.Text as T

-- | Final AST except with all extra information.
type PRProgram = Program [ImportDecl Range] [RecordDecl Range] T.Text GVBindEnv Range
type PRGroupDef = GroupDef T.Text GVBindEnv
