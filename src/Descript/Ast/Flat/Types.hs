-- | General types and functions for flat AST data.
module Descript.Ast.Flat.Types
  ( module Descript.Ast.Flat.Types
  ) where

import qualified Data.Text as T

-- | A primitive in AST data.
data Primitive
  = PrimInteger Int
  | PrimFloat Float
  | PrimString T.Text

-- | A single lexeme in AST data.
data Lexeme
  = LexemeSplice Int
  | LexemePrimitive Primitive
  | LexemeRecordHead T.Text
