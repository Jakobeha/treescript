-- | Language specifications - allow the Descript compiler to create programs for specific languages.
module Descript.Component.LangSpec
  ( LangSpec (..)
  ) where

import qualified Data.Text as T

-- | Describes an individual node in a language's AST.
data ASTNodeSpec
  = ASTNodeSpec
  { astNodeSpecName :: T.Text
  , astNodeSpecNumArgs :: Int
  }

-- | Describes a language's AST.
newtype ASTSpec = ASTSpec{ astSpecNodes :: [ASTNodeSpec] }

-- | Describes a language's AST and provides programs to parse and print it.
data LangSpec
  = LangSpec
  { langSpecParser :: CmdProgram T.Text ASTData
  , langSpecPrinter :: CmdProgram ASTData T.Text
  , langSpecAstSpec :: ASTSpec
  }
