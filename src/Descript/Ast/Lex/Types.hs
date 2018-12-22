module Descript.Ast.Lex.Types
  ( module Descript.Ast.Lex.Types
  ) where

import Descript.Misc

import qualified Data.Text as T

-- | Punctuation, used for control flow
data Punc
  = PuncAngleBwd -- ^ @<@
  | PuncQuestion -- ^ @?@
  | PuncColon -- ^ @:@
  | PuncEqual -- ^ @=@
  | PuncPeriod -- ^ @.@
  | PuncSemicolon -- ^ @;@
  | PuncOpenBracket -- ^ @[@
  | PuncCloseBracket -- ^ @]@
  | PuncEof -- ^ End of file
  deriving (Eq, Ord, Read, Show)

-- | Fragment of a code block (in the future might also handle strings) which might contain splices.
data SpliceFrag
  = SpliceFrag
  { spliceFragStart :: Bool -- ^ Does this start the text block, or end a splice?
  , spliceFragEnd :: Bool -- ^ Does this end the text block, or start a splice?
  , spliceFragContent :: T.Text -- ^ Text in between the start and end of this enclosure.
  } deriving (Eq, Ord, Read, Show)

-- | Primitive
data Prim
  = PrimInteger Int
  | PrimFloat Float
  | PrimString T.Text
  | PrimCode SpliceFrag
  deriving (Eq, Ord, Read, Show)

-- | Whether the symbol starts with an uppercase character (e.g. record head) or lowercase character (e.g. record property).
data SymbolCase
  = SymbolCaseUpper
  | SymbolCaseLower
  deriving (Eq, Ord, Read, Show)

-- | Symbol
data Symbol
  = Symbol
  { symbolCase :: SymbolCase
  , symbolText :: T.Text
  } deriving (Eq, Ord, Read, Show)

-- | Lexeme data without source range
data Lexeme
  = LexemePunc Punc
  | LexemePrim Prim
  | LexemeSymbol Symbol
  deriving (Eq, Ord, Read, Show)

-- | Lexeme data with source range
data AnnLexeme
  = AnnLexeme
  { annLexemeRange :: Range
  , annLexeme :: Lexeme
  } deriving (Eq, Ord, Read, Show)
