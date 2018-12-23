{-# LANGUAGE OverloadedStrings #-}

module Descript.Ast.Lex.Types
  ( module Descript.Ast.Lex.Types
  ) where

import Descript.Misc

import qualified Data.Text as T

-- | Punctuation, used for control flow.
data Punc
  = PuncAngleBwd -- ^ @<@
  | PuncAngleFwd -- ^ @>@
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

-- | Lexeme data without source range.
data Lexeme
  = LexemePunc Punc
  | LexemePrim Prim
  | LexemeSymbol Symbol
  deriving (Eq, Ord, Read, Show)

-- | Lexeme data with source range.
data AnnLexeme
  = AnnLexeme
  { annLexemeRange :: Range
  , annLexeme :: Lexeme
  } deriving (Eq, Ord, Read, Show)

-- | A full Descript program.
newtype Program = Program [AnnLexeme]

instance Printable SpliceFrag where
  pprint (SpliceFrag isStart isEnd content)
    = pprintStart <> pprint content <> pprintEnd
    where pprintStart
            | isStart = "'"
            | otherwise = ")"
          pprintEnd
            | isEnd = "'"
            | otherwise = "\\("

instance Printable Punc where
  pprint PuncAngleBwd = "<"
  pprint PuncAngleFwd = ">"
  pprint PuncQuestion = "?"
  pprint PuncColon = ":"
  pprint PuncEqual = "="
  pprint PuncPeriod = "."
  pprint PuncSemicolon = ";"
  pprint PuncOpenBracket = "["
  pprint PuncCloseBracket = "]"
  pprint PuncEof = ""

instance Printable Prim where
  pprint (PrimInteger int) = pprint int
  pprint (PrimFloat float) = pprint float
  pprint (PrimString str) = pprint str
  pprint (PrimCode frag) = pprint frag

instance Printable Symbol where
  pprint = symbolText

instance Printable Lexeme where
  pprint (LexemePunc punc) = pprint punc
  pprint (LexemePrim prim) = pprint prim
  pprint (LexemeSymbol sym) = pprint sym

instance Printable AnnLexeme where
  pprint = pprint . annLexeme

instance Printable Program where
  pprint (Program lexemes) = T.concat $ map pprint lexemes

instance ReducePrintable SpliceFrag where
  reducePrint (SpliceFrag isStart isEnd content)
    = reducePrintStart <> reducePrint content <> reducePrintEnd
    where reducePrintStart
            | isStart = "'"
            | otherwise = ")"
          reducePrintEnd
            | isEnd = "'"
            | otherwise = "\\("

instance ReducePrintable Punc where
  reducePrint = pprint

instance ReducePrintable Prim where
  reducePrint (PrimInteger int) = reducePrint int
  reducePrint (PrimFloat float) = reducePrint float
  reducePrint (PrimString str) = reducePrint str
  reducePrint (PrimCode frag) = reducePrint frag

instance ReducePrintable Symbol where
  reducePrint = symbolText

instance ReducePrintable Lexeme where
  reducePrint (LexemePunc punc) = reducePrint punc
  reducePrint (LexemePrim prim) = reducePrint prim
  reducePrint (LexemeSymbol sym) = reducePrint sym

instance ReducePrintable AnnLexeme where
  reducePrint = reducePrint . annLexeme

instance ReducePrintable Program where
  reducePrint (Program lexemes) = T.concat $ map reducePrint lexemes
