{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Descript.Ast.Lex.Types
  ( module Descript.Ast.Lex.Types
  ) where

import Descript.Misc

import Data.Maybe
import qualified Data.Text as T
import GHC.Generics

-- | Punctuation, used for control flow.
data Punc an
  = PuncEqArrow an -- ^ @=>@
  | PuncVertLine an -- ^ @|@
  | PuncBackSlash an -- ^ @\\@
  | PuncColon an -- ^ @:@
  | PuncPeriod an -- ^ @.@
  | PuncSemicolon an -- ^ @;@
  | PuncOpenBracket an -- ^ @[@
  | PuncCloseBracket an -- ^ @]@
  | PuncEof an -- ^ End of file
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Fragment of a code block (in the future might also handle strings) which might contain splices.
data SpliceFrag an
  = SpliceFrag
  { spliceFragContent :: (Annd T.Text an) -- ^ Text in between the start and end of this enclosure, and the entire enclosure's annotation __(structured this way for the Happy parser)__.
  , spliceFragStart :: Bool -- ^ Does this start the text block, or end a splice?
  , spliceFragEnd :: Bool -- ^ Does this end the text block, or start a splice?
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Primitive
data Primitive an
  = PrimInteger (Annd Int an)
  | PrimFloat (Annd Float an)
  | PrimString (Annd T.Text an)
  | PrimCode (SpliceFrag an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Whether the symbol starts with an uppercase character (e.g. record head) or lowercase character (e.g. record property).
data SymbolCase
  = SymbolCaseUpper
  | SymbolCaseLower
  deriving (Eq, Ord, Read, Show)

-- | Symbol
data Symbol an
  = Symbol
  { symbolText :: Annd T.Text an
  , symbolCase :: SymbolCase
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Lexeme data without source range.
data Lexeme an
  = LexemePunc (Punc an)
  | LexemePrim (Primitive an)
  | LexemeSymbol (Symbol an)
  | LexemeSplicedBind (Annd (Maybe T.Text) an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full Descript program.
newtype Program an = Program (Annd [Lexeme an] an) deriving (Eq, Ord, Read, Show)

instance Functor Program where
  fmap f (Program (Annd ann lexemes)) = Program $ Annd (f ann) (map (fmap f) lexemes)

instance Foldable Program where
  foldMap f (Program (Annd ann lexemes)) = f ann <> foldMap (foldMap f) lexemes

instance Traversable Program where
  traverse f (Program (Annd ann lexemes)) = fmap Program $ Annd <$> f ann <*> traverse (traverse f) lexemes

instance Annotatable Program where
  getAnn (Program lexemes) = getAnn lexemes

instance Printable (SpliceFrag an) where
  pprint (SpliceFrag content isStart isEnd)
    = pprintStart <> pprint (annd content) <> pprintEnd
    where pprintStart
            | isStart = "'"
            | otherwise = ")"
          pprintEnd
            | isEnd = "'"
            | otherwise = "\\("

instance Printable (Punc an) where
  pprint (PuncEqArrow _) = "=>"
  pprint (PuncVertLine _) = "|"
  pprint (PuncBackSlash _) = "\\"
  pprint (PuncColon _) = ":"
  pprint (PuncPeriod _) = "."
  pprint (PuncSemicolon _) = ";"
  pprint (PuncOpenBracket _) = "["
  pprint (PuncCloseBracket _) = "]"
  pprint (PuncEof _) = ""

instance Printable (Primitive an) where
  pprint (PrimInteger int) = pprint $ annd int
  pprint (PrimFloat float) = pprint $ annd float
  pprint (PrimString str) = pprint $ annd str
  pprint (PrimCode frag) = pprint frag

instance Printable (Symbol an) where
  pprint = annd . symbolText

instance Printable (Lexeme an) where
  pprint (LexemePunc punc) = pprint punc
  pprint (LexemePrim prim) = pprint prim
  pprint (LexemeSymbol sym) = pprint sym
  pprint (LexemeSplicedBind txt) = "\\" <> T.empty `fromMaybe` annd txt

instance Printable (Program an) where
  pprint (Program lexemes) = T.concat $ map pprint $ annd lexemes

instance ReducePrintable (SpliceFrag an) where
  reducePrint (SpliceFrag content isStart isEnd)
    = reducePrintStart <> reducePrint (annd content) <> reducePrintEnd
    where reducePrintStart
            | isStart = "'"
            | otherwise = ""
          reducePrintEnd
            | isEnd = "'"
            | otherwise = "\\"

instance ReducePrintable (Punc an) where
  reducePrint = pprint

instance ReducePrintable (Primitive an) where
  reducePrint (PrimInteger int) = reducePrint $ annd int
  reducePrint (PrimFloat float) = reducePrint $ annd float
  reducePrint (PrimString str) = reducePrint $ annd str
  reducePrint (PrimCode frag) = reducePrint frag

instance ReducePrintable (Symbol an) where
  reducePrint = annd . symbolText

instance ReducePrintable (Lexeme an) where
  reducePrint (LexemePunc punc) = reducePrint punc
  reducePrint (LexemePrim prim) = reducePrint prim
  reducePrint (LexemeSymbol sym) = reducePrint sym
  reducePrint (LexemeSplicedBind txt) = T.empty `fromMaybe` annd txt

instance ReducePrintable (Program an) where
  reducePrint (Program lexemes) = T.concat $ map reducePrint $ annd lexemes
