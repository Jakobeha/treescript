{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeScript.Ast.Lex
  ( module TreeScript.Ast.Lex
  )
where

import           TreeScript.Misc

import qualified Data.Text                     as T
import           GHC.Generics

data EncType
  = EncTypeParen
  | EncTypeBrace
  | EncTypeBracket
  | EncTypeTab
  deriving (Eq, Ord, Read, Show, Generic)

data EncPlace
  = EncPlaceOpen
  | EncPlaceClose
  deriving (Eq, Ord, Read, Show, Generic)

data Enclosure
  = Enclosure
  { enclosureType :: EncType
  , enclosurePlace :: EncPlace
  } deriving (Eq, Ord, Read, Show, Generic)

data LexQuote
  = LexQuoteSingle
  | LexQuoteDouble
  | LexQuoteSplice
  deriving (Eq, Ord, Read, Show, Generic)

-- | Primitive
data LexPrim
  = LexPrimInteger Int
  | LexPrimFloat Float
  | LexPrimString LexQuote T.Text LexQuote
  deriving (Eq, Ord, Read, Show, Generic)

-- | Whether the symbol starts with an uppercase character (e.g. record head) or lowercase character (e.g. record property).
data SymbolCase
  = SymbolCaseUpper
  | SymbolCaseLower
  deriving (Eq, Ord, Read, Show, Generic)

-- | Symbol
data Symbol
  = Symbol
  { symbolText :: T.Text
  , symbolCase :: SymbolCase
  } deriving (Eq, Ord, Read, Show, Generic)

-- | Lexeme data without source range.
data Lexeme
  = LexemeEof
  | LexemePunc Char
  | LexemeEnc Enclosure
  | LexemePrim LexPrim
  | LexemeSymbol T.Text SymbolCase
  deriving (Eq, Ord, Read, Show, Generic)

type SLexeme = Annd SrcAnn Lexeme

-- | A full TreeScript program.
newtype Program an = Program [Annd Lexeme an] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)
