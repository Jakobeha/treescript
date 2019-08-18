{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | Lexeme data without source range.
data Atom
  = AtomPunc Char
  | AtomPrim LexPrim
  | AtomSymbol SymbolCase T.Text
  deriving (Eq, Ord, Read, Show, Generic)

-- | Unbalanced lexeme.
data Lexeme
  = LexemeEof
  | LexemeEnc Enclosure
  | LexemeAtom Atom
  deriving (Eq, Ord, Read, Show, Generic)

type SLexeme = Annd Lexeme SrcAnn

-- | A full TreeScript program.
newtype LexProgram an = LexProgram{ unLexProgram :: [Annd Lexeme an] } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1)

printEnclosure :: Enclosure -> T.Text
printEnclosure (Enclosure EncTypeParen   EncPlaceOpen ) = "("
printEnclosure (Enclosure EncTypeParen   EncPlaceClose) = ")"
printEnclosure (Enclosure EncTypeBrace   EncPlaceOpen ) = "["
printEnclosure (Enclosure EncTypeBrace   EncPlaceClose) = "]"
printEnclosure (Enclosure EncTypeBracket EncPlaceOpen ) = "{"
printEnclosure (Enclosure EncTypeBracket EncPlaceClose) = "}"

printLexQuote :: LexQuote -> T.Text
printLexQuote LexQuoteSingle = "'"
printLexQuote LexQuoteDouble = "'"
printLexQuote LexQuoteSplice = "\\"
