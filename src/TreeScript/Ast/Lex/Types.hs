{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Lex.Types
  ( module TreeScript.Ast.Lex.Types
  )
where

import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text      as T

import qualified Data.Text                     as T
import           GHC.Generics

data EncType
  = EncTypeParen
  | EncTypeBrace
  | EncTypeBracket
  | EncTypeTab
  deriving (Eq, Ord, Read, Show)

data EncPlace
  = EncPlaceOpen
  | EncPlaceClose
  deriving (Eq, Ord, Read, Show)

data Enclosure
  = Enclosure
  { enclosureType :: EncType
  , enclosurePlace :: EncPlace
  } deriving (Eq, Ord, Read, Show)

data Quote
  = QuoteSingle
  | QuoteDouble
  | QuoteSplice
  deriving (Eq, Ord, Read, Show)

-- | Primitive
data Prim
  = PrimInteger Int
  | PrimFloat Float
  | PrimString Quote T.Text Quote
  deriving (Eq, Ord, Read, Show)

-- | Whether the symbol starts with an uppercase character (e.g. record head) or lowercase character (e.g. record property).
data SymbolCase
  = SymbolCaseUpper
  | SymbolCaseLower
  deriving (Eq, Ord, Read, Show)

-- | Symbol
data Symbol
  = Symbol
  { symbolText :: T.Text
  , symbolCase :: SymbolCase
  } deriving (Eq, Ord, Read, Show)

-- | Lexeme data without source range.
data Lex
  = LexEof
  | LexPunc Char
  | LexEnc Enclosure
  | LexPrim Prim
  | LexSymbol T.Text SymbolCase
  deriving (Eq, Ord, Read, Show)

type SLex = Annd SrcAnn Lex

-- | A full TreeScript program.
newtype Program an = Program [Annd Lex an] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Printable Enclosure where
  pprint (Enclosure EncTypeParen   EncPlaceOpen ) = "("
  pprint (Enclosure EncTypeParen   EncPlaceClose) = ")"
  pprint (Enclosure EncTypeBrace   EncPlaceOpen ) = "["
  pprint (Enclosure EncTypeBrace   EncPlaceClose) = "]"
  pprint (Enclosure EncTypeBracket EncPlaceOpen ) = "{"
  pprint (Enclosure EncTypeBracket EncPlaceClose) = "}"

instance Printable Quote where
  pprint QuoteSingle = "'"
  pprint QuoteDouble = "'"
  pprint QuoteSplice = "\\"

instance Printable Prim where
  pprint (PrimInteger x       ) = pprint x
  pprint (PrimFloat   x       ) = pprint x
  pprint (PrimString  x       ) = pprint x
  pprint (PrimCode start x end) = pprint start <> T.escape x <> pprint end

instance Printable Symbol where
  pprint = symbolText

instance Printable Lex where
  pprint (LexPunc   punc) = pprint punc
  pprint (LexEnc    enc ) = pprint enc
  pprint (LexPrim   prim) = pprint prim
  pprint (LexSymbol sym ) = pprint sym

instance (PrintableAnn an) => Printable (Program an) where
  pprint (Program lexs) = T.concat $ map pprint lexs
