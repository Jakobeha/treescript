{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Print.Lex
  ()
where

import           TreeScript.Ast
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text      as T

import qualified Data.Text                     as T
import           GHC.Generics

instance Printable Enclosure where
  pprint (Enclosure EncTypeParen   EncPlaceOpen ) = "("
  pprint (Enclosure EncTypeParen   EncPlaceClose) = ")"
  pprint (Enclosure EncTypeBrace   EncPlaceOpen ) = "["
  pprint (Enclosure EncTypeBrace   EncPlaceClose) = "]"
  pprint (Enclosure EncTypeBracket EncPlaceOpen ) = "{"
  pprint (Enclosure EncTypeBracket EncPlaceClose) = "}"

instance Printable LexQuote where
  pprint LexQuoteSingle = "'"
  pprint LexQuoteDouble = "'"
  pprint LexQuoteSplice = "\\"

instance Printable LexPrim where
  pprint (LexPrimInteger x       ) = pprint x
  pprint (LexPrimFloat   x       ) = pprint x
  pprint (LexPrimString  x       ) = pprint x
  pprint (LexPrimCode start x end) = pprint start <> T.escape x <> pprint end

instance Printable Symbol where
  pprint = symbolText

instance Printable Lexeme where
  pprint (LexemePunc   punc) = pprint punc
  pprint (LexemeEnc    enc ) = pprint enc
  pprint (LexemePrim   prim) = pprint prim
  pprint (LexemeSymbol sym ) = pprint sym

instance (PrintableAnn an) => Printable (Program an) where
  pprint (Program lexs) = T.concat $ map pprint lexs
