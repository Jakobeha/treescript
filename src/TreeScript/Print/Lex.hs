{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Print.Lex
  ()
where

import           TreeScript.Ast
import           TreeScript.Print.Class
import           TreeScript.Print.Misc          ( )
import           TreeScript.Print.PrintM

import qualified Data.Text                     as T

instance Printable Enclosure where
  mprint (Enclosure EncTypeTab EncPlaceOpen) = "" <$ indentM
  mprint (Enclosure EncTypeTab EncPlaceClose) = "" <$ unindentM
  mprint enc = pure $ printEnclosure enc

instance Printable LexQuote where
  pprint = printLexQuote

instance Printable LexPrim where
  pprint (LexPrimInteger x       ) = pprint x
  pprint (LexPrimFloat   x       ) = pprint x
  pprint (LexPrimString beg x end) = pprint beg <> pprint x <> pprint end

instance Printable Atom where
  mprint (AtomPunc punc   ) = mprint punc
  mprint (AtomPrim prim   ) = mprint prim
  mprint (AtomSymbol _ sym) = pure sym

instance Printable Lexeme where
  mprint LexemeEof        = pure ""
  mprint (LexemeEnc  enc) = mprint enc
  mprint (LexemeAtom atm) = mprint atm

instance (AnnPrintable an) => Printable (LexProgram an) where
  mprint (LexProgram lexs) = T.concat <$> traverse mprint lexs
