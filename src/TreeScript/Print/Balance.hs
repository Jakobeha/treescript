{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Print.Balance
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

instance (AnnPrintable an) => Printable (Balance an) where
  mprint (BalanceAtom atm) = mprint atm
  mprint (BalanceEnc enc xs) =
    mprint (Enclosure enc EncPlaceOpen) <> foldMap mprint xs <> mprint
      (Enclosure enc EncPlaceClose)

instance (AnnPrintable an) => Printable (BalanceProgram an) where
  mprint (BalanceProgram lexs) = T.concat <$> traverse mprint lexs
