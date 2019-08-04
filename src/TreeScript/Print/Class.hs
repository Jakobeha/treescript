{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Printing classes, from debug-freindly "pretty-printing" to production AST reprints.
module TreeScript.Print.Class
  ( module TreeScript.Print.Class
  )
where

import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Print.PrintM

import           Data.String
import qualified Data.Text                     as T
import           GHC.Generics

-- | Get a "user-friendly" description: reasonable to put in a message shown to the user.
-- Depending on the implementation, this could be for debugging or the actual AST reprint used in production.
class Printable a where
  {-# MINIMAL pprint | mprint #-}

  pprint :: a -> T.Text
  pprint = runPrintM . mprint
  mprint :: a -> PrintM T.Text
  mprint = pure . pprint

class AnnPrintable an where
  printAnnd :: PrintM T.Text -> an -> PrintM T.Text

-- | Abstract output type which trees can be printed into. Usually just text, but can also be a patch or "smart" type.
--
-- - 'fromString' / `ppunc` converts punctuation (e.g. separators, delimiters) where the content of the text may not be significant beyond helping the parser.
-- - 'pliteral' converts (e.g. symbols, data already printed and then flattened) where the content of the text is significant.
-- Basic implementations, like 'Text', handle these both the same, but patches wouldn't.
class (IsString a, Monoid a) => PrintOut a where
  -- | Punctuation (e.g. separators, delimiters)
  ppunc :: T.Text -> a
  ppunc = fromString . T.unpack
  -- | Significant text, e.g. symbols
  pliteral :: T.Text -> a
  -- | Indent
  pindent :: a -> a

-- | Print AST nodes for AST rewriting
class (Functor a) => TreePrintable a where
  treePrint :: (PrintOut o) => a (PrintM o) -> PrintM o
  -- | Default implementation for sum types
  default treePrint :: (Generic1 a, PrintOut o, TreePrintable' (Rep1 a)) => a (PrintM o) -> PrintM o
  treePrint = treePrint' . from1

class TreePrintable' a where
  treePrint' :: (PrintOut o) => a (PrintM o) -> PrintM o

instance (TreePrintable' a) => TreePrintable' (M1 i t a) where
  treePrint' (M1 x) = treePrint' x

instance (TreePrintable a) => TreePrintable' (Rec1 a) where
  treePrint' (Rec1 x) = treePrint x

instance (TreePrintable' a, TreePrintable' b) => TreePrintable' (a :+: b) where
  treePrint' (L1 x) = treePrint' x
  treePrint' (R1 x) = treePrint' x

instance PrintOut T.Text where
  ppunc    = id
  pliteral = id
  pindent  = T.indent

treeMPrint :: (TreePrintable a, Printable r) => a r -> PrintM T.Text
treeMPrint = treePrint . fmap mprint

mindent :: (PrintOut o) => PrintM o -> PrintM o
mindent = withIndent . fmap pindent

printFirstLine :: Printable a => [a] -> T.Text
printFirstLine []       = ""
printFirstLine (x : xs) = case T.firstLine xp of
  Nothing  -> xp <> printFirstLine xs
  Just xp' -> xp'
  where xp = pprint x
