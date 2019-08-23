{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
  pprint :: a -> T.Text

class AnnPrintable (an :: k -> *) where
  printAnnd :: (PrintOut o) => an x -> PrintM o -> PrintM o

-- | Abstract output type which trees can be printed into. Usually just text, but can also be a patch or "smart" type.
--
-- - 'fromString' / `ppunc` converts punctuation (e.g. separators, delimiters) where the content of the text may not be significant beyond helping the parser.
-- - 'pliteral' converts (e.g. symbols, data already printed and then flattened) where the content of the text is significant.
-- Basic implementations, like 'Text', handle these both the same, but patches wouldn't.
class (IsString a, Monoid a) => PrintOut a where
  -- | Punctuation (e.g. separators, delimiters)
  ppunc :: T.Text -> PrintM a
  -- | Significant text, e.g. symbols
  pliteral :: T.Text -> PrintM a
  -- | From annotation
  pann :: T.Text -> PrintM a
  pann = ppunc

-- | Print AST nodes for AST rewriting
class AstPrintable a where
  mprintAst :: (PrintOut o) => a -> PrintM o
  -- | Default implementation for sum types
  default mprintAst :: (Generic a, GAstPrintable (Rep a), PrintOut o) => a -> PrintM o
  mprintAst = gmprintAst . from

class GAstPrintable a where
  gmprintAst :: (PrintOut o) => a x -> PrintM o

instance (GAstPrintable a) => GAstPrintable (M1 i t a) where
  gmprintAst (M1 x) = gmprintAst x

instance (AstPrintable a) => GAstPrintable (K1 i a) where
  gmprintAst (K1 x) = mprintAst x

instance (GAstPrintable a, GAstPrintable b) => GAstPrintable (a :+: b) where
  gmprintAst (L1 x) = gmprintAst x
  gmprintAst (R1 x) = gmprintAst x

instance PrintOut T.Text where
  ppunc    = apIndentText
  pliteral = apIndentText

pprintAst :: (AstPrintable a) => a -> T.Text
pprintAst = runPrintM . mprintAst

printFirstLine :: Printable a => [a] -> T.Text
printFirstLine []       = ""
printFirstLine (x : xs) = case T.firstLine xp of
  Nothing  -> xp <> printFirstLine xs
  Just xp' -> xp'
  where xp = pprint x
