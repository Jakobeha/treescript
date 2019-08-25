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

import           Data.String
import qualified Data.Text                     as T
import           GHC.Generics

-- | Get a "user-friendly" description: reasonable to put in a message shown to the user.
-- Depending on the implementation, this could be for debugging or the actual AST reprint used in production.
class Printable a where
  pprint :: a -> T.Text

-- | Print AST nodes for AST rewriting
class AstPrintable a where
  printAst :: (PrintOut o) => a -> o
  -- | Default implementation for sum types
  default printAst :: (Generic a, GAstPrintable (Rep a), PrintOut o) => a -> o
  printAst = gprintAst . from

class AnnPrintable (an :: k -> *) where
  printAnnd :: (PrintOut o) => an x -> o -> o

class GAstPrintable a where
  gprintAst :: (PrintOut o) => a x -> o

-- | Abstract output type which trees can be printed into. Usually just text, but can also be a patch or "smart" type.
--
-- - 'fromString' / `ppunc` converts punctuation (e.g. separators, delimiters) where the content of the text may not be significant beyond helping the parser.
-- - 'pliteral' converts (e.g. symbols, data already printed and then flattened) where the content of the text is significant.
-- Basic implementations, like 'Text', handle these both the same, but patches wouldn't.
class (IsString o, Monoid o) => PrintOut o where
  -- | Punctuation (e.g. separators, delimiters)
  ppunc :: T.Text -> o
  -- | Significant text, e.g. symbols
  pliteral :: T.Text -> o
  -- | From annotation
  pann :: T.Text -> o
  pindent :: o -> o

instance (GAstPrintable a) => GAstPrintable (M1 i t a) where
  gprintAst (M1 x) = gprintAst x

instance (AstPrintable a) => GAstPrintable (K1 i a) where
  gprintAst (K1 x) = printAst x

instance (GAstPrintable a, GAstPrintable b) => GAstPrintable (a :+: b) where
  gprintAst (L1 x) = gprintAst x
  gprintAst (R1 x) = gprintAst x

instance PrintOut T.Text where
  ppunc    = id
  pliteral = id
  pann     = id
  pindent  = T.indent

printFirstLine :: Printable a => [a] -> T.Text
printFirstLine []       = ""
printFirstLine (x : xs) = case T.firstLine xp of
  Nothing  -> xp <> printFirstLine xs
  Just xp' -> xp'
  where xp = pprint x
