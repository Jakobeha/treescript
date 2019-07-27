{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Printing classes, from debug-freindly "pretty-printing" to production AST reprints.
module TreeScript.Print.Class
  ( module TreeScript.Print.Class
  )
where

import qualified TreeScript.Misc.Ext.Text      as T

import           Control.Monad.State.Strict
import           Data.String
import qualified Data.Text                     as T
import           GHC.Generics

-- | Print monad: keeps track of indentation level
type PrintM = State Int

-- | Get a "user-friendly" description: reasonable to put in a message shown to the user.
-- Depending on the implementation, this could be for debugging or the actual AST reprint used in production.
class Printable a where
  {-# MINIMAL pprint | mprint #-}

  pprint :: a -> T.Text
  pprint = (`evalState` 0) . mprint
  mprint :: a -> PrintM T.Text
  mprint = pure . pprint

class AnnPrintable an where
  printAnnd :: PrintM T.Text -> an -> PrintM T.Text

-- | Abstract output type which trees can be printed into. Usually just text, but can also be a patch or "smart" type.
--
-- __The 'IsString' / 'fromString' and 'fromLiteral' are different - they have similar types, but the /interpretation/ of the text being converted is different:__
-- - 'fromString' converts punctuation (e.g. separators, delimiters) where the content of the text may not be significant beyond helping the parser.
-- - 'fromLiteral' converts (e.g. symbols, data already printed and then flattened) where the content of the text is significant.
-- Basic implementations, like 'Text', handle these both the same, but patches wouldn't.
class (IsString a, Monoid a) => PrintOut a where
  -- | Convert raw text into this format.
  fromLiteral :: T.Text -> a
  pindent :: a -> a

-- | Print AST nodes for AST rewriting
class (Functor a) => TreePrintable a where
  treePrint :: (PrintOut o) => a (PrintM o) -> PrintM o
  -- | Default implementation for sum types
  default treePrint :: (Generic1 a, PrintOut o, TreePrintable' (Rep1 a)) => a (PrintM o) -> PrintM o
  treePrint = treePrint' . from1

class TreePrintable' a where
  treePrint' :: (PrintOut o) => a (PrintM o) -> PrintM o

instance (TreePrintable a) => TreePrintable' (Rec1 a) where
  treePrint' (Rec1 x) = treePrint x

instance (TreePrintable' a, TreePrintable' b) => TreePrintable' (a :+: b) where
  treePrint' (L1 x) = treePrint' x
  treePrint' (R1 x) = treePrint' x

instance PrintOut T.Text where
  fromLiteral = id
  pindent     = T.indent

treeMPrint :: (TreePrintable a, Printable r) => a r -> PrintM T.Text
treeMPrint = treePrint . fmap mprint

mindent :: (PrintOut o) => PrintM o -> PrintM o
mindent = withState (+ 1) . fmap pindent
