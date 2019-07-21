{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- | Printing classes, from debug-freindly "pretty-printing" to production AST reprints.
module TreeScript.Misc.Print
  ( Printable(..)
  , AnnPrintable(..)
  , PrintOut(..)
  , LeafPrintable(..)
  , TreePrintable(..)
  , treePPrint
  )
where

import           TreeScript.Misc.Ann
import           TreeScript.Misc.Loc
import qualified TreeScript.Misc.Ext.Text      as T

import           Control.Monad.Catch
import           Control.Monad.State.Strict
import           Data.String
import qualified Data.Text                     as T

-- | Print monad: keeps track of indentation level
type PrintM = State Int

-- | Get a "user-friendly" description: reasonable to put in a message shown to the user.
-- Depending on the implementation, this could be for debugging or the actual AST reprint used in production.
class Printable a where
  pprint :: a -> T.Text
  default pprint :: (a ~ a2 an, TreePrintable a2, AnnPrintable an) => a -> T.Text
  pprint = treePPrint

class AnnPrintable an where
  printAnnd :: T.Text -> an -> T.Text

-- | Abstract output type which trees can be printed into. Usually just text, but can also be a patch or "smart" type.
--
-- __The 'IsString' / 'fromString' and 'fromLiteral' are different - they have similar types, but the /interpretation/ of the text being converted is different:__
-- - 'fromString' converts punctuation (e.g. separators, delimiters) where the content of the text may not be significant beyond helping the parser.
-- - 'fromLiteral' converts (e.g. symbols, data already printed and then flattened) where the content of the text is significant.
-- Basic implementations, like 'Text', handle these both the same, but patches wouldn't.
class (IsString a, Monoid a) => PrintOut a where
  -- | Convert raw text into this format.
  fromLiteral :: T.Text -> a

-- | Print atoms in AST elements (e.g. numbers) for AST rewriting
class LeafPrintable a where
  leafPrint :: a -> T.Text
  default leafPrint :: (Printable a) => a -> T.Text
  leafPrint = pprint

-- | Print AST nodes for AST rewriting
class (Annotatable a) => TreePrintable a where
  treePrint
    :: (PrintOut o, AnnPrintable an)
    => (forall p . (TreePrintable p) => p an -> PrintM o) -- ^ Function to recursively print nodes.
    -> (forall l . (LeafPrintable l) => l -> PrintM o) -- ^ Function to recursively print leaves.
    -> a an
    -> PrintM o

instance Printable () where
  pprint () = "()"

instance Printable Bool where
  pprint = T.pack . show

instance Printable Char where
  pprint = T.pack . show

instance Printable Int where
  pprint = T.pack . show

instance Printable Float where
  pprint = T.pack . show

instance Printable T.Text where
  pprint txt = "\"" <> T.escapeString txt <> "\""

instance Printable SomeException where
  pprint = T.pack . displayException

instance (Printable a) => Printable [a] where
  pprint xs = "[" <> T.intercalate ", " (map pprint xs) <> "]"

instance Printable Loc where
  pprint loc = pprint (locLine loc) <> ":" <> pprint (locColumn loc)

instance Printable Range where
  pprint (Range start end) = pprint start <> "-" <> pprint end

instance AnnPrintable SrcAnn where
  printAnnd _ = srcAnnText

instance (AnnPrintable a) => AnnPrintable (Maybe a) where
  printAnnd txt Nothing  = txt
  printAnnd txt (Just x) = printAnnd txt x

instance PrintOut T.Text where
  fromLiteral = id

instance LeafPrintable Char

instance LeafPrintable Int

instance LeafPrintable Float

instance LeafPrintable T.Text

treePPrint' :: (TreePrintable a, AnnPrintable an) => a an -> PrintM T.Text
treePPrint' x =
  (`printAnnd` getAnn x) <$> treePrint treePPrint' (pure . leafPrint) x

treePPrint :: (TreePrintable a, AnnPrintable an) => a an -> T.Text
treePPrint x = evalState (treePPrint' x) 0
