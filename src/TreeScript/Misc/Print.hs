{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- | Get a user-friendly description.
module TreeScript.Misc.Print
  ( Printable (..)
  , ReducePrintable (..)
  , PrintOut (..)
  , LeafPrintable
  , TreePrintable (..)
  ) where

import TreeScript.Misc.Ann
import qualified TreeScript.Misc.Ext.Text as T

import Control.Monad.Catch
import Data.String
import qualified Data.Text as T

newtype ReducePrintText = ReducePrintText{ reducePrintText :: T.Text } deriving (Semigroup, Monoid)

-- | Can get a user-friendly description. Unlike 'Show', doesn't need to contain all information and be readable.
class Printable a where
  -- | Get a user-friendly description.
  pprint :: a -> T.Text
  default pprint :: (a ~ a2 an, TreePrintable a2) => a -> T.Text
  pprint = treePPrint

-- | Can get a description which is a subsequence of what was read. This is for ASTs.
class ReducePrintable a where
  -- | Get a description which is a subsequence of what was read. This is for ASTs.
  reducePrint :: a -> T.Text
  default reducePrint :: (a ~ a2 an, TreePrintable a2) => a -> T.Text
  reducePrint = treeReducePrint

-- | Abstract output type which trees can be printed into. Usually just text, but can also be a patch or "smart" type.
--
-- __The 'IsString' / 'fromString' and 'fromLiteral' are different - they have similar types, but the /interpretation/ of the text being converted is different:__
-- - 'fromString' converts punctuation (e.g. separators, delimiters) where the content of the text may not be significant beyond helping the parser.
-- - 'fromLiteral' converts (e.g. symbols, data already printed and then flattened) where the content of the text is significant.
-- Basic implementations, like 'Text', handle these both the same, but patches wouldn't.
class (IsString a, Monoid a) => PrintOut a where
  -- | Convert raw text into this format.
  fromLiteral :: T.Text -> a

-- | A leaf of a printable annotated tree. This class implements many functions for printing the leaf many ways. Then, the entire tree only needs one function to print each of these ways.
class (Printable a, ReducePrintable a) => LeafPrintable a

-- | A printable annotated tree. All leaves must implement 'LeafPrintable' and be printable in a few formats. Additionally, nodes must support annotations. This class only needs to implement one method, to recursively print in any of these formats, and allow annotations to alter printing.
class (Annotatable a) => TreePrintable a where
  -- | Prints the tree
  treePrint :: (PrintOut o) -- ^ Type of format of the print. Would require a proxy, but the type can be inferred by the recursive printers.
            => (forall p. (TreePrintable p) => p an -> o) -- ^ Function to recursively print subtrees (not leaves).
            -> (forall l. (LeafPrintable l) => l -> o) -- ^ Function to recursively print leaves.
            -> a an
            -> o

instance IsString ReducePrintText where
  fromString x = ReducePrintText $ T.strip $ T.pack x

instance Printable () where
  pprint () = "()"

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

instance ReducePrintable Char where
  reducePrint = T.singleton

instance ReducePrintable Int where
  reducePrint = pprint

instance ReducePrintable Float where
  reducePrint = pprint

instance ReducePrintable T.Text where
  reducePrint = id

instance (ReducePrintable a) => ReducePrintable [a] where
  reducePrint = T.concat . map reducePrint

instance PrintOut T.Text where
  fromLiteral = id

instance PrintOut ReducePrintText where
  fromLiteral = ReducePrintText

instance LeafPrintable Char

instance LeafPrintable Int

instance LeafPrintable Float

instance LeafPrintable T.Text

treePPrint :: (TreePrintable a) => a an -> T.Text
treePPrint = treePrint treePPrint pprint

treeReducePrint :: (TreePrintable a) => a an -> T.Text
treeReducePrint
  = reducePrintText
  . treePrint (ReducePrintText . treeReducePrint) (ReducePrintText . reducePrint)
