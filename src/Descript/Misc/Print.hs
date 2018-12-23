-- | Get a user-friendly description.
module Descript.Misc.Print
  ( Printable (..)
  , ReducePrintable (..)
  ) where

import qualified Descript.Misc.Ext.Text as T
import qualified Data.Text as T

-- | Can get a user-friendly description. Unlike 'Show', doesn't need to contain all information and be readable.
class Printable a where
  -- | Get a user-friendly description.
  pprint :: a -> T.Text

-- | Can get a description which is a subsequence of what was read. This is for ASTs.
class ReducePrintable a where
  -- | Get a description which is a subsequence of what was read. This is for ASTs.
  reducePrint :: a -> T.Text

instance Printable Char where
  pprint = T.pack . show

instance Printable Int where
  pprint = T.pack . show

instance Printable Float where
  pprint = T.pack . show

instance Printable T.Text where
  pprint = T.escapeString

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
