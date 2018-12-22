-- | Get a user-friendly description.
module Descript.Misc.Print
  ( Printable (..)
  ) where

import qualified Data.Text as T

-- | Can get a user-friendly description. Unlike 'Show', doesn't need to contain all information and be readable.
class Printable a where
  -- | Get a user-friendly description.
  pprint :: a -> T.Text

instance Printable Int where
  pprint = T.pack . show

instance Printable Char where
  pprint = T.pack . show

instance Printable Float where
  pprint = T.pack . show
