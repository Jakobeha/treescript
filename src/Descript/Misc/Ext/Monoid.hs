-- | Helpers for monoids.
module Descript.Misc.Ext.Monoid
  ( mintercalate
  ) where

import Data.List

-- | 'mconcat' @.@ 'intersperse'.
mintercalate :: (Monoid a) => a -> [a] -> a
mintercalate sep = mconcat . intersperse sep
