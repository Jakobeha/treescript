-- | Helpers for semigroups and monoids.
module TreeScript.Misc.Ext.Monoid
  ( Max0 (..)
  , mintercalate
  ) where

import Data.List

-- | 'Max', where 0 is the empty element.
newtype Max0 = Max0{ getMax0 :: Int }

instance Semigroup Max0 where
  Max0 x <> Max0 y = Max0 $ max x y

instance Monoid Max0 where
  mempty = Max0 0

-- | 'mconcat' @.@ 'intersperse'.
mintercalate :: (Monoid a) => a -> [a] -> a
mintercalate sep = mconcat . intersperse sep
