-- | Helpers for 'Maybe'.
module Descript.Misc.Ext.Maybe
  ( justIf
  ) where

-- | 'Nothing' if the predicate is false.
justIf :: Bool -> a -> Maybe a
justIf False _ = Nothing
justIf True x = Just x
