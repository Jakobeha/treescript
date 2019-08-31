module TreeScript.Misc.Ext.List
  ( headOpt
  , unconsLast
  , splitAtFail
  , lookupReplace
  )
where

import           Data.Bifunctor

headOpt :: [a] -> Maybe a
headOpt []      = Nothing
headOpt (x : _) = Just x

unconsLast :: [a] -> Maybe ([a], a)
unconsLast []       = Nothing
unconsLast [x     ] = Just ([], x)
unconsLast (x : xs) = (first (x :)) <$> unconsLast xs

splitAtFail :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFail _ [] = ([], [])
splitAtFail prd (x : xs) | prd x     = first (x :) $ splitAtFail prd xs
                         | otherwise = ([], x : xs)

lookupReplace :: (Eq k) => k -> v -> [(k, v)] -> Maybe [(k, v)]
lookupReplace _ _ [] = Nothing
lookupReplace key val ((key', val') : xs)
  | key == key' = Just $ (key, val) : xs
  | otherwise   = ((key', val') :) <$> lookupReplace key val xs
