module TreeScript.Misc.Ext.List
  ( headOpt
  , splitAtFail
  )
where

import           Data.Bifunctor

headOpt :: [a] -> Maybe a
headOpt []      = Nothing
headOpt (x : _) = Just x

splitAtFail :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFail _ [] = ([], [])
splitAtFail prd (x : xs) | prd x     = first (x :) $ splitAtFail prd xs
                         | otherwise = ([], x : xs)
