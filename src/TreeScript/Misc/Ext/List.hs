module TreeScript.Misc.Ext.List
  ( headOpt
  , partitionTuple
  ) where

import TreeScript.Misc.Side

import Data.Bifunctor

headOpt :: [a] -> Maybe a
headOpt [] = Nothing
headOpt (x : _) = Just x

partitionTuple :: [(Side, a)] -> ([a], [a])
partitionTuple [] = ([], [])
partitionTuple ((SideLeft, x) : xs) = first (x :) $ partitionTuple xs
partitionTuple ((SideRight, x) : xs) = second (x :) $ partitionTuple xs
