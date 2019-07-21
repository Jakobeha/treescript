module TreeScript.Misc.Ext.List
  ( headOpt
  )
where

headOpt :: [a] -> Maybe a
headOpt []      = Nothing
headOpt (x : _) = Just x
