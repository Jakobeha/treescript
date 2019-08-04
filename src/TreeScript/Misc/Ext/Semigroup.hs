-- | Helpers for semigroups and monoids.
module TreeScript.Misc.Ext.Semigroup
  ( (<<>>)
  )
where

import           Control.Applicative

infixr 6 <<>>
(<<>>) :: (Applicative w, Semigroup a) => w a -> w a -> w a
(<<>>) = liftA2 (<>)
