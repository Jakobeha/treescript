module Descript.Misc.Ext.Applicative
  ( pure2
  , pure3
  , (<<*>>)
  , (<<<*>>>) ) where

-- | 'pure' for 2 nested applicatives.
pure2 :: (Applicative wa, Applicative wb) => a -> wa (wb a)
pure2 = pure . pure

-- | 'pure' for 3 nested applicatives.
pure3 :: (Applicative wa, Applicative wb, Applicative wc) => a -> wa (wb (wc a))
pure3 = pure2 . pure

-- | '<*>' for 2 nested applicatives.
infixr 3 <<*>>
(<<*>>) :: (Applicative wa, Applicative wb) => wa (wb (a -> b)) -> wa (wb a) -> wa (wb b)
f <<*>> x = (<*>) <$> f <*> x

-- | '<*>' for 3 nested applicatives.
infixr 3 <<<*>>>
(<<<*>>>) :: (Applicative wa, Applicative wb, Applicative wc) => wa (wb (wc (a -> b))) -> wa (wb (wc a)) -> wa (wb (wc b))
f <<<*>>> x = (<<*>>) <$> f <*> x
