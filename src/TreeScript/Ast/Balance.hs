{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parsed nodes.
module TreeScript.Ast.Balance
  ( module TreeScript.Ast.Balance
  )
where

import           TreeScript.Ast.Lex
import           TreeScript.Misc

import           Data.String
import           GHC.Generics

data Balance an
  = BalanceAtom Atom
  | BalanceEnc EncType [ABalance an]
  deriving (Eq, Ord, Read, Show, Generic)

type ABalance an = Annd (Balance an) an

type SBalance = ABalance SrcAnn

  -- | A full TreeScript program.
newtype BalanceProgram an = BalanceProgram{ unBalanceProgram :: [ABalance an] } deriving (Eq, Ord, Read, Show, Generic)

instance Functor Balance where
  fmap _ (BalanceAtom atm  ) = BalanceAtom atm
  fmap f (BalanceEnc enc xs) = BalanceEnc enc $ map mapInner xs
    where mapInner (Annd ann x) = Annd (f ann) $ f <$> x

instance Foldable Balance where
  foldMap _ (BalanceAtom _  ) = mempty
  foldMap f (BalanceEnc _ xs) = foldMap foldInner xs
    where foldInner (Annd ann x) = f ann <> foldMap f x

instance Traversable Balance where
  traverse _ (BalanceAtom atm  ) = pure $ BalanceAtom atm
  traverse f (BalanceEnc enc xs) = BalanceEnc enc <$> traverse traverseInner xs
    where traverseInner (Annd ann x) = Annd <$> f ann <*> traverse f x
