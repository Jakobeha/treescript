{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parsed nodes.
module TreeScript.Ast.Ann
  ( module TreeScript.Ast.Ann
  )
where

import           TreeScript.Ast.Type
import           TreeScript.Misc

data NoAnn (r :: StxType) = NoAnn

-- | Cons annotation
data (a :@: b) (r :: StxType) = a r :@: b r

newtype SrcAnn (r :: StxType) = SrcAnn{ srcAnn :: SrcInfo }

class AnnHas a (r :: StxType -> *) | r -> a where
  fromAnn :: r t -> a

instance AnnHas SrcInfo SrcAnn where
  fromAnn = srcAnn

instance AnnHas SrcInfo (SrcAnn :@: rs) where
  fromAnn (x :@: _) = srcAnn x

instance (AnnHas a rs) => AnnHas a (r :@: rs) where
  fromAnn (_ :@: xs) = fromAnn xs
