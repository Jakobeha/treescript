{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Parsed nodes.
module TreeScript.Ast.Ann
  ( module TreeScript.Ast.Ann
  )
where

import           TreeScript.Ast.Type
import           TreeScript.Misc

data NoAnn (r :: StxType) = NoAnn
newtype SrcAnn (r :: StxType) = SrcAnn{ srcAnn :: SrcInfo }
