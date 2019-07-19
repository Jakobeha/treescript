-- | Bind equality
module TreeScript.Ast.Core.Types.BindEq
  ( BindEq(..)
  )
where

-- | Bind equality
class BindEq a where
  -- | Bind equality
  (=$=) :: a -> a -> Bool
