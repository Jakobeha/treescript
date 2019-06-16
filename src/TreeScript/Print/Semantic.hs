{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, TypeOperators #-}
-- | Derived from semantic's 'Parsing.TreeSitter'.
module TreeScript.Print.Semantic () where
{-  ( value2Semantic
  )
where

import           TreeScript.Ast

import qualified Control.Exception             as Exc
                                                ( bracket )
import           Data.ByteString.Unsafe         ( unsafeUseAsCStringLen )
import           Foreign
import           Foreign.C.Types                ( CBool(..) )
import           Foreign.Marshal.Array          ( allocaArray )
import           Data.AST                       ( AST
                                                , Node(Node)
                                                )
import           Data.Blob
import           Data.Duration
import           Data.Location
import           Data.Source
import           Data.Span
import           Data.Term
import           Prologue                hiding ( bracket )

-- | Converts an untyped value back into the specified grammar via the specified enum.
value2Semantic :: (Enum grammar, Bounded grammar, ConstructorName grammar) => Value an -> IO grammar
value2Semantic = anaM toAst

toAst
  :: forall grammar an
   . (NamedEnum grammar)
  => Value an
  -> Base (AST [] grammar) (Value an)
toAst (ValuePrimitive prim) = undefined
toAst (ValueRecord (Record _ head' props)) =
  In (Node $ enumFromConstructor nodeSymbol) (Location emptyRange emptySpan)
toAst (ValueBind _) = error "can't convert binds into semantic values"

anaM
  :: (Corecursive t, Monad m, Traversable (Base t))
  => (a -> m (Base t a))
  -> a
  -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g
-}