{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
-- | Derived from 'module Serializing.SExpression' from semantic, converts a typed semantic AST into an untyped value
module TreeScript.Parse.Semantic
  ( semantic2Value
  )
where

import           TreeScript.Ast

import           Analysis.ConstructorName
import qualified Data.Text                     as T
import           Prelude
import           Prologue

semantic2Value
  :: (Recursive t, ConstructorName (Base t), Foldable (Base t)) => t -> Value ()
semantic2Value = cata branch

branch
  :: (ConstructorName syntax, Foldable syntax) => syntax (Value ()) -> Value ()
branch stx = ValueRecord Record
  { recordAnn   = ()
  , recordHead  = mkLangSymbol $ T.pack $ constructorName stx
  , recordProps = toList stx
  }
