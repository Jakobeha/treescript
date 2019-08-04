{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Printing classes, from debug-freindly "pretty-printing" to production AST reprints.
module TreeScript.Print.PrintM
  ( PrintM
  , runPrintM
  , putIndent
  , indentM
  , unindentM
  , withIndent
  )
where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.String

-- | Print monad: keeps track of indentation level
newtype PrintM a = PrintM{ unPrintM :: State Int a } deriving (Functor, Applicative, Monad)

instance (IsString a) => IsString (PrintM a) where
  fromString = pure . fromString

instance (Semigroup a) => Semigroup (PrintM a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (PrintM a) where
  mempty = pure mempty

runPrintM :: PrintM a -> a
runPrintM = (`evalState` 0) . unPrintM

putIndent :: Int -> PrintM ()
putIndent = PrintM . put

indentM :: PrintM ()
indentM = PrintM $ modify succ

unindentM :: PrintM ()
unindentM = PrintM $ modify pred

withIndent :: PrintM a -> PrintM a
withIndent = PrintM . withState (+ 1) . unPrintM
