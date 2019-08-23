{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Printing classes, from debug-freindly "pretty-printing" to production AST reprints.
module TreeScript.Print.PrintM
  ( PrintM
  , runPrintM
  , putIndent
  , indentM
  , unindentM
  , pindent
  , apIndentText
  )
where

import qualified TreeScript.Misc.Ext.Text      as T

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.String
import qualified Data.Text                     as T

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

pindent :: PrintM a -> PrintM a
pindent = PrintM . withState (+ 1) . unPrintM

apIndentText :: T.Text -> PrintM T.Text
apIndentText txt = PrintM $ (`T.indentn` txt) <$> get
