-- | Defines unparsed code blocks annotated with source language.
module Descript.Misc.Code
  ( Code (..)
  ) where

import qualified Data.Text as T

-- | Unparsed code annotated with source language.
data Code
  = Code
  { codeLanguage :: T.Text -- ^ Source language
  , codeContent :: T.Text -- ^ Raw text
  }
