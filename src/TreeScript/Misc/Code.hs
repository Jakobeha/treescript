-- | Defines unparsed code blocks annotated with source language.
module TreeScript.Misc.Code
  ( Code (..)
  ) where

import qualified Data.Text as T

-- | Unparsed code annotated with source language.
data Code
  = Code
  { codeLangExt :: T.Text -- ^ Source language extension.
  , codeContent :: T.Text -- ^ Raw text.
  }
