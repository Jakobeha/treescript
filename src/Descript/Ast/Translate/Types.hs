{-# LANGUAGE OverloadedStrings #-}

-- | Types to represent the generated C code.
module Descript.Ast.Translate.Types
  ( module Descript.Ast.Translate.Types
  ) where

import Descript.Misc

import qualified Data.Text as T

-- | All the generated C code for a program.
data Translated
  = Translated
  { translatedNumProps :: T.Text
  , translatedReduce :: T.Text
  }

instance Printable Translated where
  pprint (Translated numProps reduce)
     = "// \\get_record_num_props\n"
    <> numProps
    <> "\n\n// \\reduce\n"
    <> reduce
