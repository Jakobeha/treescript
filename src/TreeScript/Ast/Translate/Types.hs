{-# LANGUAGE OverloadedStrings #-}

-- | Types to represent the generated C code.
module TreeScript.Ast.Translate.Types
  ( module TreeScript.Ast.Translate.Types
  ) where

import TreeScript.Misc

import qualified Data.Text as T

-- | All the generated C code for a program.
data Translated
  = Translated
  { translatedNumProps :: T.Text
  , translatedReduceSurface :: T.Text
  }

instance Printable Translated where
  pprint (Translated numProps reduceSurface)
     = "// \\get_record_num_props\n"
    <> numProps
    <> "\n\n// \\reduce_surface\n"
    <> reduceSurface
