{-# LANGUAGE OverloadedStrings #-}

-- | Types to represent the generated C code.
module TreeScript.Ast.Translate.Types
  ( module TreeScript.Ast.Translate.Types
  ) where

import TreeScript.Misc

import qualified Data.Text as T

-- | The source and destination for a copy operation.
data CopyInfo
  = CopyInfo
  { copyInfoSource :: FilePath
  , copyInfoDestination :: FilePath
  }

-- | All the generated C code for a program.
data Translated
  = Translated
  { translatedLibCopies :: [CopyInfo]
  , translatedLibImports :: T.Text
  , translatedMaxNumBinds :: T.Text
  , translatedNumProps :: T.Text
  , translatedMainReduceSurface :: T.Text
  , translatedExtraReduceSurfaces :: T.Text
  }

instance Printable Translated where
  pprint (Translated _ libImports maxNumBinds numProps mainReduceSurface extraReduceSurfaces)
     = "// \\lib_imports"
    <> libImports
    <> "\n\n// \\max_num_binds\n"
    <> maxNumBinds
    <> "\n\n// \\get_record_num_props\n"
    <> numProps
    <> "\n\n// \\reduce_main\n"
    <> mainReduceSurface
    <> "\n\n// \\reduce_extras\n"
    <> extraReduceSurfaces
