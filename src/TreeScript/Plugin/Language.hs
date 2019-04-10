{-# LANGUAGE OverloadedStrings #-}

-- | Language plugins - allow the TreeScript compiler to create programs for specific languages.
module TreeScript.Plugin.Language
  ( LangSpec (..)
  , Language (..)
  ) where

import TreeScript.Plugin.CmdProgram
import TreeScript.Plugin.Common

import qualified Data.Text as T
import Data.Yaml

-- | Describes a language.
data LangSpec
  = LangSpec
  { langSpecName :: T.Text
  , langSpecExtension :: T.Text -- ^ Language's file extension.
  , langSpecNodes :: [DeclSpec] -- ^ Records provided for a language's AST.
  } deriving (Eq, Ord, Read, Show)

-- | Describes a language and provides programs to parse and print it.
data Language
  = Language
  { languageSpec :: LangSpec
  , languageParser :: CmdProgram
  , languagePrinter :: CmdProgram
  } deriving (Read, Show)

instance FromJSON LangSpec where
  parseJSON = withObject "LangSpec" $ \x -> LangSpec
    <$> x .: "name"
    <*> x .: "extension"
    <*> x .: "nodes"
