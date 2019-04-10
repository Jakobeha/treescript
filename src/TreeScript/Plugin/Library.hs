{-# LANGUAGE OverloadedStrings #-}

-- | Library plugins - allow TreeScript programs to call extra functions.
module TreeScript.Plugin.Library
  ( LibrarySpec (..)
  , Library (..)
  ) where

import TreeScript.Plugin.Common

import qualified Data.Text as T
import Data.Yaml

-- | Describes a library's capabilities.
data LibrarySpec
  = LibrarySpec
  { librarySpecName :: T.Text
  , librarySpecRecords :: [DeclSpec]
  , librarySpecFunctions :: [DeclSpec]
  } deriving (Eq, Ord, Read, Show)

-- | Describes a library and provides its code.
data Library
  = Library
  { librarySpec :: LibrarySpec
  , libraryDirName :: T.Text
  } deriving (Read, Show)

instance FromJSON LibrarySpec where
  parseJSON = withObject "LibrarySpec" $ \x -> LibrarySpec
    <$> x .: "name"
    <*> x .: "records"
    <*> x .: "functions"
