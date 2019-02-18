{-# LANGUAGE OverloadedStrings #-}

-- | Library plugins - allow TreeScript programs to call extra functions.
module TreeScript.Plugin.Library
  ( FunctionSpec (..)
  , LibrarySpec (..)
  , Library (..)
  ) where

import qualified Data.Text as T
import Data.Yaml

-- | Describes an individual function.
data FunctionSpec
  = FunctionSpec
  { functionSpecName :: T.Text
  , functionSpecNumArgs :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Describes a library's capabilities.
data LibrarySpec
  = LibrarySpec
  { librarySpecName :: T.Text
  , librarySpecFunctions :: [FunctionSpec]
  } deriving (Eq, Ord, Read, Show)

-- | Describes a library and provides its code.
data Library
  = Library
  { librarySpec :: LibrarySpec
  , libraryDirName :: T.Text
  } deriving (Read, Show)

instance FromJSON FunctionSpec where
  parseJSON = withObject "FunctionSpec" $ \x -> FunctionSpec
    <$> x .: "name"
    <*> x .: "args"

instance FromJSON LibrarySpec where
  parseJSON = withObject "LibrarySpec" $ \x -> LibrarySpec
    <$> x .: "name"
    <*> x .: "functions"
