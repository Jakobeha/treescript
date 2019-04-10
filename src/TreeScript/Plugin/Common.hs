{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Plugin.Common
  ( DeclSpec (..)
  ) where

import qualified Data.Text as T
import Data.Yaml

-- | Describes an individual node in a language's AST.
data DeclSpec
  = DeclSpec
  { declSpecName :: T.Text
  , declSpecNumArgs :: Int
  } deriving (Eq, Ord, Read, Show)

instance FromJSON DeclSpec where
  parseJSON = withObject "DeclSpec" $ \x -> DeclSpec
    <$> x .: "name"
    <*> x .: "args"
