{-# LANGUAGE OverloadedStrings #-}

-- | Server plugins - allow Descript programs to call extra functions.
module Descript.Plugin.Server
  ( ServerSpec (..)
  ) where

import qualified Data.Text as T
import Data.Yaml

-- | Describes a server's capabilities.
data ServerSpec
  = ServerSpec
  { serverSpecName :: T.Text
  , serverSpecFunctions :: [T.Text]
  } deriving (Eq, Ord, Read, Show)

instance FromJSON ServerSpec where
  parseJSON = withObject "ServerSpec" $ \x -> ServerSpec
    <$> x .: "name"
    <*> x .: "functions"
