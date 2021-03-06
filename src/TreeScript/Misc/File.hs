{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Misc.File
  ( File (..)
  , getRealAppDataDirectory
  , ifile
  , mkFile
  , loadFile
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.Info

-- | A self-contained source "file". Technically it doesn't have to be a real file, because it doesn't need a path - just a name and contents.
data File
  = File
  { fileName :: T.Text
  , fileContents :: T.Text
  } deriving (Eq, Ord, Read, Show)

-- | Fix for macOS, returns "~/Library/Application Support/<app name>"
getRealAppDataDirectory :: FilePath -> IO FilePath
getRealAppDataDirectory name
  | os == "darwin" = (</> ("Library" </> "Application Support" </> name)) <$> getHomeDirectory
  | otherwise = getAppUserDataDirectory name

ifileName :: T.Text
ifileName = "<interactive>"

-- | Creates a file for text in "interactive mode" (e.g. from a console).
ifile :: T.Text -> File
ifile contents
  = File
  { fileName = ifileName
  , fileContents = contents
  }

-- | Creates a file at the given path.
mkFile :: FilePath -> T.Text -> File
mkFile path contents
 = File
 { fileName = T.pack $ takeBaseName path
 , fileContents = contents
 }

-- | Reads the file with the given path.
loadFile :: FilePath -> IO File
loadFile path = mkFile path <$> T.readFile path
