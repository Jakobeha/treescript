{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Misc.Ext.Directory
  ( getRealAppDataDirectory
  )
where

import           System.Directory
import           System.FilePath
import           System.Info

-- | Fix for macOS, returns "~/Library/Application Support/<app name>"
getRealAppDataDirectory :: FilePath -> IO FilePath
getRealAppDataDirectory name
  | os == "darwin"
  = (</> ("Library" </> "Application Support" </> name)) <$> getHomeDirectory
  | otherwise
  = getAppUserDataDirectory name
