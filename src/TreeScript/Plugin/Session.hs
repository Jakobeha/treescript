{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Integrates plugins into compilation and other actions.
module TreeScript.Plugin.Session
  ( Settings (..)
  , SessionEnv (..)
  , Session
  , SessionRes
  , mkPluginUseError
  , getInitialEnv
  , getSessionEnv
  , runSessionResVirtual
  , runPreSessionRes
  , runSessionResReal
  , langWithExt
  , libraryWithName
  ) where

import TreeScript.Plugin.CmdProgram
import TreeScript.Plugin.Language
import TreeScript.Plugin.Library
import TreeScript.Misc

import Control.Monad.Logger hiding (LogLevel (..))
import qualified Control.Monad.Logger as L (LogLevel (..))
import Control.Monad.Reader
import Data.Char
import Data.List
import qualified Data.Text as T
import Data.Yaml
import qualified Filesystem.Path.CurrentOS as P
import qualified Shelly as S
import System.Directory
import System.FilePath

data LogLevel
  = LogLevelDebug
  | LogLevelWarning
  | LogLevelError
  deriving (Eq, Ord, Read, Show)

-- | General settings for the TreeScript compiler.
data Settings
  = Settings
  { settingsLogLevel :: LogLevel
  , settingsOverwriteWithDefault :: Bool
  } deriving (Eq, Ord, Read, Show)

-- | General global data for every session.
data SessionEnv
  = SessionEnv
  { sessionEnvSettings :: Settings
  , sessionEnvLanguages :: [Language]
  , sessionEnvLibraries :: [Library]
  }

type PreSessionRes a = forall r. ResultT (ReaderT r (LoggingT IO)) a

-- | A value computed using plugins.
type Session a = ReaderT SessionEnv (LoggingT IO) a

-- | A result computed using plugins.
type SessionRes a = ResultT (ReaderT SessionEnv (LoggingT IO)) a

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ parseStr . T.unpack
    where parseStr "debug" = pure LogLevelDebug
          parseStr "warning" = pure LogLevelWarning
          parseStr "error" = pure LogLevelError
          parseStr lvl = fail $ "invalid log level: " ++ lvl

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \x -> Settings
    <$> x .: "logLevel"
    <*> x .: "overwriteWithDefault"

logLevelToMonadLogLevel :: LogLevel -> L.LogLevel
logLevelToMonadLogLevel LogLevelDebug = L.LevelDebug
logLevelToMonadLogLevel LogLevelWarning = L.LevelWarn
logLevelToMonadLogLevel LogLevelError = L.LevelError

defaultSettings :: Settings
defaultSettings
  = Settings
  { settingsLogLevel = LogLevelDebug
  , settingsOverwriteWithDefault = True
  }

emptySessionEnv :: SessionEnv
emptySessionEnv
  = SessionEnv
  { sessionEnvSettings = defaultSettings
  , sessionEnvLanguages = []
  , sessionEnvLibraries = []
  }

mkPluginLoadError :: T.Text -> Error
mkPluginLoadError msg
  = Error
  { errorStage = StagePluginLoad
  , errorRange = Nothing
  , errorMsg = msg
  }

mkPluginUseError :: T.Text -> Error
mkPluginUseError msg
  = Error
  { errorStage = StagePluginUse
  , errorRange = Nothing
  , errorMsg = msg
  }

liftLoadIO :: IO a -> PreSessionRes a
liftLoadIO = liftIOAndCatch StagePluginLoad

setupInitialPlugins :: FilePath -> PreSessionRes ()
setupInitialPlugins pluginPath = do
  logDebugN "Setting up initial plugins."
  liftLoadIO $ S.shelly $ S.cp_r "resources/env" $ P.decodeString pluginPath

getRealPluginPath :: PreSessionRes FilePath
getRealPluginPath = do
  path <- liftLoadIO $ getRealAppDataDirectory "treescript"
  pluginsWereSetup <- liftLoadIO $ doesPathExist path
  unless pluginsWereSetup $ do
    logDebugN "Local plugins not created yet."
    setupInitialPlugins path
  pure path

validatePluginName :: String -> T.Text -> PreSessionRes ()
validatePluginName name actualName = do
  let expectedName
        = case name of
            [] -> T.empty
            (nameHead : nameTail) -> T.pack $ toUpper nameHead : nameTail
  when (expectedName /= actualName) $
    tellError $ mkPluginLoadError $ T.concat
      [ "from folder name, expected specification to be named '"
      , expectedName
      , "' but it's actually named '"
      , actualName
      , "'"
      ]

mkLanguage :: FilePath -> String -> PreSessionRes Language
mkLanguage pluginPath name = do
  let path = pluginPath </> name
      specPath = path </> "spec.yaml"
      parserPath = path </> "parser"
      printerPath = path </> "printer"
  specDecoded <- liftLoadIO $ decodeFileEither specPath
  spec <-
    case specDecoded of
      Left err
        -> mkFail $ mkPluginLoadError $ "bad specification - " <> T.pack (prettyPrintParseException err)
      Right res -> pure res
  validatePluginName name $ langSpecName spec
  pure Language
    { languageSpec = spec
    , languageParser
        = CmdProgram
        { cmdProgramStage = StagePluginUse
        , cmdProgramPath = parserPath
        }
    , languagePrinter
        = CmdProgram
        { cmdProgramStage = StagePluginUse
        , cmdProgramPath = printerPath
        }
    }

mkLibrary :: FilePath -> String -> PreSessionRes Library
mkLibrary pluginPath name = do
  let path = pluginPath </> name
      specPath = path </> "spec.yaml"
  specDecoded <- liftLoadIO $ decodeFileEither specPath
  spec <-
    case specDecoded of
      Left err
        -> mkFail $ mkPluginLoadError $ "bad specification - " <> T.pack (prettyPrintParseException err)
      Right res -> pure res
  validatePluginName name $ librarySpecName spec
  pure Library
    { librarySpec = spec
    , libraryDirName = T.pack name
    }

listDirPlugins :: FilePath -> PreSessionRes [String]
listDirPlugins dir = filter (not . isHidden) <$> liftLoadIO (listDirectory dir)
  where isHidden name = "." `isPrefixOf` name

getEnvAtPath :: FilePath -> PreSessionRes SessionEnv
getEnvAtPath pluginPath = do
  let settingsPath = pluginPath </> "settings.yaml"
      languagesPath = pluginPath </> "languages"
      librariesPath = pluginPath </> "libraries"
  settingsDecoded <- liftLoadIO $ decodeFileEither settingsPath
  settings <-
    case settingsDecoded of
      Left err -> do
        tellError $ mkPluginLoadError $ "bad settings - " <> T.pack (prettyPrintParseException err)
        pure defaultSettings
      Right res -> pure res
  languages <- traverseDropFatals (mkLanguage languagesPath) =<< listDirPlugins languagesPath
  libraries <- traverseDropFatals (mkLibrary librariesPath) =<< listDirPlugins librariesPath
  pure SessionEnv
    { sessionEnvSettings = settings
    , sessionEnvLanguages = languages
    , sessionEnvLibraries = libraries
    }

-- | Loads the environment which is shipped with this package.
getInitialEnv :: PreSessionRes SessionEnv
getInitialEnv = getEnvAtPath "resources/env"

-- | Loads the environment for the current user.
getRealEnv :: PreSessionRes SessionEnv
getRealEnv = do
  pluginPath <- getRealPluginPath
  env <- getEnvAtPath pluginPath
  if settingsOverwriteWithDefault $ sessionEnvSettings env then do
    logDebugN "Overwriting plugins with defaults - this was specified in settings."
    S.shelly $ S.rm_rf (P.decodeString pluginPath)
    setupInitialPlugins pluginPath
    getInitialEnv
  else
    pure env

getSessionEnv :: SessionRes SessionEnv
getSessionEnv = ask

-- | Logs info about the plugin environment, applies initial actions, and (TODO) warns if it's broken.
setupEnv :: SessionRes ()
setupEnv = do
  _ <- getSessionEnv
  logDebugN "Loaded session."

runSessionVirtualRaw :: SessionEnv -> Session a -> IO a
runSessionVirtualRaw env
  = runStdoutLoggingT
  . filterLogger (\_ lvl -> lvl >= envLvl)
  . (`runReaderT` env)
  where envLvl = logLevelToMonadLogLevel $ settingsLogLevel $ sessionEnvSettings env

-- | Evaluates a session result in a given environment. Useful for tests.
runSessionResVirtual :: SessionEnv -> SessionRes a -> IO (Result a)
runSessionResVirtual env session = runSessionVirtualRaw env $ runResultT $ do
  setupEnv
  session

-- | Evaluates a pre-session result (like session result but doesn't use any environment).
runPreSessionRes :: PreSessionRes a -> IO (Result a)
runPreSessionRes session = runSessionVirtualRaw emptySessionEnv $ runResultT session

-- | Evaluates a session result in the user's environment.
runSessionResReal :: SessionRes a -> IO (Result a)
runSessionResReal session = runPreSessionRes $ do
  env <- getRealEnv
  mapResultT (withReaderT $ \_ -> env) $ do
    setupEnv
    session

-- | Gets the language for the given extension in the session. Fails if no language found.
langWithExt :: Stage -> T.Text -> SessionRes Language
langWithExt stage ext = do
  langs <- sessionEnvLanguages <$> getSessionEnv
  case find (\lang -> langSpecExtension (languageSpec lang) == ext) langs of
    Nothing -> mkFail Error
      { errorStage = stage
      , errorRange = Nothing
      , errorMsg = "no (valid) plugin for language with extension '" <> ext <> "'"
      }
    Just res -> pure res

-- | Gets the library with the given name in the session. Fails if no language found.
libraryWithName :: Stage -> T.Text -> SessionRes Library
libraryWithName stage name = do
  libraries <- sessionEnvLibraries <$> getSessionEnv
  case find (\library -> librarySpecName (librarySpec library) == name) libraries of
    Nothing -> mkFail Error
      { errorStage = stage
      , errorRange = Nothing
      , errorMsg = "no (valid) plugin for library with name '" <> name <> "'"
      }
    Just res -> pure res
