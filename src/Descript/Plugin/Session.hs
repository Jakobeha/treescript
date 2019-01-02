{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Integrates plugins into compilation and other actions.
module Descript.Plugin.Session
  ( Settings (..)
  , SessionEnv (..)
  , Session
  , SessionRes
  , getInitialEnv
  , getSessionEnv
  , runSessionResVirtual
  , runPreSessionRes
  , runSessionResReal
  ) where

import Descript.Plugin.CmdProgram
import Descript.Plugin.Language
import Descript.Plugin.Server
import Descript.Misc

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

-- | General settings for the Descript compiler.
data Settings
  = Settings
  { settingsLogLevel :: LogLevel
  , settingsOverwriteWithDefault :: Bool
  } deriving (Eq, Ord, Read, Show)

-- | General global data for every session.
data SessionEnv
  = SessionEnv
  { sessionEnvSettings :: Settings
  , sessionEnvLanguages :: [Language] -- ^ Languages with plugins.
  , sessionEnvServers :: [ServerSpec] -- ^ Servers (just specifications).
  , sessionEnvTemplateDir :: FilePath
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
  , sessionEnvServers = []
  , sessionEnvTemplateDir = ""
  }

setupInitialPlugins :: FilePath -> PreSessionRes ()
setupInitialPlugins pluginPath = do
  logDebugN "Setting up initial plugins."
  liftIO $ S.shelly $ S.cp_r "resources" (P.decodeString pluginPath)

getRealPluginPath :: PreSessionRes FilePath
getRealPluginPath = do
  path <- liftIO $ getAppUserDataDirectory "descript"
  pluginsWereSetup <- liftIO $ doesPathExist path
  unless pluginsWereSetup $ do
    logDebugN "Local plugins not created yet."
    setupInitialPlugins path
  pure path

mkError :: T.Text -> Error
mkError msg
  = Error
  { errorStage = StagePluginLoad
  , errorRange = Nothing
  , errorMsg = msg
  }

validatePluginName :: String -> T.Text -> PreSessionRes ()
validatePluginName name actualName = do
  let expectedName
        = case name of
            [] -> T.empty
            (nameHead : nameTail) -> T.pack $ toUpper nameHead : nameTail
  when (expectedName /= actualName) $
    tellError $ mkError $ T.concat
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
  specDecoded <- liftIO $ decodeFileEither specPath
  spec <-
    case specDecoded of
      Left err
        -> mkFail $ mkError $ "bad specification - " <> T.pack (prettyPrintParseException err)
      Right res -> pure res
  validatePluginName name $ langSpecName spec
  pure Language
    { languageSpec = spec
    , languageParser = CmdProgram{ cmdProgramPath = parserPath }
    , languagePrinter = CmdProgram{ cmdProgramPath = printerPath }
    }

mkServer :: FilePath -> String -> PreSessionRes ServerSpec
mkServer pluginPath name = do
  let path = pluginPath </> name
      specPath = path </> "spec.yaml"
  specDecoded <- liftIO $ decodeFileEither specPath
  spec <-
    case specDecoded of
      Left err
        -> mkFail $ mkError $ "bad specification - " <> T.pack (prettyPrintParseException err)
      Right res -> pure res
  validatePluginName name $ serverSpecName spec
  pure spec

listDirPlugins :: FilePath -> PreSessionRes [String]
listDirPlugins dir = filter (not . isHidden) <$> liftIO (listDirectory dir)
  where isHidden name = "." `isPrefixOf` name

getEnvAtPath :: FilePath -> PreSessionRes SessionEnv
getEnvAtPath pluginPath = do
  let settingsPath = pluginPath </> "settings.yaml"
      languagesPath = pluginPath </> "languages"
      serversPath = pluginPath </> "servers"
      templateDirPath = pluginPath </> "template"
  settingsDecoded <- liftIO $ decodeFileEither settingsPath
  settings <-
    case settingsDecoded of
      Left err -> do
        tellError $ mkError $ "bad settings - " <> T.pack (prettyPrintParseException err)
        pure defaultSettings
      Right res -> pure res
  languages <- traverseDropFatals (mkLanguage languagesPath) =<< listDirPlugins languagesPath
  servers <- traverseDropFatals (mkServer serversPath) =<< listDirPlugins serversPath
  pure SessionEnv
    { sessionEnvSettings = settings
    , sessionEnvLanguages = languages
    , sessionEnvServers = servers
    , sessionEnvTemplateDir = templateDirPath
    }

-- | Loads the environment which is shipped with this package.
getInitialEnv :: PreSessionRes SessionEnv
getInitialEnv = getEnvAtPath "resources"

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
