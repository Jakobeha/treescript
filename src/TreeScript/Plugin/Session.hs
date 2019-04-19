{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Integrates plugins into compilation and other actions.
module TreeScript.Plugin.Session
  ( Settings (..)
  , Language (..)
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
  , builtinModWithQual
  ) where

import TreeScript.Plugin.CmdProgram
import TreeScript.Misc

import Control.Monad.Logger hiding (LogLevel (..))
import qualified Control.Monad.Logger as L (LogLevel (..))
import Control.Monad.Reader
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml
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
  } deriving (Eq, Ord, Read, Show)

-- | Describes a language and provides programs to parse and print it.
data Language
  = Language
  { languageParser :: CmdProgram
  , languagePrinter :: CmdProgram
  } deriving (Read, Show)

-- | General global data for every session.
data SessionEnv
  = SessionEnv
  { sessionEnvSettings :: Settings
  , sessionEnvLanguages :: M.Map T.Text Language
  , sessionEnvBuiltinMods :: M.Map T.Text FilePath
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

logLevelToMonadLogLevel :: LogLevel -> L.LogLevel
logLevelToMonadLogLevel LogLevelDebug = L.LevelDebug
logLevelToMonadLogLevel LogLevelWarning = L.LevelWarn
logLevelToMonadLogLevel LogLevelError = L.LevelError

defaultSettings :: Settings
defaultSettings
  = Settings
  { settingsLogLevel = LogLevelDebug
  }

emptySessionEnv :: SessionEnv
emptySessionEnv
  = SessionEnv
  { sessionEnvSettings = defaultSettings
  , sessionEnvLanguages = M.empty
  , sessionEnvBuiltinMods = M.empty
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

getRealPluginPath :: PreSessionRes FilePath
getRealPluginPath = liftLoadIO $ getRealAppDataDirectory "treescript"

mkLanguage :: FilePath -> String -> PreSessionRes (T.Text, Language)
mkLanguage pluginPath ext = do
  let path = pluginPath </> ext
      parserPath = path </> "parser"
      printerPath = path </> "printer"
      ext' = T.pack ext
  unless (isLower $ T.head ext') $
    tellError $ mkPluginLoadError $ "language folder name (extension) be lowercase: " <> ext'
  pure (ext', Language
    { languageParser
        = CmdProgram
        { cmdProgramStage = StagePluginUse
        , cmdProgramPath = parserPath
        , cmdProgramEnv = []
        }
    , languagePrinter
        = CmdProgram
        { cmdProgramStage = StagePluginUse
        , cmdProgramPath = printerPath
        , cmdProgramEnv = []
        }
    } )


mkBuiltinMod :: FilePath -> String -> PreSessionRes (T.Text, FilePath)
mkBuiltinMod pluginPath name = do
  let path = pluginPath </> name
      name' = T.pack name
  unless (isUpper $ T.head name') $
    tellError $ mkPluginLoadError $ "module name (extension) must be uppercase: " <> name'
  pure (name', path)

listDirPlugins :: FilePath -> PreSessionRes [String]
listDirPlugins dir = filter (not . isHidden) <$> liftLoadIO (listDirectory dir)
  where isHidden name = "." `isPrefixOf` name

getEnvAtPath :: FilePath -> PreSessionRes SessionEnv
getEnvAtPath pluginPath = do
  let settingsPath = pluginPath </> "settings.yaml"
      languagesPath = pluginPath </> "languages"
      modsPath = pluginPath </> "modules"
  settingsDecoded <- liftLoadIO $ decodeFileEither settingsPath
  settings <-
    case settingsDecoded of
      Left err -> do
        tellError $ mkPluginLoadError $ "bad settings - " <> T.pack (prettyPrintParseException err)
        pure defaultSettings
      Right res -> pure res
  languages <- fmap M.fromList . traverseDropFatals (mkLanguage languagesPath) =<< listDirPlugins languagesPath
  mods <- fmap M.fromList . traverseDropFatals (mkBuiltinMod modsPath) =<< listDirPlugins modsPath
  pure SessionEnv
    { sessionEnvSettings = settings
    , sessionEnvLanguages = languages
    , sessionEnvBuiltinMods = mods
    }

-- | Loads the environment which is shipped with this package.
getInitialEnv :: PreSessionRes SessionEnv
getInitialEnv = getEnvAtPath "resources/env"

-- | Loads the environment for the current user.
getRealEnv :: PreSessionRes SessionEnv
getRealEnv = getEnvAtPath =<< getRealPluginPath

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

-- | Gets the language for the given extension in the session.
langWithExt :: T.Text -> SessionRes (Maybe Language)
langWithExt ext = do
  langs <- sessionEnvLanguages <$> getSessionEnv
  pure $ langs M.!? ext

-- | Gets the path of the module with the given qualifier in the session.
builtinModWithQual :: T.Text -> SessionRes (Maybe FilePath)
builtinModWithQual qual = do
  mods <- sessionEnvBuiltinMods <$> getSessionEnv
  let res = mods M.!? qual
  unless (isJust res) $
    logDebugN $ "Module '" <> qual <> "' " <> "not found"
  pure res
