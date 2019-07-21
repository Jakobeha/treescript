{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Integrates plugins into compilation and other actions.
module TreeScript.Misc.IO.Session
  ( Settings(..)
  , LangExt(..)
  , Language(..)
  , SessionEnv(..)
  , Session
  , SessionRes
  , getInitialEnv
  , getSessionEnv
  , runSessionResVirtual
  , runPreSessionRes
  , runSessionResReal
  , langWithExt
  , forceLangWithExt
  , forceLangForPath
  )
where

import           TreeScript.Misc.IO.CmdProgram

import           Control.Monad.Logger    hiding ( LogLevel(..) )
import qualified Control.Monad.Logger          as L
                                                ( LogLevel(..) )
import           Control.Monad.Reader
import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           Data.Yaml
import           System.Directory
import           System.FilePath

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

-- | Provides programs to parse and print an external language.
data LangExt
  = LangExt
  { langExtParser :: CmdProgram
  , langExtPrinter :: CmdProgram
  } deriving (Read, Show)

-- | Describes a language.
data Language
  = LanguageStx
  | LanguageExt LangExt

-- | General global data for every session.
data SessionEnv
  = SessionEnv
  { sessionEnvSettings :: Settings
  , sessionEnvLanguages :: M.Map T.Text LangExt
  , sessionEnvBuiltinModsPath :: FilePath
  }

type PreSessionRes a = forall r . ResultT (ReaderT r (LoggingT IO)) a

-- | A value computed using plugins.
type Session = ReaderT SessionEnv (LoggingT IO)

-- | A result computed using plugins.
type SessionRes = ResultT (ReaderT SessionEnv (LoggingT IO))

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ parseStr . T.unpack
   where
    parseStr "debug"   = pure LogLevelDebug
    parseStr "warning" = pure LogLevelWarning
    parseStr "error"   = pure LogLevelError
    parseStr lvl       = fail $ "invalid log level: " ++ lvl

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \x -> Settings <$> x .: "logLevel"

logLevelToMonadLogLevel :: LogLevel -> L.LogLevel
logLevelToMonadLogLevel LogLevelDebug   = L.LevelDebug
logLevelToMonadLogLevel LogLevelWarning = L.LevelWarn
logLevelToMonadLogLevel LogLevelError   = L.LevelError

defaultSettings :: Settings
defaultSettings = Settings { settingsLogLevel = LogLevelDebug }

emptySessionEnv :: SessionEnv
emptySessionEnv = SessionEnv { sessionEnvSettings        = defaultSettings
                             , sessionEnvLanguages       = M.empty
                             , sessionEnvBuiltinModsPath = ""
                             }

mkPluginLoadError :: T.Text -> Error
mkPluginLoadError msg =
  Error { errorStage = StagePluginLoad, errorRange = r0, errorMsg = msg }

liftLoadIO :: IO a -> PreSessionRes a
liftLoadIO = liftIOAndCatch StagePluginLoad

getRealPluginPath :: PreSessionRes FilePath
getRealPluginPath = liftLoadIO $ getRealAppDataDirectory "treescript"

mkLangExt :: FilePath -> String -> PreSessionRes (T.Text, LangExt)
mkLangExt pluginPath ext = do
  let path        = pluginPath </> ext
      parserPath  = path </> "parser"
      printerPath = path </> "printer"
      ext'        = T.pack ext
  unless (isLower $ T.head ext')
    $  tellError
    $  mkPluginLoadError
    $  "language folder name (extension) be lowercase: "
    <> ext'
  pure
    ( ext'
    , LangExt
      { langExtParser  = CmdProgram { cmdProgramStage = StageReadInput
                                    , cmdProgramPath  = parserPath
                                    , cmdProgramEnv   = []
                                    }
      , langExtPrinter = CmdProgram { cmdProgramStage = StageWriteOutput
                                    , cmdProgramPath  = printerPath
                                    , cmdProgramEnv   = []
                                    }
      }
    )


listDirPlugins :: FilePath -> PreSessionRes [String]
listDirPlugins dir = filter (not . isHidden) <$> liftLoadIO (listDirectory dir)
  where isHidden name = "." `isPrefixOf` name

getEnvAtPath :: FilePath -> PreSessionRes SessionEnv
getEnvAtPath pluginPath = do
  let settingsPath  = pluginPath </> "settings.yaml"
      languagesPath = pluginPath </> "languages"
      modsPath      = pluginPath </> "modules"
  settingsDecoded <- liftLoadIO $ decodeFileEither settingsPath
  settings        <- case settingsDecoded of
    Left err -> do
      tellError $ mkPluginLoadError $ "bad settings - " <> T.pack
        (prettyPrintParseException err)
      pure defaultSettings
    Right res -> pure res
  languages <-
    fmap M.fromList
    .   traverseDropFatals (mkLangExt languagesPath)
    =<< listDirPlugins languagesPath
  pure SessionEnv { sessionEnvSettings        = settings
                  , sessionEnvLanguages       = languages
                  , sessionEnvBuiltinModsPath = modsPath
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
runSessionVirtualRaw env =
  runStdoutLoggingT
    . filterLogger (\_ lvl -> lvl >= envLvl)
    . (`runReaderT` env)
 where
  envLvl = logLevelToMonadLogLevel $ settingsLogLevel $ sessionEnvSettings env

-- | Evaluates a session result in a given environment. Useful for tests.
runSessionResVirtual :: SessionEnv -> SessionRes a -> IO (Result a)
runSessionResVirtual env session = runSessionVirtualRaw env $ runResultT $ do
  setupEnv
  session

-- | Evaluates a pre-session result (like session result but doesn't use any environment).
runPreSessionRes :: PreSessionRes a -> IO (Result a)
runPreSessionRes session =
  runSessionVirtualRaw emptySessionEnv $ runResultT session

-- | Evaluates a session result in the user's environment.
runSessionResReal :: SessionRes a -> IO (Result a)
runSessionResReal session = runPreSessionRes $ do
  env <- getRealEnv
  mapResultT (withReaderT $ \_ -> env) $ do
    setupEnv
    session

-- | Gets the language for the given extension in the session.
langWithExt :: T.Text -> SessionRes (Maybe Language)
langWithExt ext
  | T.null ext = pure $ Just LanguageStx
  | otherwise = do
    langs <- sessionEnvLanguages <$> getSessionEnv
    pure $ LanguageExt <$> langs M.!? ext

-- | Gets the language for the given extension. Fails if it can't find.
forceLangWithExt :: T.Text -> SessionRes Language
forceLangWithExt ext = do
  lang <- langWithExt ext
  case lang of
    Nothing -> mkFail Error
      { errorStage = StageReadInput
      , errorRange = r0
      , errorMsg   = "unknown language with extension: " <> ext
      }
    Just lang' -> pure lang'

-- | Gets the language for the given path in the session. Fails if it can't find.
forceLangForPath :: FilePath -> SessionRes Language
forceLangForPath = forceLangWithExt . T.pack . dropDot . takeExtension
 where
  dropDot ('.' : ext) = ext
  dropDot "" = ""
  dropDot ext = error $ "impossible: takeExtension returned " ++ show ext
