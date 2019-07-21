{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Integrates plugins into compilation and other actions.
module TreeScript.Misc.IO.Session
  ( Settings(..)
  , SessionEnv(..)
  , Session
  , GlSession
  , StSession
  , getInitialEnv
  , getSessionEnv
  , runSessionVirtual
  , runPreSession
  , runSessionReal
  )
where

import           TreeScript.Misc.Error
import           TreeScript.Misc.Ext

import           Control.Monad.Logger    hiding ( LogLevel(..) )
import qualified Control.Monad.Logger          as L
                                                ( LogLevel(..) )
import           Control.Monad.Reader
import qualified Data.Text                     as T
import           Data.Yaml
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

-- | General global data for every session.
data SessionEnv
  = SessionEnv
  { sessionEnvSettings :: Settings
  , sessionEnvBuiltinModsPath :: FilePath
  }

-- | Session without an environment.
type PreSession e a = forall r . ResultT e (ReaderT r (LoggingT IO)) a

type Session e = ResultT e (ReaderT SessionEnv (LoggingT IO))

-- | Session in multiple stages.
type GlSession = Session SError

-- | Session in 1 stage (e.g. parsing).
type StSession = Session Error

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
                             , sessionEnvBuiltinModsPath = ""
                             }

getRealEnvPath :: PreSession Error FilePath
getRealEnvPath = liftIOCatch $ getRealAppDataDirectory "treescript"

getEnvAtPath :: FilePath -> PreSession Error SessionEnv
getEnvAtPath path = do
  let settingsPath = path </> "settings.yaml"
      modsPath     = path </> "modules"
  settingsDecoded <- liftIOCatch $ decodeFileEither settingsPath
  settings        <- case settingsDecoded of
    Left err -> do
      tellError $ Error Nothing $ "bad settings - " <> T.pack
        (prettyPrintParseException err)
      pure defaultSettings
    Right res -> pure res
  pure SessionEnv { sessionEnvSettings        = settings
                  , sessionEnvBuiltinModsPath = modsPath
                  }

-- | Loads the environment which is shipped with this package.
getInitialEnv :: PreSession Error SessionEnv
getInitialEnv = getEnvAtPath "resources/env"

-- | Loads the environment for the current user.
getRealEnv :: PreSession Error SessionEnv
getRealEnv = getEnvAtPath =<< getRealEnvPath

getSessionEnv :: (Ord e) => Session e SessionEnv
getSessionEnv = ask

-- | Logs info about the plugin environment, applies initial actions, and (TODO) warns if it's broken.
setupEnv :: (Ord e) => Session e ()
setupEnv = do
  _ <- getSessionEnv
  logDebugN "Loaded session."

runSessionVirtualRaw :: SessionEnv -> Session e a -> IO (Result e a)
runSessionVirtualRaw env =
  runStdoutLoggingT
    . filterLogger (\_ lvl -> lvl >= envLvl)
    . (`runReaderT` env)
    . runResultT
 where
  envLvl = logLevelToMonadLogLevel $ settingsLogLevel $ sessionEnvSettings env

-- | Runs a session in a given environment. Useful for tests.
runSessionVirtual :: (Ord e) => SessionEnv -> Session e a -> IO (Result e a)
runSessionVirtual env session = runSessionVirtualRaw env $ do
  setupEnv
  session

-- | Runs a pre-session.
runPreSession :: PreSession e a -> IO (Result e a)
runPreSession = runSessionVirtualRaw emptySessionEnv

-- | Evaluates a session in the user's environment.
runSessionReal :: GlSession a -> IO (SResult a)
runSessionReal session = runPreSession $ do
  env <- addStage StageSetup getRealEnv
  mapResultT (withReaderT $ \_ -> env) $ do
    setupEnv
    session
