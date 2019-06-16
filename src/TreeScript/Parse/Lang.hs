{-# LANGUAGE OverloadedStrings #-}

-- | Parse other languages (not treescript)
module TreeScript.Parse.Lang
  ( parseLang
  , parseLangSplice
  )
where

import           TreeScript.Ast
import           TreeScript.Parse.Semantic
import           TreeScript.Misc

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Language
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Parsing.Parser
import           Semantic.Util
import           System.Directory
import qualified System.IO.Streams             as S
import           System.IO.Temp

parseLang
  :: (MonadCatch m, MonadIO m, MonadResult m)
  => FilePath
  -> m (S.InputStream (Value ()))
parseLang path = do
  let failParseIfNothing msg = failIfNothing Error
        { errorStage = StageReadInput
        , errorRange = r0
        , errorMsg   = msg
        }
  let lang = languageForFilePath path
  SomeASTParser parser <- failParseIfNothing "unsupported language"
    $ someASTParser lang
  grammar <- liftIOAndCatch StageReadInput $ parseFile parser path
  liftIOAndCatch StageReadInput $ S.fromList [semantic2Value grammar]

parseLangSplice
  :: (MonadCatch m, MonadIO m, MonadResult m)
  => Range
  -> Range
  -> T.Text
  -> T.Text
  -> m [Value ()]
parseLangSplice langExtRng rng ext txt = do
  -- Unless creating a tempfile has noticeable drawbacks, this is the easiest way
  let failParseIfNothing msg = failIfNothing Error { errorStage = StageDesugar
                                                   , errorRange = langExtRng
                                                   , errorMsg   = msg
                                                   }
      liftDsrIO = overErrors (addRangeToErr rng) . liftIOAndCatch StageDesugar
  tmpPath <- liftDsrIO $ emptySystemTempFile "treescript"
  liftDsrIO $ T.writeFile tmpPath txt
  let lang = languageForFilePath $ '.' : T.unpack ext
  SomeASTParser parser <- failParseIfNothing "unsupported language"
    $ someASTParser lang
  grammar <- liftIOAndCatch StageReadInput $ parseFile parser tmpPath
  let res = [semantic2Value grammar]
  liftIO $ removeFile tmpPath `catch` ignoreException
  pure res
