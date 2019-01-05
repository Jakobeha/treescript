{-# LANGUAGE OverloadedStrings #-}

-- | Parse lexemes from AST data.
module TreeScript.Ast.Flat.Parse
  ( parse
  , langFromAstData
  ) where

import TreeScript.Ast.Flat.Types
import TreeScript.Misc
import TreeScript.Plugin

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

mkError :: T.Text -> Error
mkError msg
  = Error
  { errorStage = StageLexing
  , errorRange = Nothing
  , errorMsg = msg
  }

parse :: T.Text -> Result [[Lexeme]]
parse txt = traverse parse1 $ filter (not . T.null) $ T.lines txt
  where
    parse1 txt1
      = case P.runParser astDataParser "" txt1 of
          Left err -> mkFail $ mkError $ T.pack $ P.errorBundlePretty (err :: P.ParseErrorBundle T.Text Void)
          Right res -> pure res
    astDataParser = P.many lexemeParser <* P.eof
    lexemeParser = do
      word <- wordParser
      case word of
        "splice" -> do
          idx <- P.decimal
          separatorParser
          pure $ LexemeSplice idx
        "integer" -> do
          value <- P.decimal
          separatorParser
          pure $ LexemePrimitive $ PrimInteger value
        "float" -> do
          value <- P.float
          separatorParser
          pure $ LexemePrimitive $ PrimFloat value
        "string" -> do
          value <- stringParser
          separatorParser
          pure $ LexemePrimitive $ PrimString value
        _ | isUpper $ head word -> pure $ LexemeRecordHead $ T.pack word
          | otherwise -> fail $ "word has unknown type: " ++ word
    stringParser = T.pack <$> (P.char '"' *> P.manyTill P.charLiteral (P.char '"'))
    wordParser = do
      res <- P.some (P.anySingleBut ' ')
      separatorParser
      pure res
    separatorParser = (() <$ P.char ' ') P.<|> P.eof

lexLangSrc :: Lexeme -> SessionRes (Maybe T.Text)
lexLangSrc (LexemeSplice _) = pure Nothing
lexLangSrc (LexemePrimitive _) = pure Nothing
lexLangSrc (LexemeRecordHead head')
  = case T.splitOn "_" head' of
      [_] -> mkFail $ mkError $ "output record not in language: " <> head'
      [lang, _] -> pure $ Just lang
      _ -> mkFail $ mkError $ "malformed output record head: " <> head'

-- | Infers the language of the AST data.
langFromAstData :: T.Text -> SessionRes Language
langFromAstData astData = do
  lexs <- ResultT $ pure $ concat <$> parse astData
  lexLangs <- nub . catMaybes <$> traverse lexLangSrc lexs
  case lexLangs of
    [] -> mkFail $ mkError $ "output doesn't have a language"
    [lexLang] -> do
      langs <- sessionEnvLanguages <$> getSessionEnv
      case find ((== lexLang) . langSpecName . languageSpec) langs of
        Nothing -> mkFail $ mkPluginUseError $ "no (valid) plugin for language with name '" <> lexLang <> "'"
        Just res -> pure res
    _ -> mkFail $ mkError $ "output has multiple languages: " <> T.intercalate ", " lexLangs
