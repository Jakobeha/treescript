{-# LANGUAGE OverloadedStrings #-}

-- | Parse lexemes from AST data.
module TreeScript.Ast.Flat.Parse
  ( parse
  ) where

import TreeScript.Ast.Flat.Types
import TreeScript.Misc

import Data.Char
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

mkError :: T.Text -> Error
mkError msg
  = Error
  { errorStage = StageLex
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
          pure $ LexemeEnterSplice idx
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
        _ | isUpper $ head word -> do
              numProps <- P.decimal
              separatorParser
              pure $ LexemeRecordHead (T.pack word) numProps
          | otherwise -> fail $ "word has unknown type: " ++ word
    stringParser = T.pack <$> (P.char '"' *> P.manyTill P.charLiteral (P.char '"'))
    wordParser = do
      res <- P.some (P.anySingleBut ' ')
      separatorParser
      pure res
    separatorParser = (() <$ P.char ' ') P.<|> P.eof
