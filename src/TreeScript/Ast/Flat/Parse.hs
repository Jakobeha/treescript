{-# LANGUAGE OverloadedStrings #-}

-- | Parse lexemes from AST data.
module TreeScript.Ast.Flat.Parse
  ( parseList
  , parseStream
  )
where

import           TreeScript.Ast.Flat.Types
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Attoparsec
                                               as P

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text          as P
import           Data.Char
import qualified Data.Text                     as T
import qualified System.IO.Streams             as S
import qualified System.IO.Streams.Attoparsec.Text
                                               as S

mkError :: T.Text -> Error
mkError msg = Error { errorStage = StageLex, errorRange = r0, errorMsg = msg }

separatorParser :: P.Parser ()
separatorParser = (() <$ P.char ' ') <|> P.endOfInput

wordParser :: P.Parser String
wordParser = do
  res <- some (P.notChar ' ')
  separatorParser
  pure res

lexemeParser :: P.Parser Lexeme
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
      value <- realToFrac <$> P.double
      separatorParser
      pure $ LexemePrimitive $ PrimFloat value
    "string" -> do
      value <- P.stringLiteral '"'
      separatorParser
      pure $ LexemePrimitive $ PrimString value
    _
      | isUpper $ head word -> do
        numProps <- P.decimal
        separatorParser
        pure $ LexemeRecordHead (T.pack word) numProps
      | otherwise -> fail $ "word has unknown type: " ++ word

astParser :: P.Parser [Lexeme]
astParser = lexemeParser `P.manyTill` ((() <$ P.endOfLine) <|> P.endOfInput)

parseList :: T.Text -> Result [[Lexeme]]
parseList txt = case P.parseOnly parser txt of
  Left  err -> mkFail $ mkError $ T.pack err
  Right xs  -> pure $ filter (not . null) xs
  where parser = astParser `P.manyTill` P.endOfInput

parseStream :: S.InputStream T.Text -> IO (S.InputStream [Lexeme])
parseStream = S.filter (not . null) <=< S.parserToInputStream parser
  where parser = (Nothing <$ P.endOfInput) <|> (Just <$> astParser)
