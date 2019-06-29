{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.Parse.StxLisp
  ( parseStxText
  , parseStxFile
  )
where

import           TreeScript.Ast.Core.Analyze
import           TreeScript.Ast.Core.Types
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Attoparsec
                                               as P
import           TreeScript.Plugin

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text          as P
import qualified Data.Text                     as T
import qualified System.IO.Streams             as S
import qualified System.IO.Streams.Attoparsec.Text
                                               as S

mkError :: T.Text -> Error
mkError msg =
  Error { errorStage = StageReadInput, errorRange = r0, errorMsg = msg }

exprParser :: P.Parser Stx
exprParser =
  wordParser <|> puncParser <|> stringParser <|> intParser <|> blockParser
 where
  wordParser = StxWord <$> P.takeWhile1 (P.inClass "A-Za-z_")
  puncParser = StxPunc <$> P.takeWhile1 (P.inClass "#$%&*+,-./:;<=>?@\\^_`|~!")
  stringParser =
    (StxString '"' <$> P.stringLiteral '"')
      <|> (StxString '\'' <$> P.stringLiteral '\'')
  intParser =
    (StxInt 10 <$> P.signed P.decimal)
      <|> (StxInt 16 <$> P.signed (P.string "0x" *> P.hexadecimal))
  blockParser =
    (StxBlock '(' <$> (P.char '(' *> exprsParser (P.char ')')))
      <|> (StxBlock '[' <$> (P.char '[' *> exprsParser (P.char ']')))
      <|> (StxBlock '{' <$> (P.char '{' *> exprsParser (P.char '}')))

exprsParser :: P.Parser a -> P.Parser [Stx]
exprsParser end = P.skipSpace *> ((exprParser <* P.skipSpace) `P.manyTill` end)

astParser :: P.Parser [Value Range]
astParser = map stx2Value <$> exprsParser P.endOfInput

parseStxStream :: S.InputStream T.Text -> IO (S.InputStream (Value Range))
parseStxStream = S.parserToInputStream parser
 where
  parser =
    (Nothing <$ (P.try P.skipSpace *> P.endOfInput))
      <|> (Just . stx2Value <$> (P.skipSpace *> exprParser))

parseStxText :: Range -> T.Text -> [Value Range] -> SessionRes [Value Range]
parseStxText rng txt splices = case P.parseOnly astParser txt of
  Left  err -> mkFail $ mkError $ T.pack err
  Right res -> pure $ map (substSplices splices . (rng <$)) res

parseStxFile :: FilePath -> SessionRes (S.InputStream (Value Range))
parseStxFile pinp =
  liftIOAndCatch StageReadInput
    $   withFileAsInput pinp
    $   parseStxStream
    <=< S.decodeUtf8
