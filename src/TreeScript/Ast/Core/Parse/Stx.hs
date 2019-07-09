{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.Parse.Stx
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
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.Text          as P
import qualified Data.Text                     as T
import qualified Data.UUID                     as U
import qualified Data.UUID.V4                  as U
import qualified System.IO.Streams             as S
import qualified System.IO.Streams.Attoparsec.Text
                                               as S

mkError :: T.Text -> Error
mkError msg =
  Error { errorStage = StageReadInput, errorRange = r0, errorMsg = msg }

exprParser :: P.Parser Stx
exprParser =
  wordParser
    <|> spliceParser
    <|> puncParser
    <|> stringParser
    <|> intParser
    <|> blockParser
 where
  wordParser = (P.<?> "word") $ StxWord <$> P.takeWhile1 (P.inClass "A-Za-z_")
  puncParser = (P.<?> "punc") $ StxPunc <$> P.takeWhile1
    (P.inClass "#$%&*+,-./:;<=>?@\\^_`|~!")
  stringParser =
    (P.<?> "string")
      $   (StxString '"' <$> P.stringLiteral '"')
      <|> (StxString '\'' <$> P.stringLiteral '\'')
  intParser =
    (P.<?> "int")
      $   (StxInt 10 <$> P.signed P.decimal)
      <|> (StxInt 16 <$> P.signed (P.string "0x" *> P.hexadecimal))
  spliceParser = (P.<?> "splice") $ P.try
    (  P.char '\\'
    *> (   StxSplice
       <$> (P.option False $ True <$ P.string "...")
       <*> ((+ 1) <$> P.decimal)
       )
    )
  blockParser =
    (P.<?> "block")
      $   (StxBlock '(' <$> (P.char '(' *> blobParser (P.char ')')))
      <|> (StxBlock '[' <$> (P.char '[' *> blobParser (P.char ']')))
      <|> (StxBlock '{' <$> (P.char '{' *> blobParser (P.char '}')))

iddParser :: P.Parser (Idd Stx)
iddParser = Idd U.nil <$> exprParser

blobParser :: P.Parser a -> P.Parser StxBlob
blobParser end =
  (P.<?> "syntax")
    $  fmap StxBlob
    $  P.skipSpace
    *> ((iddParser <* P.skipSpace) `P.manyTill` end)

astParser :: P.Parser StxBlob
astParser = blobParser P.endOfInput

annotateId1 :: Idd Stx -> IO (Idd Stx)
annotateId1 (Idd uid stx) | U.null uid = (`Idd` stx) <$> U.nextRandom
                          | otherwise  = pure $ Idd uid stx

parseStxStream :: S.InputStream T.Text -> IO (S.InputStream (Idd Stx))
parseStxStream = S.mapM (traverseStx TStxIdd TStxIdd annotateId1)
  <=< S.parserToInputStream parser
 where
  parser =
    (Nothing <$ P.try (P.skipSpace *> P.endOfInput))
      <|> (Just <$> (P.skipSpace *> iddParser))

parseStxText
  :: (Monad m, MonadCatch m, MonadIO m, MonadResult m) => T.Text -> m StxBlob
parseStxText txt = case P.parseOnly astParser txt of
  Left err -> mkFail $ mkError $ T.pack err
  Right res ->
    liftIOAndCatch StageReadInput $ traverseStx TStxBlob TStxIdd annotateId1 res

parseStxFile :: FilePath -> SessionRes (S.InputStream (Idd Stx))
parseStxFile pinp =
  liftIOAndCatch StageReadInput
    $   parseStxStream
    =<< S.decodeUtf8
    =<< fileToInputStream pinp
