{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Misc.Ext.Attoparsec
  ( stringLiteral
  , between
  )
where

import           Control.Applicative
import qualified Data.Attoparsec.Text          as P
import           Data.Char
import           Data.List
import           Data.String
import qualified Data.Text                     as T
import           Numeric

-- | Stolen from parsec: https://hackage.haskell.org/package/parsers-0.12.10/docs/src/Text.Parser.Token.html#stringLiteral
escapeCode :: P.Parser Char
escapeCode =
  (charEsc <|> charNum <|> charAscii <|> charControl) P.<?> "escape code"
 where
  charControl, charNum :: P.Parser Char
  charControl =
    (\c -> toEnum (fromEnum c - fromEnum '@'))
      <$> (P.char '^' *> (P.satisfy (P.inClass "A-Z") <|> P.char '@'))
  charNum = toEnum <$> num
   where
    num :: P.Parser Int
    num =
      bounded 10 maxchar
        <|> (P.char 'o' *> bounded 8 maxchar)
        <|> (P.char 'x' *> bounded 16 maxchar)
    maxchar = fromEnum (maxBound :: Char)

  bounded :: Int -> Int -> P.Parser Int
  bounded base bnd = foldl' (\x d -> base * x + digitToInt d) 0 <$> bounded'
    (take base thedigits)
    (map digitToInt $ showIntAtBase base intToDigit bnd "")
   where
    thedigits :: [P.Parser Char]
    thedigits = map P.char ['0' .. '9']
      ++ map (P.satisfy . flip elem) (transpose [['A' .. 'F'], ['a' .. 'f']])

    toomuch :: P.Parser a
    toomuch = fail "out-of-range numeric escape sequence"

    bounded', bounded'' :: [P.Parser Char] -> [Int] -> P.Parser [Char]
    bounded' dps@(zero : _) bds =
      zero
        *>  P.skipMany zero
        *>  ([] <$ notFollowedBy (P.choice dps) <|> bounded'' dps bds)
        <|> bounded'' dps bds
    bounded' [] _ = error "bounded called with base 0"
    bounded'' dps [] = [] <$ notFollowedBy (P.choice dps) <|> toomuch
    bounded'' dps (bd : bds) =
      let anyd :: P.Parser Char
          anyd = P.choice dps

          nomore :: P.Parser ()
          nomore           = notFollowedBy anyd <|> toomuch
          (low, ex : high) = splitAt bd dps
      in  ((:) <$> P.choice low <*> atMost (length bds) anyd)
            <*  nomore
            <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
            <|> if not (null bds)
                  then
                    (:)
                    <$> P.choice high
                    <*> atMost (length bds - 1) anyd
                    <*  nomore
                  else empty
    atMost n p | n <= 0    = pure []
               | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

  charEsc :: P.Parser Char
  charEsc = P.choice $ parseEsc <$> escMap

  parseEsc (c, code) = code <$ P.char c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

  charAscii :: P.Parser Char
  charAscii = P.choice $ parseAscii <$> asciiMap

  parseAscii (asc, code) = P.try $ code <$ P.string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [T.Text]
  ascii2codes =
    [ "BS"
    , "HT"
    , "LF"
    , "VT"
    , "FF"
    , "CR"
    , "SO"
    , "SI"
    , "EM"
    , "FS"
    , "GS"
    , "RS"
    , "US"
    , "SP"
    ]
  ascii3codes =
    [ "NUL"
    , "SOH"
    , "STX"
    , "ETX"
    , "EOT"
    , "ENQ"
    , "ACK"
    , "BEL"
    , "DLE"
    , "DC1"
    , "DC2"
    , "DC3"
    , "DC4"
    , "NAK"
    , "SYN"
    , "ETB"
    , "CAN"
    , "SUB"
    , "ESC"
    , "DEL"
    ]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3
    = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

-- | This token parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
--
-- Stolen from parsec: https://hackage.haskell.org/package/parsers-0.12.10/docs/src/Text.Parser.Token.html#stringLiteral
stringLiteral :: (IsString s) => Char -> P.Parser s
stringLiteral qot = fromString <$> lit where
  lit :: P.Parser [Char]
  lit =
    Prelude.foldr (maybe id (:)) ""
      <$>   between (P.char qot)
                    (P.char qot P.<?> "end of string")
                    (many stringChar)
      P.<?> "string"

  stringChar :: P.Parser (Maybe Char)
  stringChar =
    Just <$> stringLetter <|> stringEscape P.<?> "string P.character"

  stringLetter :: P.Parser Char
  stringLetter = P.satisfy (\c -> (c /= qot) && (c /= '\\') && (c > '\026'))

  stringEscape :: P.Parser (Maybe Char)
  stringEscape = P.char '\\' *> esc

  esc :: P.Parser (Maybe Char)
  esc = Nothing <$ escapeGap <|> Nothing <$ escapeEmpty <|> Just <$> escapeCode

  escapeEmpty, escapeGap :: P.Parser Char
  escapeEmpty = P.char '&'
  escapeGap   = P.skipSpace *> (P.char '\\' P.<?> "end of string gap")
{-# INLINE stringLiteral #-}

notFollowedBy :: Show a => P.Parser a -> P.Parser ()
notFollowedBy p = P.try (P.try p >>= fail . show) <|> return ()

between :: P.Parser a -> P.Parser a -> P.Parser b -> P.Parser b
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x
