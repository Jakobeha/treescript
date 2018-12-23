-- | Helper functions for 'Data.Text'
module Descript.Misc.Ext.Text
  ( parseInt
  , parseFloat
  , unescapeString
  , unescapeChar
  , escapeString
  ) where

import qualified Data.Text as T

-- | Decode an integer from text.
parseInt :: T.Text -> Int
parseInt = read . T.unpack

-- | Decode a float from text.
parseFloat :: T.Text -> Float
parseFloat = read . T.unpack

-- | Convert all escape sequences into their actual characters.
unescapeString :: T.Text -> T.Text
unescapeString str = T.pack $ read $ "\"" ++ T.unpack str ++ "\""

-- | Decode a single character from an escape sequence.
unescapeChar :: T.Text -> Char
unescapeChar str
  | T.length unescaped == 1 = T.head unescaped
  | otherwise = error "unescapeChar: not a single character"
  where unescaped = unescapeString str

-- | Convert all escapable characters into their escape sequences
escapeString :: T.Text -> T.Text
escapeString str = T.drop 1 $ T.dropEnd 1 $ T.pack $ show $ T.unpack str
