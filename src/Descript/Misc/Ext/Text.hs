-- | Helper functions for 'Data.Text'
module Descript.Misc.Ext.Text
  ( parseInt
  , parseFloat
  , unescapeString
  , unescapeChar
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
