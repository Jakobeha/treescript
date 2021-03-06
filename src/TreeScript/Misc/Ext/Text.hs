{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for 'Data.Text'.
module TreeScript.Misc.Ext.Text
  ( parseInt
  , parseFloat
  , unescapeString
  , unescapeChar
  , escapeString
  , indent
  , bullet
  , blockQuote
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

-- | Convert all escapable characters into their escape sequences.
escapeString :: T.Text -> T.Text
escapeString str = T.drop 1 $ T.dropEnd 1 $ T.pack $ show $ T.unpack str

-- | Doesn't indent the first line.
indentRest :: T.Text -> T.Text
indentRest = T.replace "\n" "\n  "

-- | Also indents the first line.
indent :: T.Text -> T.Text
indent txt = "  " <> indentRest txt

-- | Formats into a Markdown-style bulleted list item.
bullet :: T.Text -> T.Text
bullet subPr = "- " <> indentRest subPr

-- | Formats into a Markdown-style block quote.
blockQuote :: T.Text -> T.Text
blockQuote contentPr = "> " <> T.replace "\n" "\n> " contentPr
