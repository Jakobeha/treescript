{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for 'Data.Text'.
module TreeScript.Misc.Ext.Text
  ( parseInt
  , parseFloat
  , unescapeString
  , unescapeChar
  , escapeString
  , indent
  , indentRestN
  , indentN
  , bullet
  , blockQuote
  , firstLine
  , indentOnLastLine
  )
where

import qualified Data.Text                     as T

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
unescapeChar str | T.length unescaped == 1 = T.head unescaped
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

-- | Doesn't indent the first line.
indentRestN :: Int -> T.Text -> T.Text
indentRestN n = T.replace "\n" $ "\n" <> T.replicate n "  "


-- | Also indents the first line.
indentN :: Int -> T.Text -> T.Text
indentN n txt = T.replicate n "  " <> indentRestN n txt

-- | Formats into a Markdown-style bulleted list item.
bullet :: T.Text -> T.Text
bullet subPr = "- " <> indentRest subPr

-- | Formats into a Markdown-style block quote.
blockQuote :: T.Text -> T.Text
blockQuote contentPr = "> " <> T.replace "\n" "\n> " contentPr

-- | Returns @Nothing@ unless there are multiple lines
firstLine :: T.Text -> Maybe T.Text
firstLine txt | T.length lst == T.length txt = Nothing
              | otherwise                    = Just lst
  where lst = T.takeWhile (/= '\n') txt

-- | Returns @Nothing@ unless there are multiple lines
lastLine :: T.Text -> Maybe T.Text
lastLine txt | T.length lst == T.length txt = Nothing
             | otherwise                    = Just lst
  where lst = T.takeWhileEnd (/= '\n') txt

-- | The indentation on the last line of the text.
indentOnLastLine :: T.Text -> Maybe Int
indentOnLastLine =
  fmap ((`quot` 2) . T.length . T.takeWhile (== ' ')) . lastLine
