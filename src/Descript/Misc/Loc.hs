{-# LANGUAGE OverloadedStrings #-}

-- | Text location and range.
module Descript.Misc.Loc
  ( Loc (..)
  , Range (..)
  , mkRange
  ) where

import Descript.Misc.Print

import qualified Data.Text as T

-- | A location in text.
data Loc
  = Loc
  { locOffset :: Int -- ^ Absolute character offset.
  , locLine :: Int -- ^ Line number.
  , locColumn :: Int -- ^ Column number.
  } deriving (Eq, Ord, Read, Show)

-- | A range in text.
data Range
  = Range
  { rangeStart :: Loc -- ^ Location before the first character.
  , rangeEnd :: Loc -- ^ Location after the last character.
  } deriving (Eq, Ord, Read, Show)

instance Printable Loc where
  pprint loc = pprint (locLine loc) <> ":" <> pprint (locColumn loc)

-- | Advance location past the given text.
advanceLoc :: Loc -> T.Text -> Loc
advanceLoc loc text
  = Loc
  { locOffset = locOffset loc + T.length text
  , locLine = locLine loc + numLines - 1
  , locColumn = locColumn'
  }
  where lines' = T.splitOn "\n" text
        numLines = length lines'
        lastLineLen = T.length $ last lines'
        locColumn'
          | numLines == 1 = locColumn loc + lastLineLen
          | otherwise = lastLineLen

-- | Create a range given start location and text.
mkRange :: Loc -> T.Text -> Range
mkRange loc text
  = Range
  { rangeStart = loc
  , rangeEnd = advanceLoc loc text
  }
