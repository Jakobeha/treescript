{-# LANGUAGE OverloadedStrings #-}

-- | Text location and range.
module Descript.Misc.Loc
  ( Loc (..)
  , Range (..)
  , loc1
  , advanceLoc
  , mkRange
  , singletonRange
  , boundingRange
  ) where

import Descript.Misc.Print

import qualified Data.List.NonEmpty as N
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

instance Printable Range where
  pprint (Range start end) = pprint start <> "-" <> pprint end

  -- | Line 1, column 1
loc1 :: Loc
loc1
  = Loc
  { locOffset = 0
  , locLine = 1
  , locColumn = 1
  }

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

-- | Create a range starting and ending at the same location
singletonRange :: Loc -> Range
singletonRange loc
  = Range
  { rangeStart = loc
  , rangeEnd = loc
  }

-- | The smallest range containing all sub-ranges.
boundingRange :: N.NonEmpty Range -> Range
boundingRange = foldr1 boundingRange2
  where boundingRange2 (Range xStart xEnd) (Range yStart yEnd)
          = Range
          { rangeStart = min xStart yStart
          , rangeEnd = max xEnd yEnd
          }
