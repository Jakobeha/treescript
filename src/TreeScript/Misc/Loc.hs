{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Text location and range.
module TreeScript.Misc.Loc
  ( Loc(..)
  , Range(..)
  , loc1
  , advanceLoc
  , mkRange
  , singletonRange
  , printLoc
  , printRange
  )
where

import           Data.Binary
import           Data.Semigroup
import qualified Data.Text                     as T
import           GHC.Generics

-- | A location in text.
data Loc
  = Loc
  { locOffset :: Int -- ^ Absolute character offset.
  , locLine :: Int -- ^ Line number.
  , locColumn :: Int -- ^ Column number.
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | A range in text.
data Range
  = Range
  { rangeStart :: Loc -- ^ Location before the first character.
  , rangeEnd :: Loc -- ^ Location after the last character.
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | The result is the smallest range containing all sub-ranges.
instance Semigroup Range where
  Range xStart xEnd <> Range yStart yEnd =
    Range { rangeStart = min xStart yStart, rangeEnd = max xEnd yEnd }
  sconcat = foldr1 (<>)
  stimes _ x = x

-- | Line 1, column 1.
loc1 :: Loc
loc1 = Loc { locOffset = 0, locLine = 1, locColumn = 1 }

-- | Advance location past the given text.
advanceLoc :: Loc -> T.Text -> Loc
advanceLoc loc text = Loc { locOffset = locOffset loc + T.length text
                          , locLine   = locLine loc + numLines - 1
                          , locColumn = locColumn'
                          }
 where
  lines'      = T.splitOn "\n" text
  numLines    = length lines'
  lastLineLen = T.length $ last lines'
  locColumn' | numLines == 1 = locColumn loc + lastLineLen
             | otherwise     = lastLineLen

-- | Create a range given start location and text.
mkRange :: Loc -> T.Text -> Range
mkRange loc text = Range { rangeStart = loc, rangeEnd = advanceLoc loc text }

-- | Create a range starting and ending at the same location
singletonRange :: Loc -> Range
singletonRange loc = Range { rangeStart = loc, rangeEnd = loc }

printLoc :: Loc -> T.Text
printLoc loc =
  T.pack (show $ locLine loc) <> ":" <> T.pack (show $ locColumn loc)

printRange :: Range -> T.Text
printRange (Range start end) = printLoc start <> "-" <> printLoc end
