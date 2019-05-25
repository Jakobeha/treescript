{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Text location and range.
module TreeScript.Misc.Loc
  ( Loc (..)
  , Range (..)
  , loc0
  , loc1
  , r0
  , advanceLoc
  , mkRange
  , singletonRange
  ) where

import TreeScript.Misc.Print

import Data.Binary
import Data.Semigroup
import qualified Data.Text as T
import GHC.Generics

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
  x@(Range xStart xEnd) <> y@(Range yStart yEnd)
    | x == r0 = y
    | y == r0 = x
    | otherwise
    = Range
    { rangeStart = min xStart yStart
    , rangeEnd = max xEnd yEnd
    }
  sconcat = foldr1 (<>)
  stimes _ x = x

-- | 'r0' is empty.
instance Monoid Range where
  mempty = r0

instance Printable Loc where
  pprint loc = pprint (locLine loc) <> ":" <> pprint (locColumn loc)

instance Printable Range where
  pprint (Range start end) = pprint start <> "-" <> pprint end

-- | Line 0, column 0 (null location).
loc0 :: Loc
loc0
  = Loc
  { locOffset = 0
  , locLine = 0
  , locColumn = 0
  }

-- | Line 1, column 1.
loc1 :: Loc
loc1
  = Loc
  { locOffset = 0
  , locLine = 1
  , locColumn = 1
  }

-- | Null range.
r0 :: Range
r0 = singletonRange loc0

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
