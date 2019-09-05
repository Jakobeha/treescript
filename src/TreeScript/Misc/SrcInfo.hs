-- | Extra information for parsed nodes.
module TreeScript.Misc.SrcInfo
  ( module TreeScript.Misc.SrcInfo
  )
where

import           TreeScript.Misc.Loc
import qualified Data.Text                     as T

data SrcInfo
  = SrcInfo
  { srcInfoRange :: Range
  , srcInfoText :: T.Text
  , srcInfoComments :: [Comment]
  } deriving (Eq, Ord, Read, Show)

data Comment
  = Comment
  { commentRange :: Range
  , commentText :: T.Text
  } deriving (Eq, Ord, Read, Show)
