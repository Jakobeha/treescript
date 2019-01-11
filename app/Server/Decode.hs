module Server.Decode
  ( decodeLoc
  , decodeRange
  ) where

import TreeScript

import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J

decodeLoc :: T.Text -> J.Position -> Loc
decodeLoc src (J.Position line col)
  = mkLocFromSrc src (succ line) (succ col)

decodeRange :: T.Text -> J.Range -> Range
decodeRange src (J.Range start end)
  = Range
  { rangeStart = decodeLoc src start
  , rangeEnd = decodeLoc src end
  }
