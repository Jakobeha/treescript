module TreeScript.Parse.Node
  ( parseNode
  )
where

import           TreeScript.Ast
import           TreeScript.Misc
import           TreeScript.Parse.ParsecHelpers

import qualified Text.Megaparsec               as M

programParser :: a
programParser = undefined

parseNode :: BalanceProgram SrcAnn -> EResult (ANodeProgram SrcAnn)
parseNode (BalanceProgram xs) = parsecRes2Result $ ANodeProgram <$> M.runParser
  programParser
  ""
  (BalanceStream xs)
