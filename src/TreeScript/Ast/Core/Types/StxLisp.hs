{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.Types.StxLisp
  ( Stx(..)
  , stx2Value
  , value2Stx
  )
where

import           TreeScript.Ast.Core.Types.Gen
import           TreeScript.Misc

import           Data.Char
import qualified Data.Text                     as T
import           Numeric

data Stx
  = StxWord T.Text
  | StxPunc T.Text
  | StxString Char T.Text
  | StxInt Int Int
  | StxSplice Int
  | StxBlock Char [Stx]
  deriving (Eq, Ord, Read, Show)

instance Printable Stx where
  pprint (StxWord word       ) = word
  pprint (StxPunc punc       ) = punc
  pprint (StxString delim str) = T.singleton delim <> str <> T.singleton delim
  pprint (StxInt    base  num) = case base of
    10 -> pprint num
    16 -> "0x" <> T.pack (showHex num "")
    _  -> pprint base <> "?" <> T.pack (showIntAtBase base intToDigit num "")
  pprint (StxSplice splice) = "\\" <> pprint splice
  pprint (StxBlock delim contents) =
    T.singleton delim <> T.intercalate " " (map pprint contents) <> T.singleton
      (oppositeDelim delim)

oppositeDelim :: Char -> Char
oppositeDelim '(' = ')'
oppositeDelim '[' = ']'
oppositeDelim '{' = '}'
oppositeDelim x   = error $ "Invalid delimiter: " ++ [x]

stxModule :: T.Text
stxModule = "Stx"

stxBases :: [Int]
stxBases = [10, 16]

mkStxRecord :: T.Text -> [Value Range] -> Value Range
mkStxRecord head' props = ValueRecord Record
  { recordAnn   = r0
  , recordHead  = Symbol { symbolAnn    = r0
                         , symbolModule = stxModule
                         , symbol       = head'
                         }
  , recordProps = props
  }

mkStxInt :: Int -> Value Range
mkStxInt = ValuePrimitive . PrimInteger r0

mkStxString :: T.Text -> Value Range
mkStxString = ValuePrimitive . PrimString r0

stx2Value :: Stx -> Value Range
stx2Value (StxWord word) = mkStxRecord "Word" [mkStxString word]
stx2Value (StxPunc punc) = mkStxRecord "Punc" [mkStxString punc]
stx2Value (StxString delim text) =
  mkStxRecord "String" [mkStxString $ T.singleton delim, mkStxString text]
stx2Value (StxInt base num) = mkStxRecord "Int" [mkStxInt base, mkStxInt num]
stx2Value (StxSplice idx) = ValueBind $ Bind r0 idx
stx2Value (StxBlock delim children) = mkStxRecord
  "Block"
  [mkStxString $ T.singleton delim, mkIListValue r0 $ map stx2Value children]

value2Stx :: Value Range -> Maybe Stx
value2Stx (ValuePrimitive _) = Nothing
value2Stx (ValueRecord (Record _ (Symbol _ mdl head') props))
  | mdl == stxModule = case (head', props) of
    ("Word", [ValuePrimitive (PrimString _ word)]) -> Just $ StxWord word
    ("Punc", [ValuePrimitive (PrimString _ punc)]) -> Just $ StxPunc punc
    ("String", [ValuePrimitive (PrimString _ delim), ValuePrimitive (PrimString _ str)])
      -> case delim of
        "\"" -> Just $ StxString '"' str
        "'"  -> Just $ StxString '\'' str
        _    -> Nothing
    ("Int", [ValuePrimitive (PrimInteger _ base), ValuePrimitive (PrimInteger _ num)])
      | base `elem` stxBases
      -> Just $ StxInt base num
      | otherwise
      -> Nothing
    ("Block", [ValuePrimitive (PrimString _ delim), children]) -> do
      children' <- traverse value2Stx $ unwrapIList children
      case delim of
        "(" -> pure $ StxBlock '(' children'
        "[" -> pure $ StxBlock '[' children'
        "{" -> pure $ StxBlock '{' children'
        _   -> Nothing
    _ -> Nothing
  | otherwise = Nothing
value2Stx (ValueBind (Bind _ idx)) = Just $ StxSplice idx
