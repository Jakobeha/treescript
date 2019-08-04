{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Parse.ParsecHelpers
  ( BalanceStream(..)
  , ParseError(..)
  , parsecRes2Result
  )
where

import           TreeScript.Ast
import           TreeScript.Misc
import           TreeScript.Print

import qualified Data.List.NonEmpty            as N
import           Data.Proxy
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as M

newtype BalanceStream = BalanceStream{ unBalanceStream :: [SBalance] }

data ParseError
  = ParseError
  { parseErrorRange :: Range
  , parseErrorMsg :: T.Text
  } deriving (Eq, Ord, Read, Show)

instance M.Stream BalanceStream where
  type Token BalanceStream = SBalance
  type Tokens BalanceStream = [SBalance]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy x = x
  chunkToTokens Proxy x = x
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (BalanceStream []      ) = Nothing
  take1_ (BalanceStream (x : xs)) = Just (x, BalanceStream xs)
  takeN_ 0 srm                = Just ([], srm)
  takeN_ _ (BalanceStream []) = Nothing
  takeN_ n (BalanceStream xs) = Just (hd, BalanceStream tl)
    where (hd, tl) = splitAt n xs
  takeWhile_ pred' (BalanceStream xs) = (hd, BalanceStream tl)
    where (hd, tl) = splitAtFail pred' xs
  showTokens Proxy tks = T.unpack $ pprint $ BalanceProgram $ N.toList tks
  reachOffset off st@(M.PosState _ ioff _ _ _) = reachRelOffset (off - ioff) st

instance M.ShowErrorComponent ParseError where
  showErrorComponent = T.unpack . pprint . parseError2Error

advancePosState :: M.PosState BalanceStream -> M.PosState BalanceStream
advancePosState (M.PosState (BalanceStream []) _ _ _ _) =
  error "can't advance - no tokens left"
advancePosState (M.PosState (BalanceStream (x : xs)) ioff (M.SourcePos file _ _) tab pre)
  = M.PosState (BalanceStream xs) (ioff + 1) (loc2Pos file end) tab
    $  T.unpack
    $  T.takeWhileEnd (/= '\n')
    $  T.pack pre
    <> pprint x
  where end = rangeEnd $ srcAnnRange $ getAnn x

loc2Pos :: FilePath -> Loc -> M.SourcePos
loc2Pos file (Loc _ line col) = M.SourcePos file (M.mkPos line) $ M.mkPos col

parseError2Error :: ParseError -> Error
parseError2Error (ParseError rng msg) = Error (Just rng) msg

parsecRes2Result
  :: (M.Stream s) => Either (M.ParseErrorBundle s ParseError) a -> EResult a
parsecRes2Result (Left err) =
  mkFail $ Error Nothing $ T.pack $ M.errorBundlePretty err
parsecRes2Result (Right res) = pure res

printLine :: T.Text -> String
printLine "" = "<empty line>"
printLine x  = T.unpack x

reachRelOffset
  :: Int
  -> M.PosState BalanceStream
  -> (M.SourcePos, String, M.PosState BalanceStream)
reachRelOffset 0 st@(M.PosState (BalanceStream xs) _ pos _ pre) =
  (pos, printLine $ T.pack pre <> printFirstLine xs, st)
reachRelOffset off st = reachRelOffset (off - 1) $ advancePosState st
