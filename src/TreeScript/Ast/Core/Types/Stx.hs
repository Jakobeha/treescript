{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Types.Stx
  ( module TreeScript.Ast.Core.Types.Stx
  )
where

import           TreeScript.Ast.Core.Types.BindEq
import           TreeScript.Misc
import           TreeScript.Plugin

import           Data.Binary
import           Data.Char
import qualified Data.Text                     as T
import qualified Data.UUID                     as U
import           GHC.Generics
import           Numeric

data StxTerm a where
  TStxWord ::StxTerm T.Text
  TStxPunc ::StxTerm T.Text
  TStxString ::StxTerm (Char, T.Text)
  TStxInt ::StxTerm (Int, Int)
  TStxSplice ::StxTerm (Bool, Int)
  TStxBlock ::StxTerm (Char, StxBlob)
  TStx ::StxTerm Stx
  TStxIdd ::StxTerm (Idd Stx)
  TStxBlob ::StxTerm StxBlob

data Stx
  = StxWord T.Text
  | StxPunc T.Text
  | StxString Char T.Text
  | StxInt Int Int
  | StxSplice Bool Int
  | StxBlock Char StxBlob
  deriving (Eq, Ord, Read, Show, Generic, Binary)

data Idd a =
  Idd
  { iddId :: U.UUID
  , iddPre :: T.Text
  , iddPost :: T.Text
  , idd :: a
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable)

newtype StxBlob = StxBlob{ unStxBlob :: [Idd Stx] } deriving (Eq, Ord, Read, Show, Generic, Binary)

data LStxBlob
  = LStxBlob
  { lStxBlobLang :: Language
  , lStxBlob :: StxBlob
  } deriving (Eq, Ord, Read, Show, Generic, Binary)

instance BindEq Stx where
  StxBlock xd xblob =$= StxBlock yd yblob = xd == yd && xblob =$= yblob
  x                 =$= y                 = x == y

instance (BindEq a) => BindEq (Idd a) where
  Idd _ _ _ x =$= Idd _ _ _ y = x =$= y

instance BindEq StxBlob where
  StxBlob xs =$= StxBlob ys =
    length xs == length ys && and (zipWith (=$=) xs ys)

instance BindEq LStxBlob where
  LStxBlob xlang xb =$= LStxBlob ylang yb = xlang == ylang && xb =$= yb

instance Printable Stx where
  pprint (StxWord word       ) = word
  pprint (StxPunc punc       ) = punc
  pprint (StxString delim str) = T.singleton delim <> str <> T.singleton delim
  pprint (StxInt    base  num) = case base of
    10 -> pprint num
    16 -> "0x" <> T.pack (showHex num "")
    _  -> pprint base <> "?" <> T.pack (showIntAtBase base intToDigit num "")
  pprint (StxSplice isElp splice) = "\\" <> printElp isElp <> pprint splice
   where
    printElp False = ""
    printElp True  = "..."
  pprint (StxBlock delim contents) =
    T.singleton delim <> pprint contents <> T.singleton (oppositeDelim delim)

instance (Printable a) => Printable (Idd a) where
  pprint (Idd _ pre post x) = pre <> pprint x <> post

instance Printable StxBlob where
  pprint (StxBlob nodes) = T.concat $ map pprint nodes

instance Printable LStxBlob where
  pprint (LStxBlob lang stx) = languageExt lang <> "'" <> pprint stx <> "'"

oppositeDelim :: Char -> Char
oppositeDelim '(' = ')'
oppositeDelim '[' = ']'
oppositeDelim '{' = '}'
oppositeDelim x   = error $ "Invalid delimiter: " ++ [x]

concatTraverseStxBlob
  :: (Applicative w) => (Idd Stx -> w StxBlob) -> StxBlob -> w StxBlob
concatTraverseStxBlob f (StxBlob stxs) =
  StxBlob . concatMap unStxBlob <$> traverse f stxs
