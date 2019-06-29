{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.Print.StxLisp
  ( printStxStream
  )
where

import           TreeScript.Ast.Core.Types
import           TreeScript.Misc
import           TreeScript.Plugin

import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified System.IO.Streams             as S

printStx1 :: Value Range -> T.Text
printStx1 val = case value2Stx val of
  Nothing  -> "<? " <> pprint val <> " ?>"
  Just stx -> pprint stx

printStxStream
  :: S.OutputStream B.ByteString -> SessionRes (S.OutputStream (Value Range))
printStxStream = liftIOAndCatch StageWriteOutput
  . S.contramap (T.encodeUtf8 . (<> "\n") . printStx1)
