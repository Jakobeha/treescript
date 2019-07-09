{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.Print.Stx
  ( printStxStream
  )
where

import           TreeScript.Ast.Core.Types
import           TreeScript.Misc
import           TreeScript.Plugin

import qualified Data.ByteString               as B
import qualified Data.Text.Encoding            as T
import qualified System.IO.Streams             as S

printStxStream
  :: S.OutputStream B.ByteString -> SessionRes (S.OutputStream (Idd Stx))
printStxStream = liftIOAndCatch StageWriteOutput
  . S.contramap (T.encodeUtf8 . (<> "\n") . pprint)
