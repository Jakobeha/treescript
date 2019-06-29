{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.Print.Ast
  ( printAstStream
  )
where

import           TreeScript.Ast.Core.Types
import           TreeScript.Ast.Core.Print.StxLisp
import           TreeScript.Misc
import           TreeScript.Plugin

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text.Encoding            as T
import qualified System.IO.Streams             as S

encodeAstData1 :: Value Range -> B.ByteString
encodeAstData1 (ValuePrimitive (PrimInteger _ int)) =
  "integer " <> BC.pack (show int)
encodeAstData1 (ValuePrimitive (PrimFloat _ flt)) =
  "float " <> BC.pack (show flt)
encodeAstData1 (ValuePrimitive (PrimString _ str)) =
  "string " <> BC.pack (show str)
encodeAstData1 (ValueRecord (Record _ head' props)) =
  T.encodeUtf8 (pprint head')
    <> " "
    <> BC.pack (show $ length props)
    <> BC.concat (map ((" " <>) . encodeAstData1) props)
encodeAstData1 (ValueBind (Bind _ idx)) = "splice " <> BC.pack (show idx)

encodeAstDataStream
  :: (MonadCatch m, MonadIO m, MonadResult m)
  => S.OutputStream B.ByteString
  -> m (S.OutputStream (Value Range))
encodeAstDataStream =
  liftIOAndCatch StageWriteOutput . S.contramap ((<> "\n") . encodeAstData1)

printAstStream
  :: Language
  -> S.OutputStream B.ByteString
  -> SessionRes (S.OutputStream (Value Range))
printAstStream lang out = case lang of
  LanguageStx      -> printStxStream out
  LanguageExt lext -> do
    astData <- runCmdProgramStream (langExtPrinter lext) out
    encodeAstDataStream astData
