{-# LANGUAGE OverloadedStrings #-}

module Server.Encode
  ( encodeError
  , encodeLoc
  , encodeRange
  ) where

import TreeScript

import Data.Maybe
import qualified Language.Haskell.LSP.Types as J

encodeError :: Error -> J.Diagnostic
encodeError (Error stage rng msg)
  = J.Diagnostic range severity code source message relatedInformation
  where range = encodeRange $ singletonRange loc1 `fromMaybe` rng
        -- Technically an error, but warnings aren't implemented, and
        -- this distinguishes from parse errors which are more severe.
        severity = Just J.DsError
        code = Nothing
        message = "while " <> pprint stage <> " - " <> msg
        source = Just "treescript"
        relatedInformation = Just $ J.List []

encodeLoc :: Loc -> J.Position
encodeLoc (Loc _ line col) = J.Position (pred line) (pred col)

encodeRange :: Range -> J.Range
encodeRange (Range start end) = J.Range (encodeLoc start) (encodeLoc end)
