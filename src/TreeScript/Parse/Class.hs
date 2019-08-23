{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TreeScript.Parse.Class
  ( module TreeScript.Parse.Class
  )
where

import           TreeScript.Ast
import           TreeScript.Misc
import           TreeScript.Print

import           Control.Monad.Reader
import           Data.AST
import           Data.Blob
import qualified Data.ByteString               as B
import qualified Data.Location                 as S
import qualified Data.Source                   as S
import qualified Data.Span                     as S
import           Data.Term
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHC.Generics
import           Parsing.Parser
import qualified Semantic.Task                 as S
import qualified TreeSitter.NominalScript      as G

type ParseM = EResultT (ReaderT (B.ByteString, [AST [] G.Grammar]) IO)

class UntypedParseable a where
  untypedParse :: G.Grammar -> ParseM a
  default untypedParse
    :: (Generic a, GUntypedParseable (Rep a)) => G.Grammar -> ParseM a
  untypedParse = fmap to . guntypedParse

class Parseable a where
  -- | Example for parsing a let: @parse = untypedParse G.Let@
  parse :: ParseM a
  default parse :: (Generic a, GParseable (Rep a)) => ParseM a
  parse = to <$> gparse

class GUntypedParseable a where
  guntypedParse :: G.Grammar -> ParseM (a x)

class GParseable a where
  gparse :: ParseM (a x)

instance (GUntypedParseable a) => GUntypedParseable (M1 i c a) where
  guntypedParse = fmap M1 . guntypedParse

instance (UntypedParseable a) => GUntypedParseable (K1 i a) where
  guntypedParse = fmap K1 . untypedParse

instance (GParseable a, GParseable b) => GParseable (a :+: b) where
  gparse = (L1 <$> gparse) `orResT` (R1 <$> gparse)

instance (GProduct2 (a :*: b), Head (a :*: b) ~ S1 h1 (Rec0 (SrcAnn h2)), GParseable (Linear (Tail (a :*: b))), GProductList (Tail (a :*: b))) => GUntypedParseable (a :*: b) where
  guntypedParse egram = do
    (fullSrc, [term]) <- ask
    let
      Node agram (S.Location (S.Range spos epos) (S.Span (S.Pos sline scol) (S.Pos eline ecol)))
        = termAnnotation term
      rng = Range (Loc spos sline scol) (Loc epos eline ecol)
      mkErr msg = mkFail Error { errorRange = Just rng, errorMsg = msg }
    src <- case T.decodeUtf8' $ B.take (epos - spos) $ B.drop spos fullSrc of
      Left  exc  -> mkErr $ pprint exc
      Right src' -> pure src'
    let ann      = SrcAnn $ SrcInfo rng src
        children = termOut term
    unless (egram == agram) $ mkFail Error
      { errorRange = Just rng
      , errorMsg   = "expected " <> pprint egram <> " got " <> pprint agram
      }
    bodyRes <-
      lift
      $   runResultT
      $   gunlinearize
      <$> local (\_ -> (fullSrc, children)) gparse
    case bodyRes of
      ResultFail bodyErr -> fail $ T.unpack $ pprint bodyErr
      Result errs body   -> do
        tellErrors errs
        pure $ gproductCons (M1 $ K1 ann) body

instance GParseable U1 where
  gparse = pure U1

instance (GParseable a) => GParseable (M1 i c a) where
  gparse = M1 <$> gparse

instance (Parseable a) => GParseable (K1 i a) where
  gparse = K1 <$> parse

instance (GParseable a, GParseable b) => GParseable (a :*: b) where
  gparse = do
    (fullSrc, term : terms) <- ask
    (:*:)
      <$> local (\_ -> (fullSrc, [term])) gparse
      <*> local (\_ -> (fullSrc, terms))  gparse

instance (Parseable a) => Parseable [a] where
  parse = do
    (fullSrc, terms) <- ask
    traverse (\term -> local (\_ -> (fullSrc, [term])) parse) terms

parseSpecialSum
  :: (SrcAnn a -> b -> c) -> [(G.Grammar, T.Text -> b)] -> ParseM c
parseSpecialSum mkTerm opts = do
  (fullSrc, [term]) <- ask
  let
    Node agram (S.Location (S.Range spos epos) (S.Span (S.Pos sline scol) (S.Pos eline ecol)))
      = termAnnotation term
    rng = Range (Loc spos sline scol) (Loc epos eline ecol)
    mkErr msg = mkFail Error { errorRange = Just rng, errorMsg = msg }
  src <- case T.decodeUtf8' $ B.take (epos - spos) $ B.drop spos fullSrc of
    Left  exc  -> mkErr $ pprint exc
    Right src' -> pure src'
  let ann = SrcAnn $ SrcInfo rng src
  case lookup agram opts of
    Nothing -> mkFail Error
      { errorRange = Just rng
      , errorMsg   = "expected one of "
                     <> pprint (map fst opts)
                     <> " got "
                     <> pprint agram
      }
    Just mkContent -> pure $ mkTerm ann $ mkContent src

parseSpecialSum' :: (SrcAnn a -> b -> c) -> [(G.Grammar, b)] -> ParseM c
parseSpecialSum' mkTerm =
  parseSpecialSum mkTerm . map (\(gram, val) -> (gram, \_ -> val))

parseIdentifier :: (SrcAnn a -> T.Text -> c) -> G.Grammar -> ParseM c
parseIdentifier mkTerm egram = parseSpecialSum mkTerm [(egram, id)]

parseFile :: (Parseable a) => FilePath -> EResultT IO a
parseFile pth = do
  (ast, fullSrc) <-
    eitherExcToResult <=< liftIO . S.runTaskWithOptions S.defaultOptions $ do
      blob <- S.readBlob $ fileForPath pth
      ast' <- S.parse nominalScriptASTParser blob
      pure (ast', S.sourceBytes $ blobSource blob)
  mapResultT (`runReaderT` (fullSrc, [ast])) parse
