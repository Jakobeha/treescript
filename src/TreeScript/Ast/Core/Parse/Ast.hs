{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Extracts a 'Core' AST from raw text using an external language parser.
module TreeScript.Ast.Core.Parse.Ast
  ( parseAstList
  , parseAstStream
  )
where

import           TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Flat           as F
import           TreeScript.Misc
import           TreeScript.Plugin

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified System.IO.Streams             as S

parseAstSymbol :: Range -> T.Text -> Symbol Range
parseAstSymbol rng txt = Symbol { symbolAnn    = rng
                                , symbolModule = path
                                , symbol       = lcl
                                }
 where
  (path_, lcl) = T.breakOnEnd "_" txt
  path         = T.dropEnd 1 path_


decodeAstData1
  :: (MonadIO m, MonadResult m)
  => Range
  -> V.Vector (Value Range)
  -> [F.Lexeme]
  -> m (Value Range)
decodeAstData1 rng splicesVec lexs = do
  let
    valueParser [] =
      mkFail $ desugarError rng "tried to decode value from no nodes"
    valueParser (F.LexemeEnterSplice idx : rest) = case splicesVec V.!? idx of
      Nothing ->
        mkFail $ desugarError rng $ "invalid splice index: " <> pprint idx
      Just splice -> pure (splice, rest)
    valueParser (F.LexemePrimitive prim : rest) =
      pure (ValuePrimitive $ primParser prim, rest)
    valueParser (F.LexemeRecordHead head' numProps : rest) =
      first ValueRecord <$> recordParser head' numProps rest
    primParser (F.PrimInteger value) = PrimInteger rng value
    primParser (F.PrimFloat   value) = PrimFloat rng value
    primParser (F.PrimString  value) = PrimString rng value
    recordParser qualHead numProps rest = do
      (props, rest') <- propsParser numProps rest
      let head' = parseAstSymbol rng qualHead
      pure
        ( Record { recordAnn = rng, recordHead = head', recordProps = props }
        , rest'
        )
    propsParser 0 rest = pure ([], rest)
    propsParser n rest = do
      (x , rest' ) <- valueParser rest
      (xs, rest'') <- propsParser (n - 1) rest'
      pure (x : xs, rest'')
  (res, rest) <- valueParser lexs
  unless (null rest) $ mkFail $ desugarError
    rng
    "unexpected extra nodes after decoded value"
  pure res

decodeAstDataList
  :: (MonadIO m) => Range -> T.Text -> [Value Range] -> ResultT m [Value Range]
decodeAstDataList rng txt splices = traverse (decodeAstData1 rng splicesVec)
  =<< ResultT (pure $ F.parseList txt)
  where splicesVec = V.fromList splices

decodeAstDataStream
  :: (MonadIO m)
  => Range
  -> ResultInputStream T.Text
  -> [Value Range]
  -> ResultT m (ResultInputStream (Value Range))
decodeAstDataStream rng srm splices =
  mapImpureInputStream (runResultT . decodeAstData1 rng splicesVec)
    =<< mapPureInputStream F.parseStream srm
  where splicesVec = V.fromList splices

parseAstList
  :: Range -> Language -> T.Text -> [Value Range] -> SessionRes [Value Range]
parseAstList rng lang txt splices = do
  let parser = languageParser lang
  astData <-
    overErrors (addRangeToErr rng . prependMsgToErr "couldn't parse code")
      $ runCmdProgram (languageParser lang) txt
  decodeAstDataList rng astData splices

parseAstStream
  :: Range
  -> Language
  -> S.OutputStream B.ByteString
  -> [Value Range]
  -> SessionRes (ResultInputStream (Value Range))
parseAstStream rng lang srm splices = do
  let parser = languageParser lang
  astData <-
    overStreamErrors (addRangeToErr rng . prependMsgToErr "couldn't parse code")
      $ runCmdProgramStream (languageParser lang) srm
  decodeAstDataStream rng astData splices
