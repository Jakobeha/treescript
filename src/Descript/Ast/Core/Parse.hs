{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module Descript.Ast.Core.Parse
  ( parse
  ) where

import Descript.Ast.Core.Types
import qualified Descript.Ast.Sugar as S
import Descript.Misc

import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T

-- = Bind identifier/index environment

data BindEnv
  = BindEnv
  { bindEnvBinds :: M.Map T.Text Int
  , bindEnvNextFree :: Int
  }

emptyBindEnv :: BindEnv
emptyBindEnv
  = BindEnv
  { bindEnvBinds = M.empty
  , bindEnvNextFree = 1
  }

bindEnvLookup :: T.Text -> BindEnv -> (Int, BindEnv)
bindEnvLookup bind env@(BindEnv binds nextFree)
  = case binds M.!? bind of
      Nothing ->
        ( nextFree,
          BindEnv
          { bindEnvBinds = M.insert bind nextFree binds
          , bindEnvNextFree = nextFree + 1
          }
        )
      Just idx -> (idx, env)

-- = Parsing

undefinedSym :: T.Text
undefinedSym = "<undefined>"

parseError :: Range -> T.Text -> Error
parseError rng msg
  = Error
  { errorStage = StageExtracting
  , errorRange = rng
  , errorMsg = "at " <> pprint rng <> " - " <> msg
  }

parsePropDecl :: S.GenProperty Range -> Result T.Text
parsePropDecl (S.GenPropertyDecl (S.Symbol _ decl)) = pure decl
parsePropDecl (S.GenProperty val) = do
  tellError $ parseError (getAnn val) "expected lowercase symbol, got value"
  pure undefinedSym

parseRecordDecl :: S.RecordDecl Range -> Result (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ (S.Symbol _ head') props))
  = RecordDecl rng head' <$> traverse parsePropDecl props

parsePrim :: S.Primitive Range -> StateT BindEnv Result (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseProperty :: ValueType -> S.GenProperty Range -> StateT BindEnv Result (Value Range)
parseProperty _ (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected value, got lowercase symbol"
parseProperty typ (S.GenProperty val) = parseValue typ val

parseRecord :: ValueType -> S.Record Range -> StateT BindEnv Result (Record Range)
parseRecord typ (S.Record rng (S.Symbol _ head') props)
  = Record rng head' <$> traverseDropFatals (parseProperty typ) props

parseBind :: ValueType -> S.Bind Range -> StateT BindEnv Result (Bind Range)
parseBind typ (S.Bind rng sym)
  = case sym of
      Nothing -> do
        when (typ == ValueTypeOutput) $
          tellError $ parseError rng "output bind must have an identifier"
        pure $ Bind rng 0
      Just (S.Symbol _ txt) -> do
        env <- get
        when (typ == ValueTypeOutput && M.notMember txt (bindEnvBinds env)) $
          tellError $ parseError rng "output bind must have the same identifier as an input bind"
        let (idx, newEnv) = bindEnvLookup txt env
        put newEnv
        pure $ Bind rng idx


parseValue :: ValueType -> S.Value Range -> StateT BindEnv Result (Value Range)
parseValue _ (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue typ (S.ValueRecord record) = ValueRecord <$> parseRecord typ record
parseValue typ (S.ValueBind bind) = ValueBind <$> parseBind typ bind
parseValue _ (S.ValueSpliceCode code)
  = mkFail $ parseError (getAnn code) "TODO"

parseReducer :: S.Reducer Range -> Result (Reducer Range)
parseReducer (S.Reducer rng input output)
  = (`evalStateT` emptyBindEnv)
  $ Reducer rng <$> parseValue ValueTypeInput input <*> parseValue ValueTypeOutput output

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: S.Program Range -> Result (Program Range)
parseRaw (S.Program rng topLevels)
    = Program rng
  <$> traverse parseRecordDecl decls
  <*> traverseDropFatals parseReducer reds
  where decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
        reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevels

-- == Validation

duplicateDeclErrs :: [RecordDeclCompact] -> [RecordDecl Range] -> [Error]
duplicateDeclErrs imported decls
  = catMaybes $ zipWith duplicateDeclErr decls (inits decls)
  where duplicateDeclErr (RecordDecl rng head' _) prevs
          | any (\other -> recordDeclCompactHead other == head') others
          = Just $ parseError rng $ "duplicate record declaration: " <> head'
          | otherwise = Nothing
          where others = map compactRecordDecl prevs ++ imported

invalidRecordErrs :: [RecordDeclCompact] -> [Reducer Range] -> [Error]
invalidRecordErrs decls reds
  = concatMap invalidRecordErrorsReducer reds
  where invalidRecordErrorsReducer (Reducer _ input output)
          = invalidRecordErrorsValue input ++ invalidRecordErrorsValue output
        invalidRecordErrorsValue (ValuePrimitive _) = []
        invalidRecordErrorsValue (ValueRecord record) = invalidRecordErrorsRecord record
        invalidRecordErrorsValue (ValueBind _) = []
        invalidRecordErrorsRecord record =
          invalidRecordErrorsRecordIndiv record ++ invalidRecordErrorsRecordChildren record
        invalidRecordErrorsRecordIndiv (Record rng head' props) =
          case find (\decl -> recordDeclCompactHead decl == head') decls of
            Nothing -> [parseError rng $ "undeclared record: " <> head']
            Just decl
              | numDeclProps /= numProps -> [ parseError rng
               $ "record has wrong number of properties: expected "
              <> pprint numDeclProps
              <> ", got "
              <> pprint numProps ]
              | otherwise -> []
              where numDeclProps = recordDeclCompactNumProps decl
                    numProps = length props
        invalidRecordErrorsRecordChildren = concatMap invalidRecordErrorsValue . recordProps

validationErrs :: Program Range -> [Error]
validationErrs (Program _ decls reds)
   = duplicateDeclErrs [] decls
  ++ invalidRecordErrs allDecls reds
  where allDecls = map compactRecordDecl decls -- TODO add builtins and code spliced

-- | Adds syntax errors which didn't affect parsing but would cause problems during compilation.
validate :: Result (Program Range) -> Result (Program Range)
validate res = do
  x <- res
  tellErrors $ validationErrs x
  pure x

-- = Export

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result.
parse :: S.Program Range -> Result (Program Range)
parse = validate . parseRaw
