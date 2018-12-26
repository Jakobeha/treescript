{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module Descript.Ast.Core.Parse
  ( parse
  ) where

import Descript.Ast.Core.Types
import qualified Descript.Ast.Sugar as S
import Descript.Misc

import Data.Maybe
import qualified Data.Text as T

parsePropDecl :: S.GenProperty Range -> Result T.Text
parsePropDecl (S.GenPropertyDecl (S.Symbol _ decl)) = pure decl
parsePropDecl (S.GenProperty (S.Property _ (S.Symbol _ decl) val)) = do
  tellError Error
    { errorStage = StageExtracting
    , errorRange = getAnn val
    , errorMsg = "properties in record declarations can't have definitions"
    }
  pure decl

parseRecordDecl :: S.RecordDecl Range -> Result RecordDecl
parseRecordDecl (S.RecordDecl _ (S.Record _ head' props))
    = RecordDecl
  <$> head'
  <*> traverse parsePropDecl props

parsePrim :: S.Primitive Range -> Result Primitive
parsePrim (S.PrimInteger _ int)
  = pure Primitive
  { primitiveType = "int"
  , primitiveCode = pprint int
  }
parsePrim (S.PrimFloat _ float)
  = pure Primitive
  { primitiveType = "float"
  , primitiveCode = pprint float
  }
parsePrim (S.PrimString _ string)
  = pure Primitive
  { primitiveType = "const char*"
  , primitiveCode = pprint string
  }
parsePrim (S.PrimCode (S.SpliceCode _ lang txt))
  = processCode -- TODO. If 'lang' is a target, processes it into a record. If it's a backend, get its type (from explicit cast or bool) and convert it directly into a primitive.

parseProperty :: (S.Value Range -> Result (Value abs))
              -> S.GenProperty Range
              -> Result (Property abs)
parseProperty _ (S.GenPropertyDecl key)
  = ResultFail Error
  { errorStage = StageExtracting
  , errorRange = getAnn key
  , errorMsg = "properties in records must have definitions"
  }
parseProperty parsePropVal (S.GenProperty (S.Property _ (S.Symbol _ key) val))
  = Property key <$> parsePropVal val

parseRecord :: (S.Value Range -> Result (Value abs))
            -> S.Record Range
            -> Result (Record abs)
parseRecord parsePropVal (S.Record _ head' props)
    = Record
  <$> head'
  <*> traverseDropFatals (parseProperty parsePropVal) props

parseInput :: S.Value Range -> Result InValue
parseInput (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseInput (S.ValueRecord record) = ValueRecord <$> parseRecord parseInput record
parseInput (S.ValueMatcher matcher) = ValueAbstraction <$> parseMatcher matcher
parseInput (S.ValuePath path)
  = ResultFail Error
  { errorStage = StageExtracting
  , errorRange = getAnn path
  , errorMsg = "input values can't have paths. However, you can replace with an equality matcher which uses a path (and an input path might become syntax sugar in the future)"
  }

parseOutput :: S.Value Range -> Result OutValue
parseOutput (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseOutput (S.ValueRecord record) = ValueRecord <$> parseRecord parseOutput record
parseOutput (S.ValueMatcher matcher)
  = ResultFail Error
  { errorStage = StageExtracting
  , errorRange = getAnn matcher
  , errorMsg = "output values can't have matchers"
  }
parseOutput (S.ValuePath path) = ValueAbstraction <$> parsePath path

parseReducer :: S.Reducer Range -> Result Reducer
parseReducer (S.Reducer _ input output)
  = Reducer <$> parseInput input <*> parseOutput output

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result.
parse :: S.Program Range -> Result Program
parse (S.Program _ topLevels)
    = Program
  <$> traverse parseRecordDecl decls
  <*> traverseDropFatals parseReducer reds
  where decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
        reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevels
