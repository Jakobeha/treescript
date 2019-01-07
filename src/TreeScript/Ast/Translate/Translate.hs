{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Generates C code from a @Core@ AST.
module TreeScript.Ast.Translate.Translate
  ( translate
  ) where

import TreeScript.Ast.Translate.Types
import TreeScript.Ast.Core
import TreeScript.Misc
import TreeScript.Plugin

import Data.Maybe
import qualified Data.Text as T
import NeatInterpolation

translateNumProps :: Program an -> SessionRes T.Text
translateNumProps prog = do
  let translateNumPropsDecl (RecordDeclCompact (RecordHead isFunc name) numProps)
        | isFunc = Nothing
        | otherwise = Just
          [text|
            if (strings_equal(head, $nameEncoded)) {
              return $numPropsEncoded;
            }|]
        where nameEncoded = pprint name
              numPropsEncoded = pprint numProps
      translateNumPropsTail
        = [text|
            {
              fprintf(stderr, "unknown record head: %s\n", head);
              exit(1);
            }|]
  decls <- getAllProgramDecls prog
  pure $ T.intercalate " else " $ (mapMaybe translateNumPropsDecl decls ++ [translateNumPropsTail])

consumePrimExpr :: T.Text -> T.Text -> Primitive an -> T.Text
consumePrimExpr suc inp (PrimInteger _ int)
  = [text|
      if ($inp.type == PRIM_INTEGER && $inp.as_integer == $intEncoded) {
        $suc
      }|]
  where intEncoded = pprint int
consumePrimExpr suc inp (PrimFloat _ float)
  = [text|
      if ($inp.type == PRIM_FLOAT && $inp.as_float == $floatEncoded) {
        $suc
      }|]
  where floatEncoded = pprint float
consumePrimExpr suc inp (PrimString _ str)
  = [text|
      if ($inp.type == PRIM_STRING && strings_equal($inp.as_string, $strEncoded)) {
        //printf("<Consume primitive %s> ", $strEncoded);
        $suc
      }|]
  where strEncoded = pprint str

consumePropsExpr :: T.Text -> T.Text -> Int -> [Value an] -> T.Text
consumePropsExpr suc _ _ [] = suc
consumePropsExpr suc inp n (prop : props)
  = [text|
      value $inp' = ${inp}_props[$nEncoded];
      $consumeProps|]
  where nEncoded = pprint n
        inp' = inp <> "_" <> nEncoded
        consumeRest = consumePropsExpr suc inp (n + 1) props
        consumeProps = consumeValueExpr consumeRest inp' prop

consumeRecordExpr :: T.Text -> T.Text -> Record an -> T.Text
consumeRecordExpr suc inp (Record _ (RecordHead False head') props)
  = [text|
      if ($inp.type == RECORD && strings_equal($inp.as_record.head, $headEncoded)) {
        //printf("<Consume record %s> ", $headEncoded);
        value* ${inp}_props = $inp.as_record.props;
        $consumeProps
      }|]
  where headEncoded = pprint head'
        consumeProps = consumePropsExpr suc inp 0 props
consumeRecordExpr suc inp (Record _ (RecordHead True head') props)
  = [text|
      {
        value func_props[$numPropsEncoded];
        $produceProps
        value func_out = apply_function($headEncoded, func_props);
        temp_bool = values_equal($inp, func_out);
        for (int i = 0; i < $numPropsEncoded; i++) {
          free_value(func_props[i]);
        }
        free_value(func_out);
      }
      if (temp_bool) {
        $suc
      }|]
  where headEncoded = pprint head'
        numPropsEncoded = pprint $ length props
        produceProps = producePropsExpr "func" props

consumeBindExpr :: T.Text -> T.Text -> Bind an -> T.Text
consumeBindExpr suc inp (Bind _ idx)
  | idx == 0 = suc
  | otherwise
  = [text|
      if (!set_matches[$idx0Encoded] || values_equal($inp, matches[$idx0Encoded])) {
        //printf("<Consume bind %d> ", $idx0Encoded);
        set_matches[$idx0Encoded] = true;
        matches[$idx0Encoded] = $inp;
        $suc
      }|]
  where idx0Encoded = pprint $ idx - 1

consumeValueExpr :: T.Text -> T.Text -> Value an -> T.Text
consumeValueExpr suc inp (ValuePrimitive prim) = consumePrimExpr suc inp prim
consumeValueExpr suc inp (ValueRecord record) = consumeRecordExpr suc inp record
consumeValueExpr suc inp (ValueBind bind) = consumeBindExpr suc inp bind

consumeClauseExpr :: T.Text -> T.Text -> ReducerClause an -> T.Text
consumeClauseExpr suc inp (ReducerClause _ value groups)
  = consumeValueExpr suc inp value -- TODO Add groups

producePrimExpr :: T.Text -> Primitive an -> T.Text
producePrimExpr out (PrimInteger _ int)
  = [text|
      value $out = {
        .type = PRIM_INTEGER,
        .as_integer = $intEncoded
      };|]
  where intEncoded = pprint int
producePrimExpr out (PrimFloat _ float)
  = [text|
      value $out = {
        .type = PRIM_FLOAT,
        .as_float = $floatEncoded
      };|]
  where floatEncoded = pprint float
producePrimExpr out (PrimString _ str)
  = [text|
      value $out = {
        .type = PRIM_STRING,
        .as_string = strdup($strEncoded)
      };|]
  where strEncoded = pprint str

producePropExpr :: T.Text -> Int -> Value an -> T.Text
producePropExpr out n prop
  = [text|
      $produceValue
      ${out}_props[$nEncoded] = $out';|]
  where produceValue = produceValueExpr out' prop
        nEncoded = pprint n
        out' = out <> "_" <> nEncoded

producePropsExpr :: T.Text -> [Value an] -> T.Text
producePropsExpr out
  = T.unlines . zipWith (producePropExpr out) [0..]

produceRecordExpr :: T.Text -> Record an -> T.Text
produceRecordExpr out (Record _ (RecordHead False head') props)
  = [text|
      value* ${out}_props = malloc0(sizeof(value) * $numPropsEncoded);
      $produceProps
      value $out = {
        .type = RECORD,
        .as_record = (value_record){
          .head = $headEncoded,
          .num_props = $numPropsEncoded,
          .props = ${out}_props
        }
      };|]
  where produceProps = producePropsExpr out props
        headEncoded = pprint head'
        numPropsEncoded = pprint $ length props
produceRecordExpr out (Record _ (RecordHead True head') props)
  = [text|
      value $out;
      {
        value func_props[$numPropsEncoded];
        $produceProps
        $out = apply_function($headEncoded, func_props);
        for (int i = 0; i < $numPropsEncoded; i++) {
          free_value(func_props[i]);
        }
      }|]
  where headEncoded = pprint head'
        numPropsEncoded = pprint $ length props
        produceProps = producePropsExpr "func" props

produceBindExpr :: T.Text -> Bind an -> T.Text
produceBindExpr out (Bind _ idx)
  | idx == 0
  = [text|
      fprintf(stderr, "WARNING: Output bind with unbound identifier. The TreeScript source this was compiled from should be fixed.");
      $out = {
        .type = PRIM_STRING,
        .as_string = strdup("<WARNING: output bind with unbound identifier>")
      };|] -- Should never happen in valid code
  | otherwise = [text|value $out = dup_value(matches[$idx0Encoded]);|]
  where idx0Encoded = pprint $ idx - 1

produceValueExpr :: T.Text -> Value an -> T.Text
produceValueExpr out (ValuePrimitive prim) = producePrimExpr out prim
produceValueExpr out (ValueRecord record) = produceRecordExpr out record
produceValueExpr out (ValueBind bind) = produceBindExpr out bind

produceClauseExpr :: T.Text -> ReducerClause an -> T.Text
produceClauseExpr out (ReducerClause _ value groups)
  = produceValueExpr out value -- TODO add groups

reduceExpr :: Int -> Reducer an -> T.Text
reduceExpr maxNumBinds (Reducer _ input output)
  = [text|
      for (int i = 0; i < $maxNumBindsEncoded; i++) {
        set_matches[i] = false;
      }
      $consumeAndProduce|]
  where maxNumBindsEncoded = pprint maxNumBinds
        consumeAndProduce = consumeClauseExpr suc "in" input
        suc
          = [text|
              //printf("<Produce!> ");
              $produceOut
              free_value(in);
              *x = out;
              return true;|]
        produceOut = produceClauseExpr "out" output

translateReduceSurface :: Program an -> SessionRes T.Text
translateReduceSurface prog = do
  let reducers = programMainReducers prog
      maxNumBinds = maxNumBindsInReducers reducers
      maxNumBindsEncoded = pprint maxNumBinds
      reduceExprs = T.unlines $ map (reduceExpr maxNumBinds) reducers
  pure $
    [text|
      value in = *x;
      bool set_matches[$maxNumBindsEncoded];
      value matches[$maxNumBindsEncoded];
      bool temp_bool;
      $reduceExprs
      //printf("<Reduce fail> ");
      return false;|]

-- | Generates C code from a @Core@ AST.
translate :: Program an -> SessionRes Translated
translate prog = do
  numProps <- translateNumProps prog
  reduceSurface <- translateReduceSurface prog
  pure Translated
    { translatedNumProps = numProps
    , translatedReduceSurface = reduceSurface
    }
