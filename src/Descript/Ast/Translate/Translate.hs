{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Generates C code from a @Core@ AST.
module Descript.Ast.Translate.Translate
  ( translate
  ) where

import Descript.Ast.Translate.Types
import Descript.Ast.Core
import Descript.Misc
import Descript.Plugin

import qualified Data.Text as T
import NeatInterpolation

translateNumProps :: Program an -> SessionRes T.Text
translateNumProps prog = do
  let translateNumPropsDecl (RecordDeclCompact name numProps)
        = [text|
            if (strings_equal(head, "$name")) {
              return $numPropsEncoded;
            }|]
        where numPropsEncoded = pprint numProps
      translateNumPropsTail
        = [text|
            {
              fprintf(stderr, "unknown record head: %s\n", head);
              exit(1);
            }|]
  decls <- getAllProgramDecls prog
  pure $ T.intercalate " else " $ (map translateNumPropsDecl decls ++ [translateNumPropsTail])

numBindsInValue1 :: Value an -> Int
numBindsInValue1 (ValuePrimitive _) = 0
numBindsInValue1 (ValueRecord _) = 0
numBindsInValue1 (ValueBind (Bind _ idx)) = idx

maxNumBindsInProgram :: Program an -> Int
maxNumBindsInProgram = getMax0 . foldValuesInProgram (Max0 . numBindsInValue1)

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
        consumeProps = consumeExpr consumeRest inp' prop

consumeRecordExpr :: T.Text -> T.Text -> Record an -> T.Text
consumeRecordExpr suc inp (Record _ head' props)
  = [text|
      if ($inp.type == PRIM_RECORD && strings_equal($inp.as_record.head, $headEncoded)) {
        value* ${inp}_props = $inp.as_record.props;
        $consumeProps
      }|]
  where headEncoded = pprint head'
        consumeProps = consumePropsExpr suc inp 0 props

consumeBindExpr :: T.Text -> T.Text -> Bind an -> T.Text
consumeBindExpr suc inp (Bind _ idx)
  | idx == 0 = suc
  | otherwise
  = [text|
      if (!set_matches[$idx0Encoded] || values_equal($inp, matches[$idx0Encoded])) {
        set_matches[$idx0Encoded] = true;
        matches[$idx0Encoded] = $inp;
        $suc
      }|]
  where idx0Encoded = pprint $ idx - 1

consumeExpr :: T.Text -> T.Text -> Value an -> T.Text
consumeExpr suc inp (ValuePrimitive prim) = consumePrimExpr suc inp prim
consumeExpr suc inp (ValueRecord record) = consumeRecordExpr suc inp record
consumeExpr suc inp (ValueBind bind) = consumeBindExpr suc inp bind

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
  where produceValue = produceExpr out' prop
        nEncoded = pprint n
        out' = out <> "_" <> nEncoded

producePropsExpr :: T.Text -> [Value an] -> T.Text
producePropsExpr out
  = T.unlines . zipWith (producePropExpr out) [0..]

produceRecordExpr :: T.Text -> Record an -> T.Text
produceRecordExpr out (Record _ head' props)
  = [text|
      value* ${out}_props = malloc(sizeof(value) * $numPropsEncoded);
      $produceProps
      value $out = {
        .type = RECORD,
        .as_record = (value_record){
          .head = strdup($headEncoded),
          .num_props = $numPropsEncoded,
          .props = ${out}_props
        }
      };|]
  where produceProps = producePropsExpr out props
        headEncoded = pprint head'
        numPropsEncoded = pprint $ length props

produceBindExpr :: T.Text -> Bind an -> T.Text
produceBindExpr out (Bind _ idx)
  | idx == 0
  = [text|
      fprintf(stderr, "WARNING: Output bind with unbound identifier. The Descript source this was compiled from should be fixed.");
      $out = {
        .type = PRIM_STRING,
        .as_string = strdup("<WARNING: output bind with unbound identifier>")
      };|] -- Should never happen in valid code
  | otherwise = [text|value $out = matches[$idx0Encoded];|]
  where idx0Encoded = pprint $ idx - 1

produceExpr :: T.Text -> Value an -> T.Text
produceExpr out (ValuePrimitive prim) = producePrimExpr out prim
produceExpr out (ValueRecord record) = produceRecordExpr out record
produceExpr out (ValueBind bind) = produceBindExpr out bind

reduceExpr :: Reducer an -> T.Text
reduceExpr (Reducer _ input output) = consumeExpr suc "in" input
  where suc
          = [text|
              $produceOut
              *x = out;
              return true;|]
        produceOut = produceExpr "out" output

translateReduce :: Program an -> SessionRes T.Text
translateReduce prog = do
  let maxNumBindsEncoded = pprint $ maxNumBindsInProgram prog
      reduceExprs = T.unlines $ map reduceExpr $ programReducers prog
  pure $
    [text|
      value in = *x;
      bool set_matches[$maxNumBindsEncoded];
      value matches[$maxNumBindsEncoded];
      $reduceExprs
      return false;|]

-- | Generates C code from a @Core@ AST.
translate :: Program an -> SessionRes Translated
translate prog = do
  numProps <- translateNumProps prog
  reduce <- translateReduce prog
  pure Translated
    { translatedNumProps = numProps
    , translatedReduce = reduce
    }
