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

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import NeatInterpolation

type ExtraFuncs an a = WriterT [(Int, Bool, [Statement an])] (State Int) a

-- * Libraries

libCopy :: Library -> CopyInfo
libCopy lib = CopyInfo (libraryCodeDir lib) (T.unpack $ librarySpecName $ librarySpec lib)

libImport :: Library -> T.Text
libImport library = "#include \"" <> name <> "/interface.h\""
  where name = librarySpecName $ librarySpec library

translateLibraries :: Program an -> SessionRes ([CopyInfo], T.Text)
translateLibraries prog = do
  libs <- getAllProgramUsedLibraries prog
  let libCopies = map libCopy libs
      libImports = T.unlines $ map libImport libs
  pure (libCopies, libImports)

-- * Num Props

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

-- * Reduce

reduceSetup :: T.Text
reduceSetup
  = [text|
      for (int i = 0; i < MAX_NUM_BINDS; i++) {
        //matches[i] = dup_match(in_matches[i]);
        matches[i] = in_matches[i];
      }|]

reduceCleanup :: T.Text
reduceCleanup
  = [text|
      for (int i = 0; i < MAX_NUM_BINDS; i++) {
        if (matches[i].is_set) {
          //free_value(matches[i].value);
          matches[i].is_set = false;
        }
      }|]

standardReduceEnd :: T.Text
standardReduceEnd
  = [text|
      reduced = true;
      in = *x;|]

repeatReduceEnd :: T.Text
repeatReduceEnd
  = [text|
      $reduceCleanup
      return true;|]

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
        value func_out = call_$head'(func_props);
        temp_bool = values_equal($inp, func_out);
        for (int i = 0; i < $numPropsEncoded; i++) {
          free_value(func_props[i]);
        }
        free_value(func_out);
      }
      if (temp_bool) {
        $suc
      }|]
  where numPropsEncoded = pprint $ length props
        produceProps = producePropsExpr False "func" props

consumeBindExpr :: T.Text -> T.Text -> Bind an -> T.Text
consumeBindExpr suc inp (Bind _ idx)
  | idx == 0 = suc
  | otherwise
  = [text|
      if (!matches[$idx0Encoded].is_set || values_equal($inp, matches[$idx0Encoded].value)) {
        //printf("<Consume bind %d> ", $idx0Encoded);
        matches[$idx0Encoded] = (match){
          .is_set = true,
          .value = dup_value($inp)
        };
        $suc
      }|]
  where idx0Encoded = pprint $ idx - 1

consumeValueExpr :: T.Text -> T.Text -> Value an -> T.Text
consumeValueExpr suc inp (ValuePrimitive prim) = consumePrimExpr suc inp prim
consumeValueExpr suc inp (ValueRecord record) = consumeRecordExpr suc inp record
consumeValueExpr suc inp (ValueBind bind) = consumeBindExpr suc inp bind

consumeGroupGroupExpr :: V.Vector (GroupDef an) -> T.Text -> GroupRef an -> ExtraFuncs an T.Text
consumeGroupGroupExpr groupDefs suc groupRef
  = T.unlines <$> traverse (consumeGroupStatementExpr groupDefs suc) stmts
  where (_, stmts) = allGroupRefStatements groupDefs groupRef

requireBind :: T.Text -> Int -> T.Text
requireBind suc idx
  = [text|
      if (matches[$idx0Encoded].is_set) {
        $suc
      }|]
  where idx0Encoded = pprint $ idx - 1

consumeGroupReducerExpr :: V.Vector (GroupDef an) -> T.Text -> Reducer an -> ExtraFuncs an T.Text
consumeGroupReducerExpr groups suc (Reducer _ input output) = do
  produceInput <- produceClauseExpr groups "aux" input
  consumeOutput <- consumeClauseExpr groups "temp_bool = true;" "aux" output
  let binds = bindsInClause False input
      bindsSuc
        = [text|
            temp_bool = false;
            {
              $produceInput
              reduce(&aux);
              $consumeOutput
              free_value(aux);
            }|]
      checkReducer = S.foldl' requireBind bindsSuc binds
  pure
    [text|
      temp_bool = true;
      $checkReducer
      if (temp_bool) {
        $suc
      }|]

consumeGroupStatementExpr :: V.Vector (GroupDef an) -> T.Text -> Statement an -> ExtraFuncs an T.Text
consumeGroupStatementExpr groupDefs suc (StatementGroup groupRef) = consumeGroupGroupExpr groupDefs suc groupRef
consumeGroupStatementExpr groups suc (StatementReducer red) = consumeGroupReducerExpr groups suc red

consumeGroupExpr :: V.Vector (GroupDef an) -> T.Text -> GroupRef an -> ExtraFuncs an T.Text
consumeGroupExpr groups suc group
  = foldM (\suc' -> consumeGroupStatementExpr groups suc') suc stmts
  where (_, stmts) = allGroupRefStatements groups group

consumeClauseExpr :: V.Vector (GroupDef an) -> T.Text -> T.Text -> ReducerClause an -> ExtraFuncs an T.Text
consumeClauseExpr groupDefs suc inp (ReducerClause _ value groupRefs) = do
  consumeGroups <- foldM (\suc' -> consumeGroupExpr groupDefs suc') suc groupRefs
  pure $ consumeValueExpr consumeGroups inp value

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

producePropExpr :: Bool -> T.Text -> Int -> Value an -> T.Text
producePropExpr flush out n prop
  = [text|
      $produceValue
      ${out}_props[$nEncoded] = $out';|]
  where produceValue = produceValueExpr flush out' prop
        nEncoded = pprint n
        out' = out <> "_" <> nEncoded

producePropsExpr :: Bool -> T.Text -> [Value an] -> T.Text
producePropsExpr flush out
  = T.unlines . zipWith (producePropExpr flush out) [0..]

produceRecordExpr :: Bool -> T.Text -> Record an -> T.Text
produceRecordExpr flush out (Record _ (RecordHead False head') props)
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
  where produceProps = producePropsExpr flush out props
        headEncoded = pprint head'
        numPropsEncoded = pprint $ length props
produceRecordExpr flush out (Record _ (RecordHead True head') props)
  | head' == "Flush"
  = case props of
      [prop] -> produceValueExpr True out prop
      _  ->
        [text|
          fprintf(stderr, "WARNING: #Flush doesn't have exactly 1 argument. The TreeScript source this was compiled from should be fixed.");
          $out = {
            .type = PRIM_STRING,
            .as_string = strdup("<WARNING: #Flush without exactly 1 argument>")
          };|] -- Should never happen in valid code
  | otherwise
  = [text|
      value $out;
      {
        value func_props[$numPropsEncoded];
        $produceProps
        $out = call_$head'(func_props);
        for (int i = 0; i < $numPropsEncoded; i++) {
          free_value(func_props[i]);
        }
      }|]
  where numPropsEncoded = pprint $ length props
        produceProps = producePropsExpr flush "func" props

produceBindExpr :: Bool -> T.Text -> Bind an -> T.Text
produceBindExpr flush out (Bind _ idx)
  | idx == 0
  = [text|
      fprintf(stderr, "WARNING: Output bind with unbound identifier. The TreeScript source this was compiled from should be fixed.");
      $out = {
        .type = PRIM_STRING,
        .as_string = strdup("<WARNING: output bind with unbound identifier>")
      };|] -- Should never happen in valid code
  | flush
  = [text|
      value $out = matches[$idx0Encoded].is_set ?
        dup_value(matches[$idx0Encoded].value) :
        hole_value($idx0Encoded);|]
  | otherwise
  = [text|
      assert(matches[$idx0Encoded].is_set);
      value $out = dup_value(matches[$idx0Encoded].value);|]
  where idx0Encoded = pprint $ idx - 1

produceValueExpr :: Bool -> T.Text -> Value an -> T.Text
produceValueExpr _ out (ValuePrimitive prim) = producePrimExpr out prim
produceValueExpr flush out (ValueRecord record) = produceRecordExpr flush out record
produceValueExpr flush out (ValueBind bind) = produceBindExpr flush out bind

applyGroupExpr :: V.Vector (GroupDef an) -> T.Text -> T.Text -> GroupRef an -> ExtraFuncs an (Int, T.Text)
applyGroupExpr groupDefs matches outRef groupRef = do
  idx <- get
  let idxEncoded = pprint idx
      (repeats, stmts) = allGroupRefStatements groupDefs groupRef
  tell [(idx, repeats, stmts)]
  put $ idx + 1
  pure (idx, [text|reduce_aux(reduce_extra_$idxEncoded, REDUCE_STANDARD, $matches, $outRef)|])

produceGroupExpr :: V.Vector (GroupDef an) -> T.Text -> GroupRef an -> ExtraFuncs an T.Text
produceGroupExpr groupDefs out = fmap ((<> ";") . snd) . applyGroupExpr groupDefs "matches" ("&" <> out)

produceClauseExpr :: V.Vector (GroupDef an) -> T.Text -> ReducerClause an -> ExtraFuncs an T.Text
produceClauseExpr groupDefs out (ReducerClause _ value groupRefs)
  = T.unlines . (produceValue :) <$> produceGroups
  where produceValue = produceValueExpr False out value
        produceGroups = traverse (produceGroupExpr groupDefs out) groupRefs

reduceGroupExpr :: V.Vector (GroupDef an) -> T.Text -> GroupRef an -> ExtraFuncs an T.Text
reduceGroupExpr groupDefs end groupRef = do
  (idx, applyGroup) <- applyGroupExpr groupDefs "in_matches" "x" groupRef
  let idxEncoded = pprint idx
  pure
    [text|
      if (type != REDUCE_EVALCTX && $applyGroup) {
        //printf("<Produce extra %d> ", $idxEncoded);
        $end
      }|]

reduceReducerExpr :: V.Vector (GroupDef an) -> T.Text -> T.Text -> Reducer an -> ExtraFuncs an T.Text
reduceReducerExpr groups end funcName (Reducer _ input output) = do
  produceOut <- produceClauseExpr groups "out" output
  let suc
        = [text|
            //printf("<Produce!> ");
            $produceOut
            switch (type) {
            case REDUCE_STANDARD:
              free_value(in);
              *x = out;
              $end
              break;
            case REDUCE_EVALCTX:
              if (reduce_nested(reduce_$funcName, in_matches, &out)) {
                free_value(in);
                *x = out;
                $end
              } else {
                free_value(out);
              }
              break;
            }|]
  consumeAndProduce <- consumeClauseExpr groups suc "in" input
  pure
    [text|
      $reduceSetup
      $consumeAndProduce
      $reduceCleanup|]

reduceStatementExpr :: V.Vector (GroupDef an) -> T.Text -> T.Text -> Statement an -> ExtraFuncs an T.Text
reduceStatementExpr groupDefs end _ (StatementGroup groupRef) = reduceGroupExpr groupDefs end groupRef
reduceStatementExpr groups end funcName (StatementReducer red) = reduceReducerExpr groups end funcName red

translateExtraReduceSurface :: V.Vector (GroupDef an) -> (Int, Bool, [Statement an]) -> WriterT T.Text (State Int) ()
translateExtraReduceSurface groups (idx, repeats, stmts) = do
  let idxEncoded = pprint idx
      end
        | repeats = repeatReduceEnd
        | otherwise = standardReduceEnd
      funcName
        | repeats = "extra_" <> idxEncoded <> "_once"
        | otherwise = "extra_" <> idxEncoded
  body <- translateReduceSurface groups end funcName stmts
  if repeats then
    tell
      [text|
        bool reduce_extra_$idxEncoded(reduce_type type, const match_arr in_matches, value* x);
        bool reduce_extra_${idxEncoded}_once(reduce_type type, const match_arr in_matches, value* x) {
          //printf("<Reduce extra %d> ", $idxEncoded);
          $body
        }
        bool reduce_extra_$idxEncoded(reduce_type type, const match_arr in_matches, value* x) {
          if (reduce_extra_${idxEncoded}_once(type, in_matches, x)) {
            reduce_aux(reduce_extra_${idxEncoded}, REDUCE_STANDARD, in_matches, x);
            return true;
          } else {
            return false;
          }
        }
      |]
  else
    tell
      [text|
        bool reduce_extra_$idxEncoded(reduce_type type, const match_arr in_matches, value* x) {
          //printf("<Reduce extra %d> ", $idxEncoded);
          $body
        }
      |]

translateReduceSurface :: V.Vector (GroupDef an) -> T.Text -> T.Text -> [Statement an] -> WriterT T.Text (State Int) T.Text
translateReduceSurface groups end funcName stmts = do
  (reduceExprs, extraReduces)
     <- lift
      $ runWriterT
      $ T.unlines
    <$> traverse (reduceStatementExpr groups end funcName) stmts
  let body
        = [text|
            bool reduced = false;
            value in = *x;
            match_arr matches;
            bool temp_bool;
            $reduceExprs
            //printf("<Reduce end %d> ", reduced);
            return reduced;|]
  forM_ extraReduces $ translateExtraReduceSurface groups
  pure body

translateReduceSurfaces :: Program an -> SessionRes (T.Text, T.Text)
translateReduceSurfaces prog = do
  let mainStmts = programMainStatements prog
      groups = V.fromList $ programGroups prog
  pure
    $ (`evalState` 0)
    $ runWriterT
    $ translateReduceSurface groups standardReduceEnd "main" mainStmts

-- | Generates C code from a @Core@ AST.
translate :: Program an -> SessionRes Translated
translate prog = do
  (libCopies, libImports) <- translateLibraries prog
  let maxNumBinds = pprint $ maxNumBindsInProgram prog
  numProps <- translateNumProps prog
  (mainReduceSurface, extraReduceSurfaces) <- translateReduceSurfaces prog
  pure Translated
    { translatedLibCopies = libCopies
    , translatedLibImports = libImports
    , translatedMaxNumBinds = maxNumBinds
    , translatedNumProps = numProps
    , translatedMainReduceSurface = mainReduceSurface
    , translatedExtraReduceSurfaces = extraReduceSurfaces
    }
