{
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Descript.Ast.Sugar.Parse
  ( parse
  ) where

import qualified Descript.Ast.Lex as L
import Descript.Ast.Sugar.Types
import Descript.Misc

import qualified Data.List.NonEmpty as N
}

%name parseSequence
%error { parseError }
%monad { Result }
%tokentype { L.Lexeme Range }
%token
  '<' { L.LexemePunc (L.PuncAngleBwd $$) }
  '>' { L.LexemePunc (L.PuncAngleFwd $$) }
  '=' { L.LexemePunc (L.PuncEqual $$) }
  ':' { L.LexemePunc (L.PuncColon $$) }
  '.' { L.LexemePunc (L.PuncPeriod $$) }
  ';' { L.LexemePunc (L.PuncSemicolon $$) }
  '[' { L.LexemePunc (L.PuncOpenBracket $$) }
  ']' { L.LexemePunc (L.PuncCloseBracket $$) }
  eof { L.LexemePunc (L.PuncEof $$) }
  int { L.LexemePrim (L.PrimInteger $$) }
  float { L.LexemePrim (L.PrimFloat $$) }
  string { L.LexemePrim (L.PrimString $$) }
  codeWhole { L.LexemePrim (L.PrimCode (L.SpliceFrag $$ True True)) }
  codeStart { L.LexemePrim (L.PrimCode (L.SpliceFrag $$ True False)) }
  codeMiddle { L.LexemePrim (L.PrimCode (L.SpliceFrag $$ False False)) }
  codeEnd { L.LexemePrim (L.PrimCode (L.SpliceFrag $$ False True)) }
  upperSym { L.LexemeSymbol (L.Symbol $$ L.SymbolCaseUpper) }
  lowerSym { L.LexemeSymbol (L.Symbol $$ L.SymbolCaseLower) }

%%

Program : eof { Program $1 [] }
        | NonEmptyProgram eof { Program (boundingRange $ $2 N.:| map getAnn $1) (reverse $1) }
        ;
NonEmptyProgram : TopLevel { [$1] }
                | NonEmptyProgram TopLevel { $2 : $1 }
                ;
TopLevel : RecordDecl { TopLevelRecordDecl $1 }
         | Reducer { TopLevelReducer $1 }
         ;
RecordDecl : Record '.' { RecordDecl (boundingRange [getAnn $1, $2]) $1 }
           ;
Reducer : Value ':' Value ';' { Reducer (boundingRange [getAnn $1, $2, getAnn $3, $4]) $1 $3 }
        ;
Value : Primitive { ValuePrimitive $1 }
      | Record { ValueRecord $1 }
      | Matcher { ValueMatcher $1 }
      | Path { ValuePath $1 }
      ;
Primitive : int { PrimInteger (getAnn $1) (annd $1) }
          | float { PrimFloat (getAnn $1) (annd $1) }
          | string { PrimString (getAnn $1) (annd $1) }
          | SpliceCode { PrimCode $1 }
          ;
SpliceCode : LowerSym SpliceText { SpliceCode (boundingRange [getAnn $1, getAnn $2]) $1 $2 }
           ;
SpliceText : codeWhole { SpliceTextNil (getAnn $1) (annd $1) }
           | codeStart Value SpliceTextTail { SpliceTextCons (boundingRange [getAnn $1, getAnn $2, getAnn $3]) (annd $1) $2 $3 }
           ;
SpliceTextTail : codeEnd { SpliceTextNil (getAnn $1) (annd $1) }
               | codeMiddle Value SpliceTextTail { SpliceTextCons (boundingRange [getAnn $1, getAnn $2, getAnn $3]) (annd $1) $2 $3 }
               ;
Record : UpperSym '[' ']' { Record (boundingRange [getAnn $1, $2, $3]) $1 [] }
       | UpperSym '[' NonEmptyGenProperties ']' { Record (boundingRange $ (getAnn $1) N.<| $2 N.<| $4 N.:| map getAnn $3) $1 (reverse $3) }
       ;
NonEmptyGenProperties : GenProperty { [$1] }
                      | NonEmptyGenProperties ';' GenProperty { $3 : $1 }
                      ;
GenProperty : LowerSym { GenPropertyDecl $1 }
            | Property { GenProperty $1 }
            ;
Property : LowerSym ':' Value { Property (boundingRange [getAnn $1, $2, getAnn $3]) $1 $3 }
Matcher : '<' { Matcher $1 Nothing Nothing }
        | '<' LowerSym { Matcher (boundingRange [$1, getAnn $2]) (Just $2) Nothing }
        | '<' '?' Value { Matcher (boundingRange [$1, $2, getAnn $3]) Nothing (Just $3) }
        | '<' LowerSym '?' Value { Matcher (boundingRange [$1, getAnn $2, $3, getAnn $4]) (Just $2) (Just $4) }
        ;
Path : '>' LowerSym { Path (boundingRange [$1, getAnn $2]) $2 }
     ;
LowerSym : lowerSym { Symbol (getAnn $1) (annd $1) }
         ;
UpperSym : upperSym { Symbol (getAnn $1) (annd $1) }
         ;

{
parse :: L.Program Range -> Result (Program Range)
parse (L.Program lexemes) = parseSequence $ annd lexemes

parseError :: [L.Lexeme Range] -> Result a
parseError rem
  = ResultFail Error
  { errorStage = StageParsing
  , errorMsg = errorDesc rem
  }
  where errorDesc rem = "gave up at " <> locDesc rem
        locDesc [] = "end of file (not finished?)"
        locDesc (lexeme : _)
          = pprint (rangeStart (getAnn lexeme)) <> " (" <> lexDesc lexeme <> ")"
        lexDesc (L.LexemePunc (L.PuncEof _)) = "end of file - not finished?"
        lexDesc lexeme = "before \"" <> pprint lexeme <> "\""
}
