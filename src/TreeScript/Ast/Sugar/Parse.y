{
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Sugar.Parse
  ( parse
  ) where

import qualified TreeScript.Ast.Lex as L
import TreeScript.Ast.Sugar.Types
import TreeScript.Misc

import qualified Data.List.NonEmpty as N
import Data.Semigroup
}

%name parseSequence
%error { parseError }
%monad { Result }
%tokentype { L.Lexeme Range }
%token
  '---' { L.LexemePunc (L.PuncLineSep $$) }
  '...' { L.LexemePunc (L.PuncEllipsis $$) }
  '_' { L.LexemePunc (L.PuncUnderscore $$) }
  '#' { L.LexemePunc (L.PuncHash $$) }
  '\\' { L.LexemePunc (L.PuncBackSlash $$) }
  '&' { L.LexemePunc (L.PuncAnd $$) }
  ':' { L.LexemePunc (L.PuncColon $$) }
  '.' { L.LexemePunc (L.PuncPeriod $$) }
  ';' { L.LexemePunc (L.PuncSemicolon $$) }
  ',' { L.LexemePunc (L.PuncComma $$) }
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
        | NonEmptyProgram eof { Program (sconcat $ $2 N.:| map getAnn $1) (reverse $1) }
        ;
NonEmptyProgram : TopLevel { [$1] }
                | NonEmptyProgram TopLevel { $2 : $1 }
                ;
TopLevel : RecordDecl { TopLevelRecordDecl $1 }
         | TopLevelStatement { TopLevelStatement $1 }
         | GroupDecl { TopLevelGroupDecl $1 }
         ;
RecordDecl : Record '.' { RecordDecl (getAnn $1 <> $2) $1 }
           ;
TopLevelStatement : Statement ';' { $1 }
                  ;
Statement : Value { StatementGroup $1 }
          | Reducer { StatementReducer $1 }
          ;
GroupDecl : Group '---' { GroupDecl (getAnn $1 <> $2) $1 }
Reducer : Value ':' Value Groups Guards
        { Reducer (sconcat $ getAnn $1 N.<| $2 N.<| getAnn $3 N.:| map getAnn $4 <> map getAnn $5) $1 $3 (reverse $4) (reverse $5) }
        ;
Groups : { [] }
       | NonEmptyGroups { $1 }
       ;
NonEmptyGroups : Group { [$1] }
               | NonEmptyGroups Group { $2 : $1 }
               ;
Guards : { [] }
       | NonEmptyGuards { $1 }
       ;
NonEmptyGuards : Guard { [$1] }
               | NonEmptyGuards Guard { $2 : $1 }
               ;
Guard : ',' Statement { $2 }
Value : Primitive { ValuePrimitive $1 }
      | Record { ValueRecord $1 }
      | Bind { ValueBind $1 }
      | SpliceCode { ValueSpliceCode $1 }
      | Hole { ValueHole $1 }
      | Group { ValueGroup $1 }
      ;
Primitive : int { PrimInteger (getAnn $1) (annd $1) }
          | float { PrimFloat (getAnn $1) (annd $1) }
          | string { PrimString (getAnn $1) (annd $1) }
          ;
Record : UpperSym GenProperties { Record (getAnn $1 <> getAnn $2) False $1 (annd $2) }
       | '#' UpperSym GenProperties { Record ($1 <> getAnn $2 <> getAnn $3) True $2 (annd $3) }
       ;
Group : '&' LowerSym GenProperties { Group ($1 <> getAnn $2 <> getAnn $3) True $2 [] (annd $3) }
      | '&' UpperSym GenProperties { Group ($1 <> getAnn $2 <> getAnn $3) False $2 [] (annd $3) }
      | '&' LowerSym GenProperties GenProperties { Group ($1 <> getAnn $2 <> getAnn $3 <> getAnn $4) True $2 (annd $3) (annd $4) }
      | '&' UpperSym GenProperties GenProperties { Group ($1 <> getAnn $2 <> getAnn $3 <> getAnn $4) False $2 (annd $3) (annd $4) }
      ;
GenProperties : '[' ']' { Annd ($1 <> $2) [] }
              | '[' NonEmptyGenProperties ']' { Annd (sconcat $ $1 N.<| $3 N.:| map getAnn $2) (reverse $2) }
              ;
NonEmptyGenProperties : GenProperty { [$1] }
                      | NonEmptyGenProperties ';' GenProperty { $3 : $1 }
                      ;
GenProperty : LowerSym { GenPropertyDecl $1 }
            | SubGroupProperty { GenPropertySubGroup $1 }
            | Value { GenPropertyRecord $1 }
            ;
SubGroupProperty : '&' LowerSym { SubGroupProperty ($1 <> getAnn $2) $2 }
                 ;
Bind : '\\' BindTarget { Bind ($1 <> getAnn $2) $2 }
     ;
BindTarget : '_' { BindTargetNone $1 }
           | LowerSym { BindTargetSome $1 }
           ;
SpliceCode : LowerSym SpliceText { SpliceCode (getAnn $1 <> getAnn $2) $1 $2 }
           ;
SpliceText : codeWhole { SpliceTextNil (getAnn $1) (annd $1) }
           | codeStart Splice SpliceTextTail
           { SpliceTextCons (getAnn $1 <> getAnn $2 <> getAnn $3) (annd $1) False $2 $3 }
           | codeStart '...' Splice SpliceTextTail
           { SpliceTextCons (getAnn $1 <> $2 <> getAnn $3 <> getAnn $4) (annd $1) True $3 $4 }
           ;
Splice : BindTarget { SpliceBind $1 }
       | int { SpliceHole (HoleIdx (getAnn $1) (annd $1)) }
       ;
SpliceTextTail : codeEnd { SpliceTextNil (getAnn $1) (annd $1) }
               | codeMiddle Splice SpliceTextTail
               { SpliceTextCons (getAnn $1 <> getAnn $2 <> getAnn $3) (annd $1) False $2 $3 }
               | codeMiddle '...' Splice SpliceTextTail
               { SpliceTextCons (getAnn $1 <> $2 <> getAnn $3 <> getAnn $4) (annd $1) True $3 $4 }
               ;
Hole : '\\' int { Hole ($1 <> getAnn $2) (HoleIdx (getAnn $2) (annd $2)) }
     ;
LowerSym : lowerSym { Symbol (getAnn $1) (annd $1) }
         ;
UpperSym : upperSym { Symbol (getAnn $1) (annd $1) }
         ;

{
parse :: L.Program Range -> Result (Program Range)
parse (L.Program lexemes) = parseSequence $ annd lexemes

parseError :: [L.Lexeme Range] -> Result a
parseError [] = error "unexpected parse error without leftover lexemes - should at least have EOF"
parseError (nextLex : _)
  = ResultFail Error
  { errorStage = StageParsing
  , errorRange = Just nextLexRange
  , errorMsg = "gave up at " <> locDesc
  }
  where nextLexRange = getAnn nextLex
        locDesc = pprint (rangeStart nextLexRange) <> " (" <> lexDesc nextLex <> ")"
        lexDesc (L.LexemePunc (L.PuncEof _)) = "end of file - not finished?"
        lexDesc lexeme = "before \"" <> pprint lexeme <> "\""
}
