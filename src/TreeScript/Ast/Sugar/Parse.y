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
  '---' { L.LexemePunc (L.PuncThinLineSep $$) }
  '===' { L.LexemePunc (L.PuncThickLineSep $$) }
  '#' { L.LexemePunc (L.PuncHash $$) }
  '\\' { L.LexemePunc (L.PuncBackSlash $$) }
  '&' { L.LexemePunc (L.PuncAnd $$) }
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
  splicedBind { L.LexemeSplicedBind $$ }

%%

Program : eof { Program $1 [] }
        | NonEmptyProgram eof { Program (sconcat $ $2 N.:| map getAnn $1) (reverse $1) }
        ;
NonEmptyProgram : TopLevel { [$1] }
                | NonEmptyProgram TopLevel { $2 : $1 }
                ;
TopLevel : RecordDecl { TopLevelRecordDecl $1 }
         | Statement { TopLevelStatement $1 }
         | GroupDecl { TopLevelGroupDecl $1 }
         ;
RecordDecl : Record '.' { RecordDecl (getAnn $1 <> $2) $1 }
           ;
Statement : Value ';' { StatementGroup $1 }
          | Reducer ';' { StatementReducer $1 }
          ;
GroupDecl : Group '.' '---' { GroupDecl (getAnn $1 <> $2 <> $3) $1 False }
          | Group '.' '===' { GroupDecl (getAnn $1 <> $2 <> $3) $1 True }
          ;
Reducer : ReducerClause ':' ReducerClause { Reducer (getAnn $1 <> $2 <> getAnn $3) $1 $3 }
        ;
ReducerClause : Value Groups { ReducerClause (sconcat $ getAnn $1 N.:| map getAnn $2) $1 (reverse $2) }
              ;
Groups : { [] }
       | NonEmptyGroups { $1 }
       ;
NonEmptyGroups : Group { [$1] }
               | Groups Group { $2 : $1 }
               ;
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
Group : '&' UpperSym GenProperties { Group ($1 <> getAnn $2 <> getAnn $3) $2 (annd $3) }
      ;
GenProperties : '[' ']' { Annd ($1 <> $2) [] }
              | '[' NonEmptyGenProperties ']' { Annd (sconcat $ $1 N.<| $3 N.:| map getAnn $2) (reverse $2) }
NonEmptyGenProperties : GenProperty { [$1] }
                      | NonEmptyGenProperties ';' GenProperty { $3 : $1 }
                      ;
GenProperty : LowerSym { GenPropertyDecl $1 }
            | Value { GenPropertyRecord $1 }
            | GroupProperty { GenPropertyGroup $1 }
            ;
GroupProperty : LowerSym ':' Value { GroupProperty (getAnn $1 <> $2 <> getAnn $3) $1 $3 }
              ;
Bind : '\\' { Bind $1 Nothing }
     | '\\' LowerSym { Bind ($1 <> getAnn $2) (Just $2) }
     | splicedBind { Bind (getAnn $1) (fmap (Symbol (getAnn $1)) (annd $1)) }
     ;
SpliceCode : LowerSym SpliceText { SpliceCode (getAnn $1 <> getAnn $2) $1 $2 }
           ;
SpliceText : codeWhole { SpliceTextNil (getAnn $1) (annd $1) }
           | codeStart Value SpliceTextTail { SpliceTextCons (getAnn $1 <> getAnn $2 <> getAnn $3) (annd $1) $2 $3 }
           ;
SpliceTextTail : codeEnd { SpliceTextNil (getAnn $1) (annd $1) }
               | codeMiddle Value SpliceTextTail { SpliceTextCons (getAnn $1 <> getAnn $2 <> getAnn $3) (annd $1) $2 $3 }
               ;
Hole : '\\' int { Hole ($1 <> getAnn $2) (getAnn $2) (annd $2) }
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
