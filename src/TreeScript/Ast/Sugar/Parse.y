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
%error { desugarError }
%monad { Result }
%tokentype { L.Lexeme Range }
%token
  '...' { L.LexemePunc (L.PuncEllipsis $$) }
  '_' { L.LexemePunc (L.PuncUnderscore $$) }
  '&' { L.LexemePunc (L.PuncAnd $$) }
  '#' { L.LexemePunc (L.PuncHash $$) }
  '\\' { L.LexemePunc (L.PuncBackSlash $$) }
  '@' { L.LexemePunc (L.PuncAt $$) }
  '->' { L.LexemePunc (L.PuncFwdArrow $$) }
  '<-' { L.LexemePunc (L.PuncBwdArrow $$) }
  '=>' { L.LexemePunc (L.PuncFwdEq $$) }
  '|' { L.LexemePunc (L.PuncVerticalBar $$) }
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
TopLevel : ImportDecl { TopLevelImportDecl $1 }
         | RecordDecl { TopLevelRecordDecl $1 }
         | GroupDecl { TopLevelGroupDecl $1 }
         | TypeAlias { TopLevelTypeAlias $1 }
         | Reducer { TopLevelReducer $1 }
         ;
ImportDecl : '#' LowerSym UpperSym '->' UpperSym ';' -- SOON: Replace with '=>'
           { ImportDecl ($1 <> getAnn $2 <> getAnn $3 <> $4 <> getAnn $5 <> $6) $2 $3 (Just $5) }
           | '#' LowerSym UpperSym ';'
           { ImportDecl ($1 <> getAnn $2 <> getAnn $3 <> $4) $2 $3 Nothing }
           ;
RecordDecl : Record '.' { RecordDecl (getAnn $1 <> $2) $1 }
           ;
GroupDecl : Group '.' { GroupDecl (getAnn $1 <> $2) $1 Nothing }
          | Group '->' Type '.' { GroupDecl (getAnn $1 <> $2 <> getAnn $3 <> $4) $1 (Just $3) }
TypeAlias : '@' LowerSym '<-' Type ';' { TypeAlias ($1 <> getAnn $2 <> getAnn $4) $2 $4 }
          ;
Reducer : ReducerBase Guards ';'
        { Reducer (sconcat $ getAnn (snd $1) N.<| $3 N.:| map getAnn $2) (fst $1) (snd $1) (reverse $2) }
        ;
ReducerBase : Value ReducerType Value Groups
            { ($2, Guard (sconcat $ getAnn $1 N.<| getAnn $2 N.<| getAnn $3 N.:| map getAnn $4) $1 $3 (reverse $4)) }
            ;
ReducerType : '->' { ReducerTypeReg $1 }
            | '=>' { ReducerTypeCast $1 }
            ;
Guards : { [] }
       | NonEmptyGuards { $1 }
       ;
NonEmptyGuards : ',' Guard { [$2] }
               | NonEmptyGuards ',' Guard { $3 : $1 }
               ;
Guard : Value '<-' Value Groups
      { Guard (sconcat $ getAnn $1 N.<| $2 N.<| getAnn $3 N.:| map getAnn $4) $1 $3 (reverse $4)}
Groups : { [] }
       | NonEmptyGroups { $1 }
       ;
NonEmptyGroups : Group { [$1] }
               | NonEmptyGroups Group { $2 : $1 }
               ;
Value : Primitive { ValuePrimitive $1 }
      | Record { ValueRecord $1 }
      | Bind { ValueBind $1 }
      | SpliceCode { ValueSpliceCode $1 }
      | Hole { ValueHole $1 }
      ;
Primitive : int { PrimInteger (getAnn $1) (annd $1) }
          | float { PrimFloat (getAnn $1) (annd $1) }
          | string { PrimString (getAnn $1) (annd $1) }
          ;
Record : UpperSym GenProperties { Record (getAnn $1 <> getAnn $2) $1 (annd $2) }
       ;
Group : '&' UpperSym GenProperties { Group ($1 <> getAnn $2 <> getAnn $3) (GroupLocGlobal $1) $2 (annd $3) }
      | '&' LowerSym GenProperties { Group ($1 <> getAnn $2 <> getAnn $3) (GroupLocLocal $1) $2 (annd $3) }
      | '#' UpperSym GenProperties { Group ($1 <> getAnn $2 <> getAnn $3) (GroupLocFunction $1) $2 (annd $3) }
      ;
GenProperties : '[' ']' { Annd ($1 <> $2) [] }
              | '[' NonEmptyGenProperties ']' { Annd (sconcat $ $1 N.<| $3 N.:| map getAnn $2) (reverse $2) }
              ;
NonEmptyGenProperties : GenProperty { [$1] }
                      | NonEmptyGenProperties ',' GenProperty { $3 : $1 }
                      ;
GenProperty : Type { GenPropertyDecl $1 }
            | SubGroupProperty { GenPropertySubGroup $1 }
            | Value { GenPropertyRecord $1 }
            | Group { GenPropertyGroup $1 }
            ;
Type : TypeParts { Type (sconcat $ N.map getAnn $1) (reverse $ N.toList $1) }
     ;
TypeParts : TypePart { $1 N.:| [] }
          | TypeParts '|' TypePart { $3 N.<| $1 }
          ;
TypePart : '@' LowerSym { TypePartSymbol ($1 <> getAnn $2) $2 }
         | '@' UpperSym { TypePartSymbol ($1 <> getAnn $2) $2 }
         | '@' TransparentTypePart { TypePartTransparent ($1 <> getAnn $2) $2 }
         ;
TransparentTypePart : LowerSym GenProperties { Record (getAnn $1 <> getAnn $2) $1 (annd $2) }
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

desugarError :: [L.Lexeme Range] -> Result a
desugarError [] = error "unexpected parse error without leftover lexemes - should at least have EOF"
desugarError (nextLex : _)
  = ResultFail Error
  { errorStage = StageParse
  , errorRange = Just nextLexRange
  , errorMsg = "gave up at " <> locDesc
  }
  where nextLexRange = getAnn nextLex
        locDesc = pprint (rangeStart nextLexRange) <> " (" <> lexDesc nextLex <> ")"
        lexDesc (L.LexemePunc (L.PuncEof _)) = "end of file - not finished?"
        lexDesc lexeme = "before \"" <> pprint lexeme <> "\""
}
