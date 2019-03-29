{-# LANGUAGE OverloadedStrings #-}

-- | Converts a @Core@ AST into a @Translate@ AST.
module TreeScript.Ast.Translate.Parse
  ( parse
  ) where

import TreeScript.Ast.Translate.Types
import qualified TreeScript.Ast.Core as C
import TreeScript.Misc
import TreeScript.Plugin

import qualified Data.Array as A

parseLibraries :: C.Program an -> SessionRes [Lib]
parseLibraries = fmap (map parseLibrary) . C.getAllProgramUsedLibraries
  where parseLibrary (Library spec dirName)
          = Lib
          { libName = librarySpecName spec
          , libDirName = dirName
          }

parsePrim :: C.Primitive an -> Primitive
parsePrim (C.PrimInteger _ x) = PrimInteger x
parsePrim (C.PrimFloat _ x) = PrimFloat x
parsePrim (C.PrimString _ x) = PrimString x

parseRecord :: C.Record an -> Record
parseRecord (C.Record _ head' props) = Record
  { recordHead = pprint head'
  , recordProps = map parseValue props
  }

parseValue :: C.Value an -> Value
parseValue (C.ValuePrimitive prim) = ValuePrimitive $ parsePrim prim
parseValue (C.ValueRecord record) = ValueRecord $ parseRecord record
parseValue (C.ValueBind bind) = ValueSplice $ C.bindIdx bind

parseConsumes :: C.Value an -> [Consume]
parseConsumes (C.ValuePrimitive prim) = [ConsumePrimitive $ parsePrim prim]
parseConsumes (C.ValueRecord (C.Record _ (C.RecordHead False head') props))
  = ConsumeRecord head' : concatMap parseConsumes props
parseConsumes (C.ValueRecord (C.Record _ (C.RecordHead True head') props))
  = [ConsumeFunction head' $ map parseValue props]
parseConsumes (C.ValueBind bind) = [ConsumeSplice $ C.bindIdx bind]

parseGroupRef :: C.GroupRef an -> GroupRef
parseGroupRef (C.GroupRef _ isProp idx gprops vprops) = GroupRef
  { groupRefIsProp = isProp
  , groupRefIdx = idx
  , groupRefGroupProps = map parseGroupRef gprops
  , groupRefValueProps = map parseValue vprops
  }

parseReducer :: C.Reducer an -> Reducer
parseReducer (C.Reducer _ input output nexts guards) = Reducer
  { reducerInput = parseConsumes input
  , reducerOutput = parseValue output
  , reducerNexts = map parseGroupRef nexts
  , reducerGuards = map parseStatement guards
  }

parseStatement :: C.Statement an -> Statement
parseStatement (C.StatementGroup group)
  = StatementGroup $ parseGroupRef group
parseStatement (C.StatementReducer reducer)
  = StatementReducer $ parseReducer reducer

parseGroupDef :: C.GroupDef an -> GroupDef
parseGroupDef (C.GroupDef _ gprops vprops stmts) = GroupDef
  { groupDefGroupProps = map C.bindIdx gprops
  , groupDefValueProps = map C.bindIdx vprops
  , groupDefStatements = map (map parseStatement) $ A.elems stmts
  }

parseMainStatements :: C.Program an -> SessionRes [Statement]
parseMainStatements = pure . map parseStatement . C.programMainStatements

parseGroups :: C.Program an -> SessionRes [GroupDef]
parseGroups = pure . map parseGroupDef . C.programGroups

-- | Converts a @Core@ AST into a @Translate@ AST.
parse :: C.Program an -> SessionRes Program
parse prog
    = Program
  <$> parseLibraries prog
  <*> parseMainStatements prog
  <*> parseGroups prog
