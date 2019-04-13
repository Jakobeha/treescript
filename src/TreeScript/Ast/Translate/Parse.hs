{-# LANGUAGE OverloadedStrings #-}

-- | Converts a @Core@ AST into a @Translate@ AST.
module TreeScript.Ast.Translate.Parse
  ( parse
  ) where

import TreeScript.Ast.Translate.Types
import qualified TreeScript.Ast.Core as C
import TreeScript.Plugin

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
  { recordHead = head'
  , recordProps = map parseValue props
  }

parseValue :: C.Value an -> Value
parseValue (C.ValuePrimitive prim) = ValuePrimitive $ parsePrim prim
parseValue (C.ValueRecord record) = ValueRecord $ parseRecord record
parseValue (C.ValueBind bind) = ValueSplice $ C.bindIdx bind

parseConsumes :: C.Value an -> [Consume]
parseConsumes (C.ValuePrimitive prim) = [ConsumePrimitive $ parsePrim prim]
parseConsumes (C.ValueRecord (C.Record _ head' props))
  = ConsumeRecord head' : concatMap parseConsumes props
parseConsumes (C.ValueBind bind) = [ConsumeSplice $ C.bindIdx bind]

parseGroupLoc :: C.GroupLoc an -> GroupLoc
parseGroupLoc (C.GroupLocGlobal _ idx) = GroupLocGlobal idx
parseGroupLoc (C.GroupLocLocal _ idx) = GroupLocLocal idx
parseGroupLoc (C.GroupLocFunction _ txt) = GroupLocFunction txt

parseGroupRef :: C.GroupRef an -> GroupRef
parseGroupRef (C.GroupRef _ loc vprops gprops)
  = GroupRef
  { groupRefLoc = parseGroupLoc loc
  , groupRefValueProps = map parseValue vprops
  , groupRefGroupProps = map parseGroupRef gprops
  }

parseGuard :: C.Guard an -> Guard
parseGuard (C.Guard _ input output nexts) = Guard
  { guardInput = parseConsumes input
  , guardOutput = parseValue output
  , guardNexts = map parseGroupRef nexts
  }

parseReducer :: C.Reducer an -> Reducer
parseReducer (C.Reducer _ main guards) = Reducer
  { reducerMain = parseGuard main
  , reducerGuards = reverse $ map parseGuard guards
  }

parseGroupDef :: C.GroupDef an -> GroupDef
parseGroupDef (C.GroupDef _ vprops gprops reds) = GroupDef
  { groupDefValueProps = map C.bindIdx vprops
  , groupDefGroupProps = map C.bindIdx gprops
  , groupDefReducers = map parseReducer reds
  }

parseGroups :: C.Program an -> SessionRes [GroupDef]
parseGroups = pure . map parseGroupDef . C.programGroups

-- | Converts a @Core@ AST into a @Translate@ AST.
parse :: C.Program an -> SessionRes Program
parse prog
    = Program
  <$> parseLibraries prog
  <*> parseGroups prog
