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
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V

parseNumPropsByHead :: C.Program an -> SessionRes (M.Map T.Text Int)
parseNumPropsByHead = fmap (M.fromList . mapMaybe parseDecl) . C.getAllProgramDecls
  where parseDecl (C.RecordDeclCompact (C.RecordHead isFunc name) numProps)
          | isFunc = Nothing
          | otherwise = Just (name, numProps)

parseLibraries :: C.Program an -> SessionRes [T.Text]
parseLibraries = fmap (map $ T.pack . libraryCodeDir) . C.getAllProgramUsedLibraries

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

parseConsumes :: Value -> [Consume]
parseConsumes (ValueSplice idx) = [ConsumeSplice idx]
parseConsumes (ValuePrimitive prim) = [ConsumePrimitive prim]
parseConsumes (ValueRecord (Record head' props))
  = ConsumeRecord head' : concatMap parseConsumes props

parseReducerClause :: V.Vector (C.GroupDef an) -> C.ReducerClause an -> ReducerClause
parseReducerClause groupDefs (C.ReducerClause _ val groupRefs)
  = ReducerClause
  { reducerClauseConsumes = parseConsumes val'
  , reducerClauseProduce = val'
  , reducerClauseGroups = map (parseGroup groupDefs) groupRefs
  }
  where val' = parseValue val

parseGroup :: V.Vector (C.GroupDef an) -> C.GroupRef an -> Group
parseGroup groupDefs groupRef = Group
  { groupRepeats = repeats
  , groupStatements = map (map $ parseStatement groupDefs) $ A.elems stmts
  }
  where (repeats, stmts) = C.allGroupRefStatements groupDefs groupRef

parseReducer :: V.Vector (C.GroupDef an) -> C.Reducer an -> Reducer
parseReducer groups (C.Reducer _ input output) = Reducer
  { reducerInput = parseReducerClause groups input
  , reducerOutput = parseReducerClause groups output
  }

parseStatement :: V.Vector (C.GroupDef an) -> C.Statement an -> Statement
parseStatement groupDefs (C.StatementGroup groupRef)
  = StatementGroup $ parseGroup groupDefs groupRef
parseStatement groups (C.StatementReducer reducer)
  = StatementReducer $ parseReducer groups reducer

parseMainGroup :: C.Program an -> SessionRes Group
parseMainGroup prog = pure Group
  { groupRepeats = False
  , groupStatements = [map (parseStatement groups) stmts]
  }
  where stmts = C.programMainStatements prog
        groups = V.fromList $ C.programGroups prog

-- | Converts a @Core@ AST into a @Translate@ AST.
parse :: C.Program an -> SessionRes Program
parse prog
    = Program
  <$> parseNumPropsByHead prog
  <*> parseLibraries prog
  <*> parseMainGroup prog
