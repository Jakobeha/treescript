{-# LANGUAGE OverloadedStrings #-}

-- | Functions to manipulate @Core@ ASTs.
module Descript.Ast.Core.Analyze
  ( foldValue
  , foldValuesInProgram
  , langSpecDecls
  , allImportedDecls
  , getAllProgramDecls
  ) where

import Descript.Ast.Core.Types
import Descript.Plugin

import Data.List

-- | Applies to each child value, then combines all results.
foldValue :: (Semigroup r) => (Value an -> r) -> Value an -> r
foldValue f (ValuePrimitive prim) = f $ ValuePrimitive prim
foldValue f (ValueRecord record)
  = foldl' foldInValue (f (ValueRecord record)) (recordProps record)
  where foldInValue res val = res <> f val
foldValue f (ValueBind bind) = f $ ValueBind bind

-- | Applies to each value, then combines all results.
foldValuesInProgram :: (Monoid r) => (Value an -> r) -> Program an -> r
foldValuesInProgram f = foldMap foldValuesInReducer . programReducers
  where foldValuesInReducer (Reducer _ input output) = foldValue f input <> foldValue f output

langSpecDecls :: LangSpec -> [RecordDeclCompact]
langSpecDecls spec
  = map nodeSpecToCompactDecl $ langSpecNodes spec
  where langName = langSpecName spec
        nodeSpecToCompactDecl (AstNodeSpec nodeName numArgs)
          = RecordDeclCompact
          { recordDeclCompactHead = langName <> "_" <> nodeName
          , recordDeclCompactNumProps = numArgs
          }

-- | All record declarations imported by a program in the given environment.
allImportedDecls :: SessionEnv -> [RecordDeclCompact]
allImportedDecls env
  = builtinDecls ++ concatMap (langSpecDecls . languageSpec) (sessionEnvLanguages env)

-- | All declarations accessible from the program, declared and imported.
getAllProgramDecls :: Program an -> SessionRes [RecordDeclCompact]
getAllProgramDecls prog = do
  env <- getSessionEnv
  let declaredDecls = map compactRecordDecl $ programRecordDecls prog
      importedDecls = allImportedDecls env
  pure $ declaredDecls ++ importedDecls
