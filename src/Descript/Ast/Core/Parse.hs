{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module Descript.Ast.Core.Parse
  ( parse
  ) where

import Descript.Ast.Core.Types
import qualified Descript.Ast.Sugar as S
import Descript.Misc
import Descript.Plugin

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

-- = Bind identifier/index environment

data BindEnv
  = BindEnv
  { bindEnvBinds :: M.Map T.Text Int
  , bindEnvNextFree :: Int
  }

type BindSessionRes a = StateT BindEnv (ResultT (ReaderT SessionEnv (LoggingT IO))) a

emptyBindEnv :: BindEnv
emptyBindEnv
  = BindEnv
  { bindEnvBinds = M.empty
  , bindEnvNextFree = 1
  }

bindEnvLookup :: T.Text -> BindEnv -> (Int, BindEnv)
bindEnvLookup bind env@(BindEnv binds nextFree)
  = case binds M.!? bind of
      Nothing ->
        ( nextFree,
          BindEnv
          { bindEnvBinds = M.insert bind nextFree binds
          , bindEnvNextFree = nextFree + 1
          }
        )
      Just idx -> (idx, env)

-- = Plugin support

languageSpecDecls :: LangSpec -> [RecordDeclCompact]
languageSpecDecls spec
  = map nodeSpecToCompactDecl $ langSpecNodes spec
  where langName = langSpecName spec
        nodeSpecToCompactDecl (AstNodeSpec nodeName numArgs)
          = RecordDeclCompact
          { recordDeclCompactHead = langName <> "_" <> nodeName
          , recordDeclCompactNumProps = numArgs
          }

allImportedDecls :: SessionEnv -> [RecordDeclCompact]
allImportedDecls env
  = builtinDecls ++ concatMap (languageSpecDecls . languageSpec) (sessionEnvLanguages env)

-- = Parsing from AST data

decodeError :: T.Text -> Error
decodeError msg
  = Error
  { errorStage = StagePluginUse
  , errorRange = Nothing
  , errorMsg = msg
  }

decodeAstData :: LangSpec -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
decodeAstData spec txt splices = traverse decodeAstData1 $ filter (not . T.null) $ T.lines txt
  where
    decls = builtinDecls ++ languageSpecDecls spec
    splicesVec = V.fromList splices
    decodeAstData1 txt1
      = case P.runParser astDataParser "" txt1 of
          Left err -> mkFail $ decodeError $ T.pack $ P.errorBundlePretty (err :: P.ParseErrorBundle T.Text Void)
          Right res -> pure res
    astDataParser = valueParser <* P.eof
    valueParser = do
      word <- wordParser
      case word of
        "splice" -> do
          idx <- P.decimal
          separatorParser
          case splicesVec V.!? idx of
            Nothing -> fail $ "invalid splice index: " ++ show idx
            Just splice -> pure $ Just <$> splice
        "integer" -> do
          value <- P.decimal
          separatorParser
          pure $ ValuePrimitive $ PrimInteger Nothing value
        "float" -> do
          value <- P.float
          separatorParser
          pure $ ValuePrimitive $ PrimFloat Nothing value
        "string" -> do
          value <- stringParser
          separatorParser
          pure $ ValuePrimitive $ PrimString Nothing value
        _ | isUpper $ head word -> ValueRecord <$> recordParser (T.pack word)
          | otherwise -> fail $ "word has unknown type: " ++ word
    recordParser head' = do
      nodeDecl <-
        case find (\decl -> recordDeclCompactHead decl == head') decls of
          Nothing -> fail $ T.unpack $ "unknown record head: " <> head'
          Just res -> pure res
      let numProps = recordDeclCompactNumProps nodeDecl
      props <- P.count numProps valueParser
      pure Record
        { recordAnn = Nothing
        , recordHead = head'
        , recordProps = props
        }
    stringParser = T.pack <$> (P.char '"' *> P.manyTill P.charLiteral (P.char '"'))
    wordParser = do
      res <- P.some (P.anySingleBut ' ')
      separatorParser
      pure res
    separatorParser = (() <$ P.char ' ') P.<|> P.eof

runLanguageParser :: Language -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
runLanguageParser lang txt splices = do
  let spec = languageSpec lang
      parser = languageParser lang
  astData <- overErrors (prependMsgToErr "couldn't parse code") $ runCmdProgram parser txt
  overErrors (prependMsgToErr "parsing plugin returned bad AST data") $ decodeAstData spec astData splices

-- = Parsing from Sugar

undefinedSym :: T.Text
undefinedSym = "<undefined>"

parseError :: Range -> T.Text -> Error
parseError rng msg
  = addRangeToErr rng $ Error
  { errorStage = StageExtracting
  , errorRange = Nothing
  , errorMsg = msg
  }

parsePropDecl :: S.GenProperty Range -> SessionRes T.Text
parsePropDecl (S.GenPropertyDecl (S.Symbol _ decl)) = pure decl
parsePropDecl (S.GenProperty val) = do
  tellError $ parseError (getAnn val) "expected lowercase symbol, got value"
  pure undefinedSym

parseRecordDecl :: S.RecordDecl Range -> SessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ (S.Symbol _ head') props))
  = RecordDecl rng head' <$> traverse parsePropDecl props

parsePrim :: S.Primitive Range -> BindSessionRes (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseProperty :: ValueType -> S.GenProperty Range -> BindSessionRes (Value Range)
parseProperty _ (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected value, got lowercase symbol"
parseProperty typ (S.GenProperty val) = parseValue typ val

parseRecord :: ValueType -> S.Record Range -> BindSessionRes (Record Range)
parseRecord typ (S.Record rng (S.Symbol _ head') props)
  = Record rng head' <$> traverseDropFatals (parseProperty typ) props

parseBind :: ValueType -> S.Bind Range -> BindSessionRes (Bind Range)
parseBind typ (S.Bind rng sym)
  = case sym of
      Nothing -> do
        when (typ == ValueTypeOutput) $
          tellError $ parseError rng "output bind must have an identifier"
        pure $ Bind rng 0
      Just (S.Symbol _ txt) -> do
        env <- get
        when (typ == ValueTypeOutput && M.notMember txt (bindEnvBinds env)) $
          tellError $ parseError rng "output bind must have the same identifier as an input bind"
        let (idx, newEnv) = bindEnvLookup txt env
        put newEnv
        pure $ Bind rng idx

flattenSpliceText :: S.SpliceText Range -> T.Text
flattenSpliceText spliceText = flattenRest 0 spliceText
  where flattenRest :: Int -> S.SpliceText Range -> T.Text
        flattenRest _ (S.SpliceTextNil _ txt) = txt
        flattenRest n (S.SpliceTextCons _ txt _ rst) = txt <> "\\" <> pprint n <> flattenRest (n + 1) rst

spliceTextValues :: S.SpliceText Range -> [S.Value Range]
spliceTextValues (S.SpliceTextNil _ _) = []
spliceTextValues (S.SpliceTextCons _ _ val rst) = val : spliceTextValues rst

parseSpliceCode :: ValueType -> S.SpliceCode Range -> BindSessionRes (Value Range)
parseSpliceCode typ (S.SpliceCode rng (S.Symbol langExtRng langExt) spliceText) = do
  let txt = flattenSpliceText spliceText
      unparsedSplices = spliceTextValues spliceText
  splices <- traverse (parseValue typ) unparsedSplices
  env <- lift getSessionEnv
  lang <-
    case find (\lang -> langSpecExtension (languageSpec lang) == langExt) $ sessionEnvLanguages env of
      Nothing -> mkFail $ parseError langExtRng $ "no (valid) plugin for language with extension '" <> langExt <> "'"
      Just res -> pure res
  ress <- overErrors (addRangeToErr rng) $ lift $ runLanguageParser lang txt splices
  case ress of
    [res] -> pure $ (rng `fromMaybe`) <$> res
    _ -> mkFail $ parseError rng $ "expected 1 value, got " <> pprint (length ress)

parseValue :: ValueType -> S.Value Range -> BindSessionRes (Value Range)
parseValue _ (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue typ (S.ValueRecord record) = ValueRecord <$> parseRecord typ record
parseValue typ (S.ValueBind bind) = ValueBind <$> parseBind typ bind
parseValue typ (S.ValueSpliceCode code) = parseSpliceCode typ code

parseReducer :: S.Reducer Range -> SessionRes (Reducer Range)
parseReducer (S.Reducer rng input output)
  = (`evalStateT` emptyBindEnv)
  $ Reducer rng <$> parseValue ValueTypeInput input <*> parseValue ValueTypeOutput output

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: S.Program Range -> SessionRes (Program Range)
parseRaw (S.Program rng topLevels)
    = Program rng
  <$> traverse parseRecordDecl decls
  <*> traverseDropFatals parseReducer reds
  where decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
        reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevels

-- == Validation

duplicateDeclErrs :: [RecordDeclCompact] -> [RecordDecl Range] -> [Error]
duplicateDeclErrs imported decls
  = catMaybes $ zipWith duplicateDeclErr decls (inits decls)
  where duplicateDeclErr (RecordDecl rng head' _) prevs
          | any (\other -> recordDeclCompactHead other == head') others
          = Just $ parseError rng $ "duplicate record declaration: " <> head'
          | otherwise = Nothing
          where others = map compactRecordDecl prevs ++ imported

invalidRecordErrs :: [RecordDeclCompact] -> [Reducer Range] -> [Error]
invalidRecordErrs decls reds
  = concatMap invalidRecordErrorsReducer reds
  where invalidRecordErrorsReducer (Reducer _ input output)
          = invalidRecordErrorsValue input ++ invalidRecordErrorsValue output
        invalidRecordErrorsValue (ValuePrimitive _) = []
        invalidRecordErrorsValue (ValueRecord record) = invalidRecordErrorsRecord record
        invalidRecordErrorsValue (ValueBind _) = []
        invalidRecordErrorsRecord record =
          invalidRecordErrorsRecordIndiv record ++ invalidRecordErrorsRecordChildren record
        invalidRecordErrorsRecordIndiv (Record rng head' props) =
          case find (\decl -> recordDeclCompactHead decl == head') decls of
            Nothing -> [parseError rng $ "undeclared record: " <> head']
            Just decl
              | numDeclProps /= numProps -> [ parseError rng
               $ "record has wrong number of properties: expected "
              <> pprint numDeclProps
              <> ", got "
              <> pprint numProps ]
              | otherwise -> []
              where numDeclProps = recordDeclCompactNumProps decl
                    numProps = length props
        invalidRecordErrorsRecordChildren = concatMap invalidRecordErrorsValue . recordProps

validationErrs :: SessionEnv -> Program Range -> [Error]
validationErrs env (Program _ decls reds)
   = duplicateDeclErrs importedDecls decls
  ++ invalidRecordErrs allDecls reds
  where importedDecls = allImportedDecls env
        allDecls = map compactRecordDecl decls ++ importedDecls -- TODO add builtins and code spliced

-- | Adds syntax errors which didn't affect parsing but would cause problems during compilation.
validate :: SessionRes (Program Range) -> SessionRes (Program Range)
validate res = do
  env <- getSessionEnv
  x <- res
  tellErrors $ validationErrs env x
  pure x

-- = Export

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result.
parse :: S.Program Range -> SessionRes (Program Range)
parse = validate . parseRaw
