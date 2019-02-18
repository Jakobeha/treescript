{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module TreeScript.Ast.Core.Parse
  ( parse
  ) where

import TreeScript.Ast.Core.Analyze
import qualified TreeScript.Ast.Core.Intermediate as I
import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Flat as F
import qualified TreeScript.Ast.Sugar as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Array as A
import Data.Bifunctor
import Data.List hiding (group)
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- = Parsing from AST data

decodeError :: T.Text -> Error
decodeError msg
  = Error
  { errorStage = StagePluginUse
  , errorRange = Nothing
  , errorMsg = msg
  }

decodeAstData :: LangSpec -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
decodeAstData spec txt splices = traverse decodeAstData1 =<< ResultT (pure $ F.parse txt)
  where
    decls = builtinDecls ++ langSpecDecls spec
    splicesVec = V.fromList splices
    decodeAstData1 lexs = do
      (res, rest) <- valueParser lexs
      unless (null rest) $
        mkFail $ decodeError "unexpected extra nodes after decoded value"
      pure res
    valueParser [] = mkFail $ decodeError "tried to decode value from no nodes"
    valueParser (F.LexemeSplice idx : rest)
      = case splicesVec V.!? idx of
          Nothing -> mkFail $ decodeError $ "invalid splice index: " <> pprint idx
          Just splice -> pure (Just <$> splice, rest)
    valueParser (F.LexemePrimitive prim : rest) = pure (ValuePrimitive $ primParser prim, rest)
    valueParser (F.LexemeRecordHead head' : rest) = first ValueRecord <$> recordParser head' rest
    primParser (F.PrimInteger value) = PrimInteger Nothing value
    primParser (F.PrimFloat value) = PrimFloat Nothing value
    primParser (F.PrimString value) = PrimString Nothing value
    recordParser head' rest = do
      nodeDecl <-
        case find (\decl -> recordDeclCompactHead decl == RecordHead False head') decls of
          Nothing -> mkFail $ decodeError $ "unknown record head: " <> head'
          Just res -> pure res
      let numProps = recordDeclCompactNumProps nodeDecl
      (props, rest') <- propsParser numProps rest
      pure
        ( Record
          { recordAnn = Nothing
          , recordHead
              = RecordHead
              { recordHeadIsFunc = False
              , recordHeadName = head'
              }
          , recordProps = props
          },
          rest'
        )
    propsParser 0 rest = pure ([], rest)
    propsParser n rest = do
      (x, rest') <- valueParser rest
      (xs, rest'') <- propsParser (n - 1) rest'
      pure (x : xs, rest'')


runLanguageParser :: Language -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
runLanguageParser lang txt splices = do
  let spec = languageSpec lang
      parser = languageParser lang
  astData <- overErrors (prependMsgToErr $ "couldn't parse code") $ runCmdProgram parser txt
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
parsePropDecl (S.GenPropertyRecord val) = do
  tellError $ parseError (getAnn val) "expected lowercase symbol, got value"
  pure undefinedSym
parsePropDecl (S.GenPropertyGroup prop) = do
  tellError $ parseError (getAnn prop) "expected lowercase symbol, got group property"
  pure undefinedSym

parseRecordDecl :: S.RecordDecl Range -> SessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ isFunc (S.Symbol _ head') props))
  | isFunc = mkFail $ parseError rng $ "can't declare function"
  | otherwise = RecordDecl rng head' <$> traverse parsePropDecl props

parsePrim :: S.Primitive Range -> I.BindSessionRes (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseRecordProperty :: S.GenProperty Range -> I.BindSessionRes (Value Range)
parseRecordProperty (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected value, got lowercase symbol"
parseRecordProperty (S.GenPropertyRecord val) = parseValue val
parseRecordProperty (S.GenPropertyGroup prop)
  = mkFail $ parseError (getAnn prop) "expected value, got group property"

parseRecord :: S.Record Range -> I.BindSessionRes (Record Range)
parseRecord (S.Record rng isFunc (S.Symbol _ head') props)
  = Record rng (RecordHead isFunc head') <$> traverseDropFatals parseRecordProperty props

parseBind :: S.Bind Range -> I.BindSessionRes (Bind Range)
parseBind (S.Bind rng sym)
  = case sym of
      Nothing -> pure $ Bind rng 0
      Just (S.Symbol _ txt) -> do
        env <- get
        let (idx, newEnv) = I.bindEnvLookup txt env
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

parseSpliceCode :: S.SpliceCode Range -> I.BindSessionRes (Value Range)
parseSpliceCode (S.SpliceCode rng (S.Symbol langExtRng langExt) spliceText) = do
  let txt = flattenSpliceText spliceText
      unparsedSplices = spliceTextValues spliceText
  splices <- traverse parseValue unparsedSplices
  lang <- lift $ overErrors (addRangeToErr langExtRng) $ langWithExt StageExtracting langExt
  ress <- overErrors (addRangeToErr rng) $ lift $ runLanguageParser lang txt splices
  case ress of
    [res] -> pure $ (rng `fromMaybe`) <$> res
    _ -> mkFail $ parseError rng $ "expected 1 value, got " <> pprint (length ress)

parseValue :: S.Value Range -> I.BindSessionRes (Value Range)
parseValue (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue (S.ValueRecord record) = ValueRecord <$> parseRecord record
parseValue (S.ValueBind bind) = ValueBind <$> parseBind bind
parseValue (S.ValueSpliceCode code) = parseSpliceCode code
parseValue (S.ValueGroup group)
  = mkFail $ parseError (getAnn group) "expected value, got group"

parseGroupProperty1 :: S.GenProperty Range -> I.BindSessionRes (I.GroupProperty Range)
parseGroupProperty1 (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected group property, got lowercase symbol"
parseGroupProperty1 (S.GenPropertyRecord val)
  = mkFail $ parseError (getAnn val) "expected group property, got value"
parseGroupProperty1 (S.GenPropertyGroup (S.GroupProperty rng sym val))
  = I.GroupProperty rng sym <$> parseValue val

parseGroupRef1 :: S.Group Range -> I.BindSessionRes (I.GroupRef Range)
parseGroupRef1 (S.Group rng head' props)
  = I.GroupRef rng head' <$> traverseDropFatals parseGroupProperty1 props

parseGroup1 :: S.Value Range -> I.BindSessionRes (I.Group Range)
parseGroup1 (S.ValueRecord (S.Record _ False (S.Symbol headAnn headSpec) [S.GenPropertyRecord (S.ValueGroup group)]))
  | headSpec == "E"
  = I.Group (getAnn group) ReduceTypeEvalCtx <$> parseGroupRef1 group
  | headSpec == "C"
  = I.Group (getAnn group) ReduceTypeAltConsume <$> parseGroupRef1 group
  | otherwise
  = mkFail $ parseError headAnn $ "invalid group reduce type specifier: " <> headSpec
parseGroup1 (S.ValueGroup group)
  = I.Group (getAnn group) ReduceTypeRegular <$> parseGroupRef1 group
parseGroup1 val
  = mkFail $ parseError (getAnn val) "expected group, got value"

parseReducerClause1 :: S.ReducerClause Range -> I.BindSessionRes (I.ReducerClause Range)
parseReducerClause1 (S.ReducerClause rng val groups)
  = I.ReducerClause rng <$> parseValue val <*> traverseDropFatals parseGroupRef1 groups

parseReducer1 :: S.Reducer Range -> I.BindSessionRes (I.Reducer Range)
parseReducer1 (S.Reducer rng input output)
  = I.Reducer rng <$> parseReducerClause1 input <*> parseReducerClause1 output

parseStatement1 :: S.Statement Range -> I.BindSessionRes (I.Statement Range)
parseStatement1 (S.StatementGroup group) = I.StatementGroup <$> parseGroup1 group
parseStatement1 (S.StatementReducer red) = I.StatementReducer <$> parseReducer1 red

parseGroupPropDecl1 :: S.GenProperty Range -> I.BindSessionRes (T.Text, Bind Range)
parseGroupPropDecl1 (S.GenPropertyDecl (S.Symbol rng txt)) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt env
  put newEnv
  pure (txt, Bind rng idx)
parseGroupPropDecl1 (S.GenPropertyRecord val) = do
  mkFail $ parseError (getAnn val) "expected lowercase symbol, got value"
parseGroupPropDecl1 (S.GenPropertyGroup prop) = do
  mkFail $ parseError (getAnn prop) "expected lowercase symbol, got group property"

parseEmptyGroupDef1 :: S.GroupDecl Range -> I.FreeSessionRes (I.GroupDef Range)
parseEmptyGroupDef1 (S.GroupDecl rng (S.Group _ (S.Symbol _ head') props) repeats) = do
  nextFree <- get
  (props', bindEnv)
     <- lift
      $ (`runStateT` I.BindEnv{ I.bindEnvBinds = M.empty, I.bindEnvNextFree = nextFree })
      $ traverseDropFatals parseGroupPropDecl1 props
  put $ I.bindEnvNextFree bindEnv
  pure I.GroupDef
    { I.groupDefAnn = rng
    , I.groupDefHead = head'
    , I.groupDefProps = props'
    , I.groupDefRepeats = repeats
    , I.groupDefStatements = []
    , I.groupDefBindEnv = bindEnv
    }

parseRestGroupDefs1 :: S.GroupDecl Range -> [S.TopLevel Range] -> I.FreeSessionRes (N.NonEmpty (I.GroupDef Range))
parseRestGroupDefs1 decl [] = (N.:| []) <$> parseEmptyGroupDef1 decl
parseRestGroupDefs1 decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs1 decl xs
parseRestGroupDefs1 decl (S.TopLevelStatement stmt : xs) = do
  I.GroupDef yRng yHead yProps yRepeats yStmts yBindEnv N.:| ys <- parseRestGroupDefs1 decl xs
  nextFree <- get
  let yBindEnv' = yBindEnv{ I.bindEnvNextFree = nextFree }
  (stmt', yBindEnv'') <- lift $ runStateT (parseStatement1 stmt) yBindEnv'
  put $ I.bindEnvNextFree yBindEnv''
  let yStmts' = stmt' : yStmts
  pure $ I.GroupDef (getAnn stmt <> yRng) yHead yProps yRepeats yStmts' yBindEnv'' N.:| ys
parseRestGroupDefs1 decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef1 decl <*> parseRestGroupDefs1 decl' xs

parseAllGroupDefs1 :: Int -> [S.TopLevel Range] -> SessionRes [I.GroupDef Range]
parseAllGroupDefs1 _ [] = pure []
parseAllGroupDefs1 nextFree (S.TopLevelGroupDecl x : xs)
  = N.toList <$> evalStateT (parseRestGroupDefs1 x xs) nextFree
parseAllGroupDefs1 _ (_ : _) = error "expected group declaration when parsing group definitions"

parseGroupPropIdx :: M.Map T.Text Int -> S.Symbol Range -> I.GroupSessionRes Int
parseGroupPropIdx propEnv (S.Symbol rng propName)
  = case propEnv M.!? propName of
      Nothing -> do
        tellError $ parseError rng $ "bind not exported by group: " <> propName
        pure (-1)
      Just idx -> pure idx

parseGroupRef2 :: I.BindEnv -> I.GroupRef Range -> I.GroupSessionRes (GroupRef Range)
parseGroupRef2 bindEnv (I.GroupRef rng (S.Symbol headRng head') props) = do
  groupEnv <- ask
  let (propNames, propVals) = V.unzip $ V.map (\(I.GroupProperty _ propHead propVal) -> (propHead, propVal)) $ V.fromList props
  case groupEnv M.!? head' of
    Nothing -> mkFail $ parseError headRng $ "undefined group: " <> head'
    Just (groupIdx, propLocals, propEnv) -> do
      propIdxs <- traverse (parseGroupPropIdx propEnv) propNames
      let bindIdxs = M.mapKeys (\name -> M.findWithDefault (-1) name propEnv) $ I.bindEnvBinds bindEnv
          propForIdx idx propLocal
            = case V.elemIndex idx propIdxs of
                Nothing ->
                  case bindIdxs M.!? idx of
                    Nothing -> ValueBind propLocal
                    Just bindIdx' -> ValueBind $ Bind headRng bindIdx'
                Just propIdx -> propVals V.! propIdx
          props' = V.toList $ V.imap propForIdx propLocals
      pure GroupRef
        { groupRefAnn = rng
        , groupRefIdx = groupIdx
        , groupRefProps = props'
        }

parseReducerClause2 :: I.BindEnv -> I.ReducerClause Range -> I.GroupSessionRes (ReducerClause Range)
parseReducerClause2 bindEnv (I.ReducerClause rng val groups)
  = ReducerClause rng val <$> traverseDropFatals (parseGroupRef2 bindEnv) groups

parseReducer2 :: I.BindEnv -> I.Reducer Range -> I.GroupSessionRes (Reducer Range)
parseReducer2 bindEnv (I.Reducer rng input output)
    = Reducer rng
  <$> parseReducerClause2 bindEnv input
  <*> parseReducerClause2 bindEnv output

parseStatement2 :: I.BindEnv -> I.Statement Range -> I.GroupSessionRes (Statement Range)
parseStatement2 bindEnv (I.StatementGroup group) = StatementGroup <$> parseGroupRef2 bindEnv (I.groupRef group)
parseStatement2 bindEnv (I.StatementReducer red) = StatementReducer <$> parseReducer2 bindEnv red

statementReduceType :: I.Statement Range -> ReduceType
statementReduceType (I.StatementGroup group) = I.groupReduceType group
statementReduceType (I.StatementReducer reducer)
  = case I.reducerClauseValue $ I.reducerInput reducer of
      ValuePrimitive _ -> ReduceTypeRegular
      ValueRecord (Record _ (RecordHead isFunc head') _)
        | not isFunc && head' == "E" -> ReduceTypeEvalCtx
        | not isFunc && head' == "C" -> ReduceTypeAltConsume
        | otherwise -> ReduceTypeRegular
      ValueBind _ -> ReduceTypeRegular

groupStmtsByReduceType :: [I.Statement Range] -> A.Array ReduceType [I.Statement Range]
groupStmtsByReduceType
  = fmap reverse
  . A.accumArray (flip (:)) [] (minBound, maxBound)
  . map (\stmt -> (statementReduceType stmt, stmt))

parseGroupDef2 :: I.GroupDef Range -> I.GroupSessionRes (GroupDef Range)
parseGroupDef2 (I.GroupDef rng _ props repeats stmts bindEnv)
    = GroupDef rng (map (\(_, bind) -> bind) props) repeats
  <$> traverse (traverseDropFatals $ parseStatement2 bindEnv) (groupStmtsByReduceType stmts)

parseGroupDefEnv :: Int -> I.GroupDef Range -> (T.Text, (Int, V.Vector (Bind Range), M.Map T.Text Int))
parseGroupDefEnv idx group
  = ( I.groupDefHead group
    , ( idx
      , V.fromList $ map (\(_, bind) -> bind) props
      , M.fromList $ zipWith (\propIdx (propName, _) -> (propName, propIdx)) [0..] props
      )
    )
  where props = I.groupDefProps group

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: S.Program Range -> SessionRes (Program Range)
parseRaw (S.Program rng topLevels) = do
  let (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl _ -> True; _ -> False) topLevels
      decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      stmts = mapMaybe (\case S.TopLevelStatement x -> Just x; _ -> Nothing) topLevelsOutGroups
  decls' <- traverse parseRecordDecl decls
  (stmts1, stmtsBindEnv) <- runStateT (traverseDropFatals parseStatement1 stmts) I.emptyBindEnv
  groups1 <- parseAllGroupDefs1 (I.bindEnvNextFree stmtsBindEnv) topLevelsInGroups
  let groupEnv = M.fromList $ zipWith parseGroupDefEnv [0..] groups1
  stmts' <- runReaderT (traverseDropFatals (parseStatement2 stmtsBindEnv) stmts1) groupEnv
  groups' <- runReaderT (traverseDropFatals parseGroupDef2 groups1) groupEnv
  pure Program
    { programAnn = rng
    , programRecordDecls = decls'
    , programMainStatements = stmts'
    , programGroups = groups'
    }

-- == Validation

duplicateDeclErrs :: [RecordDeclCompact] -> [RecordDecl Range] -> [Error]
duplicateDeclErrs imported decls
  = catMaybes $ zipWith duplicateDeclErr decls (inits decls)
  where duplicateDeclErr (RecordDecl rng head' _) prevs
          | any (\other -> recordDeclCompactHead other == RecordHead False head') others
          = Just $ parseError rng $ "duplicate record declaration: " <> head'
          | otherwise = Nothing
          where others = map compactRecordDecl prevs ++ imported

invalidRecordErrs :: [RecordDeclCompact] -> [Reducer Range] -> [Error]
invalidRecordErrs decls reds
  = concatMap invalidRecordErrorsReducer reds
  where invalidRecordErrorsReducer (Reducer _ input output)
           = foldValuesInClause invalidRecordErrorsValue1 True input
          ++ foldValuesInClause invalidRecordErrorsValue1 True output
        invalidRecordErrorsValue1 (ValuePrimitive _) = []
        invalidRecordErrorsValue1 (ValueRecord (Record rng head' props))
          = case find (\decl -> recordDeclCompactHead decl == head') decls of
              Nothing -> [parseError rng $ "undeclared record: " <> pprint head']
              Just decl
                | numDeclProps /= numProps -> [ parseError rng
                 $ "record has wrong number of properties: expected "
                <> pprint numDeclProps
                <> ", got "
                <> pprint numProps ]
                | otherwise -> []
                where numDeclProps = recordDeclCompactNumProps decl
                      numProps = length props
        invalidRecordErrorsValue1 (ValueBind _) = []

unboundOutputErrsValue :: I.Variance -> Value Range -> [Error]
unboundOutputErrsValue _ (ValuePrimitive _) = []
unboundOutputErrsValue I.VarianceContravariant (ValueRecord (Record _ head' props))
  | head' == flushRecordHead = []
  | recordHeadIsFunc head' = concatMap (unboundOutputErrsValue $ I.VarianceCovariant S.empty) props
  | otherwise = concatMap (unboundOutputErrsValue I.VarianceContravariant) props
unboundOutputErrsValue I.VarianceContravariant (ValueBind _) = []
unboundOutputErrsValue (I.VarianceCovariant binds) (ValueRecord (Record _ head' props))
  | head' == flushRecordHead = []
  | otherwise = foldMap (unboundOutputErrsValue $ I.VarianceCovariant binds) props
unboundOutputErrsValue (I.VarianceCovariant binds) (ValueBind (Bind rng idx))
  | S.member idx binds = []
  | idx == 0 = [parseError rng "unlabeled bind in non-matching position"]
  | otherwise = [parseError rng "bind in non-matching position has unassigned label"]

unboundOutputErrsGroup :: S.Set Int -> Bool -> S.Set Int -> GroupRef Range -> State (V.Vector (GroupDef Range, Bool, Bool)) [Error]
unboundOutputErrsGroup extraBinds isCovariant stk (GroupRef _ idx props)
  | S.member idx stk = pure []
  | otherwise = do
    groups' <- get
    let stk' = S.insert idx stk
        (group', coChecked, contraChecked) = groups' V.! idx
        checked
          | isCovariant = coChecked
          | otherwise = contraChecked
    if checked then
      pure []
    else do
      let (_, stmts) = allGroupDefStatements props group'
          stkStmts = map (stk', ) $ concat $ A.elems stmts
          newGroups
            | isCovariant = groups' V.// [(idx, (group', True, contraChecked))]
            | otherwise = groups' V.// [(idx, (group', coChecked, True))]
      put newGroups
      unboundOutputErrs extraBinds isCovariant stkStmts

unboundOutputErrsClause :: I.Variance -> S.Set Int -> ReducerClause Range -> State (V.Vector (GroupDef Range, Bool, Bool)) [Error]
unboundOutputErrsClause variance stk clause@(ReducerClause _ val groups)
  = (unboundOutputErrsValue variance val ++) . concat <$> traverse (unboundOutputErrsGroup extraBinds isCovariant stk) groups
  where isCovariant
          = case variance of
              I.VarianceContravariant -> False
              I.VarianceCovariant _ -> True
        extraBinds
          = case variance of
              I.VarianceContravariant -> bindsInClause True clause
              I.VarianceCovariant binds -> binds

unboundOutputErrsReducer :: S.Set Int -> Bool -> S.Set Int -> Reducer Range -> State (V.Vector (GroupDef Range, Bool, Bool)) [Error]
unboundOutputErrsReducer extraBinds isCovariant stk (Reducer _ input output)
    | isCovariant
    = (++)
  <$> unboundOutputErrsClause I.VarianceContravariant stk input
  <*> unboundOutputErrsClause (I.VarianceCovariant $ extraBinds <> bindsInClause True input) stk output
    | otherwise
    = (++)
  <$> unboundOutputErrsClause (I.VarianceCovariant $ extraBinds <> bindsInClause True output) stk input
  <*> unboundOutputErrsClause I.VarianceContravariant stk output

unboundOutputErrs1 :: S.Set Int -> Bool -> S.Set Int -> Statement Range -> State (V.Vector (GroupDef Range, Bool, Bool)) [Error]
unboundOutputErrs1 extraBinds isCovariant stk (StatementGroup group) = unboundOutputErrsGroup extraBinds isCovariant stk group
unboundOutputErrs1 extraBinds isCovariant stk (StatementReducer red) = unboundOutputErrsReducer extraBinds isCovariant stk red

unboundOutputErrs :: S.Set Int -> Bool -> [(S.Set Int, Statement Range)] -> State (V.Vector (GroupDef Range, Bool, Bool)) [Error]
unboundOutputErrs extraBinds isCovariant stmts
  = concat <$> traverse (uncurry $ unboundOutputErrs1 extraBinds isCovariant) stmts

validationErrs :: SessionEnv -> Program Range -> [Error]
validationErrs env (Program _ decls stmts groups)
   = duplicateDeclErrs importedDecls decls
  ++ invalidRecordErrs allDecls allReds
  ++ evalState (unboundOutputErrs S.empty True $ map (S.empty, ) stmts) groupsVec
  where importedDecls = allImportedDecls env
        allDecls = map compactRecordDecl decls ++ importedDecls
        allStmts = stmts ++ concatMap groupDefStatementList groups
        allReds = mapMaybe (\case StatementReducer x -> Just x; _ -> Nothing) allStmts
        groupsVec = V.fromList $ map (, False, False) groups

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
