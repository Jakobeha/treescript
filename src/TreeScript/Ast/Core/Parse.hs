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

decodeAstData :: T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
decodeAstData txt splices = traverse decodeAstData1 =<< ResultT (pure $ F.parse txt)
  where
    splicesVec = V.fromList splices
    decodeAstData1 lexs = do
      (res, rest) <- valueParser lexs
      unless (null rest) $
        mkFail $ decodeError "unexpected extra nodes after decoded value"
      pure res
    valueParser [] = mkFail $ decodeError "tried to decode value from no nodes"
    valueParser (F.LexemeEnterSplice idx : rest)
      = case splicesVec V.!? idx of
          Nothing -> mkFail $ decodeError $ "invalid splice index: " <> pprint idx
          Just splice -> pure (Just <$> splice, rest)
    valueParser (F.LexemePrimitive prim : rest) = pure (ValuePrimitive $ primParser prim, rest)
    valueParser (F.LexemeRecordHead head' numProps : rest) = first ValueRecord <$> recordParser head' numProps rest
    primParser (F.PrimInteger value) = PrimInteger Nothing value
    primParser (F.PrimFloat value) = PrimFloat Nothing value
    primParser (F.PrimString value) = PrimString Nothing value
    recordParser head' numProps rest = do
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
  let parser = languageParser lang
  astData <- overErrors (prependMsgToErr $ "couldn't parse code") $ runCmdProgram parser txt
  overErrors (prependMsgToErr "parsing plugin returned bad AST data") $ decodeAstData astData splices

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

parseValPropDecl :: S.GenProperty Range -> SessionRes T.Text
parseValPropDecl (S.GenPropertyDecl (S.Symbol _ decl)) = pure decl
parseValPropDecl (S.GenPropertySubGroup prop) = do
  tellError $ parseError (getAnn prop) "expected lowercase symbol, got group property declaration"
  pure undefinedSym
parseValPropDecl (S.GenPropertyRecord val) = do
  tellError $ parseError (getAnn val) "expected lowercase symbol, got value"
  pure undefinedSym

parseRecordDecl :: S.RecordDecl Range -> SessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ isFunc (S.Symbol _ head') props))
  | isFunc = mkFail $ parseError rng $ "can't declare function"
  | otherwise = RecordDecl rng head' <$> traverse parseValPropDecl props

parsePrim :: S.Primitive Range -> I.PropSessionRes (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseValueProperty :: S.GenProperty Range -> I.PropSessionRes (Value Range)
parseValueProperty (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected value, got lowercase symbol"
parseValueProperty (S.GenPropertySubGroup prop)
  = mkFail $ parseError (getAnn prop) "expected value, got group property declaration"
parseValueProperty (S.GenPropertyRecord val) = parseValue val

parseRecord :: S.Record Range -> I.PropSessionRes (Record Range)
parseRecord (S.Record rng isFunc (S.Symbol _ head') props)
  = Record rng (RecordHead isFunc head') <$> traverseDropFatals parseValueProperty props

parseBind :: S.Bind Range -> I.PropSessionRes (Bind Range)
parseBind (S.Bind rng tgt)
  = case tgt of
      S.BindTargetNone _ -> pure $ Bind rng 0
      S.BindTargetSome (S.Symbol _ txt) -> do
        env <- get
        let (idx, newEnv) = I.bindEnvLookup txt $ I.groupValEnvValue env
        put env{ I.groupValEnvValue = newEnv }
        pure $ Bind rng idx

flattenSpliceText :: S.SpliceText Range -> T.Text
flattenSpliceText spliceText = flattenRest 0 spliceText
  where flattenRest :: Int -> S.SpliceText Range -> T.Text
        flattenRest _ (S.SpliceTextNil _ txt) = txt
        flattenRest n (S.SpliceTextCons _ txt isMulti _ rst)
           = txt
          <> "\\"
          <> printIsMulti isMulti
          <> pprint n
          <> flattenRest (n + 1) rst
        printIsMulti False = ""
        printIsMulti True = "."

spliceTextSplices :: S.SpliceText Range -> [S.Splice Range]
spliceTextSplices (S.SpliceTextNil _ _) = []
spliceTextSplices (S.SpliceTextCons _ _ _ val rst) = val : spliceTextSplices rst

parseSpliceCode :: S.SpliceCode Range -> I.PropSessionRes (Value Range)
parseSpliceCode (S.SpliceCode rng (S.Symbol langExtRng langExt) spliceText) = do
  let txt = flattenSpliceText spliceText
      unparsedSplices = spliceTextSplices spliceText
  splices <- traverse parseSplice unparsedSplices
  lang <- lift $ overErrors (addRangeToErr langExtRng) $ langWithExt StageExtracting langExt
  ress <- overErrors (addRangeToErr rng) $ lift $ runLanguageParser lang txt splices
  case ress of
    [res] -> pure $ (rng `fromMaybe`) <$> res
    _ -> mkFail $ parseError rng $ "expected 1 value, got " <> pprint (length ress)

parseSplice :: S.Splice Range -> I.PropSessionRes (Value Range)
parseSplice (S.SpliceBind targ) = ValueBind <$> parseBind bind
  where bind = S.Bind (getAnn targ) targ
parseSplice (S.SpliceHole (S.HoleIdx ann idx)) = pure $ hole ann ann idx

parseValue :: S.Value Range -> I.PropSessionRes (Value Range)
parseValue (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue (S.ValueRecord record) = ValueRecord <$> parseRecord record
parseValue (S.ValueBind bind) = ValueBind <$> parseBind bind
parseValue (S.ValueSpliceCode code) = parseSpliceCode code
parseValue (S.ValueHole (S.Hole ann (S.HoleIdx idxAnn idx))) = pure $ hole ann idxAnn idx
parseValue (S.ValueGroup group)
  = mkFail $ parseError (getAnn group) "expected value, got group"

parseGroupHead1 :: Bool -> S.Symbol Range -> I.PropSessionRes (I.GroupHead Range)
parseGroupHead1 False head' = pure $ I.GroupHeadGlobal head'
parseGroupHead1 True (S.Symbol rng txt) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt $ I.groupValEnvGroup env
  put env{ I.groupValEnvGroup = newEnv }
  pure $ I.GroupHeadProp $ Bind rng idx

parseGroupProperty1 :: S.GenProperty Range -> I.PropSessionRes (I.GroupRef Range)
parseGroupProperty1 (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected group, got lowercase symbol"
parseGroupProperty1 (S.GenPropertySubGroup prop)
  = mkFail $ parseError (getAnn prop) "expected group, got group property declaration"
parseGroupProperty1 (S.GenPropertyRecord val) = parseGroupRef1 val

parseGroupRefFromGroup1 :: S.Group Range -> I.PropSessionRes (I.GroupRef Range)
parseGroupRefFromGroup1 (S.Group rng isProp head' gprops vprops)
    = I.GroupRef rng
  <$> parseGroupHead1 isProp head'
  <*> traverseDropFatals parseGroupProperty1 gprops
  <*> traverseDropFatals parseValueProperty vprops

parseGroupRef1 :: S.Value Range -> I.PropSessionRes (I.GroupRef Range)
parseGroupRef1 (S.ValueRecord (S.Record _ False (S.Symbol headAnn _) [S.GenPropertyRecord (S.ValueGroup _)]))
  = mkFail $ parseError headAnn $ "can't specify reduce type of group not used as a statement"
parseGroupRef1 (S.ValueGroup group)
  = parseGroupRefFromGroup1 group
parseGroupRef1 val
  = mkFail $ parseError (getAnn val) "expected group (not statement), got value"

parseGroupStmt1 :: S.Value Range -> I.PropSessionRes (I.GroupStmt Range)
parseGroupStmt1 (S.ValueRecord (S.Record _ False (S.Symbol headAnn headSpec) [S.GenPropertyRecord (S.ValueGroup group)]))
  | headSpec == "E"
  = I.GroupStmt (getAnn group) ReduceTypeEvalCtx <$> parseGroupRefFromGroup1 group
  | otherwise
  = mkFail $ parseError headAnn $ "invalid reduce type specifier: " <> headSpec
parseGroupStmt1 (S.ValueGroup group)
  = I.GroupStmt (getAnn group) ReduceTypeRegular <$> parseGroupRefFromGroup1 group
parseGroupStmt1 val
  = mkFail $ parseError (getAnn val) "expected group statement, got value"

parseGuard1 :: S.Statement Range -> I.PropSessionRes (I.Guard Range)
parseGuard1 (S.StatementGroup group) = I.GuardGroup <$> parseGroupRef1 group
parseGuard1 (S.StatementReducer red) = I.GuardReducer <$> parseReducer1 red

parseReducer1 :: S.Reducer Range -> I.PropSessionRes (I.Reducer Range)
parseReducer1 (S.Reducer rng input output nexts guards)
    = I.Reducer rng
  <$> parseValue input
  <*> parseValue output
  <*> traverse parseGroupRefFromGroup1 nexts
  <*> traverse parseGuard1 guards

parseStatement1 :: S.Statement Range -> I.PropSessionRes (I.Statement Range)
parseStatement1 (S.StatementGroup group) = I.StatementGroup <$> parseGroupStmt1 group
parseStatement1 (S.StatementReducer red) = I.StatementReducer <$> parseReducer1 red

parseSubGroupPropDecl1 :: S.GenProperty Range -> I.BindSessionRes (T.Text, Bind Range)
parseSubGroupPropDecl1 (S.GenPropertyDecl prop) = do
  mkFail $ parseError (getAnn prop) "expected group property declaration, got lowercase symbol"
parseSubGroupPropDecl1 (S.GenPropertySubGroup (S.SubGroupProperty rng (S.Symbol _ txt))) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt env
  put newEnv
  pure (txt, Bind rng idx)
parseSubGroupPropDecl1 (S.GenPropertyRecord val) = do
  mkFail $ parseError (getAnn val) "expected group property declaration, got value"

parseGroupValPropDecl1 :: S.GenProperty Range -> I.BindSessionRes (T.Text, Bind Range)
parseGroupValPropDecl1 (S.GenPropertyDecl (S.Symbol rng txt)) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt env
  put newEnv
  pure (txt, Bind rng idx)
parseGroupValPropDecl1 (S.GenPropertySubGroup prop) = do
  mkFail $ parseError (getAnn prop) "expected lowercase symbol, got group property declaration"
parseGroupValPropDecl1 (S.GenPropertyRecord val) = do
  mkFail $ parseError (getAnn val) "expected lowercase symbol, got value"

parseEmptyGroupDef1 :: S.GroupDecl Range -> I.FreeSessionRes (I.GroupDef Range)
parseEmptyGroupDef1 (S.GroupDecl rng (S.Group _ isProp (S.Symbol headRng head') gprops vprops))
  | isProp = mkFail $ parseError headRng "can't declare a lowercase group - lowercase groups are group properties"
  | otherwise = do
    (gprops', gbindEnv)
      <- lift
        $ (`runStateT` I.emptyBindEnv)
        $ traverseDropFatals parseSubGroupPropDecl1 gprops
    (vprops', vbindEnv)
      <- lift
        $ (`runStateT` I.emptyBindEnv)
        $ traverseDropFatals parseGroupValPropDecl1 vprops
    let propEnv = I.GroupValEnv
          { I.groupValEnvGroup = gbindEnv
          , I.groupValEnvValue = vbindEnv
          }
    put $ I.bindEnvNextFree <$> propEnv
    pure I.GroupDef
      { I.groupDefAnn = rng
      , I.groupDefHead = head'
      , I.groupDefGroupProps = gprops'
      , I.groupDefValueProps = vprops'
      , I.groupDefStatements = []
      , I.groupDefPropEnv = propEnv
      }

parseRestGroupDefs1 :: S.GroupDecl Range -> [S.TopLevel Range] -> I.FreeSessionRes (N.NonEmpty (I.GroupDef Range))
parseRestGroupDefs1 decl [] = (N.:| []) <$> parseEmptyGroupDef1 decl
parseRestGroupDefs1 decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs1 decl xs
parseRestGroupDefs1 decl (S.TopLevelStatement stmt : xs) = do
  I.GroupDef yRng yHead yGProps yVProps yStmts yPropEnv N.:| ys <- parseRestGroupDefs1 decl xs
  nextFree <- get
  let yPropEnv' = I.zipGroupValEnvWith (\bindEnv nextFree' -> bindEnv{ I.bindEnvNextFree = nextFree' }) yPropEnv nextFree
  (stmt', yPropEnv'') <- lift $ runStateT (parseStatement1 stmt) yPropEnv'
  put $ I.bindEnvNextFree <$> yPropEnv''
  let yStmts' = stmt' : yStmts
  pure $ I.GroupDef (getAnn stmt <> yRng) yHead yGProps yVProps yStmts' yPropEnv'' N.:| ys
parseRestGroupDefs1 decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef1 decl <*> parseRestGroupDefs1 decl' xs

parseAllGroupDefs1 :: [S.TopLevel Range] -> SessionRes [I.GroupDef Range]
parseAllGroupDefs1 [] = pure []
parseAllGroupDefs1 (S.TopLevelGroupDecl x : xs)
  = N.toList <$> evalStateT (parseRestGroupDefs1 x xs) I.emptyFreeEnv
parseAllGroupDefs1 (_ : _) = error "expected group declaration when parsing group definitions"

parseGroupRef2 :: I.GroupRef Range -> I.GroupSessionRes (GroupRef Range)
parseGroupRef2 (I.GroupRef rng head' gprops vprops) = do
  groupEnv <- ask
  (isProp, groupIdx) <-
    case head' of
      I.GroupHeadGlobal (S.Symbol headRng headSym) ->
        case groupEnv M.!? headSym of
          Nothing -> mkFail $ parseError headRng $ "undefined group: " <> headSym
          Just idx -> pure (False, idx)
      I.GroupHeadProp (Bind _ idx) -> pure (True, idx)
  gprops' <- traverse parseGroupRef2 gprops
  pure GroupRef
    { groupRefAnn = rng
    , groupRefIsProp = isProp
    , groupRefIdx = groupIdx
    , groupRefGroupProps = gprops'
    , groupRefValueProps = vprops
    }

parseReducer2 :: I.Reducer Range -> I.GroupSessionRes (Reducer Range)
parseReducer2 (I.Reducer rng input output nexts guards)
    = Reducer rng input output
  <$> traverse parseGroupRef2 nexts
  <*> traverse parseGuard2 guards

parseGuard2 :: I.Guard Range -> I.GroupSessionRes (Statement Range)
parseGuard2 (I.GuardGroup group)
  = StatementGroup <$> parseGroupRef2 group
parseGuard2 (I.GuardReducer red)
  = StatementReducer <$> parseReducer2 red

parseStatement2 :: I.Statement Range -> I.GroupSessionRes (Statement Range)
parseStatement2 (I.StatementGroup group)
  = StatementGroup <$> parseGroupRef2 (I.groupStmtRef group)
parseStatement2 (I.StatementReducer red)
  = StatementReducer <$> parseReducer2 red

statementReduceType :: I.Statement Range -> ReduceType
statementReduceType (I.StatementGroup group) = I.groupStmtReduceType group
statementReduceType (I.StatementReducer reducer)
  = case I.reducerInput reducer of
      ValuePrimitive _ -> ReduceTypeRegular
      ValueRecord (Record _ (RecordHead isFunc head') _)
        | not isFunc && head' == "E" -> ReduceTypeEvalCtx
        | otherwise -> ReduceTypeRegular
      ValueBind _ -> ReduceTypeRegular

groupStmtsByReduceType :: [I.Statement Range] -> A.Array ReduceType [I.Statement Range]
groupStmtsByReduceType
  = fmap reverse
  . A.accumArray (flip (:)) [] (minBound, maxBound)
  . map (\stmt -> (statementReduceType stmt, stmt))

parseGroupDef2 :: I.GroupDef Range -> I.GroupSessionRes (GroupDef Range)
parseGroupDef2 (I.GroupDef rng _ gprops vprops stmts _)
    = GroupDef rng (map (\(_, bind) -> bind) gprops) (map (\(_, bind) -> bind) vprops)
  <$> traverse (traverseDropFatals parseStatement2) (groupStmtsByReduceType stmts)

parseGroupDefEnv :: I.GroupDef Range -> Int -> (T.Text, Int)
parseGroupDefEnv group idx = (I.groupDefHead group, idx)

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: S.Program Range -> SessionRes (Program Range)
parseRaw (S.Program rng topLevels) = do
  let (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl _ -> True; _ -> False) topLevels
      decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      stmts = mapMaybe (\case S.TopLevelStatement x -> Just x; _ -> Nothing) topLevelsOutGroups
  decls' <- traverse parseRecordDecl decls
  stmts1 <- evalStateT (traverseDropFatals parseStatement1 stmts) I.emptyPropEnv
  groups1 <- parseAllGroupDefs1 topLevelsInGroups
  let groupEnv = M.fromList $ zipWith parseGroupDefEnv groups1 [0..]
  stmts' <- runReaderT (traverseDropFatals parseStatement2 stmts1) groupEnv
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
  = concatMap (foldValuesInReducer invalidRecordErrorsValue1 True) reds
  where invalidRecordErrorsValue1 (ValuePrimitive _) = []
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

unboundOutputErrsGroup :: S.Set Int -> S.Set Int -> GroupRef Range -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrsGroup extraBinds stk (GroupRef _ False idx gprops vprops)
  | S.member idx stk = pure []
  | otherwise = do
    groups <- get
    let stk' = S.insert idx stk
        (group, checked) = groups V.! idx
    if checked then
      pure []
    else do
      let stmts = allGroupDefStatements gprops vprops group
          stkStmts = map (stk', ) $ concat $ A.elems stmts
          newGroups = groups V.// [(idx, (group, True))]
      put newGroups
      unboundOutputErrs extraBinds stkStmts
unboundOutputErrsGroup _ _ (GroupRef _ True _ _ _)
  = error "unboundOutputErrsGroup: expected group property to be resolved"

unboundOutputErrsReducer :: S.Set Int -> S.Set Int -> Reducer Range -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrsReducer extraBinds stk (Reducer _ input output nexts guards) = do
    groups <- V.map fst <$> get
    let binds
           = extraBinds
          <> bindsInValue input
          <> foldMap (bindsInGuardInput groups) guards
    concat <$> sequence
      [ pure $ unboundOutputErrsValue I.VarianceContravariant input -- @pure@ = singleton
      , pure $ unboundOutputErrsValue (I.VarianceCovariant binds) output -- @pure@ = singleton
      , concat <$> traverse (unboundOutputErrsGroup binds stk) nexts
      , concat <$> traverse (unboundOutputErrs1 binds stk) guards
      ]

unboundOutputErrs1 :: S.Set Int -> S.Set Int -> Statement Range -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrs1 extraBinds stk (StatementGroup group) = unboundOutputErrsGroup extraBinds stk group
unboundOutputErrs1 extraBinds stk (StatementReducer red) = unboundOutputErrsReducer extraBinds stk red

unboundOutputErrs :: S.Set Int -> [(S.Set Int, Statement Range)] -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrs extraBinds stmts
  = concat <$> traverse (uncurry $ unboundOutputErrs1 extraBinds) stmts

-- TODO: Unbound group errors

validationErrs :: SessionEnv -> Program Range -> [Error]
validationErrs env (Program _ decls stmts groups)
   = duplicateDeclErrs importedDecls decls
  ++ invalidRecordErrs allDecls allReds
  ++ evalState (unboundOutputErrs S.empty $ map (S.empty, ) stmts) groupsVec
  where importedDecls = allImportedDecls env
        allDecls = map compactRecordDecl decls ++ importedDecls
        allStmts = stmts ++ concatMap groupDefStatementList groups
        allReds = mapMaybe (\case StatementReducer x -> Just x; _ -> Nothing) allStmts
        groupsVec = V.fromList $ map (, False) groups

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
