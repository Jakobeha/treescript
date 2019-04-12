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
          , recordHead = head'
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
parseValPropDecl (S.GenPropertyGroup grp) = do
  tellError $ parseError (getAnn grp) "expected value, got group"
  pure undefinedSym

parseRecordDecl :: S.RecordDecl Range -> SessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ (S.Symbol _ head') props))
  = RecordDecl rng head' <$> traverse parseValPropDecl props

parsePrim :: S.Primitive Range -> I.GVBindSessionRes (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseValueProperty :: S.GenProperty Range -> I.GVBindSessionRes (Value Range)
parseValueProperty (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected value, got lowercase symbol"
parseValueProperty (S.GenPropertySubGroup prop)
  = mkFail $ parseError (getAnn prop) "expected value, got group property declaration"
parseValueProperty (S.GenPropertyRecord val) = parseValue val
parseValueProperty (S.GenPropertyGroup grp)
  = mkFail $ parseError (getAnn grp) "expected value, got group"

parseRecord :: S.Record Range -> I.GVBindSessionRes (Record Range)
parseRecord (S.Record rng (S.Symbol _ head') props)
  = Record rng head' <$> traverseDropFatals parseValueProperty props

parseBind :: S.Bind Range -> I.GVBindSessionRes (Bind Range)
parseBind (S.Bind rng tgt)
  = case tgt of
      S.BindTargetNone _ -> pure $ Bind rng 0
      S.BindTargetSome (S.Symbol _ txt) -> do
        env <- get
        let (idx, newEnv) = I.bindEnvLookup txt $ I.gvEnvValue env
        put env{ I.gvEnvValue = newEnv }
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

parseSpliceCode :: S.SpliceCode Range -> I.GVBindSessionRes (Value Range)
parseSpliceCode (S.SpliceCode rng (S.Symbol langExtRng langExt) spliceText) = do
  let txt = flattenSpliceText spliceText
      unparsedSplices = spliceTextSplices spliceText
  splices <- traverse parseSplice unparsedSplices
  lang <- lift $ overErrors (addRangeToErr langExtRng) $ langWithExt StageExtracting langExt
  ress <- overErrors (addRangeToErr rng) $ lift $ runLanguageParser lang txt splices
  case ress of
    [res] -> pure $ (rng `fromMaybe`) <$> res
    _ -> mkFail $ parseError rng $ "expected 1 value, got " <> pprint (length ress)

parseSplice :: S.Splice Range -> I.GVBindSessionRes (Value Range)
parseSplice (S.SpliceBind targ) = ValueBind <$> parseBind bind
  where bind = S.Bind (getAnn targ) targ
parseSplice (S.SpliceHole (S.HoleIdx ann idx)) = pure $ hole ann ann idx

parseValue :: S.Value Range -> I.GVBindSessionRes (Value Range)
parseValue (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue (S.ValueRecord record) = ValueRecord <$> parseRecord record
parseValue (S.ValueBind bind) = ValueBind <$> parseBind bind
parseValue (S.ValueSpliceCode code) = parseSpliceCode code
parseValue (S.ValueHole (S.Hole ann (S.HoleIdx idxAnn idx))) = pure $ hole ann idxAnn idx

parseGroupHead1 :: S.GroupLoc Range -> S.Symbol Range -> I.GVBindSessionRes (I.GroupLoc Range)
parseGroupHead1 (S.GroupLocGlobal _) head' = pure $ I.GroupLocGlobal head'
parseGroupHead1 (S.GroupLocLocal _) (S.Symbol rng txt) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt $ I.gvEnvGroup env
  put env{ I.gvEnvGroup = newEnv }
  pure $ I.GroupLocLocal $ Bind rng idx
parseGroupHead1 (S.GroupLocFunction _) head' = pure $ I.GroupLocFunction head'

parseGroupProperty1 :: S.GenProperty Range -> I.GVBindSessionRes (I.GroupRef Range)
parseGroupProperty1 (S.GenPropertyDecl key)
  = mkFail $ parseError (getAnn key) "expected group, got lowercase symbol"
parseGroupProperty1 (S.GenPropertySubGroup prop)
  = mkFail $ parseError (getAnn prop) "expected group, got group property declaration"
parseGroupProperty1 (S.GenPropertyRecord val)
  = mkFail $ parseError (getAnn val) "expected group, got value"
parseGroupProperty1 (S.GenPropertyGroup grp)
  = parseGroupRef1 grp

parseGroupRef1 :: S.Group Range -> I.GVBindSessionRes (I.GroupRef Range)
parseGroupRef1 (S.Group rng isProp head' props)
    = I.GroupRef rng
  <$> parseGroupHead1 isProp head'
  <*> traverseDropFatals parseGroupProperty1 props

parseGuard1 :: S.Guard Range -> I.GVBindSessionRes (I.Guard Range)
parseGuard1 (S.Guard rng input output nexts)
    = I.Guard rng
  <$> parseValue input
  <*> parseValue output
  <*> traverse parseGroupRef1 nexts

parseReducer1 :: I.BindEnv -> S.Reducer Range -> SessionRes (I.Reducer Range)
parseReducer1 gBindEnv (S.Reducer rng main guards)
    = evalStateT
    ( I.Reducer rng
  <$> parseGuard1 main
  <*> traverse parseGuard1 guards
    ) I.GVEnv
    { I.gvEnvGroup = gBindEnv
    , I.gvEnvValue = I.emptyBindEnv
    }

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
parseSubGroupPropDecl1 (S.GenPropertyGroup grp)
  = mkFail $ parseError (getAnn grp) "expected group property declaration, got group"

parseEmptyGroupDef1 :: S.GroupDecl Range -> SessionRes (I.GroupDef Range)
parseEmptyGroupDef1 (S.GroupDecl rng (S.Group _ loc (S.Symbol headRng head') props))
  = case loc of
      S.GroupLocGlobal _ -> do
        (props', gbindEnv)
          <- runStateT (traverseDropFatals parseSubGroupPropDecl1 props) I.emptyBindEnv
        pure I.GroupDef
          { I.groupDefAnn = rng
          , I.groupDefHead = head'
          , I.groupDefProps = props'
          , I.groupDefReducers = []
          , I.groupDefPropEnv = gbindEnv
          }
      S.GroupLocLocal _ -> mkFail $ parseError headRng "can't declare a lowercase group, lowercase groups are group properties"
      S.GroupLocFunction _ -> mkFail $ parseError headRng "can't declare a function, functions are provided by libraries"

parseRestGroupDefs1 :: S.GroupDecl Range -> [S.TopLevel Range] -> SessionRes (N.NonEmpty (I.GroupDef Range))
parseRestGroupDefs1 decl [] = (N.:| []) <$> parseEmptyGroupDef1 decl
parseRestGroupDefs1 decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs1 decl xs
parseRestGroupDefs1 decl (S.TopLevelReducer red : xs) = do
  I.GroupDef yRng yHead yProps yReds yBindEnv N.:| ys <- parseRestGroupDefs1 decl xs
  red' <- parseReducer1 yBindEnv red
  let yRng' = getAnn red <> yRng
      yReds' = red' : yReds
  pure $ I.GroupDef yRng' yHead yProps yReds' yBindEnv N.:| ys
parseRestGroupDefs1 decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef1 decl <*> parseRestGroupDefs1 decl' xs

parseAllGroupDefs1 :: [S.TopLevel Range] -> SessionRes [I.GroupDef Range]
parseAllGroupDefs1 [] = pure []
parseAllGroupDefs1 (S.TopLevelGroupDecl x : xs)
  = N.toList <$> parseRestGroupDefs1 x xs
parseAllGroupDefs1 (_ : _) = error "expected group declaration when parsing group definitions"

parseGroupRef2 :: I.GroupRef Range -> I.GroupSessionRes (GroupRef Range)
parseGroupRef2 (I.GroupRef rng loc props) = do
  groupEnv <- ask
  loc' <-
    case loc of
      I.GroupLocGlobal (S.Symbol headRng headSym) ->
        case groupEnv M.!? headSym of
          Nothing -> mkFail $ parseError headRng $ "undefined group: " <> headSym
          Just idx -> pure $ GroupLocGlobal headRng idx
      I.GroupLocLocal (Bind headRng idx) -> pure $ GroupLocLocal headRng idx
      I.GroupLocFunction (S.Symbol headRng headSym) -> pure $ GroupLocFunction headRng headSym
  props' <- traverse parseGroupRef2 props
  pure GroupRef
    { groupRefAnn = rng
    , groupRefLoc = loc'
    , groupRefProps = props'
    }

parseGuard2 :: I.Guard Range -> I.GroupSessionRes (Guard Range)
parseGuard2 (I.Guard rng input output nexts)
  = Guard rng input output <$> traverse parseGroupRef2 nexts

parseReducer2 :: I.Reducer Range -> I.GroupSessionRes (Reducer Range)
parseReducer2 (I.Reducer rng main guards)
    = Reducer rng
  <$> parseGuard2 main
  <*> traverse parseGuard2 guards

parseGroupDef2 :: I.GroupDef Range -> I.GroupSessionRes (GroupDef Range)
parseGroupDef2 (I.GroupDef rng _ props reds _)
    = GroupDef rng (map (\(_, bind) -> bind) props)
  <$> traverseDropFatals parseReducer2 reds

parseGroupDefEnv :: I.GroupDef Range -> Int -> (T.Text, Int)
parseGroupDefEnv group idx = (I.groupDefHead group, idx)

notGroupedReducerError :: Reducer Range -> Error
notGroupedReducerError red
  = parseError (getAnn red) "reducer not in group"

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: S.Program Range -> SessionRes (Program Range)
parseRaw (S.Program rng topLevels) = do
  let (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl _ -> True; _ -> False) topLevels
      decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevelsOutGroups
  decls' <- traverse parseRecordDecl decls
  reds1 <- traverseDropFatals (parseReducer1 I.emptyBindEnv) reds
  groups1 <- parseAllGroupDefs1 topLevelsInGroups
  let groupEnv = M.fromList $ zipWith parseGroupDefEnv groups1 [0..]
  reds' <- runReaderT (traverseDropFatals parseReducer2 reds1) groupEnv
  groups' <- runReaderT (traverseDropFatals parseGroupDef2 groups1) groupEnv
  -- TODO Syntax sugar a main group
  tellErrors $ map notGroupedReducerError reds'
  pure Program
    { programAnn = rng
    , programRecordDecls = decls'
    , programGroups = groups'
    }

-- == Validation

duplicateDeclErrs :: S.Set DeclCompact -> [RecordDecl Range] -> [Error]
duplicateDeclErrs imported decls
  = catMaybes $ zipWith duplicateDeclErr decls (inits decls)
  -- TODO: Optimize with a map of names (should fold decls in too)
  where duplicateDeclErr (RecordDecl rng head' _) prevs
          | any (\other -> declCompactHead other == head') others
          = Just $ parseError rng $ "duplicate record declaration: " <> head'
          | otherwise = Nothing
          where others = map compactRecordDecl prevs ++ S.toList imported

invalidRecordErrs :: DeclSet -> [Reducer Range] -> [Error]
invalidRecordErrs decls reds
  = concatMap (foldValuesInReducer invalidRecordErrorsValue1) reds
  where invalidRecordErrorsValue1 (ValuePrimitive _) = []
        invalidRecordErrorsValue1 (ValueRecord (Record rng head' props))
          -- TODO: Use a map of names to prop info instead
          = case find (\decl -> declCompactHead decl == head') $ S.toList $ declSetRecords decls of
              Nothing -> [parseError rng $ "undeclared record: " <> head']
              Just decl
                | numDeclProps /= numProps && numDeclProps /= varNumProps -> [ parseError rng
                 $ "record has wrong number of properties: expected "
                <> pprint numDeclProps
                <> ", got "
                <> pprint numProps ]
                | otherwise -> []
                where numDeclProps = declCompactNumProps decl
                      numProps = length props
        invalidRecordErrorsValue1 (ValueBind _) = []

unboundOutputErrsValue :: I.Variance -> Value Range -> [Error]
unboundOutputErrsValue _ (ValuePrimitive _) = []
unboundOutputErrsValue I.VarianceContravariant (ValueRecord record)
  = concatMap (unboundOutputErrsValue I.VarianceContravariant) $ recordProps record
unboundOutputErrsValue I.VarianceContravariant (ValueBind _) = []
unboundOutputErrsValue (I.VarianceCovariant binds) (ValueRecord record)
  = foldMap (unboundOutputErrsValue $ I.VarianceCovariant binds) $ recordProps record
unboundOutputErrsValue (I.VarianceCovariant binds) (ValueBind (Bind rng idx))
  | S.member idx binds = []
  | idx == 0 = [parseError rng "unlabeled bind in non-matching position"]
  | otherwise = [parseError rng "bind in non-matching position has unassigned label"]

unboundOutputErrsGroup :: S.Set Int -> S.Set Int -> GroupRef Range -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrsGroup extraBinds stk (GroupRef _ (GroupLocGlobal _ idx) props)
  | S.member idx stk = pure []
  | otherwise = do
    groups <- get
    let stk' = S.insert idx stk
        (group, checked) = groups V.! idx
    if checked then
      pure []
    else do
      let reds = allGroupDefReducers props group
          newGroups = groups V.// [(idx, (group, True))]
      put newGroups
      unboundOutputErrs extraBinds stk' reds
unboundOutputErrsGroup _ _ (GroupRef _ (GroupLocLocal _ _) _)
  = error "unboundOutputErrsGroup: expected group property to be resolved"
unboundOutputErrsGroup _ _ (GroupRef _ (GroupLocFunction _ _) _) = pure []


unboundOutputErrsGuard :: S.Set Int -> S.Set Int -> Guard Range -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrsGuard binds stk (Guard _ input output nexts)
  = concat <$> sequence
  [ pure $ unboundOutputErrsValue I.VarianceContravariant input -- @pure@ = singleton
  , pure $ unboundOutputErrsValue (I.VarianceCovariant binds) output -- @pure@ = singleton
  , concat <$> traverse (unboundOutputErrsGroup binds stk) nexts
  ]

unboundOutputErrs1 :: S.Set Int -> S.Set Int -> Reducer Range -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrs1 extraBinds stk red@(Reducer _ main guards)
  = concat <$> sequence
  [ unboundOutputErrsGuard binds stk main -- @pure@ = singleton
  , concat <$> traverse (unboundOutputErrsGuard binds stk) guards
  ]
  where binds = extraBinds <> bindsInReducerInput red


unboundOutputErrs :: S.Set Int -> S.Set Int -> [Reducer Range] -> State (V.Vector (GroupDef Range, Bool)) [Error]
unboundOutputErrs extraBinds stk reds
  = concat <$> traverse (unboundOutputErrs1 extraBinds stk) reds

-- TODO: Undeclared function, unbound group errors

validationErrs :: SessionEnv -> Program Range -> [Error]
validationErrs env (Program _ decls groups)
   = duplicateDeclErrs (declSetRecords importedDecls) decls
  ++ invalidRecordErrs allDecls allReds
  ++ evalState (unboundOutputErrs S.empty S.empty mainReds) groupsVec
  where importedDecls = allImportedDecls env
        localDecls
          = DeclSet
          { declSetRecords = S.fromList $ map compactRecordDecl decls
          , declSetFunctions = S.empty
          }
        allDecls = localDecls <> importedDecls
        allReds = concatMap groupDefReducers groups
        mainGroup = headOpt groups
        mainReds = foldMap groupDefReducers mainGroup
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
