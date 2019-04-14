{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module TreeScript.Ast.Core.Parse
  ( parse
  ) where

import TreeScript.Ast.Core.Validate
import qualified TreeScript.Ast.Core.Intermediate as I
import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Flat as F
import qualified TreeScript.Ast.Sugar as S
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Either
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.Maybe
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

parseAtomType :: S.Symbol Range -> SessionRes AtomType
parseAtomType (S.Symbol rng txt)
  | txt == "any" = pure AtomTypeAny
  | txt == "int" = pure AtomTypeInteger
  | txt == "float" = pure AtomTypeFloat
  | txt == "string" = pure AtomTypeString
  | otherwise = do
    tellError $ desugarError rng $ "unknown atom type: " <> txt
    pure AtomTypeAny

parseTypePart :: S.TypePart Range -> SessionRes (TypePart Range)
parseTypePart (S.TypePartAtom rng atm) = TypePartAtom rng <$> parseAtomType atm
parseTypePart (S.TypePartRecord rng (S.Symbol _ txt)) = pure $ TypePartRecord rng txt
parseTypePart (S.TypePartTransparent rng (S.Record _ (S.Symbol headRng head') props))
  | head' == "t" = TypePartTuple rng <$> traverse parseTypeProp props
  | head' == "list"
  = case props of
      [prop] -> TypePartList rng <$> parseTypeProp prop
      _ -> do
        tellError $ desugarError rng $ "list type must have 1 property, got " <> pprint (length props)
        pure $ TypePartAtom rng AtomTypeAny
  | otherwise = do
    tellError $ desugarError headRng $ "unknown transparent record type: " <> head'
    pure $ TypePartAtom rng AtomTypeAny

parseType :: S.Type Range -> SessionRes (Type Range)
parseType (S.Type rng parts) = Type rng <$> traverse parseTypePart parts

parseTypeProp :: S.GenProperty Range -> SessionRes (Type Range)
parseTypeProp (S.GenPropertyDecl typ) = parseType typ
parseTypeProp (S.GenPropertySubGroup prop) = do
  let rng = getAnn prop
  tellError $ desugarError rng "expected type, got group property declaration"
  pure $ Type rng [TypePartAtom rng AtomTypeAny]
parseTypeProp (S.GenPropertyRecord val) = do
  let rng = getAnn val
  tellError $ desugarError rng "expected type, got value"
  pure $ Type rng [TypePartAtom rng AtomTypeAny]
parseTypeProp (S.GenPropertyGroup grp) = do
  let rng = getAnn grp
  tellError $ desugarError rng "expected value, got group"
  pure $ Type rng [TypePartAtom rng AtomTypeAny]

parseRecordDecl :: S.RecordDecl Range -> SessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ (S.Symbol _ head') props))
  = RecordDecl rng head' <$> traverse parseTypeProp props

parsePrim :: S.Primitive Range -> I.GVBindSessionRes (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseValueProperty :: S.GenProperty Range -> I.GVBindSessionRes (Value Range)
parseValueProperty (S.GenPropertyDecl key)
  = mkFail $ desugarError (getAnn key) "expected value, got type"
parseValueProperty (S.GenPropertySubGroup prop)
  = mkFail $ desugarError (getAnn prop) "expected value, got group property declaration"
parseValueProperty (S.GenPropertyRecord val) = parseValue val
parseValueProperty (S.GenPropertyGroup grp)
  = mkFail $ desugarError (getAnn grp) "expected value, got group"

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
  lang <- lift $ overErrors (addRangeToErr langExtRng) $ langWithExt StageDesugar langExt
  ress <- overErrors (addRangeToErr rng) $ lift $ runLanguageParser lang txt splices
  case ress of
    [res] -> pure $ (rng `fromMaybe`) <$> res
    _ -> mkFail $ desugarError rng $ "expected 1 value, got " <> pprint (length ress)

parseSplice :: S.Splice Range -> I.GVBindSessionRes (Value Range)
parseSplice (S.SpliceBind targ) = ValueBind <$> parseBind bind
  where bind = S.Bind (getAnn targ) targ
parseSplice (S.SpliceHole (S.HoleIdx rng idx)) = pure $ hole rng rng idx

parseValue :: S.Value Range -> I.GVBindSessionRes (Value Range)
parseValue (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue (S.ValueRecord record) = ValueRecord <$> parseRecord record
parseValue (S.ValueBind bind) = ValueBind <$> parseBind bind
parseValue (S.ValueSpliceCode code) = parseSpliceCode code
parseValue (S.ValueHole (S.Hole rng (S.HoleIdx idxAnn idx))) = pure $ hole rng idxAnn idx

parseGroupHead1 :: S.GroupLoc Range -> S.Symbol Range -> I.GVBindSessionRes (I.GroupLoc Range)
parseGroupHead1 (S.GroupLocGlobal _) head' = pure $ I.GroupLocGlobal head'
parseGroupHead1 (S.GroupLocLocal _) (S.Symbol rng txt) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt $ I.gvEnvGroup env
  put env{ I.gvEnvGroup = newEnv }
  pure $ I.GroupLocLocal $ Bind rng idx
parseGroupHead1 (S.GroupLocFunction _) head' = pure $ I.GroupLocFunction head'

parseGroupProperty1 :: S.GenProperty Range -> I.GVBindSessionRes (Either (Value Range) (I.GroupRef Range))
parseGroupProperty1 (S.GenPropertyDecl key)
  = mkFail $ desugarError (getAnn key) "expected group, got type"
parseGroupProperty1 (S.GenPropertySubGroup prop)
  = mkFail $ desugarError (getAnn prop) "expected group, got group property declaration"
parseGroupProperty1 (S.GenPropertyRecord val)
  = Left <$> parseValue val
parseGroupProperty1 (S.GenPropertyGroup grp)
  = Right <$> parseGroupRef1 grp

parseGroupRef1 :: S.Group Range -> I.GVBindSessionRes (I.GroupRef Range)
parseGroupRef1 (S.Group rng isProp head' props) = do
  loc <- parseGroupHead1 isProp head'
  (vprops, gprops) <- partitionEithers <$> traverseDropFatals parseGroupProperty1 props
  pure $ I.GroupRef rng loc vprops gprops

parseGuard1 :: S.Guard Range -> I.GVBindSessionRes (I.Guard Range)
parseGuard1 (S.Guard rng input output nexts)
    = I.Guard rng
  <$> parseValue input
  <*> parseValue output
  <*> traverse parseGroupRef1 nexts

parseReducer1 :: I.GVBindEnv -> S.Reducer Range -> SessionRes (S.ReducerType Range, I.Reducer Range)
parseReducer1 bindEnv (S.Reducer rng typ main guards)
    = (typ, ) <$> evalStateT
    ( I.Reducer rng
  <$> parseGuard1 main
  <*> traverse parseGuard1 guards
    ) bindEnv

parseGroupPropDecl1 :: S.GenProperty Range -> I.GVBindSessionRes (Maybe (Side, (T.Text, Bind Range)))
parseGroupPropDecl1 (S.GenPropertyDecl prop)
  = mkFail $ desugarError (getAnn prop) "expected group property declaration, got type"
parseGroupPropDecl1 (S.GenPropertySubGroup (S.SubGroupProperty rng (S.Symbol _ txt))) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt $ I.gvEnvGroup env
  put env{ I.gvEnvGroup = newEnv }
  pure $ Just (SideRight, (txt, Bind rng idx))
parseGroupPropDecl1 (S.GenPropertyRecord (S.ValueBind (S.Bind rng (S.BindTargetSome (S.Symbol _ txt))))) = do
  env <- get
  let (idx, newEnv) = I.bindEnvLookup txt $ I.gvEnvValue env
  put env{ I.gvEnvValue = newEnv }
  pure $ Just (SideLeft, (txt, Bind rng idx))
parseGroupPropDecl1 (S.GenPropertyRecord (S.ValueBind (S.Bind _ (S.BindTargetNone _)))) = pure Nothing
parseGroupPropDecl1 (S.GenPropertyRecord val)
  = mkFail $ desugarError (getAnn val) "expected group property declaration, got (non-bind) value"
parseGroupPropDecl1 (S.GenPropertyGroup grp)
  = mkFail $ desugarError (getAnn grp) "expected group property declaration, got group"

parseEmptyGroupDef1 :: S.GroupDecl Range -> SessionRes (I.GroupDef Range)
parseEmptyGroupDef1 (S.GroupDecl rng (S.Group _ loc (S.Symbol headRng head') props))
  = case loc of
      S.GroupLocGlobal _ -> do
        (props', bindEnv)
          <- runStateT (traverseDropFatals parseGroupPropDecl1 props) I.emptyGVBindEnv
        let (vprops, gprops) = partitionTuple $ catMaybes props'
        pure I.GroupDef
          { I.groupDefAnn = rng
          , I.groupDefHead = head'
          , I.groupDefValueProps = vprops
          , I.groupDefGroupProps = gprops
          , I.groupDefReducers = []
          , I.groupDefPropEnv = bindEnv
          }
      S.GroupLocLocal _ -> mkFail $ desugarError headRng "can't declare a lowercase group, lowercase groups are group properties"
      S.GroupLocFunction _ -> mkFail $ desugarError headRng "can't declare a function, functions are provided by libraries"

parseRestGroupDefs1 :: S.GroupDecl Range -> [S.TopLevel Range] -> SessionRes (N.NonEmpty (I.GroupDef Range))
parseRestGroupDefs1 decl [] = (N.:| []) <$> parseEmptyGroupDef1 decl
parseRestGroupDefs1 decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs1 decl xs
parseRestGroupDefs1 decl (S.TopLevelReducer red : xs) = do
  I.GroupDef yRng yHead yValueProps yGroupProps yReds yBindEnv N.:| ys <- parseRestGroupDefs1 decl xs
  (redType, red') <- parseReducer1 yBindEnv red
  let yRng' = getAnn red <> yRng
  yReds' <-
    case redType of
      S.ReducerTypeReg _ -> pure $ red' : yReds
      S.ReducerTypeCast redTypeAnn -> do
        tellError $ desugarError redTypeAnn $ "cast reducer must be before all groups"
        pure yReds -- Discard the cast reducer
  pure $ I.GroupDef yRng' yHead yValueProps yGroupProps yReds' yBindEnv N.:| ys
parseRestGroupDefs1 decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef1 decl <*> parseRestGroupDefs1 decl' xs

parseAllGroupDefs1 :: [S.TopLevel Range] -> SessionRes [I.GroupDef Range]
parseAllGroupDefs1 [] = pure []
parseAllGroupDefs1 (S.TopLevelGroupDecl x : xs)
  = N.toList <$> parseRestGroupDefs1 x xs
parseAllGroupDefs1 (_ : _) = error "expected group declaration when parsing group definitions"

parseGroupRef2 :: I.GroupRef Range -> I.GroupSessionRes (GroupRef Range)
parseGroupRef2 (I.GroupRef rng loc vprops gprops) = do
  groupEnv <- ask
  loc' <-
    case loc of
      I.GroupLocGlobal (S.Symbol headRng headSym) ->
        case groupEnv M.!? headSym of
          Nothing -> mkFail $ desugarError headRng $ "undefined group: " <> headSym
          Just idx -> pure $ GroupLocGlobal headRng idx
      I.GroupLocLocal (Bind headRng idx) -> pure $ GroupLocLocal headRng idx
      I.GroupLocFunction (S.Symbol headRng headSym) -> pure $ GroupLocFunction headRng headSym
  gprops' <- traverse parseGroupRef2 gprops
  pure GroupRef
    { groupRefAnn = rng
    , groupRefLoc = loc'
    , groupRefValueProps = vprops
    , groupRefGroupProps = gprops'
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
parseGroupDef2 (I.GroupDef rng _ vprops gprops reds _)
    = GroupDef rng (map (\(_, bind) -> bind) vprops) (map (\(_, bind) -> bind) gprops)
  <$> traverseDropFatals parseReducer2 reds

parseGroupDefEnv :: I.GroupDef Range -> Int -> (T.Text, Int)
parseGroupDefEnv group idx = (I.groupDefHead group, idx)

notGroupedReducerError :: Reducer Range -> Error
notGroupedReducerError red
  = desugarError (getAnn red) "regular reducer must be in group"

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: S.Program Range -> SessionRes (Program Range)
parseRaw (S.Program rng topLevels) = do
  let (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl _ -> True; _ -> False) topLevels
      decls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevelsOutGroups
  decls' <- traverse parseRecordDecl decls
  reds1 <- traverseDropFatals (parseReducer1 I.emptyGVBindEnv) reds
  groups1 <- parseAllGroupDefs1 topLevelsInGroups
  let groupEnv = M.fromList $ zipWith parseGroupDefEnv groups1 [0..]
  reds' <- runReaderT (traverseDropFatals (\(typ, red) -> (typ, ) <$> parseReducer2 red) reds1) groupEnv
  groups' <- runReaderT (traverseDropFatals parseGroupDef2 groups1) groupEnv
  let regReds = map snd $ filter ((== S.ReducerTypeReg ()) . remAnns . fst) reds'
      castReds = map snd $ filter ((== S.ReducerTypeCast ()) . remAnns . fst) reds'
  -- TODO Syntax sugar a main group
  tellErrors $ map notGroupedReducerError regReds
  pure Program
    { programAnn = rng
    , programCastReducers = castReds
    , programRecordDecls = decls'
    , programGroups = groups'
    }

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result.
parse :: S.Program Range -> SessionRes (Program Range)
parse = validate . parseRaw
