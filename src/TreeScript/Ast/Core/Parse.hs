{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module TreeScript.Ast.Core.Parse
  ( parse1
  , parse
  ) where

import TreeScript.Ast.Core.Analyze
import TreeScript.Ast.Core.Validate
import TreeScript.Ast.Core.Env
import TreeScript.Ast.Core.Serialize
import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Flat as F
import qualified TreeScript.Ast.Sugar as S
import qualified TreeScript.Ast.Lex as L
import TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text as T
import TreeScript.Plugin

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import Data.Either
import Data.Foldable
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Directory
import System.FilePath

-- = Parsing from AST data

parseAstSymbol :: T.Text -> Symbol (Maybe an)
parseAstSymbol txt
  = Symbol
  { symbolAnn = Nothing
  , symbolModule = path
  , symbol = lcl
  }
  where (path_, lcl) = T.breakOnEnd "_" txt
        path = T.dropEnd 1 path_

decodeAstData :: Range -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
decodeAstData rng txt splices = traverse decodeAstData1 =<< ResultT (pure $ F.parse txt)
  where
    splicesVec = V.fromList splices
    decodeAstData1 lexs = do
      (res, rest) <- valueParser lexs
      unless (null rest) $
        mkFail $ desugarError rng "unexpected extra nodes after decoded value"
      pure res
    valueParser [] = mkFail $ desugarError rng "tried to decode value from no nodes"
    valueParser (F.LexemeEnterSplice idx : rest)
      = case splicesVec V.!? idx of
          Nothing -> mkFail $ desugarError rng $ "invalid splice index: " <> pprint idx
          Just splice -> pure (Just <$> splice, rest)
    valueParser (F.LexemePrimitive prim : rest) = pure (ValuePrimitive $ primParser prim, rest)
    valueParser (F.LexemeRecordHead head' numProps : rest) = first ValueRecord <$> recordParser head' numProps rest
    primParser (F.PrimInteger value) = PrimInteger Nothing value
    primParser (F.PrimFloat value) = PrimFloat Nothing value
    primParser (F.PrimString value) = PrimString Nothing value
    recordParser qualHead numProps rest = do
      (props, rest') <- propsParser numProps rest
      let head' = parseAstSymbol qualHead
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


runLanguageParser :: Range -> Language -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
runLanguageParser rng lang txt splices = do
  let parser = languageParser lang
  astData <- overErrors (addRangeToErr rng . prependMsgToErr "couldn't parse code") $ runCmdProgram parser txt
  decodeAstData rng astData splices

-- = Parsing from Sugar

invalidTxt :: T.Text
invalidTxt = "???"

-- | Gets the path of the module given the root path.
moduleFilePath :: FilePath -> ModulePath -> FilePath
moduleFilePath mroot mpath = mroot </> lpath
  where lpath = T.unpack $ T.replace "_" "/" mpath

subModule :: ModulePath -> T.Text -> ModulePath
subModule mpath subName
  | mpath == "" = subName
  | otherwise = mpath <> "_" <> subName

parseImportQual :: Maybe (S.Symbol Range) -> T.Text
parseImportQual Nothing = ""
parseImportQual (Just (S.Symbol _ qual)) = qual

getProgModule :: Range -> ModulePath -> FilePath -> ImportSessionRes (Program () () ())
getProgModule rng mpath path = do
  -- SOON: Replace program's path with mpath
  let msgPrefix = "couldn't import program: " <> T.pack path <> " - "
      liftProgIO
        = overErrors (addRangeToErr rng . prependMsgToErr msgPrefix)
        . liftIOAndCatch StageDesugar
  pmod <- liftProgIO $ fmap deserialize $ B.readFile path
  case pmod of
    Nothing -> mkFail $ desugarError rng $ msgPrefix <> "bad format"
    Just pmod' -> pure pmod'

getScriptModule :: Range -> FilePath -> ModulePath -> FilePath -> ImportSessionRes (Program () () ())
getScriptModule rng mroot mpath path = do
  res <- lift $ lift $ runResultT $ parseLocal mroot mpath path
  case res of
    ResultFail err ->
      mkFail $ desugarError rng $ "error in script: " <> T.pack path <> "\n" <> pprint err
    Result errs x -> do
      unless (null errs) $
        tellError $ desugarError rng $ "errors in script: " <> T.pack path <> "\n" <> T.unlines (map (T.bullet . pprint) errs)
      pure x

getDirModules :: Range -> FilePath -> ModulePath -> FilePath -> ImportSessionRes [Program () () ()]
getDirModules rng mroot mpath path = do
  let liftModIO
        = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't get directory contents: " <> T.pack path))
        . liftIOAndCatch StageDesugar
  subNames <- liftModIO $ listDirectory path
  subs <- traverseDropFatals (tryGetModule rng mroot . subModule mpath . T.pack . dropExtension) subNames
  pure $ concat $ rights subs

-- | If the path doesn't refer to a module, will return the error instead of failing.
-- Still fails if the path refers to a bad module.
tryGetModule :: Range -> FilePath -> ModulePath -> ImportSessionRes (Either Error [Program () () ()])
tryGetModule rng mroot mpath = do
  ienv <- getImportEnv
  let impaths = importEnvImportedModules ienv
      myPath = importEnvModulePath ienv
  if S.member mpath impaths then
    pure $ Left $ desugarError rng $ "module " <> mpath <> " already imported"
  else if myPath == mpath then
    pure $ Left $ desugarError rng $ "module " <> mpath <> " recursively imports itself"
  else do
    let liftModIO
          = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't check " <> T.pack path))
          . liftIOAndCatch StageDesugar
        path = moduleFilePath mroot mpath
        scriptPath = path <> ".tscr"
        progPath = path <> ".tprg"
    pathFileExists <- liftModIO $ doesFileExist path
    dirExists <- liftModIO $ doesDirectoryExist path
    progExists <- liftModIO $ doesFileExist progPath
    sourceExists <- liftModIO $ doesFileExist scriptPath
    when (dirExists && (progExists || sourceExists)) $
      tellError $ desugarError rng $ "both a directory and file exist for this module"
    if progExists then do
      Right . pure <$> getProgModule rng mpath progPath
    else if sourceExists then do
      Right . pure <$> getScriptModule rng mroot mpath scriptPath
    else if dirExists then do
      Right <$> getDirModules rng mroot mpath path
    else if pathFileExists then
      pure $ Left $ desugarError rng $ "no module exists at " <> T.pack path <> " (a raw file exists though - when importing, drop the extension)"
    else
      pure $ Left $ desugarError rng $ "no module exists at " <> T.pack path

tryImportModulesAtPath :: Range -> FilePath -> ModulePath -> T.Text -> ImportSessionRes (Maybe Error)
tryImportModulesAtPath rng mroot mpath qual = do
  imods <- tryGetModule rng mroot mpath
  case imods of
    Left err -> pure $ Just err
    Right imods' -> do
      mapM_ (addImportedModule rng qual) imods'
      pure Nothing

tryImportModules :: Range -> ModulePath -> T.Text -> ImportSessionRes [Error]
tryImportModules rng mpath qual = do
  root <- importEnvRoot <$> getImportEnv
  builtin <- sessionEnvBuiltinModsPath <$> lift getSessionEnv
  res1 <- tryImportModulesAtPath rng root mpath qual
  res2 <- tryImportModulesAtPath rng builtin mpath qual
  case (res1, res2) of
    (Just e1, Just e2) -> pure [e1, e2]
    _ -> pure []

importModules :: Range -> ModulePath -> T.Text -> ImportSessionRes ()
importModules rng mpath qual = do
  res <- tryImportModules rng mpath qual
  case res of
    [] -> pure ()
    err : _ -> mkFail err

parseImportDecl :: S.ImportDecl Range -> ImportSessionRes ()
parseImportDecl (S.ImportDecl _ (S.Symbol litRng lit) (S.Symbol modRange mdl) qual) = do
  unless (lit == "import") $
    tellError $ desugarError litRng "expected \"import\""
  let qual' = parseImportQual qual
  importModules modRange mdl qual'

parseSymbol :: SymbolType -> S.Symbol Range -> ImportSessionRes (Symbol Range)
parseSymbol typ (S.Symbol ann txt) = do
  let (qual_, lcl) = T.breakOnEnd "_" txt
      qual = T.dropEnd 1 qual_
  _ <- tryImportModules ann qual qual
  imps <- getImportEnv
  let cands = fold $ importEnvAllDecls imps M.!? qual
      paths = map fst $ filter (declSetContains typ lcl . snd) cands
  path <-
    case paths of
      [] -> do
        tellError $ desugarError ann $ "unknown (not local or imported): " <> txt
        pure invalidTxt
      [p] -> pure p
      ps -> do
        tellError $ desugarError ann $ T.unlines $ "ambiguous, specific modules this qualifier resolves to (some might have the same name):" : ps
        pure $ head ps
  pure Symbol
    { symbolAnn = ann
    , symbolModule = path
    , symbol = lcl
    }

parseValPropDecl :: S.GenProperty Range -> SessionRes T.Text
parseValPropDecl (S.GenPropertyDecl (S.Symbol _ decl)) = pure decl
parseValPropDecl (S.GenPropertySubGroup prop) = do
  tellError $ desugarError (getAnn prop) "expected lowercase symbol, got group property declaration"
  pure invalidTxt
parseValPropDecl (S.GenPropertyRecord val) = do
  tellError $ desugarError (getAnn val) "expected lowercase symbol, got value"
  pure invalidTxt
parseValPropDecl (S.GenPropertyGroup grp) = do
  tellError $ desugarError (getAnn grp) "expected value, got group"
  pure invalidTxt

parseRecordDecl :: S.RecordDecl Range -> SessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ (S.Symbol _ head') props))
  = RecordDecl rng head' <$> traverse parseValPropDecl props

parseGroupDecl :: S.GroupDecl Range -> SessionRes (Either GroupDeclCompact FunctionDeclCompact)
parseGroupDecl (S.GroupDecl _ (S.Group _ loc (S.Symbol hrng head') props))
  = case loc of
      S.GroupLocGlobal _ -> pure $ Left $ GroupDeclCompact head' nvps ngps
      S.GroupLocFunction _ -> pure $ Right $ FunctionDeclCompact head' nvps -- TODO: Types and checking more stuff
      S.GroupLocLocal lrng -> mkFail $ desugarError (lrng <> hrng) "can't declare local (lowercase) group"
  where nvps = length $ filter (\case S.GenPropertyRecord (S.ValueBind _) -> True; _ -> False) props
        ngps = length $ filter (\case S.GenPropertySubGroup _ -> True; _ -> False) props

parsePrim :: S.Primitive Range -> GVBindSessionRes (Primitive Range)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng string

parseValueProperty :: S.GenProperty Range -> GVBindSessionRes (Value Range)
parseValueProperty (S.GenPropertyDecl key)
  = mkFail $ desugarError (getAnn key) "expected value, got lowercase symbol"
parseValueProperty (S.GenPropertySubGroup prop)
  = mkFail $ desugarError (getAnn prop) "expected value, got group property declaration"
parseValueProperty (S.GenPropertyRecord val) = parseValue val
parseValueProperty (S.GenPropertyGroup grp)
  = mkFail $ desugarError (getAnn grp) "expected value, got group"

parseRecord :: S.Record Range -> GVBindSessionRes (Record Range)
parseRecord (S.Record rng head' props)
    = Record rng
  <$> lift (parseSymbol SymbolTypeRecord head')
  <*> traverseDropFatals parseValueProperty props

parseBind :: S.Bind Range -> GVBindSessionRes (Bind Range)
parseBind (S.Bind rng tgt)
  = case tgt of
      S.BindTargetNone _ -> pure $ Bind rng 0
      S.BindTargetSome (S.Symbol _ txt) -> do
        env <- get
        let (idx, newEnv) = bindEnvLookup txt $ gvEnvValue env
        put env{ gvEnvValue = newEnv }
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

parseSpliceCode :: S.SpliceCode Range -> GVBindSessionRes (Value Range)
parseSpliceCode (S.SpliceCode rng (S.Symbol langExtRng langExt) spliceText) = do
  let txt = flattenSpliceText spliceText
      unparsedSplices = spliceTextSplices spliceText
  splices <- traverse parseSplice unparsedSplices
  lang <- overErrors (addRangeToErr langExtRng) $ lift $ lift $ langWithExt langExt
  case lang of
    Nothing -> mkFail $ desugarError langExtRng $ "unknown language with extension: " <> langExt
    Just lang' -> do
      ress <- lift $ lift $ runLanguageParser rng lang' txt splices
      case ress of
        [res] -> pure $ (rng `fromMaybe`) <$> res
        _ -> mkFail $ desugarError rng $ "expected 1 value, got " <> pprint (length ress)

parseSplice :: S.Splice Range -> GVBindSessionRes (Value Range)
parseSplice (S.SpliceBind targ) = ValueBind <$> parseBind bind
  where bind = S.Bind (getAnn targ) targ
parseSplice (S.SpliceHole (S.HoleIdx ann idx)) = pure $ hole ann ann idx

parseValue :: S.Value Range -> GVBindSessionRes (Value Range)
parseValue (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue (S.ValueRecord record) = ValueRecord <$> parseRecord record
parseValue (S.ValueBind bind) = ValueBind <$> parseBind bind
parseValue (S.ValueSpliceCode code) = parseSpliceCode code
parseValue (S.ValueHole (S.Hole ann (S.HoleIdx idxAnn idx))) = pure $ hole ann idxAnn idx

parseGroupHead :: S.GroupLoc Range -> S.Symbol Range -> GVBindSessionRes (GroupLoc Range)
parseGroupHead (S.GroupLocGlobal lrng) head'@(S.Symbol hrng _)
  = GroupLocGlobal (lrng <> hrng) <$> lift (parseSymbol SymbolTypeGroup head')
parseGroupHead (S.GroupLocLocal _) (S.Symbol rng txt) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvGroup env
  put env{ gvEnvGroup = newEnv }
  pure $ GroupLocLocal rng idx
parseGroupHead (S.GroupLocFunction lrng) head'@(S.Symbol hrng _)
  = GroupLocFunction (lrng <> hrng) <$> lift (parseSymbol SymbolTypeFunction head')

parseGroupProperty :: S.GenProperty Range -> GVBindSessionRes (Either (Value Range) (GroupRef Range))
parseGroupProperty (S.GenPropertyDecl key)
  = mkFail $ desugarError (getAnn key) "expected group, got lowercase symbol"
parseGroupProperty (S.GenPropertySubGroup prop)
  = mkFail $ desugarError (getAnn prop) "expected group, got group property declaration"
parseGroupProperty (S.GenPropertyRecord val)
  = Left <$> parseValue val
parseGroupProperty (S.GenPropertyGroup grp)
  = Right <$> parseGroupRef grp

parseGroupRef :: S.Group Range -> GVBindSessionRes (GroupRef Range)
parseGroupRef (S.Group rng isProp head' props) = do
  loc <- parseGroupHead isProp head'
  (vprops, gprops) <- partitionEithers <$> traverseDropFatals parseGroupProperty props
  pure $ GroupRef rng loc vprops gprops

parseGuard :: S.Guard Range -> GVBindSessionRes (Guard Range)
parseGuard (S.Guard rng input output nexts)
    = Guard rng
  <$> parseValue input
  <*> parseValue output
  <*> traverse parseGroupRef nexts

parseReducer :: GVBindEnv -> S.Reducer Range -> ImportSessionRes (Reducer Range)
parseReducer bindEnv (S.Reducer rng main guards)
    = evalStateT
    ( Reducer rng
  <$> parseGuard main
  <*> traverse parseGuard guards
    ) bindEnv

parseGroupPropDecl :: S.GenProperty Range -> GVBindSessionRes (Maybe (Side, (T.Text, Bind Range)))
parseGroupPropDecl (S.GenPropertyDecl prop)
  = mkFail $ desugarError (getAnn prop) "expected group property declaration, got lowercase symbol"
parseGroupPropDecl (S.GenPropertySubGroup (S.SubGroupProperty rng (S.Symbol _ txt))) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvGroup env
  put env{ gvEnvGroup = newEnv }
  pure $ Just (SideRight, (txt, Bind rng idx))
parseGroupPropDecl (S.GenPropertyRecord (S.ValueBind (S.Bind rng (S.BindTargetSome (S.Symbol _ txt))))) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvValue env
  put env{ gvEnvValue = newEnv }
  pure $ Just (SideLeft, (txt, Bind rng idx))
parseGroupPropDecl (S.GenPropertyRecord (S.ValueBind (S.Bind _ (S.BindTargetNone _)))) = pure Nothing
parseGroupPropDecl (S.GenPropertyRecord val)
  = mkFail $ desugarError (getAnn val) "expected group property declaration, got (non-bind) value"
parseGroupPropDecl (S.GenPropertyGroup grp)
  = mkFail $ desugarError (getAnn grp) "expected group property declaration, got group"

parseEmptyGroupDef :: S.GroupDecl Range -> ImportSessionRes (Symbol (), GroupDef T.Text GVBindEnv Range)
parseEmptyGroupDef (S.GroupDecl rng (S.Group _ loc (S.Symbol headRng lhead) props))
  = case loc of
      S.GroupLocGlobal _ -> do
        (props', bindEnv)
          <- runStateT (traverseDropFatals parseGroupPropDecl props) emptyGVBindEnv
        let (vprops, gprops) = partitionTuple $ catMaybes props'
        head' <- mkLocalSymbol () lhead
        pure (head', GroupDef
          { groupDefAnn = rng
          , groupDefValueProps = vprops
          , groupDefGroupProps = gprops
          , groupDefReducers = []
          , groupDefPropEnv = bindEnv
          })
      S.GroupLocLocal _ -> mkFail $ desugarError headRng "can't declare a lowercase group, lowercase groups are group properties"
      S.GroupLocFunction _ -> mkFail $ desugarError headRng "can't declare a function, functions are provided by libraries"

parseRestGroupDefs :: S.GroupDecl Range -> [S.TopLevel Range] -> ImportSessionRes (N.NonEmpty (Symbol (), GroupDef T.Text GVBindEnv Range))
parseRestGroupDefs decl [] = (N.:| []) <$> parseEmptyGroupDef decl
parseRestGroupDefs decl (S.TopLevelImportDecl _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelReducer red : xs) = do
  (yHead, GroupDef yRng yValueProps yGroupProps yReds yBindEnv) N.:| ys <- parseRestGroupDefs decl xs
  red' <- parseReducer yBindEnv red
  let yRng' = getAnn red <> yRng
      yReds' = red' : yReds
  pure $ (yHead, GroupDef yRng' yValueProps yGroupProps yReds' yBindEnv) N.:| ys
parseRestGroupDefs decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef decl <*> parseRestGroupDefs decl' xs

parseAllGroupDefs :: [S.TopLevel Range] -> ImportSessionRes (M.Map (Symbol ()) (GroupDef T.Text GVBindEnv Range))
parseAllGroupDefs [] = pure M.empty
parseAllGroupDefs (S.TopLevelGroupDecl x : xs)
  -- SOON: Catch duplicate errors
  = M.fromList . N.toList <$> parseRestGroupDefs x xs
parseAllGroupDefs (_ : _) = error "expected group declaration when parsing group definitions"

notGroupedReducerError :: Reducer Range -> Error
notGroupedReducerError red
  = desugarError (getAnn red) "reducer not in group"

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: FilePath -> ModulePath -> S.Program Range -> SessionRes ((Program T.Text GVBindEnv Range, ImportEnv), Program () () ())
parseRaw root mpath (S.Program rng topLevels) = do
  let (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl _ -> True; _ -> False) topLevels
      -- TODO: Verify order of declarations
      idecls = mapMaybe (\case S.TopLevelImportDecl x -> Just x; _ -> Nothing) topLevels
      rdecls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevelsOutGroups
      gdecls = mapMaybe (\case S.TopLevelGroupDecl x -> Just x; _ -> Nothing) topLevels
  rdecls' <- traverse parseRecordDecl rdecls
  (gdecls', fdecls) <- partitionEithers <$> traverse parseGroupDecl gdecls
  let exps = mkDeclSet (map compactRecordDecl rdecls') gdecls' fdecls
  runImportT root mpath exps $ do
    mapM_ parseImportDecl idecls
    reds' <- traverseDropFatals (parseReducer emptyGVBindEnv) reds
    groups <- parseAllGroupDefs topLevelsInGroups
    impEnv <- getImportEnv
    let idecls' = importEnvImportDecls impEnv
    -- TODO: Syntax sugar a main group
    tellErrors $ map notGroupedReducerError reds'
    pure (Program
      { programAnn = rng
      , programPath = importEnvModulePath impEnv
      , programImportDecls = idecls'
      , programRecordDecls = rdecls'
      , programExports = exps
      , programGroups = groups
      }, impEnv)

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result. Preserves extra so the program can be further analyzed.
parse1 :: FilePath -> ModulePath -> S.Program Range -> SessionRes (Program T.Text GVBindEnv Range, Program () () ())
parse1 root mpath = validate . parseRaw root mpath

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result. Strips extra and combines with imports.
parse1Local :: FilePath -> ModulePath -> S.Program Range -> SessionRes (Program () () ())
parse1Local root mpath prev = do
  (full, imods) <- parse1 root mpath prev
  pure $ remExtra full <> imods

-- | Compile a source into a @Program@. Strips extra.
parseLocal :: FilePath -> ModulePath -> FilePath -> SessionRes (Program () () ())
parseLocal root mpath
    = parse1Local root mpath
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile

-- | Compile a source into a @Program@. Strips extra.
parse :: FilePath -> SessionRes (Program () () ())
parse path = parseLocal root mpath path
  where (root, lpath) = splitFileName $ dropExtension path
        mpath = T.replace "/" "_" $ T.pack lpath
