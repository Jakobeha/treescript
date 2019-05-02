{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module TreeScript.Ast.Core.Parse
  ( parse1Raw
  , parse
  ) where

import TreeScript.Ast.Core.Analyze
import TreeScript.Ast.Core.TypeCast
import TreeScript.Ast.Core.Classes
import TreeScript.Ast.Core.Types
import TreeScript.Ast.Core.Validate
import qualified TreeScript.Ast.Flat as F
import qualified TreeScript.Ast.Lex as L
import qualified TreeScript.Ast.Sugar as S
import TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text as T
import TreeScript.Plugin

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import Data.Char
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

parseAstSymbol :: Range -> T.Text -> PL Symbol
parseAstSymbol rng txt
  = Symbol
  { symbolAnn = rng
  , symbolModule = path
  , symbol = lcl
  }
  where (path_, lcl) = T.breakOnEnd "_" txt
        path = T.dropEnd 1 path_

decodeAstData :: Range -> T.Text -> [PL Value] -> SessionRes [PL Value]
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
          Just splice -> pure (splice, rest)
    valueParser (F.LexemePrimitive prim : rest) = pure (ValuePrimitive $ primParser prim, rest)
    valueParser (F.LexemeRecordHead head' numProps : rest) = first ValueRecord <$> recordParser head' numProps rest
    primParser (F.PrimInteger value) = PrimInteger rng () value
    primParser (F.PrimFloat value) = PrimFloat rng () value
    primParser (F.PrimString value) = PrimString rng () value
    recordParser qualHead numProps rest = do
      (props, rest') <- propsParser numProps rest
      let head' = parseAstSymbol rng qualHead
      pure
        ( Record
          { recordAnn = rng
          , recordType = ()
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


runLanguageParser :: Range -> Language -> T.Text -> [PL Value] -> SessionRes [PL Value]
runLanguageParser rng lang txt splices = do
  let parser = languageParser lang
  astData <- overErrors (addRangeToErr rng . prependMsgToErr "couldn't parse code") $ runCmdProgram parser txt
  decodeAstData rng astData splices

-- = Parsing from Sugar

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

findLibrary :: ImportSessionRes (Maybe Library)
findLibrary = do
  genv <- getGlobalEnv
  let root = globalEnvRoot genv
      mpath = globalEnvModulePath genv
      path = moduleFilePath root mpath
      jsPath = path <.> "js"
      liftLibIO
        = overErrors (prependMsgToErr ("couldn't get library for " <> T.pack path))
        . liftIOAndCatch StageDesugar
  cmdBinExists <- liftLibIO $ doesFileExist path
  jsExists <- liftLibIO $ doesFileExist jsPath
  when (cmdBinExists && jsExists) $
    tellError $ desugarError_ "both a command-line binary and javascript library exist for this program"
  if cmdBinExists then
    Just . LibraryCmdBinary <$> liftLibIO (B.readFile path)
  else if jsExists then
    Just . LibraryJavaScript <$> liftLibIO (T.readFile jsPath)
  else
    pure Nothing

findLibraries :: ImportSessionRes (M.Map ModulePath Library)
findLibraries = do
  lib <- findLibrary
  case lib of
    Nothing -> pure M.empty
    Just lib' -> do
      mpath <- globalEnvModulePath <$> getGlobalEnv
      pure $ M.singleton mpath lib'

getProgModule :: Range -> ModulePath -> FilePath -> ImportSessionRes (PF Program)
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

getScriptModule :: Range -> FilePath -> ModulePath -> FilePath -> ImportSessionRes (PF Program)
getScriptModule rng mroot mpath path = do
  res <- lift $ lift $ runResultT $ parseAt mroot mpath path
  case res of
    ResultFail err ->
      mkFail $ desugarError rng $ "error in script: " <> T.pack path <> "\n" <> pprint err
    Result errs x -> do
      unless (null errs) $
        tellError $ desugarError rng $ "errors in script: " <> T.pack path <> "\n" <> T.unlines (map (T.bullet . pprint) errs)
      pure x

getDirModules :: Range -> FilePath -> ModulePath -> FilePath -> ImportSessionRes [PF Program]
getDirModules rng mroot mpath path = do
  let liftModIO
        = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't get directory contents: " <> T.pack path))
        . liftIOAndCatch StageDesugar
  subNames <- liftModIO $ listDirectory path
  subs <- traverseDropFatals (tryGetModule rng mroot . subModule mpath . T.pack . dropExtension) subNames
  pure $ concat $ rights subs

-- | If the path doesn't refer to a module, will return the error instead of failing.
-- Still fails if the path refers to a bad module.
tryGetModule :: Range -> FilePath -> ModulePath -> ImportSessionRes (Either Error [PF Program])
tryGetModule rng mroot mpath = do
  genv <- getGlobalEnv
  let impaths = globalEnvImportedModules genv
      myPath = globalEnvModulePath genv
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
    if progExists then
      Right . pure <$> getProgModule rng mpath progPath
    else if sourceExists then
      Right . pure <$> getScriptModule rng mroot mpath scriptPath
    else if dirExists then
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
  root <- globalEnvRoot <$> getGlobalEnv
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

parseLookupSymbol :: SymbolType a -> S.Symbol Range -> ImportSessionRes (Symbol a1 a2 a3 a4 a5 a6 t Range, Maybe a)
parseLookupSymbol typ (S.Symbol ann txt) = do
  let (qual_, lcl) = T.breakOnEnd "_" txt
      qual = T.dropEnd 1 qual_
  _ <- tryImportModules ann qual qual
  imps <- getGlobalEnv
  let ocands = fold $ globalEnvAllDecls imps M.!? qual
      ress = mapMaybe (\(pth, exps) -> (Symbol ann pth lcl, ) . Just <$> declSetLookup typ lcl exps) ocands
  case ress of
    [] -> pure (Symbol ann qual lcl, Nothing)
    [res] -> pure res
    rs -> do
      tellError $ desugarError ann $ T.unlines $ "ambiguous, specific modules this qualifier resolves to (some might have the same name):" : map (symbol . fst) rs
      pure $ head rs

tryLookupSymbol :: SymbolType a -> S.Symbol Range -> ImportSessionRes (Maybe a)
tryLookupSymbol typ = fmap snd . parseLookupSymbol typ

parseSymbol :: SymbolType a -> S.Symbol Range -> ImportSessionRes (Symbol a1 a2 a3 a4 a5 a6 t Range)
parseSymbol typ sym@(S.Symbol ann txt) = do
  (res, x) <- parseLookupSymbol typ sym
  when (isNothing x) $
    tellError $ desugarError ann $ "unknown (not local or imported): " <> txt
  pure res

parsePrimType :: S.Symbol Range -> ImportSessionRes (Maybe PrimType)
parsePrimType (S.Symbol _ txt)
  | txt == "any" = pure $ Just PrimTypeAny
  | txt == "int" = pure $ Just PrimTypeInteger
  | txt == "float" = pure $ Just PrimTypeFloat
  | txt == "string" = pure $ Just PrimTypeString
  | otherwise = pure Nothing

parseRecordDeclSkipProps :: S.RecordDecl Range -> ImportSessionRes (RecordDecl Range)
parseRecordDeclSkipProps (S.RecordDecl rng (S.Record _ (S.Symbol _ head') _))
  = pure $ RecordDecl rng head' [undefined]

parseAliasType :: S.Symbol Range -> ImportSessionRes (Maybe (PFA Type Range))
parseAliasType ali
    = fmap (getAnn ali <$)
  <$> tryLookupSymbol SymbolTypeAlias ali

parseTypePart :: S.TypePart Range -> ImportSessionRes (PFA Type Range)
parseTypePart (S.TypePartSymbol rng sym)
  | loc /= "" && isLower (T.head loc) = do
    prmPart <- parsePrimType sym
    aliPart <- parseAliasType sym
    case (prmPart, aliPart) of
      (Just prm, Just ali) -> error $ T.unpack $ "type alias defined for prim: " <> pprint prm <> " -> " <> pprint ali
      (Just prm, Nothing) -> pure $ mkSType $ TypePartPrim rng prm
      (Nothing, Just ali) -> pure ali
      (Nothing, Nothing) -> do
        tellError $ desugarError rng $ "unknown type (not an alias or prim): " <> pprint sym
        pure $ anyType rng
  | otherwise = mkSType . TypePartRecord rng <$> parseSymbol SymbolTypeRecord sym
  where loc = T.takeWhileEnd (/= '_') $ S.symbol sym
parseTypePart (S.TypePartTransparent rng (S.Record _ (S.Symbol headRng head') props))
  | head' == "t"
  = mkSType . TypePartTuple rng <$> traverse parseTypeProp props
  | head' == "list"
  = case props of
      [prop] -> mkSType . TypePartList rng <$> parseTypeProp prop
      _ -> do
        tellError $ desugarError rng $ "list type must have 1 property, got " <> pprint (length props)
        pure $ anyType rng
  | otherwise = do
    tellError $ desugarError headRng $ "unknown transparent record type: " <> head'
    pure $ anyType rng

parseType :: S.Type Range -> ImportSessionRes (PFA Type Range)
parseType (S.Type rng parts) = Type rng . concatMap typeParts <$> traverse parseTypePart parts

parseTypeProp :: S.GenProperty Range -> ImportSessionRes (PFA Type Range)
parseTypeProp (S.GenPropertyDecl typ) = parseType typ
parseTypeProp (S.GenPropertySubGroup prop) = do
  let rng = getAnn prop
  tellError $ desugarError rng "expected type, got group property declaration"
  pure $ anyType rng
parseTypeProp (S.GenPropertyRecord val) = do
  let rng = getAnn val
  tellError $ desugarError rng "expected type, got value"
  pure $ anyType rng
parseTypeProp (S.GenPropertyGroup grp) = do
  let rng = getAnn grp
  tellError $ desugarError rng "expected value, got group"
  pure $ anyType rng

parseRecordDecl :: S.RecordDecl Range -> ImportSessionRes (RecordDecl Range)
parseRecordDecl (S.RecordDecl rng (S.Record _ (S.Symbol _ head') props))
  = RecordDecl rng head' <$> traverse parseTypeProp props

parseGroupDecl :: S.GroupDecl Range -> ImportSessionRes (Either GroupDecl (FunctionDecl Range))
parseGroupDecl (S.GroupDecl rng (S.Group _ loc (S.Symbol hrng head') props) funRet)
  = case loc of
      S.GroupLocGlobal _ -> do
        case funRet of
          Nothing -> pure ()
          Just f ->
            tellError $ desugarError (getAnn f) "group can't have an explicit return type"
        pure $ Left $ GroupDecl head' nvps ngps -- TODO Error on other properties
      S.GroupLocFunction _
         -> fmap Right
          $ FunctionDecl rng head'
        <$> traverse parseTypeProp props
        <*> funRet' -- TODO output type
        where funRet' =
                case funRet of
                  Nothing -> do
                    tellError $ desugarError rng "function needs an explicit return type"
                    pure $ anyType rng
                  Just f -> parseType f
      S.GroupLocLocal lrng -> mkFail $ desugarError (lrng <> hrng) "can't declare local (lowercase) group"
  where nvps = length $ filter (\case S.GenPropertyRecord (S.ValueBind _) -> True; _ -> False) props
        ngps = length $ filter (\case S.GenPropertySubGroup _ -> True; _ -> False) props

parseTypeAlias :: S.TypeAlias Range -> ImportSessionRes (TypeAlias Range)
parseTypeAlias (S.TypeAlias ann (S.Symbol _ ali) typ) = TypeAlias ann ali <$> parseType typ

parsePrim :: S.Primitive Range -> GVBindSessionRes (PL Primitive)
parsePrim (S.PrimInteger rng int)
  = pure $ PrimInteger rng () int
parsePrim (S.PrimFloat rng float)
  = pure $ PrimFloat rng () float
parsePrim (S.PrimString rng string)
  = pure $ PrimString rng () string

parseValueProperty :: S.GenProperty Range -> GVBindSessionRes (PL Value)
parseValueProperty (S.GenPropertyDecl key)
  = mkFail $ desugarError (getAnn key) "expected value, got type"
parseValueProperty (S.GenPropertySubGroup prop)
  = mkFail $ desugarError (getAnn prop) "expected value, got group property declaration"
parseValueProperty (S.GenPropertyRecord val) = parseValue val
parseValueProperty (S.GenPropertyGroup grp)
  = mkFail $ desugarError (getAnn grp) "expected value, got group"

parseRecord :: S.Record Range -> GVBindSessionRes (PL Record)
parseRecord (S.Record rng head' props)
    = Record rng ()
  <$> lift (parseSymbol SymbolTypeRecord head')
  <*> traverseDropFatals parseValueProperty props

parseBind :: S.Bind Range -> GVBindSessionRes (PL Bind)
parseBind (S.Bind rng tgt)
  = case tgt of
      S.BindTargetNone _ -> pure $ Bind rng () 0
      S.BindTargetSome (S.Symbol _ txt) -> do
        env <- get
        let (idx, newEnv) = bindEnvLookup txt $ gvEnvValue env
        put env{ gvEnvValue = newEnv }
        pure $ Bind rng () idx

flattenSpliceText :: S.SpliceText Range -> T.Text
flattenSpliceText = flattenRest 0
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

parseSpliceCode :: S.SpliceCode Range -> GVBindSessionRes (PL Value)
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
        [res] -> pure $ res
        _ -> mkFail $ desugarError rng $ "expected 1 value, got " <> pprint (length ress)

parseSplice :: S.Splice Range -> GVBindSessionRes (PL Value)
parseSplice (S.SpliceBind targ) = ValueBind <$> parseBind bind
  where bind = S.Bind (getAnn targ) targ
parseSplice (S.SpliceHole (S.HoleIdx rng idx)) = pure $ hole rng rng idx

parseValue :: S.Value Range -> GVBindSessionRes (PL Value)
parseValue (S.ValuePrimitive prim) = ValuePrimitive <$> parsePrim prim
parseValue (S.ValueRecord record) = ValueRecord <$> parseRecord record
parseValue (S.ValueBind bind) = ValueBind <$> parseBind bind
parseValue (S.ValueSpliceCode code) = parseSpliceCode code
parseValue (S.ValueHole (S.Hole rng (S.HoleIdx idxAnn idx))) = pure $ hole rng idxAnn idx

parseGroupHead :: S.GroupLoc Range -> S.Symbol Range -> GVBindSessionRes (PL GroupLoc)
parseGroupHead (S.GroupLocGlobal lrng) head'@(S.Symbol hrng _)
  = GroupLocGlobal (lrng <> hrng) <$> lift (parseSymbol SymbolTypeGroup head')
parseGroupHead (S.GroupLocLocal _) (S.Symbol rng txt) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvGroup env
  put env{ gvEnvGroup = newEnv }
  pure $ GroupLocLocal rng idx
parseGroupHead (S.GroupLocFunction lrng) head'@(S.Symbol hrng _)
  = GroupLocFunction (lrng <> hrng) <$> lift (parseSymbol SymbolTypeFunction head')

parseGroupProperty :: S.GenProperty Range -> GVBindSessionRes (Either (PL Value) (PL GroupRef))
parseGroupProperty (S.GenPropertyDecl key)
  = mkFail $ desugarError (getAnn key) "expected group, got type"
parseGroupProperty (S.GenPropertySubGroup prop)
  = mkFail $ desugarError (getAnn prop) "expected group, got group property declaration"
parseGroupProperty (S.GenPropertyRecord val)
  = Left <$> parseValue val
parseGroupProperty (S.GenPropertyGroup grp)
  = Right <$> parseGroupRef grp

parseGroupRef :: S.Group Range -> GVBindSessionRes (PL GroupRef)
parseGroupRef (S.Group rng isProp head' props) = do
  loc <- parseGroupHead isProp head'
  (vprops, gprops) <- partitionEithers <$> traverseDropFatals parseGroupProperty props
  pure $ GroupRef rng loc vprops gprops

parseGuard :: S.Guard Range -> GVBindSessionRes (PL Guard)
parseGuard (S.Guard rng input output nexts)
    = Guard rng
  <$> parseValue input
  <*> parseValue output
  <*> traverse parseGroupRef nexts

parseReducer :: GVBindEnv -> S.Reducer Range -> ImportSessionRes (S.ReducerType Range, PL Reducer)
parseReducer bindEnv (S.Reducer rng typ main guards)
    = (typ, ) <$> evalStateT
    ( Reducer rng
  <$> parseGuard main
  <*> traverse parseGuard guards
    ) bindEnv

parseGroupPropDecl :: S.GenProperty Range -> GVBindSessionRes (Maybe (Side, (T.Text, Int)))
parseGroupPropDecl (S.GenPropertyDecl prop)
  = mkFail $ desugarError (getAnn prop) "expected group property declaration, got type"
parseGroupPropDecl (S.GenPropertySubGroup (S.SubGroupProperty _ (S.Symbol _ txt))) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvGroup env
  put env{ gvEnvGroup = newEnv }
  pure $ Just (SideRight, (txt, idx))
parseGroupPropDecl (S.GenPropertyRecord (S.ValueBind (S.Bind _ (S.BindTargetSome (S.Symbol _ txt))))) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvValue env
  put env{ gvEnvValue = newEnv }
  pure $ Just (SideLeft, (txt, idx))
parseGroupPropDecl (S.GenPropertyRecord (S.ValueBind (S.Bind _ (S.BindTargetNone _)))) = pure Nothing
parseGroupPropDecl (S.GenPropertyRecord val)
  = mkFail $ desugarError (getAnn val) "expected group property declaration, got (non-bind) value"
parseGroupPropDecl (S.GenPropertyGroup grp)
  = mkFail $ desugarError (getAnn grp) "expected group property declaration, got group"

parseEmptyGroupDef :: S.GroupDecl Range -> ImportSessionRes (PF Symbol, PL GroupDef)
parseEmptyGroupDef (S.GroupDecl rng (S.Group _ loc (S.Symbol headRng lhead) props) _) =
  case loc of
    S.GroupLocGlobal _ -> do
      (props', bindEnv)
        <- runStateT (traverseDropFatals parseGroupPropDecl props) emptyGVBindEnv
      let (vprops, gprops) = partitionTuple $ catMaybes props'
      head' <- mkLocalSymbol lhead
      pure (head', GroupDef
        { groupDefAnn = rng
        , groupDefValueProps = vprops
        , groupDefGroupProps = gprops
        , groupDefReducers = []
        , groupDefPropEnv = bindEnv
        })
    S.GroupLocLocal _ -> mkFail $ desugarError headRng "can't declare a lowercase group, lowercase groups are group properties"
    S.GroupLocFunction _ -> mkFail $ desugarError headRng "can't declare a function after declaring groups"

parseRestGroupDefs :: S.GroupDecl Range -> [S.TopLevel Range] -> ImportSessionRes (N.NonEmpty (PF Symbol, PL GroupDef))
parseRestGroupDefs decl [] = (N.:| []) <$> parseEmptyGroupDef decl
parseRestGroupDefs decl (S.TopLevelImportDecl _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelTypeAlias _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelReducer red : xs) = do
  (yHead, GroupDef yRng yValueProps yGroupProps yReds yBindEnv) N.:| ys <- parseRestGroupDefs decl xs
  (redType, red') <- parseReducer yBindEnv red
  let yRng' = getAnn red <> yRng
  yReds' <-
    case redType of
      S.ReducerTypeReg _ -> pure $ red' : yReds
      S.ReducerTypeCast redTypeAnn -> do
        tellError $ desugarError redTypeAnn $ "cast reducer must be before all groups"
        pure yReds -- Discard the cast reducer
  pure $ (yHead, GroupDef yRng' yValueProps yGroupProps yReds' yBindEnv) N.:| ys
parseRestGroupDefs decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef decl <*> parseRestGroupDefs decl' xs

parseAllGroupDefs :: [S.TopLevel Range] -> ImportSessionRes (M.Map (PF Symbol) (PL GroupDef))
parseAllGroupDefs [] = pure M.empty
parseAllGroupDefs (S.TopLevelGroupDecl x : xs)
  -- SOON: Catch duplicate errors
  = M.fromList . N.toList <$> parseRestGroupDefs x xs
parseAllGroupDefs (_ : _) = error "expected group declaration when parsing group definitions"

notGroupedReducerError :: PL Reducer -> Error
notGroupedReducerError red
  = desugarError (getAnn red) "regular reducer must be in group"

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseLocal :: FilePath -> ModulePath -> S.Program Range -> SessionRes ((PL Program, GlobalEnv), PF Program)
parseLocal root mpath (S.Program rng topLevels) = runGlobalT root mpath $ do
  let (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl (S.GroupDecl _ (S.Group _ (S.GroupLocGlobal _) _ _) _) -> True; _ -> False) topLevels
      -- TODO: Verify order of declarations
      idecls = mapMaybe (\case S.TopLevelImportDecl x -> Just x; _ -> Nothing) topLevels
      rdecls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      gdecls = mapMaybe (\case S.TopLevelGroupDecl x -> Just x; _ -> Nothing) topLevels
      alis = mapMaybe (\case S.TopLevelTypeAlias x -> Just x; _ -> Nothing) topLevels
      reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevelsOutGroups
  mapM_ parseImportDecl idecls
  rdecls0 <- traverse parseRecordDeclSkipProps rdecls
  putLocals $ mkDeclSet (map remAnns rdecls0) [] [] [] []
  alis' <- traverseDropFatals parseTypeAlias alis
  putLocals $ mkDeclSet (map remAnns rdecls0) [] [] [] (map remAnns alis')
  rdecls' <- traverse parseRecordDecl rdecls
  (gdecls', fdecls) <- partitionEithers <$> traverse parseGroupDecl gdecls
  let exps = mkDeclSet (map remAnns rdecls') [] gdecls' (map remAnns fdecls) (map remAnns alis')
  putLocals exps
  reds' <- traverseDropFatals (parseReducer emptyGVBindEnv) reds
  groups <- parseAllGroupDefs topLevelsInGroups
  libs <- findLibraries
  ienv <- getGlobalEnv
  let idecls' = globalEnvImportDecls ienv
      regReds = map snd $ filter ((== S.ReducerTypeReg ()) . remAnns . fst) reds'
      castReds = map snd $ filter ((== S.ReducerTypeCast ()) . remAnns . fst) reds'
  -- TODO: Syntax sugar a main group
  tellErrors $ map notGroupedReducerError regReds
  pure (Program
    { programAnn = rng
    , programPath = globalEnvModulePath ienv
    , programImportDecls = idecls'
    , programRecordDecls = rdecls'
    , programFunctionDecls = fdecls
    , programExports = exps
    , programTypeAliases = alis'
    , programCastReducers = castReds
    , programGroups = groups
    , programLibraries = libs
    }, ienv)

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result. Preserves extra so the program can be further analyzed.
parse1Raw :: FilePath -> ModulePath -> S.Program Range -> SessionRes (PR Program, PF Program)
parse1Raw root mpath = validate' <=< castCheckTypes' <=< parseLocal root mpath
  where castCheckTypes' ((prog, genv), imps) = (, imps) . (, genv) <$> castCheckTypes prog
        validate' ((x, imps), imods) = do
          tellErrors $ validate imps x
          pure (x, imods)
-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result. Strips extra and combines with imports.
parse1 :: FilePath -> ModulePath -> S.Program Range -> SessionRes (PF Program)
parse1 root mpath prev = do
  (full, imods) <- parse1Raw root mpath prev
  pure $ remExtra full <> imods

-- | Compile a source into a @Program@. Strips extra.
parseAt :: FilePath -> ModulePath -> FilePath -> SessionRes (PF Program)
parseAt root mpath
    = parse1 root mpath
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile

-- | Compile a source into a @Program@. Strips extra.
parse :: FilePath -> SessionRes (PF Program)
parse path = parseAt root mpath path
  where (root, lpath) = splitFileName $ dropExtension path
        mpath = T.replace "/" "_" $ T.pack lpath
