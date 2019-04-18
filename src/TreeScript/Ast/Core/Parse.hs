{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Extracts a 'Core' AST from a 'Sugar' AST.
module TreeScript.Ast.Core.Parse
  ( parseProg
  , parse
  , parseFromStart
  ) where

import TreeScript.Ast.Core.Analyze
import TreeScript.Ast.Core.Validate
import TreeScript.Ast.Core.Env
import TreeScript.Ast.Core.Types
import qualified TreeScript.Ast.Flat as F
import qualified TreeScript.Ast.Sugar as S
import qualified TreeScript.Ast.Lex as L
import TreeScript.Misc
import TreeScript.Plugin

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Either
import Data.Foldable
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Directory
import System.FilePath

-- = Parsing from AST data

decodeError :: T.Text -> Error
decodeError msg
  = Error
  { errorStage = StagePluginUse
  , errorRange = Nothing
  , errorMsg = msg
  }

decodeAstData :: ModulePath -> T.Text -> [Value an] -> SessionRes [Value (Maybe an)]
decodeAstData mpath txt splices = traverse decodeAstData1 =<< ResultT (pure $ F.parse txt)
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
    recordParser lhead numProps rest = do
      (props, rest') <- propsParser numProps rest
      let head'
            = Symbol
            { symbolAnn = Nothing
            , symbolModule = mpath
            , symbol = lhead
            }
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
      mpath = langSpecName $ languageSpec lang -- TODO: New languages/libraries
  astData <- overErrors (prependMsgToErr "couldn't parse code") $ runCmdProgram parser txt
  overErrors (prependMsgToErr "parsing plugin returned bad AST data") $ decodeAstData mpath astData splices

-- = Parsing from Sugar

invalidTxt :: T.Text
invalidTxt = "???"

parseImportQual :: Maybe (S.Symbol Range) -> T.Text
parseImportQual Nothing = ""
parseImportQual (Just (S.Symbol _ qual)) = qual <> "_"

getProgModule :: S.ModulePath Range -> SessionRes (Module () ())
getProgModule (S.ModulePath rng path) = do
  let msgPrefix = "couldn't import program: " <> path
      liftProgIO
        = overErrors (addRangeToErr rng . prependMsgToErr msgPrefix)
        . liftIOAndCatch StageDesugar
  pmod <- liftProgIO $ A.eitherDecodeFileStrict' $ T.unpack path
  case pmod of
    Left msg -> mkFail $ desugarError rng $ msgPrefix <> T.pack msg
    Right pmod' -> pure pmod'

getScriptModule :: S.ModulePath Range -> SessionRes (Module () ())
getScriptModule (S.ModulePath rng path)
    = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't import script: " <> path))
    $ parseFromStart (T.unpack path)

getDirModule :: S.ModulePath Range -> SessionRes (Module () ())
getDirModule (S.ModulePath rng path) = do
  let liftModIO
        = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't get directory contents: " <> path))
        . liftIOAndCatch StageDesugar
  subPaths <- liftModIO $ listDirectory $ T.unpack path
  subs <- traverseDropFatals tryGetModule $ map (S.ModulePath rng . T.pack) subPaths
  pure $ fold $ rights subs

-- | If the path doesn't refer to a module, will return the error instead of failing.
-- Still fails if the path refers to a bad module.
tryGetModule :: S.ModulePath Range -> SessionRes (Either Error (Module () ()))
tryGetModule (S.ModulePath rng path) = do
  let liftModIO
        = overErrors (addRangeToErr rng . prependMsgToErr ("couldn't check " <> path))
        . liftIOAndCatch StageDesugar
      scriptPath = path <> ".tscr"
      progPath = path <> ".tprg"
  pathFileExists <- liftModIO $ doesFileExist $ T.unpack path
  dirExists <- liftModIO $ doesDirectoryExist $ T.unpack path
  progExists <- liftModIO $ doesFileExist $ T.unpack progPath
  sourceExists <- liftModIO $ doesFileExist $ T.unpack scriptPath
  when (dirExists && (progExists || sourceExists)) $
    tellError $ desugarError rng $ "both a directory and file exist for this module"
  if progExists then do
    Right <$> getProgModule (S.ModulePath rng progPath)
  else if sourceExists then do
    Right <$> getScriptModule (S.ModulePath rng scriptPath)
  else if dirExists then do
    Right <$> getDirModule (S.ModulePath rng path)
  else if pathFileExists then
    pure $ Left $ desugarError rng $ "no module exists at " <> path <> " (a raw file exists though - when importing, drop the extension)"
  else
    pure $ Left $ desugarError rng $ "no module exists at " <> path

getModule :: S.ModulePath Range -> SessionRes (Module () ())
getModule path = do
  mdl <- tryGetModule path
  case mdl of
    Left err -> mkFail err
    Right mdl' -> pure mdl'

parseImportDecl :: T.Text -> S.ImportDecl Range -> SessionRes (ImportDecl Range)
parseImportDecl myPath (S.ImportDecl rng (S.Symbol litRng lit) path qual) = do
  unless (lit == "import") $
    tellError $ desugarError litRng "expected \"import\""
  absPath <- resolvePath myPath path
  let qual' = parseImportQual qual
  imod <- getModule absPath
  pure $ ImportDecl rng (S.modulePathText absPath) qual' imod

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

parseSymbol :: (MonadReader ImportEnv m, MonadIO m, MonadCatch m, MonadResult m) => SymbolType -> S.Symbol Range -> m (Symbol Range)
parseSymbol typ (S.Symbol rng txt) = do
  imps <- ask
  let (qual, lcl) = T.breakOnEnd "_" txt
      cands = fold $ importEnvAllDecls imps M.!? qual
      paths = map fst $ filter (declSetContains typ lcl . snd) cands
  path <-
    case paths of
      [p] -> pure p
      [] -> do
        tellError $ desugarError rng $ "unknown (not local, imported, or builtin): " <> txt
        pure invalidTxt
      ps -> do
        tellError $ desugarError rng $ T.unlines $ "ambiguous, modules where this is defined:" : ps
        pure $ head ps
  pure Symbol
    { symbolAnn = rng
    , symbolModule = path
    , symbol = lcl
    }

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
  <$> parseSymbol SymbolTypeRecord head'
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
  lang <- overErrors (addRangeToErr langExtRng) $ lift $ lift $ langWithExt StageDesugar langExt
  ress <- overErrors (addRangeToErr rng) $ lift $ lift $ runLanguageParser lang txt splices
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
  = GroupLocGlobal (lrng <> hrng) <$> parseSymbol SymbolTypeGroup head'
parseGroupHead (S.GroupLocLocal _) (S.Symbol rng txt) = do
  env <- get
  let (idx, newEnv) = bindEnvLookup txt $ gvEnvGroup env
  put env{ gvEnvGroup = newEnv }
  pure $ GroupLocLocal rng idx
parseGroupHead (S.GroupLocFunction lrng) head'@(S.Symbol hrng _)
  = GroupLocFunction (lrng <> hrng) <$> parseSymbol SymbolTypeFunction head'

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

parseEmptyGroupDef :: S.GroupDecl Range -> ImportSessionRes (GroupDef GVBindEnv Range)
parseEmptyGroupDef (S.GroupDecl rng (S.Group _ loc (S.Symbol headRng head') props))
  = case loc of
      S.GroupLocGlobal _ -> do
        (props', bindEnv)
          <- runStateT (traverseDropFatals parseGroupPropDecl props) emptyGVBindEnv
        let (vprops, gprops) = partitionTuple $ catMaybes props'
        pure GroupDef
          { groupDefAnn = rng
          , groupDefHead = head'
          , groupDefValueProps = vprops
          , groupDefGroupProps = gprops
          , groupDefReducers = []
          , groupDefPropEnv = bindEnv
          }
      S.GroupLocLocal _ -> mkFail $ desugarError headRng "can't declare a lowercase group, lowercase groups are group properties"
      S.GroupLocFunction _ -> mkFail $ desugarError headRng "can't declare a function, functions are provided by libraries"

parseRestGroupDefs :: S.GroupDecl Range -> [S.TopLevel Range] -> ImportSessionRes (N.NonEmpty (GroupDef GVBindEnv Range))
parseRestGroupDefs decl [] = (N.:| []) <$> parseEmptyGroupDef decl
parseRestGroupDefs decl (S.TopLevelImportDecl _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelRecordDecl _ : xs) = parseRestGroupDefs decl xs
parseRestGroupDefs decl (S.TopLevelReducer red : xs) = do
  GroupDef yRng yHead yValueProps yGroupProps yReds yBindEnv N.:| ys <- parseRestGroupDefs decl xs
  red' <- parseReducer yBindEnv red
  let yRng' = getAnn red <> yRng
      yReds' = red' : yReds
  pure $ GroupDef yRng' yHead yValueProps yGroupProps yReds' yBindEnv N.:| ys
parseRestGroupDefs decl (S.TopLevelGroupDecl decl' : xs)
  = (N.<|) <$> parseEmptyGroupDef decl <*> parseRestGroupDefs decl' xs

parseAllGroupDefs :: [S.TopLevel Range] -> ImportSessionRes [GroupDef GVBindEnv Range]
parseAllGroupDefs [] = pure []
parseAllGroupDefs (S.TopLevelGroupDecl x : xs)
  = N.toList <$> parseRestGroupDefs x xs
parseAllGroupDefs (_ : _) = error "expected group declaration when parsing group definitions"

notGroupedReducerError :: Reducer Range -> Error
notGroupedReducerError red
  = desugarError (getAnn red) "reducer not in group"

-- | Parses, only adds errors when they get in the way of parsing, not when they allow parsing but would cause problems later.
parseRaw :: FilePath -> S.Program Range -> SessionRes (ImportEnv, Program GVBindEnv Range)
parseRaw path (S.Program rng topLevels) = do
  let mpath = T.pack $ dropExtension path
      (topLevelsOutGroups, topLevelsInGroups)
        = break (\case S.TopLevelGroupDecl _ -> True; _ -> False) topLevels
      -- TODO: Verify order of declarations
      idecls = mapMaybe (\case S.TopLevelImportDecl x -> Just x; _ -> Nothing) topLevels
      rdecls = mapMaybe (\case S.TopLevelRecordDecl x -> Just x; _ -> Nothing) topLevels
      reds = mapMaybe (\case S.TopLevelReducer x -> Just x; _ -> Nothing) topLevelsOutGroups
      gdecls = mapMaybe (\case S.TopLevelGroupDecl x -> Just x; _ -> Nothing) topLevels
  idecls' <- traverse (parseImportDecl mpath) idecls
  rdecls' <- traverse parseRecordDecl rdecls
  (gdecls', fdecls) <- partitionEithers <$> traverse parseGroupDecl gdecls
  let exps = mkDeclSet (map compactRecordDecl rdecls') gdecls' fdecls
      imps = mkImportEnv mpath exps idecls'
  (`runReaderT` imps) $ do
    reds' <- traverseDropFatals (parseReducer emptyGVBindEnv) reds
    groups <- parseAllGroupDefs topLevelsInGroups
    -- TODO: Syntax sugar a main group
    tellErrors $ map notGroupedReducerError reds'
    pure (imps, Program
      { programAnn = rng
      , programImportDecls = idecls'
      , programRecordDecls = rdecls'
      , programModule
          = Module
          { moduleExports = exps
          , moduleGroups = groups
          }
      })

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result. Preserves extra so the program can be further analyzed.
parseProg :: FilePath -> S.Program Range -> SessionRes (Program GVBindEnv Range)
parseProg path = validate . parseRaw path

-- | Extracts a 'Core' AST from a 'Sugar' AST. Badly-formed statements are ignored and errors are added to the result. Strips extra.
parse :: FilePath -> S.Program Range -> SessionRes (Module () ())
parse path = fmap remExtra . parseProg path

-- | Compile a source into a @Module@.
parseFromStart :: FilePath -> SessionRes (Module () ())
parseFromStart input
  = ( parse input
  <=< ResultT . pure . S.parse
  <=< ResultT . pure . L.parse
  <=< liftIOAndCatch StageReadInput . T.readFile
    ) input
