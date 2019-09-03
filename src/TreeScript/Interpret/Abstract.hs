{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

-- | Abstract interpretation - used by the interpreter, compiler, debugger, and analyses.
module TreeScript.Interpret.Abstract
  ( module TreeScript.Interpret.Abstract
  )
where

import           TreeScript.Ast
import           TreeScript.Misc
import           TreeScript.Print

import           Control.Monad
import           Data.Kind
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

data RefData m
  = RefDataArray [Val m] -- ^ elems
  | RefDataObject T.Text [Val m] -- ^ tag + props, in the same order as the first tag definition
  | RefDataClosure (ClosureVal m)

-- | Abstract interpretation monad.
class (MonadContStack m, ContRes m ~ Val m) => Interpret m where
  type HasAnns m :: (StxType -> *) -> Constraint
  data Stack m :: *
  data Heap m :: *
  data Val m :: *
  data Deref m :: *
  data ClosureVal m :: *

  runAnn :: (HasAnns m r) => r a -> m ()
  mkLiteral :: LitData -> m (Val m)
  mkReference :: RefData m -> m (Val m)
  mkClos :: m (Val m) -> [T.Text] -> S.Set T.Text -> m (ClosureVal m)
  defineVar :: (HasAnns m r) => Identifier r -> Val m -> m ()
  setVar :: (HasAnns m r) => Identifier r -> Val m -> m ()
  getVar :: (HasAnns m r) => Identifier r -> m (Val m)
  deref :: Val m -> m (Deref m)
  getProp :: (HasAnns m r) => Deref m  -> Identifier r -> m (Val m)
  getSubscript :: Deref m  -> Val m -> m (Val m)
  setProp :: (HasAnns m r) => Deref m  -> Identifier r -> Val m -> m ()
  setSubscript :: Deref m  -> Val m -> Val m -> m ()
  getClos :: Deref m -> m (ClosureVal m)
  runCall :: ClosureVal m -> [Val m] -> m (Val m)
  runUnOp :: (HasAnns m r) => UnOperator r -> Val m -> m (Val m)
  runBinOp :: (HasAnns m r) => Val m -> BinOperator r -> Val m -> m (Val m)
  getObjectOrders :: m (M.Map T.Text [T.Text])
  addDefaultObjectOrder :: T.Text -> [T.Text] -> m ()
  -- | When compiling we want to add all jumps first, then blocks.
  -- When interpreting it's easier to just work one case at a time and skip unnecessary blocks.
  tryMatchCases :: (HasAnns m r) => [Case r] -> Val m -> (Block r -> m (VirtVoid m)) -> m ()
  -- | Return an empty set of the analysis doesn't need closures to have free variables.
  freeVariables :: (HasAnns m r) => FormalList r -> Block r -> m (S.Set T.Text)
  raiseErr :: T.Text -> m (VirtVoid m)
  vabsurdVal :: VirtVoid m -> m (Val m)

runProgram :: (Interpret m, HasAnns m r) => Program r -> m ()
runProgram (Program ann stmts) = do
  runAnn ann
  forM_ stmts $ (() <$) . runStmt

runStmt :: (Interpret m, HasAnns m r) => Statement r -> m (Val m)
runStmt (StatementDeclare (Declare ann lhs rhs)) = do
  runAnn ann
  rhs' <- runExpr rhs
  defineVar lhs rhs'
  runNull
runStmt (StatementAssign (Assign ann lhs rhs)) = do
  runAnn ann
  rhs' <- runExpr rhs
  runAssign lhs rhs'
  runNull
runStmt (StatementMatch (Match ann val (MatchBody bann cs))) = do
  runAnn ann
  val' <- runRefExpr val
  runAnn bann
  mkCont $ \exit -> do
    tryMatchCases cs val' $ \body -> do
      res <- runBlock body
      exit res
    exit =<< vabsurdVal =<< raiseErr "no cases matched"
runStmt (StatementLoop (Loop ann body)) = do
  runAnn ann
  mkCont $ \exit ->
    -- Continuation stack is static even for compiled programs
                    bumpCont exit $ mkLoop $ () <$ runBlock body
runStmt (StatementBreak (Break ann res)) = do
  runAnn ann
  res'  <- runExpr res
  oexit <- topCont
  vabsurdVal =<< case oexit of
    Nothing   -> raiseErr "can't break - not in a loop"
    Just exit -> exit res'
runStmt (StatementExpr (ExprStmt ann expr)) = do
  runAnn ann
  runExpr expr

runExpr :: (Interpret m, HasAnns m r) => Expr r -> m (Val m)
runExpr (ExprP pexpr) = runPExpr pexpr
runExpr (ExprClos (Closure ann frmls@(FormalList fann frmlids) body@(Block bann _)))
  = do
    runAnn ann
    runAnn fann
    fvs <- freeVariables frmls body
    runAnn bann
    clos <- mkClos (runBlock body) (map identifierText frmlids) fvs
    mkReference $ RefDataClosure clos
runExpr (ExprUnOp (UnOp ann opr val)) = do
  runAnn ann
  val' <- runExpr val
  runUnOp opr val'
runExpr (ExprBinOp (BinOp ann lhs opr rhs)) = do
  runAnn ann
  lhs' <- runExpr lhs
  rhs' <- runExpr rhs
  runBinOp lhs' opr rhs'
runExpr (ExprCall (Call ann fun (ArgList aann args))) = do
  runAnn ann
  funC <- getClos =<< deref =<< runExpr fun
  runAnn aann
  args' <- mapM runExpr args
  runCall funC args'

runPExpr :: (Interpret m, HasAnns m r) => PExpr r -> m (Val m)
runPExpr (PExprLit (Lit ann x)) = do
  runAnn ann
  mkLiteral x
runPExpr (PExprRef ref) = runRefExpr ref
runPExpr (PExprObj (Object ann (TagIdentifier tann tag) (ObjectBody bann props)))
  = do
    runAnn ann
    runAnn tann
    order <- getObjectOrder tag $ Just $ map (identifierText . objectPropLhs)
                                             props
    runAnn bann
    oprops  <- reorderProps order props
    oprops' <- mapM runExpr oprops
    mkReference $ RefDataObject tag oprops'
runPExpr (PExprArr (Array ann elms)) = do
  runAnn ann
  elms' <- mapM runExpr elms
  mkReference $ RefDataArray elms'

runRefExpr :: (Interpret m, HasAnns m r) => RefExpr r -> m (Val m)
runRefExpr (RefExprId     lhs                   ) = getVar lhs
runRefExpr (RefExprAccess (Access ann base prop)) = do
  runAnn ann
  base' <- runRefExpr base
  baseD <- deref base'
  getProp baseD prop
runRefExpr (RefExprSubscript (Subscript ann base loc)) = do
  runAnn ann
  base' <- runRefExpr base
  baseD <- deref base'
  loc'  <- runExpr loc
  getSubscript baseD loc'

runAssign :: (Interpret m, HasAnns m r) => RefExpr r -> Val m -> m ()
runAssign (RefExprId     lhs                 ) rhs = setVar lhs rhs
runAssign (RefExprAccess (Access _ base prop)) rhs = do
  base' <- runRefExpr base
  baseD <- deref base'
  setProp baseD prop rhs
runAssign (RefExprSubscript (Subscript _ base loc)) rhs = do
  base' <- runRefExpr base
  baseD <- deref base'
  loc'  <- runExpr loc
  setSubscript baseD loc' rhs

runBlock :: (HasAnns m r, Interpret m) => Block r -> m (Val m)
runBlock (Block _ stmts) = case unconsLast stmts of
  Nothing              -> fail "empty block"
  Just (istmts, lstmt) -> do
    forM_ istmts $ (() <$) . runStmt
    runStmt lstmt

runNull :: (Interpret m) => m (Val m)
runNull = mkLiteral LitDataNull

getObjectOrder :: (Interpret m) => T.Text -> Maybe [T.Text] -> m [T.Text]
getObjectOrder tag oorderHere = do
  objOrders <- getObjectOrders
  case (objOrders M.!? tag, oorderHere) of
    (Nothing, Nothing) ->
      ([] <$)
        $  raiseErr
        $  "internal error: tried to get order of unknown tag: "
        <> tag
    (Nothing, Just orderHere) -> do
      addDefaultObjectOrder tag orderHere
      pure orderHere
    (Just estOrder, _) -> pure estOrder

reorderProps
  :: (Interpret m, HasAnns m r) => [T.Text] -> [ObjectProp r] -> m [Expr r]
reorderProps order props
  | orderLen /= propsLen
  = ([] <$)
    $  raiseErr
    $  "object with same tag previously defined with "
    <> pprint orderLen
    <> " props, but this object has "
    <> pprint propsLen
  | otherwise
  = mapM lookupProp order
 where
  lookupProp expProp = case lookup expProp propsTup of
    Nothing ->
      (snd (head propsTup) <$)
        $  raiseErr
        $  "object is missing property: '"
        <> expProp
        <> "' (an object with the same tag was previously defined with the property)"
    Just val -> pure val
  propsTup = map (\(ObjectProp _ (Identifier _ name) rhs) -> (name, rhs)) props
  orderLen = length order
  propsLen = length props
