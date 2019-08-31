{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

-- | Abstract interpretation - used by the interpreter, compiler, debugger, and analyses.
module TreeScript.Interpret.Abstract
  ( module TreeScript.Interpret.Abstract
  )
where

import           TreeScript.Ast
import           TreeScript.Misc

import           Control.Monad
import           Data.Kind
import qualified Data.Text                     as T

data RefData m
  = RefDataArray [Val m] -- ^ elems
  | RefDataObject T.Text [Val m] -- ^ tag + props, in the same order as the first tag definition
  | RefDataClosure (ClosVal m) [Val m] -- ^ start position + free variables

-- | Abstract interpretation monad.
class (MonadContStack (Val m) m) => Interpret m where
  type HasAnns m :: (StxType -> *) -> Constraint
  data Stack m :: *
  data Heap m :: *
  data Val m :: *
  data Ref m :: *
  data ClosVal m :: *

  runAnn :: (HasAnns m r) => r a -> m ()
  mkLiteral :: LitData -> m (Val m)
  mkReference :: RefData m -> m (Val m)
  mkClos :: m (Val m) -> m (ClosVal m)
  defineVar :: (HasAnns m r) => Identifier r -> Val m -> m ()
  setVar :: (HasAnns m r) => Identifier r -> Val m -> m ()
  getVar :: (HasAnns m r) => Identifier r -> m (Val m)
  deref :: Val m -> m (Ref m)
  getProp :: (HasAnns m r) => Ref m -> Identifier r -> m (Val m)
  getSubscript :: Ref m -> Val m -> m (Val m)
  setProp :: (HasAnns m r) => Ref m -> Identifier r -> Val m -> m ()
  setSubscript :: Ref m -> Val m -> Val m -> m ()
  runCall :: Val m -> [Val m] -> m (Val m)
  runUnOp :: (HasAnns m r) => UnOperator r -> Val m -> m (Val m)
  runBinOp :: (HasAnns m r) => Val m -> BinOperator r -> Val m -> m (Val m)
  getObjectOrder :: (HasAnns m r) => TagIdentifier r -> [T.Text] -> m [T.Text]
  reorderProps :: (HasAnns m r) => [T.Text] -> [ObjectProp r] -> m [Expr r]
  -- | When compiling we want to add all jumps first, then blocks.
  -- When interpreting it's easier to just work one case at a time and skip unnecessary blocks.
  tryMatchCases :: (HasAnns m r) => [Case r] -> Val m -> (forall a. Block r -> m a) -> m ()
  -- | Creates a loop which repeats until exited with a continuation.
  mkLoop :: m () -> m a
  -- TODO: Provide default implementation running a subclass (defined in another file but pure)
  freeVariables :: (HasAnns m r) => FormalList r -> Block r -> m [Identifier r]
  mkFail' :: T.Text -> m a

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
      exitCont $ exit res
    mkFail' "no cases matched"
runStmt (StatementLoop (Loop ann body)) = do
  runAnn ann
  mkCont $ \exit ->
    -- Continuation stack is static even for compiled programs
                    bumpCont exit $ mkLoop $ () <$ runBlock body
runStmt (StatementBreak (Break ann res)) = do
  runAnn ann
  res'  <- runExpr res
  oexit <- topCont
  case oexit of
    Nothing   -> mkFail' "can't break - not in a loop"
    Just exit -> exitCont $ exit res'
runStmt (StatementExpr (ExprStmt ann expr)) = do
  runAnn ann
  runExpr expr

runExpr :: (Interpret m, HasAnns m r) => Expr r -> m (Val m)
runExpr (ExprP    pexpr                   ) = runPExpr pexpr
runExpr (ExprClos (Closure ann frmls body)) = do
  runAnn ann
  syms  <- freeVariables frmls body
  syms' <- mapM getVar syms
  epos  <- mkClos $ runBlock body
  mkReference $ RefDataClosure epos syms'
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
  fun' <- runExpr fun
  runAnn aann
  args' <- mapM runExpr args
  runCall fun' args'

runPExpr :: (Interpret m, HasAnns m r) => PExpr r -> m (Val m)
runPExpr (PExprLit (Lit ann x)) = do
  runAnn ann
  mkLiteral x
runPExpr (PExprRef ref) = runRefExpr ref
runPExpr (PExprObj (Object ann tag (ObjectBody bann props))) = do
  runAnn ann
  runAnn bann
  order   <- getObjectOrder tag $ map (identifierText . objectPropLhs) props
  oprops  <- reorderProps order props
  oprops' <- mapM runExpr oprops
  mkReference $ RefDataObject (tagIdentifierText tag) oprops'
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
