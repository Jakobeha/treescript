{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Parsed nodes.
module TreeScript.Ast.Node
  ( module TreeScript.Ast.Node
  )
where

import           TreeScript.Ast.Class
import           TreeScript.Ast.Type

import qualified Data.Text                     as T
import           Generics.Kind.TH
import           GHC.Generics

data Program r
  = Program
  { programAnn :: r 'TProgram
  , programStmts :: [Statement r]
  } deriving (Generic)

data Statement r
  = StatementDeclare (Declare r)
  | StatementAssign (Assign r)
  | StatementMatch (Match r)
  | StatementLoop (Loop r)
  | StatementBreak (Break r)
  | StatementExpr (ExprStmt r)
  deriving (Generic)

-- | @let lhs = rhs@.
data Declare r
  = Declare
  { declareAnn :: r 'TDeclare
  , declareLhs :: Identifier r
  , declareRhs :: Expr r
  } deriving (Generic)

-- | @lhs = rhs@.
data Assign r
  = Assign
  { assignAnn :: r 'TAssign
  , assignLhs :: RefExpr r
  , assignRhs :: Expr r
  } deriving (Generic)

-- | @match (val) { body... }@
data Match r
  = Match
  { matchAnn :: r 'TMatch
  , matchVal :: RefExpr r
  , matchBody :: (MatchBody r)
  } deriving (Generic)

-- | @cases...@
data MatchBody r
  = MatchBody
  { matchBodyAnn :: r 'TMatchBody
  , matchBodyCases :: [Case r]
  } deriving (Generic)

-- | @cond => { body... }@
data Case r
  = Case
  { caseAnn :: r 'TCase
  , caseCond :: Pattern r
  , caseBody :: Block r
  } deriving (Generic)

-- | @loop { body... }@
data Loop r
  = Loop
  { loopAnn :: r 'TLoop
  , loopBody :: Block r
  } deriving (Generic)

-- | @break res@
data Break r
  = Break
  { breakAnn :: r 'TBreak
  , breakRes :: Expr r
  } deriving (Generic)

data ExprStmt r
  = ExprStmt
  { exprStmtAnn :: r 'TExprStmt
  , exprStmtExpr :: Expr r
  } deriving (Generic)

data Expr r
  = ExprP (PExpr r)
  | ExprClos (Closure r)
  | ExprUnOp (UnOp r)
  | ExprBinOp (BinOp r)
  | ExprCall (Call r)
  deriving (Generic)

data Pattern r
  = PatternE (PExpr r)
  | PatternTag (TagIdentifier r)
  | PatternBlank (Blank r)
  deriving (Generic)

data PExpr r
  = PExprLit (Lit r)
  | PExprRef (RefExpr r)
  | PExprObj (Object r)
  | PExprArr (Array r)
  deriving (Generic)

data RefExpr r
  = RefExprId (Identifier r)
  | RefExprAccess (Access r)
  | RefExprSubscript (Subscript r)
  deriving (Generic)

-- | @Tag{ body... }@.
data Object r
  = Object
  { objectAnn :: r 'TObject
  , objectTag :: TagIdentifier r
  , objectBody :: ObjectBody r
  } deriving (Generic)

-- | @prop...@
data ObjectBody r
  = ObjectBody
  { objectBodyAnn :: r 'TObjectBody
  , objectBodyProps :: [ObjectProp r]
  } deriving (Generic)

-- | @lhs = rhs@.
data ObjectProp r
  = ObjectProp
  { objectPropAnn :: r 'TObjectProp
  , objectPropLhs :: Identifier r
  , objectPropRhs :: Expr r
  } deriving (Generic)

-- | @[elem, ...]@.
data Array r
  = Array
  { arrayAnn :: r 'TArray
  , arrayElems :: [Expr r]
  } deriving (Generic)

-- | @(formals...) => { body... }@
data Closure r
  = Closure
  { closureAnn :: r 'TClosure
  , closureFormals :: FormalList r
  , closureBody :: Block r
  } deriving (Generic)

-- | @x, ...@
data FormalList r
  = FormalList
  { formalListAnn :: r 'TFormalList
  , formalListFormals :: [Identifier r]
  } deriving (Generic)

-- | @stmt...@
data Block r
  = Block
  { blockAnn :: r 'TBlock
  , blockStmts :: [Statement r]
  } deriving (Generic)

-- | @base.prop@.
data Access r
  = Access
  { accessAnn :: r 'TAccess
  , accessBase :: RefExpr r
  , accessProp :: Identifier r
  } deriving (Generic)

-- | @base[loc]@.
data Subscript r
  = Subscript
  { subscriptAnn :: r 'TSubscript
  , subscriptBase :: RefExpr r
  , subscriptLoc :: Expr r
  } deriving (Generic)

-- | @fun(args...)@.
data Call r
  = Call
  { callAnn :: r 'TCall
  , callFun :: Expr r
  , callArgs :: ArgList r
  } deriving (Generic)

-- | @arg, ...@
data ArgList r
  = ArgList
  { argListAnn :: r 'TArgList
  , argListArgs :: [Expr r]
  } deriving (Generic)

-- | @-val@
data UnOp r
  = UnOp
  { unOpAnn :: r 'TUnOp
  , unOperator :: UnOperator r
  , unOpVal :: Expr r
  } deriving (Generic)

-- | @lhs - rhs@
data BinOp r
  = BinOp
  { binOpAnn :: r 'TBinOp
  , binOpLhs :: Expr r
  , binOperator :: BinOperator r
  , binOpRhs :: Expr r
  } deriving (Generic)

data UnOperator r
  = UnOperator
  { unOperatorAnn :: r 'TUnOperator
  , unOperatorType :: UnOpType
  } deriving (Generic)

data BinOperator r
  = BinOperator
  { binOperatorAnn :: r 'TBinOperator
  , binOperatorType :: BinOpType
  } deriving (Generic)

data UnOpType
  = UnOpTypeNot -- ^ @!@
  | UnOpTypeNeg -- ^ @-@
  deriving (Eq, Ord, Read, Show, Generic)

data BinOpType
  = BinOpTypeAnd -- ^ @&&@
  | BinOpTypeOr -- ^ @||@
  | BinOpTypePlus -- ^ @+@
  | BinOpTypeMinus -- ^ @-@
  | BinOpTypeTimes -- ^ @*@
  | BinOpTypeDiv -- ^ @/@
  | BinOpTypeExp -- ^ @**@
  | BinOpTypeLT -- ^ @<@
  | BinOpTypeLE -- ^ @<=@
  | BinOpTypeEQ -- ^ @==@
  | BinOpTypeNE -- ^ @!=@
  | BinOpTypeGE -- ^ @>=@
  | BinOpTypeGT -- ^ @>@
  deriving (Eq, Ord, Read, Show, Generic)

-- | @foo@.
data Identifier r
  = Identifier
  { identifierAnn :: r 'TIdentifier
  , identifierText :: T.Text
  } deriving (Generic)

-- | @Foo@.
data TagIdentifier r
  = TagIdentifier
  { tagIdentiferAnn :: r 'TTagIdentifier
  , tagIdentifierText :: T.Text
  } deriving (Generic)

-- | @_@.
data Blank r
  = Blank
  { blankAnn :: r 'TBlank
  } deriving (Generic)

-- | @7@
data Lit r
  = Lit
  { litAnn :: r 'TLit
  , litData :: LitData
  } deriving (Generic)

data LitData
  = LitDataString T.Text
  | LitDataInt Int
  | LitDataFloat Float
  | LitDataBool Bool
  | LitDataNull
  deriving (Eq, Ord, Read, Show, Generic)

$(deriveGenericK ''Program)
$(deriveGenericK ''Statement)
$(deriveGenericK ''Declare)
$(deriveGenericK ''Assign)
$(deriveGenericK ''Match)
$(deriveGenericK ''MatchBody)
$(deriveGenericK ''Case)
$(deriveGenericK ''Loop)
$(deriveGenericK ''Break)
$(deriveGenericK ''ExprStmt)
$(deriveGenericK ''Expr)
$(deriveGenericK ''Pattern)
$(deriveGenericK ''PExpr)
$(deriveGenericK ''RefExpr)
$(deriveGenericK ''Object)
$(deriveGenericK ''ObjectBody)
$(deriveGenericK ''ObjectProp)
$(deriveGenericK ''Array)
$(deriveGenericK ''Closure)
$(deriveGenericK ''FormalList)
$(deriveGenericK ''Block)
$(deriveGenericK ''Access)
$(deriveGenericK ''Subscript)
$(deriveGenericK ''Call)
$(deriveGenericK ''ArgList)
$(deriveGenericK ''UnOp)
$(deriveGenericK ''BinOp)
$(deriveGenericK ''UnOperator)
$(deriveGenericK ''BinOperator)
$(deriveGenericK ''Identifier)
$(deriveGenericK ''TagIdentifier)
$(deriveGenericK ''Blank)
$(deriveGenericK ''Lit)

instance Node Program
instance Node Statement
instance Node Declare
instance Node Assign
instance Node Match
instance Node MatchBody
instance Node Case
instance Node Loop
instance Node Break
instance Node ExprStmt
instance Node Expr
instance Node Pattern
instance Node PExpr
instance Node RefExpr
instance Node Object
instance Node ObjectBody
instance Node ObjectProp
instance Node Array
instance Node Closure
instance Node FormalList
instance Node Block
instance Node Access
instance Node Subscript
instance Node Call
instance Node ArgList
instance Node UnOp
instance Node BinOp
instance Node UnOperator
instance Node BinOperator
instance Node Identifier
instance Node TagIdentifier
instance Node Blank
instance Node Lit
