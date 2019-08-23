{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parsed nodes.
module TreeScript.Ast
  ( module TreeScript.Ast
  )
where

import           TreeScript.Misc

import qualified Data.Text                     as T
import           GHC.Generics

data NoAnn (r :: StxType) = NoAnn
newtype SrcAnn (r :: StxType) = SrcAnn{ srcAnn :: SrcInfo }
-- newtype SrcAnn1 r = SrcAnn1{ srcAnn :: } 'TSrc

newtype DeriveStx f a = DeriveStx{ unDeriveStx :: f a }

data StxType
  = TProgram
  | TDeclare
  | TAssign
  | TMatch
  | TMatchBody
  | TMatchCase
  | TCase
  | TLoop
  | TBreak
  | TObject
  | TObjectBody
  | TObjectProp
  | TArray
  | TClosure
  | TFormalList
  | TBlock
  | TAccess
  | TSubscript
  | TCall
  | TArgList
  | TUnOp
  | TBinOp
  | TUnOperator
  | TBinOperator
  | TIdentifier
  | TTagIdentifier
  | TLit
  deriving (Eq, Ord, Read, Show)

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
  | StatementExpr (Expr r)
  deriving (Generic)
  -- deriving (EqK) via Generically Statement

-- | @let lhs = rhs@.
data Declare r
  = Declare
  { declareAnn :: r 'TDeclare
  , declareLhs :: Identifier r
  , declareRhs :: Expr r
  } deriving (Generic)
  -- deriving (EqK) via Generically Declare

-- | @lhs = rhs@.
data Assign r
  = Assign
  { assignAnn :: r 'TAssign
  , assignLhs :: RefExpr r
  , assignRhs :: Expr r
  } deriving (Generic)
  -- deriving (EqK) via Generically Assign

-- | @match (val) { body... }@
data Match r
  = Match
  { matchAnn :: r 'TMatch
  , matchVal :: RefExpr r
  , matchBody :: (MatchBody r)
  } deriving (Generic)
  -- deriving (EqK) via Generically Match

-- | @cases...@
data MatchBody r
  = MatchBody
  { matchBodyAnn :: r 'TMatchBody
  , matchBodyCases :: [MatchBody r]
  } deriving (Generic)

-- | @cond => { body... }@
data Case r
  = Case
  { caseAnn :: r 'TCase
  , caseCond :: Pattern r
  , caseBody :: Block r
  } deriving (Generic)
  -- deriving (EqK) via Generically Case

-- | @loop { body... }@
data Loop r
  = Loop
  { loopAnn :: r 'TLoop
  , loopBody :: Block r
  } deriving (Generic)
  -- deriving (EqK) via Generically Loop

-- | @break res@
data Break r
  = Break
  { breakAnn :: r 'TBreak
  , breakRes :: Expr r
  } deriving (Generic)
  -- deriving (EqK) via Generically Break

data Expr r
  = ExprP (PExpr r)
  | ExprClos (Closure r)
  | ExprUnOp (UnOp r)
  | ExprBinOp (BinOp r)
  | ExprCall (Call r)
  deriving (Generic)
  -- deriving (EqK) via Generically Expr

data Pattern r
  = PatternE (PExpr r)
  | PatternTag (TagIdentifier r)
  deriving (Generic)
  -- deriving (EqK) via Generically Pattern

data PExpr r
  = PExprLit (Lit r)
  | PExprRef (RefExpr r)
  | PExprObj (Object r)
  | PExprArr (Array r)
  deriving (Generic)
  -- deriving (EqK) via Generically PExpr

data RefExpr r
  = RefExprId (Identifier r)
  | RefExprAccess (Access r)
  | RefExprSubscript (Subscript r)
  deriving (Generic)
  -- deriving (EqK) via Generically RefExpr

-- | @Tag{ body... }@.
data Object r
  = Object
  { objectAnn :: r 'TObject
  , objectTag :: TagIdentifier r
  , objectBody :: ObjectBody r
  } deriving (Generic)
  -- deriving (EqK) via Generically Object

-- | @prop...@
data ObjectBody r
  = ObjectBody
  { objectBodyAnn :: r 'TObjectBody
  , objectBodyProps :: [ObjectProp r]
  } deriving (Generic)

-- | @let lhs = rhs@.
data ObjectProp r
  = ObjectProp
  { objectPropAnn :: r 'TObjectProp
  , objectPropLhs :: Identifier r
  , objectPropRhs :: Expr r
  } deriving (Generic)

-- | @lhs = rhs@
-- | @[rh's, ...]@.
data Array r
  = Array
  { arrayAnn :: r 'TArray
  , arrayElems :: [Expr r]
  } deriving (Generic)
  -- deriving (EqK) via Generically Array

-- | @(formals...) => { body... }@
data Closure r
  = Closure
  { closureAnn :: r 'TClosure
  , closureFormals :: FormalList r
  , closureBody :: Block r
  } deriving (Generic)
  -- deriving (EqK) via Generically Closure

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

-- | @rhs.x@.
data Access r
  = Access
  { accessAnn :: r 'TAccess
  , accessExpr :: RefExpr r
  , accessProp :: Identifier r
  } deriving (Generic)
  -- deriving (EqK) via Generically Access

-- | @rhs[rh's]@.
data Subscript r
  = Subscript
  { subscriptAnn :: r 'TSubscript
  , subscriptExpr :: RefExpr r
  , subscriptLoc :: Expr r
  } deriving (Generic)
  -- deriving (EqK) via Generically Subscript

-- | @fun(args...)@.
data Call r
  = Call
  { callAnn :: r 'TCall
  , callFun :: Expr r
  , callArgs :: ArgList r
  } deriving (Generic)
  -- deriving (EqK) via Generically Call

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
  -- deriving (EqK) via Generically UnOp

-- | @lhs - rhs@
data BinOp r
  = BinOp
  { binOpAnn :: r 'TBinOp
  , binOpLhs :: Expr r
  , binOperator :: BinOperator r
  , binOpRhs :: Expr r
  } deriving (Generic)
  -- deriving (EqK) via Generically BinOp

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
  , identififerText :: T.Text
  } deriving (Generic)
  -- deriving (EqK) via Generically Identifier
-- | @Foo@.
data TagIdentifier r
  = TagIdentifier
  { tagIdentiferAnn :: r 'TTagIdentifier
  , tagIdentififerText :: T.Text
  } deriving (Generic)
  -- deriving (EqK) via Generically TagIdentifier

-- | @7@
data Lit r
  = Lit
  { litAnn :: r 'TLit
  , litData :: LitData
  } deriving (Generic)
  -- deriving (EqK) via Generically Lit

data LitData
  = LitDataString T.Text
  | LitDataInt Int
  | LitDataFloat Float
  | LitDataBool Bool
  | LitDataNull
  deriving (Eq, Ord, Read, Show, Generic)
