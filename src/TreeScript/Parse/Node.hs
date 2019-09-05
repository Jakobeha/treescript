{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TreeScript.Parse.Node
  ()
where

import           TreeScript.Ast
import           TreeScript.Parse.Class

import           Data.Scientific
import qualified Data.Text                     as T
import qualified TreeSitter.NominalScript      as G

instance UntypedParseable (Program (A1 SrcAnn))
instance Parseable (Program (A1 SrcAnn)) where
  parse = untypedParse G.Program True

instance Parseable (Statement (A1 SrcAnn))

instance UntypedParseable (FunDeclare (A1 SrcAnn))
instance Parseable (FunDeclare (A1 SrcAnn)) where
  parse = untypedParse G.FunctionDeclaration True

instance UntypedParseable (Declare (A1 SrcAnn))
instance Parseable (Declare (A1 SrcAnn)) where
  parse = untypedParse G.Declaration True

instance UntypedParseable (Assign (A1 SrcAnn))
instance Parseable (Assign (A1 SrcAnn)) where
  parse = untypedParse G.Assignment True

instance UntypedParseable (Match (A1 SrcAnn))
instance Parseable (Match (A1 SrcAnn)) where
  parse = untypedParse G.MatchStatement True

instance UntypedParseable (MatchBody (A1 SrcAnn))
instance Parseable (MatchBody (A1 SrcAnn)) where
  parse = untypedParse G.MatchBody True

instance UntypedParseable (Case (A1 SrcAnn))
instance Parseable (Case (A1 SrcAnn)) where
  parse = untypedParse G.MatchCase True

instance UntypedParseable (Loop (A1 SrcAnn))
instance Parseable (Loop (A1 SrcAnn)) where
  parse = untypedParse G.LoopStatement True

instance UntypedParseable (Break (A1 SrcAnn))
instance Parseable (Break (A1 SrcAnn)) where
  parse = untypedParse G.BreakStatement True

instance UntypedParseable (ExprStmt (A1 SrcAnn))
instance Parseable (ExprStmt (A1 SrcAnn)) where
  parse = untypedParse G.ExpressionStatement True

instance Parseable (Expr (A1 SrcAnn))

instance Parseable (Pattern (A1 SrcAnn))

instance Parseable (PExpr (A1 SrcAnn))

instance Parseable (RefExpr (A1 SrcAnn))

instance UntypedParseable (Object (A1 SrcAnn))
instance Parseable (Object (A1 SrcAnn)) where
  parse = untypedParse G.Object True

instance UntypedParseable (ObjectBody (A1 SrcAnn))
instance Parseable (ObjectBody (A1 SrcAnn)) where
  parse = untypedParse G.ObjectBody True

instance UntypedParseable (ObjectProp (A1 SrcAnn))
instance Parseable (ObjectProp (A1 SrcAnn)) where
  parse = untypedParse G.ObjectProp True

instance UntypedParseable (Array (A1 SrcAnn))
instance Parseable (Array (A1 SrcAnn)) where
  parse = untypedParse G.Array True

instance UntypedParseable (Closure (A1 SrcAnn))
instance Parseable (Closure (A1 SrcAnn)) where
  parse = untypedParse G.Closure True

instance UntypedParseable (FormalList (A1 SrcAnn))
instance Parseable (FormalList (A1 SrcAnn)) where
  parse = untypedParse G.FormalParameters True

instance UntypedParseable (Block (A1 SrcAnn))
instance Parseable (Block (A1 SrcAnn)) where
  parse = untypedParse G.BracketedStatements True

instance UntypedParseable (Access (A1 SrcAnn))
instance Parseable (Access (A1 SrcAnn)) where
  parse = untypedParse G.Access True

instance UntypedParseable (Subscript (A1 SrcAnn))
instance Parseable (Subscript (A1 SrcAnn)) where
  parse = untypedParse G.Subscript True

instance UntypedParseable (Call (A1 SrcAnn))
instance Parseable (Call (A1 SrcAnn)) where
  parse = untypedParse G.Call True

instance UntypedParseable (ArgList (A1 SrcAnn))
instance Parseable (ArgList (A1 SrcAnn)) where
  parse = untypedParse G.Arguments True

instance UntypedParseable (UnOp (A1 SrcAnn))
instance Parseable (UnOp (A1 SrcAnn)) where
  parse = untypedParse G.UnaryOperation False

instance Parseable (UnOperator (A1 SrcAnn)) where
  parse = parseSpecialSum'
    UnOperator
    [(G.AnonBang, UnOpTypeNot), (G.AnonMinus, UnOpTypeNeg)]

instance UntypedParseable (BinOp (A1 SrcAnn))
instance Parseable (BinOp (A1 SrcAnn)) where
  parse = untypedParse G.BinaryOperation False

instance Parseable (BinOperator (A1 SrcAnn)) where
  parse = parseSpecialSum'
    BinOperator
    [ (G.AnonAmpersandAmpersand, BinOpTypeAnd)
    , (G.AnonPipePipe          , BinOpTypeOr)
    , (G.AnonPlus              , BinOpTypePlus)
    , (G.AnonMinus             , BinOpTypeMinus)
    , (G.AnonStar              , BinOpTypeTimes)
    , (G.AnonSlash             , BinOpTypeDiv)
    , (G.AnonStarStar          , BinOpTypeExp)
    , (G.AnonLAngle            , BinOpTypeLT)
    , (G.AnonLAngleEqual       , BinOpTypeLE)
    , (G.AnonEqualEqual        , BinOpTypeEQ)
    , (G.AnonBangEqual         , BinOpTypeNE)
    , (G.AnonRAngleEqual       , BinOpTypeGE)
    , (G.AnonRAngle            , BinOpTypeGT)
    ]

instance Parseable (Identifier (A1 SrcAnn)) where
  parse = parseIdentifier Identifier G.Identifier

instance Parseable (TagIdentifier (A1 SrcAnn)) where
  parse = parseIdentifier TagIdentifier G.TagIdentifier

instance Parseable (Blank (A1 SrcAnn)) where
  parse = parseUnit Blank G.Blank

instance Parseable (Lit (A1 SrcAnn)) where
  parse = parseSpecialSum
    Lit
    [ (G.String, LitDataString . T.pack . read . T.unpack)
    , ( G.Number
      , \src -> case floatingOrInteger $ read $ T.unpack src of
        Left  flt -> LitDataFloat flt
        Right int -> LitDataInt int
      )
    , (G.True , \_ -> LitDataBool True)
    , (G.False, \_ -> LitDataBool False)
    , (G.Null , \_ -> LitDataNull)
    ]
