{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TreeScript.Parse.Node
  ()
where

import           TreeScript.Ast
import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Parse.Class

import           Data.Scientific
import qualified Data.Text                     as T
import qualified TreeSitter.NominalScript      as G

instance (a ~ SrcAnn) => UntypedParseable (Program SrcAnn)
instance (a ~ SrcAnn) => Parseable (Program SrcAnn) where
  parse = untypedParse G.Program True

instance (a ~ SrcAnn) => Parseable (Statement SrcAnn)

instance (a ~ SrcAnn) => UntypedParseable (Declare SrcAnn)
instance (a ~ SrcAnn) => Parseable (Declare SrcAnn) where
  parse = untypedParse G.Declaration True

instance (a ~ SrcAnn) => UntypedParseable (Assign SrcAnn)
instance (a ~ SrcAnn) => Parseable (Assign SrcAnn) where
  parse = untypedParse G.Assignment True

instance (a ~ SrcAnn) => UntypedParseable (Match SrcAnn)
instance (a ~ SrcAnn) => Parseable (Match SrcAnn) where
  parse = untypedParse G.MatchStatement True

instance (a ~ SrcAnn) => UntypedParseable (MatchBody SrcAnn)
instance (a ~ SrcAnn) => Parseable (MatchBody SrcAnn) where
  parse = untypedParse G.MatchBody True

instance (a ~ SrcAnn) => UntypedParseable (Case SrcAnn)
instance (a ~ SrcAnn) => Parseable (Case SrcAnn) where
  parse = untypedParse G.MatchCase True

instance (a ~ SrcAnn) => UntypedParseable (Loop SrcAnn)
instance (a ~ SrcAnn) => Parseable (Loop SrcAnn) where
  parse = untypedParse G.LoopStatement True

instance (a ~ SrcAnn) => UntypedParseable (Break SrcAnn)
instance (a ~ SrcAnn) => Parseable (Break SrcAnn) where
  parse = untypedParse G.BreakStatement True

instance (a ~ SrcAnn) => UntypedParseable (ExprStmt SrcAnn)
instance (a ~ SrcAnn) => Parseable (ExprStmt SrcAnn) where
  parse = untypedParse G.ExpressionStatement True

instance (a ~ SrcAnn) => Parseable (Expr SrcAnn)

instance (a ~ SrcAnn) => Parseable (Pattern SrcAnn)

instance (a ~ SrcAnn) => Parseable (PExpr SrcAnn)

instance (a ~ SrcAnn) => Parseable (RefExpr SrcAnn)

instance (a ~ SrcAnn) => UntypedParseable (Object SrcAnn)
instance (a ~ SrcAnn) => Parseable (Object SrcAnn) where
  parse = untypedParse G.Object True

instance (a ~ SrcAnn) => UntypedParseable (ObjectBody SrcAnn)
instance (a ~ SrcAnn) => Parseable (ObjectBody SrcAnn) where
  parse = untypedParse G.ObjectBody True

instance (a ~ SrcAnn) => UntypedParseable (ObjectProp SrcAnn)
instance (a ~ SrcAnn) => Parseable (ObjectProp SrcAnn) where
  parse = untypedParse G.ObjectProp True

instance (a ~ SrcAnn) => UntypedParseable (Array SrcAnn)
instance (a ~ SrcAnn) => Parseable (Array SrcAnn) where
  parse = untypedParse G.Array True

instance (a ~ SrcAnn) => UntypedParseable (Closure SrcAnn)
instance (a ~ SrcAnn) => Parseable (Closure SrcAnn) where
  parse = untypedParse G.Closure True

instance (a ~ SrcAnn) => UntypedParseable (FormalList SrcAnn)
instance (a ~ SrcAnn) => Parseable (FormalList SrcAnn) where
  parse = untypedParse G.FormalParameters True

instance (a ~ SrcAnn) => UntypedParseable (Block SrcAnn)
instance (a ~ SrcAnn) => Parseable (Block SrcAnn) where
  parse = untypedParse G.BracketedStatements True

instance (a ~ SrcAnn) => UntypedParseable (Access SrcAnn)
instance (a ~ SrcAnn) => Parseable (Access SrcAnn) where
  parse = untypedParse G.Access True

instance (a ~ SrcAnn) => UntypedParseable (Subscript SrcAnn)
instance (a ~ SrcAnn) => Parseable (Subscript SrcAnn) where
  parse = untypedParse G.Subscript True

instance (a ~ SrcAnn) => UntypedParseable (Call SrcAnn)
instance (a ~ SrcAnn) => Parseable (Call SrcAnn) where
  parse = untypedParse G.Call True

instance (a ~ SrcAnn) => UntypedParseable (ArgList SrcAnn)
instance (a ~ SrcAnn) => Parseable (ArgList SrcAnn) where
  parse = untypedParse G.Arguments True

instance (a ~ SrcAnn) => UntypedParseable (UnOp SrcAnn)
instance (a ~ SrcAnn) => Parseable (UnOp SrcAnn) where
  parse = untypedParse G.UnaryOperation False

instance (a ~ SrcAnn) => Parseable (UnOperator a) where
  parse = parseSpecialSum'
    UnOperator
    [(G.AnonBang, UnOpTypeNot), (G.AnonMinus, UnOpTypeNeg)]

instance (a ~ SrcAnn) => UntypedParseable (BinOp SrcAnn)
instance (a ~ SrcAnn) => Parseable (BinOp SrcAnn) where
  parse = untypedParse G.BinaryOperation False

instance (a ~ SrcAnn) => Parseable (BinOperator a) where
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

instance (a ~ SrcAnn) => Parseable (Identifier SrcAnn) where
  parse = parseIdentifier Identifier G.Identifier

instance (a ~ SrcAnn) => Parseable (TagIdentifier SrcAnn) where
  parse = parseIdentifier TagIdentifier G.TagIdentifier

instance (a ~ SrcAnn) => Parseable (Blank SrcAnn) where
  parse = parseUnit Blank G.Blank

instance (a ~ SrcAnn) => Parseable (Lit SrcAnn) where
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
