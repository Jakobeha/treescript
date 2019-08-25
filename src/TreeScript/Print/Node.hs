{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsed nodes.
module TreeScript.Print.Node
  ()
where

import           TreeScript.Ast
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Print.Class
import           TreeScript.Print.Misc

import qualified Data.Text                     as T
import           Text.Casing             hiding ( Identifier )
import qualified TreeSitter.NominalScript      as G

instance Printable G.Grammar where
  pprint gram
    | "Anon" `T.isPrefixOf` glit
    = "'" <> foldr replace (T.take 4 glit) replacements <> "'"
    | otherwise
    = T.replace "_" " " $ T.pack $ quietSnake $ T.unpack glit
   where
    glit = T.pack $ show gram
    replace (old, new) = T.replace old new
    replacements =
      -- TODO: Incomplete
      [ ("Ampersand", "&")
      , ("Bang"     , "!")
      , ("Comma"    , ",")
      , ("Equal"    , "=")
      , ("LAngle"   , "<")
      , ("RAngle"   , ">")
      , ("Pipe"     , "|")
      ]

instance (AnnPrintable r) => Printable (Program r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Program r) where
  printAst (Program ann stmts) =
    printAnnd ann $ mintercalate "\n" $ map printAst stmts

instance (AnnPrintable r) => Printable (Statement r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Statement r)

instance (AnnPrintable r) => Printable (Declare r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Declare r) where
  printAst (Declare ann lhs rhs) =
    printAnnd ann $ "let " <> printAst lhs <> " = " <> printAst rhs <> ";"

instance (AnnPrintable r) => Printable (Assign r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Assign r) where
  printAst (Assign ann lhs rhs) =
    printAnnd ann $ printAst lhs <> " = " <> printAst rhs <> ";"

instance (AnnPrintable r) => Printable (Match r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Match r) where
  printAst (Match ann val body) =
    printAnnd ann $ "match " <> printAst val <> " " <> printAst body <> ";"

instance (AnnPrintable r) => Printable (MatchBody r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (MatchBody r) where
  printAst (MatchBody ann cases) =
    printAnnd ann $ pbracket $ map printAst cases

instance (AnnPrintable r) => Printable (Case r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Case r) where
  printAst (Case ann cond body) =
    printAnnd ann $ printAst cond <> " => " <> printAst body <> ";"

instance (AnnPrintable r) => Printable (Loop r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Loop r) where
  printAst (Loop ann body) = printAnnd ann $ "loop " <> printAst body <> ";"

instance (AnnPrintable r) => Printable (Break r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Break r) where
  printAst (Break ann res) = printAnnd ann $ "break " <> printAst res <> ";"

instance (AnnPrintable r) => Printable (ExprStmt r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (ExprStmt r) where
  printAst (ExprStmt ann x) = printAnnd ann $ printAst x <> ";"

instance (AnnPrintable r) => Printable (Expr r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Expr r)

instance (AnnPrintable r) => Printable (Pattern r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Pattern r)

instance (AnnPrintable r) => Printable (PExpr r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (PExpr r)

instance (AnnPrintable r) => Printable (RefExpr r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (RefExpr r)

instance (AnnPrintable r) => Printable (Object r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Object r) where
  printAst (Object ann tag body) =
    printAnnd ann $ printAst tag <> printAst body

instance (AnnPrintable r) => Printable (ObjectBody r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (ObjectBody r) where
  printAst (ObjectBody ann props) =
    printAnnd ann $ "[" <> (mintercalate ", " $ map printAst props) <> "]"

instance (AnnPrintable r) => Printable (ObjectProp r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (ObjectProp r) where
  printAst (ObjectProp ann lhs rhs) =
    printAnnd ann $ printAst lhs <> " = " <> printAst rhs

instance (AnnPrintable r) => Printable (Array r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Array r) where
  printAst (Array ann xs) =
    printAnnd ann $ "[" <> (mintercalate ", " $ map printAst xs) <> "]"

instance (AnnPrintable r) => Printable (Closure r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Closure r) where
  printAst (Closure ann frmls body) =
    printAnnd ann $ printAst frmls <> " => " <> printAst body

instance (AnnPrintable r) => Printable (FormalList r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (FormalList r) where
  printAst (FormalList ann args) =
    printAnnd ann $ "(" <> (mintercalate ", " $ map printAst args) <> ")"

instance (AnnPrintable r) => Printable (Block r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Block r) where
  printAst (Block ann stmts) = printAnnd ann $ pbracket $ map printAst stmts

instance (AnnPrintable r) => Printable (Access r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Access r) where
  printAst (Access ann expr prop) =
    printAnnd ann $ printAst expr <> "." <> printAst prop

instance (AnnPrintable r) => Printable (Subscript r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Subscript r) where
  printAst (Subscript ann expr loc) =
    printAnnd ann $ printAst expr <> "[" <> printAst loc <> "]"

instance (AnnPrintable r) => Printable (Call r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Call r) where
  printAst (Call ann fun args) = printAnnd ann $ printAst fun <> printAst args

instance (AnnPrintable r) => Printable (ArgList r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (ArgList r) where
  printAst (ArgList ann args) =
    printAnnd ann $ "(" <> (mintercalate ", " $ map printAst args) <> ")"

-- | TODO: Parenthesize precedence
instance (AnnPrintable r) => Printable (UnOp r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (UnOp r) where
  printAst (UnOp ann opr val) =
    printAnnd ann $ printAst opr <> " " <> printAst val

instance (AnnPrintable r) => Printable (BinOp r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (BinOp r) where
  printAst (BinOp ann lhs opr rhs) =
    printAnnd ann $ printAst lhs <> " " <> printAst opr <> " " <> printAst rhs

instance (AnnPrintable r) => Printable (UnOperator r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (UnOperator r) where
  printAst (UnOperator ann typ) = printAnnd ann $ ppunc $ pprint typ

instance (AnnPrintable r) => Printable (BinOperator r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (BinOperator r) where
  printAst (BinOperator ann typ) = printAnnd ann $ ppunc $ pprint typ

instance Printable UnOpType where
  pprint UnOpTypeNot = "!"
  pprint UnOpTypeNeg = "-"

instance Printable BinOpType where
  pprint BinOpTypeAnd   = "&&"
  pprint BinOpTypeOr    = "||"
  pprint BinOpTypePlus  = "+"
  pprint BinOpTypeMinus = "-"
  pprint BinOpTypeTimes = "*"
  pprint BinOpTypeDiv   = "/"
  pprint BinOpTypeExp   = "**"
  pprint BinOpTypeLT    = "<"
  pprint BinOpTypeLE    = "<="
  pprint BinOpTypeEQ    = "=="
  pprint BinOpTypeNE    = "!="
  pprint BinOpTypeGE    = ">="
  pprint BinOpTypeGT    = ">"

instance (AnnPrintable r) => Printable (Identifier r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Identifier r) where
  printAst (Identifier ann txt) = printAnnd ann $ pliteral txt

instance (AnnPrintable r) => Printable (TagIdentifier r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (TagIdentifier r) where
  printAst (TagIdentifier ann txt) = printAnnd ann $ pliteral txt

instance (AnnPrintable r) => Printable (Blank r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Blank r) where
  printAst (Blank ann) = printAnnd ann "_"

instance (AnnPrintable r) => Printable (Lit r) where
  pprint = printAst
instance (AnnPrintable r) => AstPrintable (Lit r) where
  printAst (Lit ann x) = printAnnd ann $ pliteral $ pprint x

instance Printable LitData where
  pprint (LitDataString txt  ) = pprint txt
  pprint (LitDataInt    int  ) = pprint int
  pprint (LitDataFloat  flt  ) = pprint flt
  pprint (LitDataBool   False) = "false"
  pprint (LitDataBool   True ) = "true"
  pprint LitDataNull           = "null"

instance AnnPrintable SrcAnn where
  printAnnd (SrcAnn (SrcInfo _ txt)) _ = pann txt

instance AnnPrintable NoAnn where
  printAnnd NoAnn x = x

pbracket :: (PrintOut o) => [o] -> o
pbracket []  = "{}"
pbracket [x] = "{ " <> x <> " }"
pbracket xs  = "{\n" <> pindent (mintercalate "\n" xs) <> "\n}"
