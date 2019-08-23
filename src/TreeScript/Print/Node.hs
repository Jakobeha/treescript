{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsed nodes.
module TreeScript.Print.Node
  ( module TreeScript.Print.Misc
  )
where

import           TreeScript.Ast
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Print.Class
import           TreeScript.Print.Misc          ( )
import           TreeScript.Print.PrintM

import qualified Data.Text                     as T
import           GHC.Generics
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
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Program r) where
  mprintAst (Program ann stmts) =
    printAnnd ann $ mintercalate "\n" (map mprintAst stmts)

instance (AnnPrintable r) => Printable (Statement r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Statement r) where
  mprintAst = fmap (<> ";") . gmprintAst . from

instance (AnnPrintable r) => Printable (Declare r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Declare r) where
  mprintAst (Declare ann lhs rhs) =
    printAnnd ann $ "let " <> mprintAst lhs <> " = " <> mprintAst rhs

instance (AnnPrintable r) => Printable (Assign r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Assign r) where
  mprintAst (Assign ann lhs rhs) =
    printAnnd ann $ mprintAst lhs <> " = " <> mprintAst rhs

instance (AnnPrintable r) => Printable (Match r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Match r) where
  mprintAst (Match ann val body) =
    printAnnd ann $ "match " <> mprintAst val <> " " <> mprintAst body

instance (AnnPrintable r) => Printable (MatchBody r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (MatchBody r) where
  mprintAst (MatchBody ann cases) =
    printAnnd ann
      $  mintercalate "\n"
      $  [pure "{"]
      <> map (pindent . mprintAst) cases
      <> [pure "}"]

instance (AnnPrintable r) => Printable (Case r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Case r) where
  mprintAst (Case ann cond body) =
    printAnnd ann $ mprintAst cond <> " => " <> mprintAst body

instance (AnnPrintable r) => Printable (Loop r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Loop r) where
  mprintAst (Loop ann body) = printAnnd ann $ "loop " <> mprintAst body

instance (AnnPrintable r) => Printable (Break r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Break r) where
  mprintAst (Break ann res) = printAnnd ann $ "break " <> mprintAst res

instance (AnnPrintable r) => Printable (Expr r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Expr r)

instance (AnnPrintable r) => Printable (Pattern r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Pattern r)

instance (AnnPrintable r) => Printable (PExpr r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (PExpr r)

instance (AnnPrintable r) => Printable (RefExpr r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (RefExpr r)

instance (AnnPrintable r) => Printable (Object r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Object r) where
  mprintAst (Object ann tag body) =
    printAnnd ann $ mprintAst tag <> mprintAst body

instance (AnnPrintable r) => Printable (ObjectBody r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (ObjectBody r) where
  mprintAst (ObjectBody ann props) =
    printAnnd ann
      $  "{"
      <> (mintercalate ", " <$> traverse mprintAst props)
      <> "}"

instance (AnnPrintable r) => Printable (ObjectProp r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (ObjectProp r) where
  mprintAst (ObjectProp ann lhs rhs) =
    printAnnd ann $ mprintAst lhs <> " = " <> mprintAst rhs

instance (AnnPrintable r) => Printable (Array r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Array r) where
  mprintAst (Array ann xs) =
    printAnnd ann $ "[" <> (mintercalate ", " <$> traverse mprintAst xs) <> "]"

instance (AnnPrintable r) => Printable (Closure r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Closure r) where
  mprintAst (Closure ann frmls body) =
    printAnnd ann $ mprintAst frmls <> " => " <> mprintAst body

instance (AnnPrintable r) => Printable (FormalList r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (FormalList r) where
  mprintAst (FormalList ann args) =
    printAnnd ann
      $  "("
      <> (mintercalate ", " <$> traverse mprintAst args)
      <> ")"

instance (AnnPrintable r) => Printable (Block r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Block r) where
  mprintAst (Block ann stmts) =
    printAnnd ann
      $  mintercalate "\n"
      $  [pure "{"]
      <> map (pindent . mprintAst) stmts
      <> [pure "}"]

instance (AnnPrintable r) => Printable (Access r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Access r) where
  mprintAst (Access ann expr prop) =
    printAnnd ann $ mprintAst expr <> "." <> mprintAst prop

instance (AnnPrintable r) => Printable (Subscript r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Subscript r) where
  mprintAst (Subscript ann expr loc) =
    printAnnd ann $ mprintAst expr <> "[" <> mprintAst loc <> "]"

instance (AnnPrintable r) => Printable (Call r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Call r) where
  mprintAst (Call ann fun args) =
    printAnnd ann $ mprintAst fun <> mprintAst args

instance (AnnPrintable r) => Printable (ArgList r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (ArgList r) where
  mprintAst (ArgList ann args) =
    printAnnd ann
      $  "("
      <> (mintercalate ", " <$> traverse mprintAst args)
      <> ")"

-- | TODO: Parenthesize precedence
instance (AnnPrintable r) => Printable (UnOp r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (UnOp r) where
  mprintAst (UnOp ann opr val) =
    printAnnd ann $ mprintAst opr <> " " <> mprintAst val

instance (AnnPrintable r) => Printable (BinOp r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (BinOp r) where
  mprintAst (BinOp ann lhs opr rhs) =
    printAnnd ann
      $  mprintAst lhs
      <> " "
      <> mprintAst opr
      <> " "
      <> mprintAst rhs

instance (AnnPrintable r) => Printable (UnOperator r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (UnOperator r) where
  mprintAst (UnOperator ann typ) = printAnnd ann $ ppunc $ pprint typ

instance (AnnPrintable r) => Printable (BinOperator r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (BinOperator r) where
  mprintAst (BinOperator ann typ) = printAnnd ann $ ppunc $ pprint typ

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
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Identifier r) where
  mprintAst (Identifier ann txt) = printAnnd ann $ pliteral txt

instance (AnnPrintable r) => Printable (TagIdentifier r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (TagIdentifier r) where
  mprintAst (TagIdentifier ann txt) = printAnnd ann $ pliteral txt

instance (AnnPrintable r) => Printable (Lit r) where
  pprint = pprintAst
instance (AnnPrintable r) => AstPrintable (Lit r) where
  mprintAst (Lit ann x) = printAnnd ann $ pliteral $ pprint x

instance Printable LitData where
  pprint (LitDataString txt  ) = pprint txt
  pprint (LitDataInt    int  ) = pprint int
  pprint (LitDataFloat  flt  ) = pprint flt
  pprint (LitDataBool   False) = "false"
  pprint (LitDataBool   True ) = "true"
  pprint LitDataNull           = "null"

instance AnnPrintable SrcAnn where
  printAnnd (SrcAnn (SrcInfo _ txt)) _ = do
    case T.indentOnLastLine txt of
      Nothing  -> pure ()
      Just lvl -> putIndent lvl
    pann txt

instance AnnPrintable NoAnn where
  printAnnd NoAnn x = x
