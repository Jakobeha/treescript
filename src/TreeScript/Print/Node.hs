{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsed nodes.
module TreeScript.Print.Node
  ()
where

import           TreeScript.Ast
import           TreeScript.Misc
import           TreeScript.Print.Class
import           TreeScript.Print.Misc          ( )

import qualified Data.Text                     as T

instance (Printable n) => Printable (Identifier n) where
  mprint = treeMPrint

instance TreePrintable Identifier where
  treePrint (Identifier txt) = pure $ pliteral txt

instance (Printable n) => Printable (SpliceText n) where
  mprint = treeMPrint

instance TreePrintable SpliceText where
  treePrint (SpliceTextNil txt) = pure $ pliteral txt
  treePrint (SpliceTextCons txt spl rst) =
    pure (pliteral txt) <> "\\" <> spl <> "\\" <> treePrint rst

instance Printable Quote where
  pprint QuoteSingle = "'"
  pprint QuoteDouble = "\""

instance (Printable n) => Printable (Quoted n) where
  mprint = treeMPrint

instance TreePrintable Quoted where
  treePrint (Quoted qut txt) =
    (ppunc <$> mprint qut) <> treePrint txt <> (ppunc <$> mprint qut)

instance (Printable n) => Printable (Prim n) where
  mprint = treeMPrint

instance TreePrintable Prim where
  treePrint (PrimInteger x  ) = pliteral <$> mprint x
  treePrint (PrimFloat   x  ) = pliteral <$> mprint x
  treePrint (PrimQuoted  qtd) = treePrint qtd

instance (Printable n) => Printable (Assign n) where
  mprint = treeMPrint

instance TreePrintable Assign where
  treePrint (Assign lhs rhs) = lhs <> " = " <> rhs

instance (Printable n) => Printable (Let n) where
  mprint = treeMPrint

instance TreePrintable Let where
  treePrint (Let asn) = "let " <> treePrint asn

instance (Printable n) => Printable (Struct n) where
  mprint = treeMPrint

instance TreePrintable Struct where
  treePrint (Struct xs) = "{ " <> mintercalate ", " (map treePrint xs) <> " }"

instance (Printable n) => Printable (Access n) where
  mprint = treeMPrint

instance TreePrintable Access where
  treePrint (Access expr prop) = expr <> "." <> treePrint prop

instance (Printable n) => Printable (Array n) where
  mprint = treeMPrint

instance TreePrintable Array where
  treePrint (Array xs) = "[" <> mintercalate ", " xs <> "]"

instance (Printable n) => Printable (Index n) where
  mprint = treeMPrint

instance TreePrintable Index where
  treePrint (Index expr loc) = expr <> "[" <> loc <> "]"

instance (Printable n) => Printable (Closure n) where
  mprint = treeMPrint

instance TreePrintable Closure where
  treePrint (Closure xs exprs) =
    "(" <> mintercalate ", " (map treePrint xs) <> ") =>" <> mindent
      (mconcat $ map ("\n" <<>>) exprs)

instance (Printable n) => Printable (Call n) where
  mprint = treeMPrint

instance TreePrintable Call where
  treePrint (Call f xs) = f <> "(" <> mintercalate ", " xs <> ")"

instance (Printable n) => Printable (Node n) where
  mprint = treeMPrint

instance TreePrintable Node

instance (AnnPrintable an) => Printable (ANode an) where
  mprint (ANode ann x) = printAnnd (mprint x) ann

instance (AnnPrintable an) => Printable (ANodeProgram an) where
  mprint (ANodeProgram xs) = T.intercalate "\n" <$> traverse mprint xs
