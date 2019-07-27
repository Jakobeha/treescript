{-# LANGUAGE OverloadedStrings #-}

-- | Parsed nodes.
module TreeScript.Print.Node
  ()
where

import           TreeScript.Ast
import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Print.Class

import qualified Data.Text                     as T
import           GHC.Generics

instance (Printable n) => Printable (Identifier n)
instance TreePrintable Identifier where
  treePrint (Identifier txt) = fromLiteral txt

instance (Printable n) => Printable (SpliceText n)
instance TreePrintable SpliceText where
  treePrint (SpliceTextNil txt) = fromLiteral txt
  treePrint (SpliceTextCons txt spl rst) =
    fromLiteral txt <> "\\" <> spl <> "\\" <> treePrint rst

instance (Printable n) => Printable (Quote n) where
  pprint QuoteSingle = "'"
  pprint QuoteDouble = "\""

instance (Printable n) => Printable (Quoted n)
instance TreePrintable Quoted where
  treePrint (Quoted quot txt) = pprint quot <> txt <> pprint quot

instance (Printable n) => Printable (Prim n)
instance TreePrintable Prim where
  treePrint (PrimInteger x  ) = pprint x
  treePrint (PrimFloat   x  ) = pprint x
  treePrint (PrimQuoted  qtd) = treePrint qtd

instance (Printable n) => Printable (Assign n)
instance TreePrintable Assign where
  treePrint (Assign lhs rhs) = lhs <> " = " <> rhs

instance (Printable n) => Printable (Let n)
instance TreePrintable Let where
  treePrint (Let asn) = "let " <> asn

instance (Printable n) => Printable (Struct n)
instance TreePrintable Struct where
  treePrint (Struct xs) = "{ " <> mintercalate ", " (map treePrint xs) <> " }"

instance (Printable n) => Printable (Access n)
instance TreePrintable Access where
  treePrint (Access expr prop) = expr <> "." <> treePrint prop

instance (Printable n) => Printable (Array n)
instance TreePrintable Array where
  treePrint (Array xs) = "[" <> mintercalate ", " xs <> "]"

instance (Printable n) => Printable (Index n)
instance TreePrintable Index where
  treePrint (Index expr loc) = expr <> "[" <> loc <> "]"

instance (Printable n) => Printable (Closure n)
instance TreePrintable Closure where
  treePrint (Closure xs exprs) =
    "(" <> mintercalate ", " xs <> ") =>" <> mindent
      (mconcat $ map ("\n" <>) exprs)

instance (Printable n) => Printable (Call n)
instance TreePrintable Call where
  treePrint (Call f xs) = f <> "(" <> mintercalate ", " xs <> ")"

instance (Printable n) => Printable (Node n)
instance TreePrintable Node
