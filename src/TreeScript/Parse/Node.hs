{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Parse.Node
  ( parseNode
  )
where

import           TreeScript.Ast
import           TreeScript.Misc
import           TreeScript.Parse.ParsecHelpers

import qualified Text.Megaparsec               as M

instance TreePrintable Identifier where
  parse (Identifier txt) = pure $ pliteral txt

instance TreePrintable SpliceText where
  parse (SpliceTextNil txt) = pure $ pliteral txt
  parse (SpliceTextCons txt spl rst) =
    pure (pliteral txt) <> "\\" <> spl <> "\\" <> parse rst

instance Printable Quote where
  pprint QuoteSingle = "'"
  pprint QuoteDouble = "\""

instance TreePrintable Quoted where
  parse (Quoted qut txt) =
    (ppunc <$> mprint qut) <> parse txt <> (ppunc <$> mprint qut)

instance TreePrintable Prim where
  parse (PrimInteger x  ) = pliteral <$> mprint x
  parse (PrimFloat   x  ) = pliteral <$> mprint x
  parse (PrimQuoted  qtd) = parse qtd

instance TreePrintable Assign where
  parse (Assign lhs rhs) = lhs <> " = " <> rhs

instance TreePrintable Let where
  parse (Let asn) = "let " <> parse asn

instance TreePrintable Struct where
  parse (Struct xs) = "{ " <> mintercalate ", " (map parse xs) <> " }"

instance TreePrintable Access where
  parse = Access <$> P.try (parse *> ".") <> parse prop

instance TreePrintable Array where
  parse = Array <$> ("[" <* xs `P.sepBy` ", " *> "]")

instance TreePrintable Index where
  parse = Index <$> P.try (parse *> "[") <*> (parse *> "]")

instance TreeParsable Closure where
  parse =
    Closure
      <$> ("(" <* parse `M.sepBy` ", " *> ") =>")
      <*> (parse `M.sepBy` "\n")

instance TreeParsable Call where
  parse = Call <$> P.try (parse *> "(") <*> (parse `M.sepBy` ", " *> ")")

prim = atom $ \case
  AtomPrim prim' -> Just prim'
  _              -> Nothing

ident = atom $ \case
  AtomSymbol SymbolCaseLower sym -> Just sym
  _                              -> Nothing

symbol txt = atom $ \case
  AtomSymbol _ sym | sym == txt -> Just ()
  _                             -> Nothing

closure :: Parser Closure
closure =
  Closure
    <$> (enclos EncTypeParen "," idents *> "=>")
    <*> enclos EncTypeBracket "\n" stmts

array :: Parser Array
array = Array <$> enclos EncTypeBrace "," stmt

struct :: Parser Struct
struct = Struct <$> enclos EncTypeBracket "," (assign <*> stmt)

assign :: Parser (Node -> Assign)
assign = Assign <$> (ident *> " = ")

assign' :: Parser Assign
assign' = assign <*> expr

let_ :: Parser (Node -> Let)
let_ = (Let .) <$> symbol "let" <* assign

exprBase :: Parser Node
exprBase =
  (NodeClosure <$> closure)
    <|> (NodeArray <$> array)
    <|> (NodeStruct <$> struct)
    <|> (NodeIdentifier <$> ident)
    <|> (NodePrim <$> prim)

exprPost :: Parser (Node -> Node)
exprPost = access <|> index <|> call

expr :: Parser Node
expr = exprBase <**> exprPost

stmtPre :: Parser (Node -> Node)
stmtPre = let_ <|> assign <|> pure id

stmt :: Parser SNode
stmt = stmtPre <*> expr

stmts :: Parser [SNode]
stmts = ((:) <$> stmt <*> many ("\n" <* stmt)) <|> pure []

programParser :: Parser (ANodeProgram SrcAnn)
programParser = ANodeProgram <$> stmts

parseNode :: BalanceProgram SrcAnn -> EResult (ANodeProgram SrcAnn)
parseNode = runParser programParser ""
