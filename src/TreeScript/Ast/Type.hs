-- | Parsed nodes.
module TreeScript.Ast.Type
  ( module TreeScript.Ast.Type
  )
where

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
  | TExprStmt
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
  | TBlank
  | TLit
  deriving (Eq, Ord, Read, Show)
