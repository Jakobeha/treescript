{
}

_Exprs_
  = exprs:_Expr* _ { return exprs; }

_Expr
  = _ expr:Expr { return expr; }

Expr
  = x:Word
  / x:Punc
  / x:Block
  / x:String
  / x:Int

Word "word"
  = [A-Za-z_]+ { return { type: "Word", text: text() } }

Punc "punctuation"
  = ("->" / "<-" / "=>" / "<=" / [#$%&*+,-./:;<=>?@\\^_`|~!]) { return { type: "Punc", punc: text() }; }

Block "block"
  = "(" expr:Nested ")" { return { type: "Block", punc: "(", expr }; }
  / "[" expr:Nested "]" { return { type: "Block", punc: "[", expr }; }
  / openBr:"{" expr:Nested "}" { return { type: "Block", punc: openBr, expr }; }

String "string"
  = '"' content:DQInner '"' { return { type: "String", punc: '"', text: content }; }
  / "'" content:SQInner "'" { return { type: "String", punc: "'", text: content }; }

DQInner
  = ("\\\"" [^"])* { return text(); }

SQInner
  = ("\\'" [^'])* { return text(); }

Int "integer"
  = [0-9]+ { return { type: "Int", base: 10, value: parseInt(text(), 10) }; }

_ "whitespace"
  = [ \t\n\r]*

Nested
  = exprs:_Exprs_ { return exprs; }
