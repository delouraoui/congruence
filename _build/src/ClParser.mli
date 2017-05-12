type token =
  | AND
  | TRUE
  | FALSE
  | NOT
  | OR
  | EQ
  | LPAREN
  | RPAREN
  | BID of (string)
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ClAst.clForm
