type token =
  | INT of (int)
  | CHAR of (string)
  | EOF
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | NOT
  | AND
  | OR
  | ASSIGN
  | SEMI
  | RETURN
  | VAR of (string)
  | LBRACE
  | RBRACE
  | NL

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
