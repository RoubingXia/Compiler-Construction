type token =
  | ADD
  | BEQ
  | JR
  | JAL
  | LI
  | LW
  | SW
  | LUI
  | ORI
  | COMMA
  | LPAREN
  | RPAREN
  | INT of (int32)
  | ID of (string)
  | REG of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mips_ast.program
