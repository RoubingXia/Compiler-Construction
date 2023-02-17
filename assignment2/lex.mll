(* Lexer for Fish --- TODO *)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let var = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit=['0'-'9']
let return = "return"
let _if = "if"
let _else = "else"

(* rules section *)
rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf }
| eof {EOF}
| ws+ { lexer lexbuf }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| return {RETURN}
| '+' {PLUS}
| ';' {SEMI}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIV}
| '(' {LPAREN}
| ')' {RPAREN}
| "==" {EQ}
| "!=" {NEQ}
| '<' {LT}
| "<=" {LTE}
| '>' {GT}
| ">=" {GTE}
| '!' {NOT}
| "&&" {AND}
| "||" {OR}
| '=' {ASSIGN}
| '{' {LBRACE}
| '}' {RBRACE}
| _if {IF}
| _else {ELSE}
| var as s {VAR s}

