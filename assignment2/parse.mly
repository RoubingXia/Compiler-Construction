/* Parser for Fish --- TODO */

%{
open Ast
open Lexing
open Hashtbl

  let symbol_table = create 10 (* create a symbol table with initial size of 10 *)

  let lookup x =
    try find symbol_table x
    with Not_found -> failwith ("Undefined variable: " ^ x)

  let assign x v =
    replace symbol_table x v
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line, "^(string_of_int l)^": "^s^"\n")

type op = Eq | Neq | Lt | Lte | Gt | Gte




let bool2int (b:bool):int = if b then 1 else 0
let int2bool (i:int):bool = i <> 0
let binop (i1:int) (b:op) (i2:int):int =
match b with
 | Eq -> bool2int (i1 = i2)
 | Neq -> bool2int (i1 <> i2)
 | Lt -> bool2int (i1 < i2)
 | Lte -> bool2int (i1 <= i2)
 | Gt -> bool2int (i1 > i2)
 | Gte -> bool2int (i1 >= i2)
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token <int> INT
%token <string> CHAR
%token EOF
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EQ NEQ LT LTE GT GTE
%token NOT AND OR
%token ASSIGN SEMI
%token RETURN
%token <string> VAR
%token LBRACE RBRACE
%token IF ELSE


/* Here's where the real grammar starts -- you'll need to add
 * more rules here... Do not remove the 2%'s!! */

%%





program:
   stmt  EOF { $1 }
   | LBRACE stmt RBRACE EOF { $2 }
   ;
stmt :
  /* empty */
  { (Ast.skip, 0) }
   | RETURN expr SEMI
         {
             let res_rstmt : rstmt = Return(Int $2, 0) in
               (res_rstmt, 0)
         }
 | LBRACE stmt RBRACE {
 $2

 }
    | stmt SEMI stmt
          {

            let stmt1 : stmt =  $1 in
            let stmt2 : stmt =  $3 in
            let res_rstmt : rstmt = Seq(stmt1, stmt2) in
            (res_rstmt, 0)
          }
 | IF expr LBRACE stmt RBRACE ELSE LBRACE stmt RBRACE stmt
 {
    let if_exp : exp = (Int $2,0) in
    let if_rstmt : rstmt = If(if_exp, $4, $8) in
    let stmt1 : stmt =  (if_rstmt, 0) in
    let stmt2 : stmt =  $10 in
    let res_rstmt : rstmt = Seq(stmt1, stmt2) in
    (res_rstmt, 0)
 }
| expr
       {
         let res_rstmt : rstmt = Exp(Int $1,0) in
         (res_rstmt, 0)
       }

expr :
    INT {$1}
    | expr PLUS expr {$1 + $3}
    | MINUS expr { -$2}
      | expr MINUS expr {$1 - $3}
      | expr TIMES expr {$1 * $3}
      | expr DIV expr {$1 / $3}
      | expr EQ expr {binop $1 Eq $3 }
      | expr NEQ expr {binop $1 Neq $3 }
      | expr LT expr {binop $1 Lt $3 }
      | expr LTE expr {binop $1 Lte $3 }
      | expr GT expr {binop $1 Gt $3}
      | expr GTE expr {binop $1 Gte $3 }
      | NOT expr {bool2int ( $2 = 0) }
      | expr AND expr { bool2int ((int2bool $1) && (int2bool $3)) }
      | expr OR expr { bool2int ((int2bool $1) || (int2bool $3)) }
      | LPAREN expr RPAREN {$2}
      | VAR ASSIGN expr { assign $1 $3; 0}
      | VAR {lookup $1}
      ;






