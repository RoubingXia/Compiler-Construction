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

open Parsing;;
let _ = parse_error;;
# 4 "parse.mly"
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
# 68 "parse.ml"
let yytransl_const = [|
    0 (* EOF *);
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* EQ *);
  266 (* NEQ *);
  267 (* LT *);
  268 (* LTE *);
  269 (* GT *);
  270 (* GTE *);
  271 (* NOT *);
  272 (* AND *);
  273 (* OR *);
  274 (* ASSIGN *);
  275 (* SEMI *);
  276 (* RETURN *);
  278 (* LBRACE *);
  279 (* RBRACE *);
  280 (* NL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* CHAR *);
  277 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\003\000\001\000\003\000\001\000\003\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\003\000\003\000\003\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000"

let yysindex = "\255\255\
\002\255\000\000\000\000\041\255\041\255\041\255\041\255\248\254\
\011\255\000\000\004\000\115\255\115\255\100\255\115\255\083\255\
\041\255\006\255\000\000\011\255\041\255\041\255\041\255\041\255\
\041\255\041\255\041\255\041\255\041\255\041\255\041\255\041\255\
\000\000\000\000\115\255\019\000\009\255\115\255\115\255\115\255\
\115\255\115\255\115\255\115\255\115\255\115\255\115\255\115\255\
\115\255\000\000"

let yyrindex = "\000\000\
\007\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\015\255\000\000\000\000\054\000\008\000\000\000\011\000\000\000\
\000\000\000\000\000\000\055\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\002\000\014\000\020\000\021\000\
\027\000\030\000\033\000\039\000\040\000\046\000\047\000\049\000\
\052\000\000\000"

let yygindex = "\000\000\
\000\000\252\255\053\000"

let yytablesize = 334
let yytable = "\001\000\
\024\000\006\000\003\000\019\000\018\000\004\000\003\000\009\000\
\005\000\017\000\019\000\003\000\023\000\008\000\004\000\037\000\
\006\000\005\000\050\000\010\000\011\000\007\000\008\000\009\000\
\020\000\006\000\012\000\020\000\036\000\013\000\007\000\008\000\
\014\000\003\000\000\000\000\000\000\000\003\000\015\000\016\000\
\000\000\003\000\000\000\000\000\004\000\017\000\018\000\005\000\
\020\000\000\000\000\000\021\000\000\000\005\000\003\000\006\000\
\013\000\014\000\015\000\016\000\000\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\049\000\021\000\022\000\023\000\
\024\000\000\000\000\000\025\000\026\000\027\000\028\000\029\000\
\030\000\000\000\031\000\032\000\000\000\034\000\021\000\022\000\
\023\000\024\000\000\000\033\000\025\000\026\000\027\000\028\000\
\029\000\030\000\000\000\031\000\032\000\021\000\022\000\023\000\
\024\000\000\000\000\000\025\000\026\000\027\000\028\000\029\000\
\030\000\000\000\031\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\024\000\024\000\024\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\009\000\
\024\000\024\000\019\000\024\000\023\000\008\000\020\000\024\000\
\006\000\003\000\009\000\010\000\011\000\019\000\009\000\023\000\
\008\000\019\000\012\000\023\000\008\000\013\000\010\000\011\000\
\014\000\000\000\010\000\011\000\000\000\012\000\015\000\016\000\
\013\000\012\000\000\000\014\000\013\000\017\000\018\000\014\000\
\020\000\015\000\016\000\021\000\000\000\015\000\016\000\000\000\
\017\000\018\000\000\000\020\000\017\000\018\000\021\000\020\000\
\005\000\003\000\021\000\000\000\005\000\003\000"

let yycheck = "\001\000\
\000\000\000\000\001\001\000\000\009\000\004\001\000\000\000\000\
\007\001\018\001\000\000\001\001\000\000\000\000\004\001\020\000\
\015\001\007\001\000\000\000\000\000\000\020\001\021\001\022\001\
\019\001\015\001\000\000\019\001\023\001\000\000\020\001\021\001\
\000\000\019\001\255\255\255\255\255\255\023\001\000\000\000\000\
\255\255\001\001\255\255\255\255\004\001\000\000\000\000\007\001\
\000\000\255\255\255\255\000\000\255\255\000\000\000\000\015\001\
\004\000\005\000\006\000\007\000\255\255\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\017\000\255\255\255\255\
\255\255\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\003\001\004\001\005\001\
\006\001\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\019\001\003\001\004\001\
\005\001\006\001\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\003\001\004\001\005\001\
\006\001\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\008\001\
\016\001\017\001\008\001\019\001\008\001\008\001\019\001\023\001\
\023\001\019\001\019\001\008\001\008\001\019\001\023\001\019\001\
\019\001\023\001\008\001\023\001\023\001\008\001\019\001\019\001\
\008\001\255\255\023\001\023\001\255\255\019\001\008\001\008\001\
\019\001\023\001\255\255\019\001\023\001\008\001\008\001\023\001\
\008\001\019\001\019\001\008\001\255\255\023\001\023\001\255\255\
\019\001\019\001\255\255\019\001\023\001\023\001\019\001\023\001\
\019\001\019\001\023\001\255\255\023\001\023\001"

let yynames_const = "\
  EOF\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  NOT\000\
  AND\000\
  OR\000\
  ASSIGN\000\
  SEMI\000\
  RETURN\000\
  LBRACE\000\
  RBRACE\000\
  NL\000\
  "

let yynames_block = "\
  INT\000\
  CHAR\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 83 "parse.mly"
             ( _1 )
# 272 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    Obj.repr(
# 84 "parse.mly"
                            ( _2 )
# 279 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parse.mly"
  ( (Ast.skip, 0) )
# 285 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 121 "parse.mly"
                 (
                 Printf.printf "Math return ";
                     let res_rstmt : rstmt = Return(Int _2, 0) in
                       (res_rstmt, 0)
                 )
# 296 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parse.mly"
               (
               Printf.printf "Math expr";
                 let res_rstmt : rstmt = Exp(Int _1,0) in
                 (res_rstmt, 0)
               )
# 307 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 133 "parse.mly"
                  (
                  Printf.printf "Math stmt ; stmt";
                    let stmt1 : stmt =  _1 in
                    let stmt2 : stmt =  _3 in
                    let res_rstmt : rstmt = Seq(stmt1, stmt2) in
                    (res_rstmt, 0)
                  )
# 321 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 143 "parse.mly"
        (_1)
# 328 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parse.mly"
                     (_1 + _3)
# 336 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parse.mly"
                 ( -_2)
# 343 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parse.mly"
                        (_1 - _3)
# 351 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parse.mly"
                        (_1 * _3)
# 359 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parse.mly"
                      (_1 / _3)
# 367 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parse.mly"
                     (binop _1 Eq _3 )
# 375 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parse.mly"
                      (binop _1 Neq _3 )
# 383 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parse.mly"
                     (binop _1 Lt _3 )
# 391 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parse.mly"
                      (binop _1 Lte _3 )
# 399 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parse.mly"
                     (binop _1 Gt _3)
# 407 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parse.mly"
                      (binop _1 Gte _3 )
# 415 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "parse.mly"
                 (bool2int ( _2 = 0) )
# 422 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parse.mly"
                      ( bool2int ((int2bool _1) && (int2bool _3)) )
# 430 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parse.mly"
                     ( bool2int ((int2bool _1) || (int2bool _3)) )
# 438 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "parse.mly"
                           (_2)
# 445 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parse.mly"
                        ( assign _1 _3; 0)
# 453 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "parse.mly"
            (Printf.printf "Math var" ;lookup _1)
# 460 "parse.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
