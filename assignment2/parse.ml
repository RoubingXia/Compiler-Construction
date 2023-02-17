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
  | IF
  | ELSE

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
# 69 "parse.ml"
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
  280 (* IF *);
  281 (* ELSE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* CHAR *);
  277 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\003\000\003\000\003\000\010\000\001\000\
\001\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\003\000\003\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000"

let yysindex = "\021\000\
\092\255\000\000\000\000\037\255\037\255\037\255\037\255\245\254\
\102\255\037\255\000\000\003\000\171\255\171\255\156\255\171\255\
\139\255\037\255\102\255\237\254\124\255\000\000\102\255\037\255\
\037\255\037\255\037\255\037\255\037\255\037\255\037\255\037\255\
\037\255\037\255\037\255\000\000\000\000\171\255\242\254\024\000\
\102\255\007\255\171\255\171\255\171\255\171\255\171\255\171\255\
\171\255\171\255\171\255\171\255\171\255\171\255\000\000\000\000\
\247\254\002\255\006\255\102\255\252\254\102\255\007\255"

let yyrindex = "\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\254\254\000\000\000\000\000\000\006\000\008\000\000\000\013\000\
\000\000\000\000\254\254\000\000\000\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\010\255\
\254\254\011\000\020\000\025\000\030\000\037\000\042\000\047\000\
\049\000\054\000\059\000\066\000\071\000\076\000\000\000\000\000\
\000\000\000\000\000\000\254\254\000\000\035\000\023\000"

let yygindex = "\000\000\
\000\000\249\255\057\000"

let yytablesize = 355
let yytable = "\023\000\
\026\000\020\000\022\000\040\000\023\000\008\000\018\000\011\000\
\055\000\023\000\006\000\039\000\021\000\058\000\023\000\042\000\
\003\000\025\000\062\000\010\000\003\000\001\000\007\000\056\000\
\012\000\023\000\059\000\060\000\005\000\013\000\000\000\003\000\
\000\000\057\000\003\000\000\000\014\000\003\000\000\000\000\000\
\004\000\015\000\000\000\005\000\000\000\000\000\016\000\000\000\
\017\000\000\000\000\000\006\000\061\000\018\000\063\000\000\000\
\000\000\008\000\019\000\000\000\014\000\015\000\016\000\017\000\
\000\000\020\000\021\000\000\000\000\000\000\000\022\000\000\000\
\000\000\000\000\038\000\023\000\000\000\000\000\000\000\000\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\003\000\000\000\000\000\004\000\
\000\000\000\000\005\000\000\000\000\000\000\000\003\000\000\000\
\000\000\004\000\006\000\000\000\005\000\000\000\000\000\007\000\
\008\000\009\000\000\000\010\000\006\000\000\000\000\000\000\000\
\000\000\007\000\008\000\019\000\000\000\010\000\024\000\025\000\
\026\000\027\000\000\000\000\000\028\000\029\000\030\000\031\000\
\032\000\033\000\000\000\034\000\035\000\024\000\025\000\026\000\
\027\000\041\000\000\000\028\000\029\000\030\000\031\000\032\000\
\033\000\000\000\034\000\035\000\000\000\037\000\024\000\025\000\
\026\000\027\000\000\000\036\000\028\000\029\000\030\000\031\000\
\032\000\033\000\000\000\034\000\035\000\024\000\025\000\026\000\
\027\000\000\000\000\000\028\000\029\000\030\000\031\000\032\000\
\033\000\000\000\034\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\026\000\026\000\026\000\000\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\011\000\
\026\000\026\000\000\000\026\000\021\000\023\000\026\000\026\000\
\008\000\025\000\011\000\010\000\008\000\011\000\011\000\021\000\
\012\000\006\000\021\000\021\000\025\000\013\000\010\000\025\000\
\025\000\010\000\010\000\012\000\014\000\007\000\012\000\012\000\
\013\000\015\000\003\000\013\000\013\000\003\000\016\000\014\000\
\017\000\003\000\014\000\014\000\015\000\018\000\000\000\015\000\
\015\000\016\000\019\000\017\000\016\000\016\000\017\000\017\000\
\018\000\020\000\000\000\018\000\018\000\019\000\022\000\000\000\
\019\000\019\000\000\000\023\000\020\000\000\000\000\000\020\000\
\020\000\022\000\000\000\000\000\022\000\022\000\023\000\000\000\
\000\000\023\000\023\000"

let yycheck = "\019\001\
\000\000\009\000\000\000\023\001\019\001\000\000\018\001\000\000\
\023\001\019\001\000\000\019\000\000\000\023\001\019\001\023\000\
\019\001\000\000\023\001\000\000\023\001\001\000\000\000\000\000\
\000\000\019\001\025\001\022\001\019\001\000\000\255\255\000\000\
\255\255\041\000\000\000\255\255\000\000\001\001\255\255\255\255\
\004\001\000\000\255\255\007\001\255\255\255\255\000\000\255\255\
\000\000\255\255\255\255\015\001\060\000\000\000\062\000\255\255\
\255\255\021\001\000\000\255\255\004\000\005\000\006\000\007\000\
\255\255\000\000\010\000\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\018\000\000\000\255\255\255\255\255\255\255\255\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\001\001\255\255\255\255\004\001\
\255\255\255\255\007\001\255\255\255\255\255\255\001\001\255\255\
\255\255\004\001\015\001\255\255\007\001\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\015\001\255\255\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\024\001\003\001\004\001\
\005\001\006\001\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\003\001\004\001\005\001\
\006\001\022\001\255\255\009\001\010\001\011\001\012\001\013\001\
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
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\008\001\
\016\001\017\001\255\255\019\001\008\001\019\001\022\001\023\001\
\019\001\008\001\019\001\008\001\023\001\022\001\023\001\019\001\
\008\001\023\001\022\001\023\001\019\001\008\001\019\001\022\001\
\023\001\022\001\023\001\019\001\008\001\023\001\022\001\023\001\
\019\001\008\001\019\001\022\001\023\001\019\001\008\001\019\001\
\008\001\023\001\022\001\023\001\019\001\008\001\255\255\022\001\
\023\001\019\001\008\001\019\001\022\001\023\001\022\001\023\001\
\019\001\008\001\255\255\022\001\023\001\019\001\008\001\255\255\
\022\001\023\001\255\255\008\001\019\001\255\255\255\255\022\001\
\023\001\019\001\255\255\255\255\022\001\023\001\019\001\255\255\
\255\255\022\001\023\001"

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
  IF\000\
  ELSE\000\
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
# 284 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    Obj.repr(
# 84 "parse.mly"
                            ( _2 )
# 291 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parse.mly"
  ( (Ast.skip, 0) )
# 297 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 121 "parse.mly"
         (
         Printf.printf "Math return \n";
             let res_rstmt : rstmt = Return(Int _2, 0) in
               (res_rstmt, 0)
         )
# 308 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 126 "parse.mly"
                      (
 _2

 )
# 318 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 131 "parse.mly"
          (
          Printf.printf "Math stmt ; stmt\n";
            let stmt1 : stmt =  _1 in
            let stmt2 : stmt =  _3 in
            let res_rstmt : rstmt = Seq(stmt1, stmt2) in
            (res_rstmt, 0)
          )
# 332 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : Ast.stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 139 "parse.mly"
 (
    Printf.printf "Match if 1 \n";
    let if_exp : exp = (Int _2,0) in
    let if_rstmt : rstmt = If(if_exp, _4, _8) in
    let stmt1 : stmt =  (if_rstmt, 0) in
    let stmt2 : stmt =  _10 in
    let res_rstmt : rstmt = Seq(stmt1, stmt2) in
    (res_rstmt, 0)
 )
# 350 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parse.mly"
       (
       Printf.printf "Math expr";
         let res_rstmt : rstmt = Exp(Int _1,0) in
         (res_rstmt, 0)
       )
# 361 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 156 "parse.mly"
        (_1)
# 368 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parse.mly"
                     (_1 + _3)
# 376 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parse.mly"
                 ( -_2)
# 383 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parse.mly"
                        (_1 - _3)
# 391 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parse.mly"
                        (_1 * _3)
# 399 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "parse.mly"
                      (_1 / _3)
# 407 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parse.mly"
                     (binop _1 Eq _3 )
# 415 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parse.mly"
                      (binop _1 Neq _3 )
# 423 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parse.mly"
                     (binop _1 Lt _3 )
# 431 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parse.mly"
                      (binop _1 Lte _3 )
# 439 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parse.mly"
                     (binop _1 Gt _3)
# 447 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parse.mly"
                      (binop _1 Gte _3 )
# 455 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parse.mly"
                 (bool2int ( _2 = 0) )
# 462 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "parse.mly"
                      ( bool2int ((int2bool _1) && (int2bool _3)) )
# 470 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parse.mly"
                     ( bool2int ((int2bool _1) || (int2bool _3)) )
# 478 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 171 "parse.mly"
                           (_2)
# 485 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parse.mly"
                        ( assign _1 _3; 0)
# 493 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 173 "parse.mly"
            (Printf.printf "Math var" ;lookup _1)
# 500 "parse.ml"
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
