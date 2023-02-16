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
# 54 "parse.ml"
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
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\003\000\002\000\004\000\001\000\001\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\023\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\002\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000"

let yysindex = "\255\255\
\012\255\000\000\000\000\158\255\158\255\158\255\158\255\243\254\
\065\255\000\000\006\000\078\255\144\255\129\255\144\255\095\255\
\158\255\240\254\000\000\158\255\158\255\158\255\158\255\158\255\
\158\255\158\255\158\255\158\255\158\255\158\255\158\255\000\000\
\000\000\000\000\112\255\008\000\144\255\144\255\144\255\144\255\
\144\255\144\255\144\255\144\255\144\255\144\255\144\255\144\255\
\000\000\000\000"

let yyrindex = "\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\248\254\000\000\000\000\012\000\001\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\004\000\009\000\010\000\
\011\000\023\000\028\000\029\000\030\000\031\000\036\000\037\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\013\000\034\000"

let yytablesize = 316
let yytable = "\001\000\
\010\000\020\000\009\000\011\000\017\000\019\000\036\000\050\000\
\012\000\013\000\014\000\007\000\003\000\003\000\003\000\004\000\
\000\000\000\000\005\000\000\000\000\000\018\000\015\000\000\000\
\000\000\000\000\006\000\016\000\017\000\018\000\019\000\007\000\
\008\000\009\000\000\000\021\000\022\000\013\000\014\000\015\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\000\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\048\000\003\000\000\000\000\000\004\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\020\000\021\000\022\000\023\000\007\000\008\000\024\000\025\000\
\026\000\027\000\028\000\029\000\000\000\030\000\031\000\000\000\
\032\000\020\000\021\000\022\000\023\000\000\000\000\000\024\000\
\025\000\026\000\027\000\028\000\029\000\000\000\030\000\031\000\
\000\000\034\000\020\000\021\000\022\000\023\000\000\000\000\000\
\024\000\025\000\026\000\027\000\028\000\029\000\000\000\030\000\
\031\000\000\000\049\000\020\000\021\000\022\000\023\000\000\000\
\033\000\024\000\025\000\026\000\027\000\028\000\029\000\000\000\
\030\000\031\000\020\000\021\000\022\000\023\000\000\000\000\000\
\024\000\025\000\026\000\027\000\028\000\029\000\003\000\030\000\
\031\000\004\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
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
\010\000\020\000\009\000\011\000\000\000\000\000\000\000\000\000\
\012\000\013\000\014\000\010\000\020\000\009\000\011\000\010\000\
\020\000\009\000\011\000\012\000\013\000\014\000\015\000\012\000\
\013\000\014\000\007\000\016\000\017\000\018\000\019\000\000\000\
\000\000\015\000\000\000\021\000\022\000\015\000\016\000\017\000\
\018\000\019\000\016\000\017\000\018\000\019\000\021\000\022\000\
\000\000\000\000\021\000\022\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\018\001\000\000\023\001\000\000\
\000\000\000\000\000\000\000\000\001\001\000\000\023\001\004\001\
\255\255\255\255\007\001\255\255\255\255\009\000\000\000\255\255\
\255\255\255\255\015\001\000\000\000\000\000\000\000\000\020\001\
\021\001\022\001\255\255\000\000\000\000\004\000\005\000\006\000\
\007\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\017\000\255\255\255\255\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\001\001\255\255\255\255\004\001\255\255\255\255\007\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\003\001\004\001\005\001\006\001\020\001\021\001\009\001\010\001\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\019\001\003\001\004\001\005\001\006\001\255\255\255\255\009\001\
\010\001\011\001\012\001\013\001\014\001\255\255\016\001\017\001\
\255\255\019\001\003\001\004\001\005\001\006\001\255\255\255\255\
\009\001\010\001\011\001\012\001\013\001\014\001\255\255\016\001\
\017\001\255\255\019\001\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\003\001\004\001\005\001\006\001\255\255\255\255\
\009\001\010\001\011\001\012\001\013\001\014\001\001\001\016\001\
\017\001\004\001\255\255\255\255\007\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\015\001\255\255\255\255\255\255\
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
\008\001\008\001\008\001\008\001\255\255\255\255\255\255\255\255\
\008\001\008\001\008\001\019\001\019\001\019\001\019\001\023\001\
\023\001\023\001\023\001\019\001\019\001\019\001\008\001\023\001\
\023\001\023\001\023\001\008\001\008\001\008\001\008\001\255\255\
\255\255\019\001\255\255\008\001\008\001\023\001\019\001\019\001\
\019\001\019\001\023\001\023\001\023\001\023\001\019\001\019\001\
\255\255\255\255\023\001\023\001"

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
# 69 "parse.mly"
             ( _1 )
# 252 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    Obj.repr(
# 70 "parse.mly"
                            ( _2 )
# 259 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parse.mly"
  ( (Ast.skip, 0) )
# 265 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parse.mly"
      (
      let res_rstmt : rstmt = Return(Int _2, 0) in
        (res_rstmt, 0)
      )
# 275 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parse.mly"
        (
          let res_rstmt : rstmt = Exp(Int _1,0) in
          (res_rstmt, 0)
        )
# 285 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "parse.mly"
    (
        let r_val : rexp = Int(_3) in
        let r_exp : exp = (r_val, 0) in
        let res_var = Var(_1) in
        let res_assign : rexp = Assign(_1, r_exp) in
      let res_rstmt : rstmt = Exp(res_assign,0) in
            (res_rstmt, 0)
    )
# 300 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parse.mly"
    (
              let res_rstmt : rstmt = Exp(Int _1,0) in
              (res_rstmt, 0)
            )
# 310 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "parse.mly"
        (_1)
# 317 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parse.mly"
                     (_1 + _3)
# 325 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parse.mly"
                 ( -_2)
# 332 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parse.mly"
                        (_1 - _3)
# 340 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parse.mly"
                        (_1 * _3)
# 348 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parse.mly"
                      (_1 / _3)
# 356 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parse.mly"
                     (binop _1 Eq _3 )
# 364 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parse.mly"
                      (binop _1 Neq _3 )
# 372 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parse.mly"
                     (binop _1 Lt _3 )
# 380 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parse.mly"
                      (binop _1 Lte _3 )
# 388 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parse.mly"
                     (binop _1 Gt _3)
# 396 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parse.mly"
                      (binop _1 Gte _3 )
# 404 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parse.mly"
                 (bool2int ( _2 = 0) )
# 411 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parse.mly"
                      ( bool2int ((int2bool _1) && (int2bool _3)) )
# 419 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parse.mly"
                     ( bool2int ((int2bool _1) || (int2bool _3)) )
# 427 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 119 "parse.mly"
                           (_2)
# 434 "parse.ml"
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
