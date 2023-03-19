(* Compile Cish AST to MIPS AST *)
(* Reserve R2 as stack pointer*)
open Mips
open Ast
open Word32
exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }
type env = (string * int) list
type exp_result =
  { code : Mips.inst list;
    reg : reg }

type stmt_result =
  { code : Mips.inst list;
    env : env
   }
(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))
let rec lookup x env =
  match env with
  | [] -> raise (Failure ("Variable " ^ x ^ " not found"))
  | (y, v)::tl ->
    if x = y then v else lookup x tl

let rec compile_exp ((re:rexp),(pos:int)) (env:env) : exp_result =
  match re with
  | Ast.Int n ->
      { code = [Mips.Li (R0, (fromInt n) )];
        reg = R0 }
  | Ast.Var x ->
      let offset = lookup x env in
      { code = [Mips.Lw (R0, R2, (fromInt offset) )];
        reg = R0 }
  | Ast.Binop (e1, op, e2) ->
      let r1 = compile_exp e1 env in
      let r2 = compile_exp e2 env in
      let op_mips = match op with
                    | Ast.Plus -> Mips.Add
                    | Ast.Minus -> Mips.Sub
                    | Ast.Times -> Mips.Mul
                    | Ast.Div -> Mips.Div in
      let code = r1.code @ [Mips.Move (R1, r1.reg)] @
                 r2.code @ [Mips.Move (R2, r2.reg)] @
                 [op_mips (R0, R1, R2)] in
      { code = code;
        reg = R0 }

let rec compile_stmt (s:Ast.stmt) (env:env) : stmt_result =
  match s with
  | Ast.Assign (x, e) ->
      let r = compile_exp e env in
      let offset = lookup x env in
      { code = r.code @ [Mips.Sw (r.reg, R2, offset)];
        env = env }
  | Ast.Seq (s1, s2) ->
      let r1 = compile_stmt s1 env in
      let r2 = compile_stmt s2 r1.env in
      { code = r1.code @ r2.code;
        env = r2.env }
  | Ast.If (e, s1, s2) ->
      let r = compile_exp e env in
      let else_label = new_label() in
      let end_label = new_label() in
      let code = r.code @ [Mips.Beqz (r.reg, else_label)] @
                 (compile_stmt s1 env).code @
                 [Mips.J end_label; Mips.Label else_label] @
                 (compile_stmt s2 env).code @
                 [Mips.Label end_label] in
      { code = code;
        env = env }
  | Ast.While (e, s) ->
      let loop_label = new_label() in
      let end_label = new_label() in
      let r = compile_exp e env in
      let code = [Mips.Label loop_label] @ r.code @
                 [Mips.Beqz (r.reg, end_label)] @
                 (compile_stmt s env).code @
                 [Mips.J loop_label; Mips.Label end_label] in
      { code = code;
        env = env }
  | Ast.Print e ->
      let r = compile_exp e env in
      { code = r.code @ [Mips.Jal "print"; Mips.Nop];
        env = env }

let rec compile (p : program) : result =
  let rec build_env vars =
    match vars with
    | [] -> []
    | (x, _)::xs -> (x, 0)::(build_env xs)
  in
  let env = build_env p.vars in
  let rec compile_funs funs =
    match funs with
    | [] -> []
    | (name, args, stmt)::xs ->
        let label = new_label () in
        let new_env = (build_env args) @ env in
        let stmt_code = compile_stmt stmt new_env in
        let func_code = [
          Label label;
          Move (Reg V0, Reg A0);
          Sub (Reg SP, Reg SP, Imm (List.length args * 4));
          Move (Reg R0, Reg SP);
          Jmp (Reg RA);
          Nop
        ] in
        stmt_code @ func_code @ (compile_funs xs)
  in
  let funs_code = compile_funs p.funs in
  let stmt_code = compile_stmt p.body env in
  {
    code = [
      Text;
      Globl "main";
      Label "main";
      Sub (Reg SP, Reg SP, Imm (List.length p.vars * 4));
      Move (Reg T0, Reg SP)
    ] @ funs_code @ stmt_code @ [
      Move (Reg A0, Reg V0);
      Add (Reg SP, Reg T0, Imm (List.length p.vars * 4));
      Jr (Reg RA);
      Nop
    ];
    data = []
  }

let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    let readfile f =
      let stream = open_in f in
      let size = in_channel_length stream in
      let text = Bytes.create size in
      let _ = really_input stream text 0 size in
		  let _ = close_in stream in 
      text in
	  let debugcode = readfile "print.asm" in
	    "\t.text\n" ^
	    "\t.align\t2\n" ^
	    "\t.globl main\n" ^
	    (String.concat "" strs) ^
	    "\n\n" ^
	    "\t.data\n" ^
	    "\t.align 0\n"^
	    (String.concat "" (List.map vaR8decl data)) ^
	    "\n" ^
	    Bytes.to_string debugcode
