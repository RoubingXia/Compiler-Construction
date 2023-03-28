(* Compile Cish AST to MIPS AST *)
open Mips
open Ast
open Word32
exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of registers -- Ocaml Set and Set.S *)
(* create a hash set *)
(* create a hash set with capacity 30 *)
let hash_set = Hashtbl.create 30

(* function to add an integer to the set *)
let add_to_set x =
  Hashtbl.add hash_set x ()

(* function to remove an integer from the set *)
let remove_from_set x =
  Hashtbl.remove hash_set x

(* function to check if an integer is in the set *)
let check_set x =
  Hashtbl.mem hash_set x

(* function to generate a random number from 1 to 30 that is not in the set *)
let random_not_in_set () : int =
  let rec loop () =
    let r = Random.int 25 in
    if  r <> 4 && r <> 0 && r <> 1 && not (check_set r) then r
    else loop ()
  in loop ()


(*Function name and it's parameter name*)
module FuncMap = Map.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

let functions : Ast.var list FuncMap.t ref = ref FuncMap.empty

let add_fun f_name f_args m_map =
    FuncMap.add f_name f_args m_map
let get_args f_name m_map =
    try FuncMap.find f_name m_map
      with Not_found ->
        (*Printf.printf "Function %s not found in map\n" f_name;*)
        []

let functions_to_string () =
  let buf = Buffer.create 1024 in
  FuncMap.iter (fun key value ->
    Buffer.add_string buf (key ^ ":\n");
    List.iter (fun arg -> Buffer.add_string buf ("\t" ^ arg ^ "\n")) value;
    Buffer.add_char buf '\n'
  ) !functions;
  Buffer.contents buf

let rec string_of_explist exprs =
  match exprs with
  | [] -> ""
  | [x] -> string_of_exp x
  | x::xs -> string_of_exp x ^ ", " ^ string_of_explist xs

and string_of_exp (e,_) =
  match e with
  | Int n -> string_of_int n
  | Var s -> s
  | Binop(e1,op,e2) ->
      string_of_exp e1 ^ " " ^
      (match op with
       | Plus -> "+"
       | Minus -> "-"
       | Times -> "*"
       | Div -> "/ "
       | Eq -> "=="
       | Neq -> "!="
       | Lt -> "<"
       | Lte -> "<="
       | Gt -> ">"
       | Gte -> ">=") ^ " " ^
      string_of_exp e2
  | Not e -> "!" ^ string_of_exp e
  | And(e1,e2) -> string_of_exp e1 ^ " && " ^ string_of_exp e2
  | Or(e1,e2) -> string_of_exp e1 ^ " || " ^ string_of_exp e2
  | Assign(v,e) -> v ^ " = " ^ string_of_exp e
  | Call(f,args) -> f ^ "(" ^ string_of_explist args ^ ")"

  (*
module VarMap = Map.Make(struct
                           type t = string
                           let compare = String.compare
                         end)


type var_info = {
  var_map: int VarMap.t;
  offset: int;
}

let variables = ref { var_map = VarMap.empty; offset = 42 }

let add_var var var_info =
    let new_offset = var_info.offset + 4 in
    let new_map = VarMap.add var new_offset var_info.var_map in
    {var_map = new_map; offset = new_offset}

let get_offset var var_info =
    VarMap.find var var_info.var_map

let getInst var offset =
    ()
VarMap.iter getInst !variables.var_map

variables := add_var "NYU" !variables;
variables := add_var "NYC" !variables;
variables := add_var "HAHA" !variables;
get_offset "NYC" !variables
let variables : var_info ref = ref empty_var_map

let add_var var map =
  let offset = match VarMap.max_binding_opt map with
    | None -> 0
    | Some (_, {offset}) -> offset + 4
  in
  VarMap.add var { name = var; offset } map

*)
(*variable maps, store local variable and their offset*)
module VarMap = Map.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)


type var_info = {
  var_map: int VarMap.t;
  offset: int;
}

let variables = ref { var_map = VarMap.empty; offset = 0 } (*var at higher address, registers at lower address*)

let num_variables vars =
  VarMap.cardinal vars.var_map

let string_of_variables vars =
  let var_strs =
    VarMap.fold
      (fun var num acc -> (var ^ " -> " ^ string_of_int num) :: acc)
      vars.var_map
      []
  in
  let offset_str = "Offset: " ^ string_of_int vars.offset in
  String.concat "\n" (var_strs @ [offset_str])

let add_var var var_info =
    let new_offset = var_info.offset + 4 in
    let new_map = VarMap.add var new_offset var_info.var_map in
    {var_map = new_map; offset = new_offset}

let get_offset var var_info =
    VarMap.find var var_info.var_map

let get_size var_info =
    var_info.offset

(* function to map a string to a reg variant *)
let reg_of_string (s: string) : reg =
  match s with
  | "R0" ->  R0
  | "R1" ->  R1
  | "R2" ->  R2
  | "R3" ->  R3
  | "R4" ->  R4
  | "R5" ->  R5
  | "R6" ->  R6
  | "R7" ->  R7
  | "R8" ->  R8
  | "R9" ->  R9
  | "R10" ->  R10
  | "R11" ->  R11
  | "R12" ->  R12
  | "R13" ->  R13
  | "R14" ->  R14
  | "R15" ->  R15
  | "R16" ->  R16
  | "R17" ->  R17
  | "R18" ->  R18
  | "R19" ->  R19
  | "R20" ->  R20
  | "R21" ->  R21
  | "R22" ->  R22
  | "R23" ->  R23
  | "R24" ->  R24
  | "R25" ->  R25
  | "R26" ->  R26
  | "R27" ->  R27
  | "R28" ->  R28
  | "R29" ->  R29
  | "R30" ->  R30
  | "R31" ->  R31


(* function to map a reg variant to an integer *)
let reg_to_int (r: reg) : int =
  match r with
  | R0 -> 0
  | R1 -> 1
  | R2 -> 2
  | R3 -> 3
  | R4 -> 4
  | R5 -> 5
  | R6 -> 6
  | R7 -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15
  | R16 -> 16
  | R17 -> 17
  | R18 -> 18
  | R19 -> 19
  | R20 -> 20
  | R21 -> 21
  | R22 -> 22
  | R23 -> 23
  | R24 -> 24
  | R25 -> 25
  | R26 -> 26
  | R27 -> 27
  | R28 -> 28
  | R29 -> 29
  | R30 -> 30
  | R31 -> 31

(* generate a fresh temporary register number and store it in the register set. *)
let rec new_reg() =
    let number = random_not_in_set() in
    let r = "R" ^ (string_of_int (number)) in
    add_to_set number;
    (reg_of_string r)

let release_reg (r : reg) =
    let number = reg_to_int r in
    (remove_from_set number)

(*For variable*)
module Env = struct
  type t = (string, int) Hashtbl.t
  let create () = Hashtbl.create 32 (* initial capacity *)
  let add env key value = Hashtbl.add env key value
  let find env key = Hashtbl.find env key
end

(*Store R31/$ra R30/$fp to stack*)
let prologue var_size =
(*allocate memory size, move $sp/R29, need to store registers still used by caller*)
    (*calculet the size of variables used in this function *)
    (*failwith ("var_size is "^(string_of_int var_size));*)
    let size = 14 * 4 + var_size in
    let tr1 = new_reg() in
    let allocate_inst = [Add(tr1, R0, Immed(fromInt size)); Sub(R29, R29, tr1)] in
    let store_inst = [Sw(R31, R29, (fromInt 0));
    Sw(R30, R29, (fromInt 4)); Sw(R16, R29, (fromInt (4 * 2)));
    Sw(R17, R29, (fromInt (4 * 3)));  Sw(R18, R29, (fromInt (4 * 4)));
    Sw(R19, R29, (fromInt (4 * 5))); Sw(R20, R29, (fromInt (4 * 6)));
    Sw(R21, R29, (fromInt (4 * 7))); Sw(R22, R29, (fromInt (4 * 8)));
    Sw(R23, R29, (fromInt (4 * 9)));  Sw(R4, R29, (fromInt (4 * 10)));
    Sw(R5, R29, (fromInt (4 * 11))); Sw(R6, R29, (fromInt (4 * 12)));
    Sw(R7, R29, (fromInt (4 * 13)))] in
    (*Set fp to sp + size*)
    release_reg tr1;
    allocate_inst @ store_inst @ [Add(R30, R29, Immed((fromInt size)))]
    (*Store variables, needed??*)


(*Place the result in $v0 ($r2).
  2. Restore the callee-saved registers saved in
  the prologue (including caller's frame pointer
  and the return address.)
  3. Pop the stack frame by adding the frame size
  (n) to $sp.
  4. Return by jumping to the return address.*)
let epilogue var_size =
    let size = 14 * 4 + var_size in
    let tr1 = new_reg() in
    let load_inst = [Lw(R31, R29, (fromInt 0));
    Lw(R30, R29, (fromInt 4)); Lw(R16, R29, (fromInt (4 * 2)));
    Lw(R17, R29, (fromInt (4 * 3)));  Lw(R18, R29, (fromInt (4 * 4)));
    Lw(R19, R29, (fromInt (4 * 5))); Lw(R20, R29, (fromInt (4 * 6)));
    Lw(R21, R29, (fromInt (4 * 7))); Lw(R22, R29, (fromInt (4 * 8)));
    Lw(R23, R29, (fromInt (4 * 9)));  Lw(R4, R29, (fromInt (4 * 10)));
    Lw(R5, R29, (fromInt (4 * 11))); Lw(R6, R29, (fromInt (4 * 12)));
    Lw(R7, R29, (fromInt (4 * 13)))] in

    release_reg tr1;
    load_inst @ [Add(R29, R29, Immed(fromInt size))] (*pop sp*)


let prologue_debug =
(*allocate memory size 10, move $sp/R29*)
    (*calculet the size of variables used in this function *)
    let size = 4  in
    let curOff = 0 in
    let tr1 = new_reg() in
    let allocate_inst = [Add(tr1, R0, Immed(fromInt size)); Sub(R29, R29, tr1)] in
    let store_inst = [Sw(R31, R29, (fromInt 0))] in
    (*Set fp to sp + size*)
    allocate_inst @ store_inst @ [Add(tr1, R0, Immed(fromInt size)); Sub(R30, R29, tr1)]
    (*Store variables, needed??*)


(*Place the result in $v0 ($r2).
  2. Restore the callee-saved registers saved in
  the prologue (including caller's frame pointer
  and the return address.)
  3. Pop the stack frame by adding the frame size
  (n) to $sp.
  4. Return by jumping to the return address.*)
let epilogue_debug =
    let size =  4  in
    let restore_inst = [Lw(R31, R29, (fromInt 0))] in
    (*Set fp to sp + size*)
    restore_inst @ [Add(R29, R29, Immed(fromInt size))] (*pop sp*)

let get_rstmt ((s:rstmt),(pos:int)) : rstmt =
    s


let collect_single_var (v : Ast.var) : unit =
    let var = "prefix_"^v in
    variables := add_var var !variables


let rec collect_vars2 ((e:rexp),(pos:int)) : unit =
  match e with
  | Var(v) ->
        let var = "prefix_"^v in
        variables := add_var var !variables
  | Assign(v, expr) ->
        let var = "prefix_"^v in
        variables := add_var var !variables;
        collect_vars2 expr
  | _ -> ()
let rec collect_vars (p : Ast.stmt) : unit =
  let rstatment = get_rstmt p in
  match rstatment with
  | Seq(m_stmt1, m_stmt2) ->
    collect_vars m_stmt1;  collect_vars m_stmt2;
  | If(_, s1, s2) ->
    collect_vars s1;
    collect_vars s2
  | While(_, s) ->
    collect_vars s
  | Exp(expr) -> collect_vars2 expr
  | Let(v, expr, s1) ->
    let var = "prefix_"^v in
    variables := add_var var !variables;
    collect_vars2 expr;
    collect_vars s1
  | For(expr1, _, _, s4) ->
    let s1 = (Exp(expr1),0) in
    collect_vars s1;
    collect_vars s4
  | _ ->
    ()
(*
let rec compile_expr2 ((re:rexp),(pos:int), (v_name: string)) : inst list =
   match re with
   | Int(n) ->
       [Li(R2, (fromInt n))]
   | Assign(x, e) ->
           let tr1 = new_reg() in
           let v = "prefix_"^x in
           let offset = get_offset v !variables in
           let res_inst = (compile_expr e) @  [Add(tr1, R0, Immed(fromInt offset)); Sub(tr1, R30, tr1);Sw(R2, tr1, (fromInt 0))] in
           release_reg tr1;
           res_inst
   | Var(vn) ->
         let v = "prefix_"^vn in
         let offset = get_offset v !variables in
         let tr1 = new_reg() in
         (*copy the value of this var to R2*)
         let res_inst = [Add(tr1, R0, Immed(fromInt offset)); Sub(tr1, R30, tr1); Lw(R2, tr1, (fromInt 0))] in
         release_reg tr1;
         res_inst
   | Call(var, list_expr) ->
        let tr1 = new_reg() in
        let compiled_args = List.concat (List.map2 (fun (rexp,pos), v_name -> compile_expr2 (rexp, pos) v_name) list_expr list_var) in
        let res_inst = compiled_args @ prologue @ [Add(R4, R2, Immed((fromInt 0))); Jal(var)] @ epilogue in
        release_reg tr1;
        res_inst
  | And(r1, r2) ->
      let tr1 = new_reg() in
      let tr2 = new_reg() in
      let res_inst = (compile_expr r1) @ [Add(tr1, R2, Immed((fromInt 0)))] @ (*move R2 to tr1*)
      (compile_expr r2) @ [Add(tr2, R2, Immed((fromInt 0)))] @ (*move R2 to tr2*)
      [And(R2, tr1, Reg(tr2))] in
      release_reg tr1;
      release_reg tr2;
      res_inst
  | Or(r1, r2) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let tr3 = new_reg() in
       let res_inst = (compile_expr r1) @ [Add(tr1, R2, Immed((fromInt 0)))] @ (*move R2 to R3*)
      (compile_expr r2) @ [Add(tr2, R2, Immed((fromInt 0)))] @ (*move R2 to R4*)
      [Li(tr3, (fromInt 0)); Sne(tr1, tr1, tr3); Sne(tr2, tr2, tr3)] @ [Or(R2, tr1, Reg(tr2))] in (*chekc (r1 != 0) || (r2 != 0)*)
      release_reg tr1;
      release_reg tr2;
        release_reg tr3;
        res_inst
  | Not(r) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let res_inst = (compile_expr r) @ [Add(tr1, R2, Immed((fromInt 0))); Li(tr2, (fromInt 0)); Seq(R2, tr1, tr2)] in (*move R2 to R3, R2 = R3 xor 1; should I consider conflict between registers?*)
       release_reg tr1;
       release_reg tr2;
       res_inst
  | Binop(e1, op, e2) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let res_inst = (compile_expr e1) @ [Add(tr1, R2, Immed((fromInt 0))) ] @
      (compile_expr e2) @ [Add(tr2, R2, Immed((fromInt 0))) ] @
      (match op with
          | Plus -> [ Add(R2, tr1, Reg(tr2)) ]
          | Minus -> [ Sub(R2, tr1, tr2) ]
          | Times -> [ Mul(R2, tr1, tr2) ]
          | Div -> [ Div(R2, tr1, tr2) ]
          | Eq -> [ Seq(R2, tr1, tr2) ]
          | Neq -> [ Sne(R2, tr1, tr2) ]
          | Lt -> [ Slt(R2, tr1, Reg(tr2)) ]
          | Lte -> [ Sle(R2, tr1, tr2) ]
          | Gt -> [ Sgt(R2, tr1, tr2) ]
          | Gte -> [ Sge(R2, tr1, tr2) ]) in
      release_reg tr1;
      release_reg tr2;
      res_inst
*)

let rec compile_expr ((re:rexp),(pos:int)) : inst list =
   match re with
   | Int(n) ->
       [Li(R2, (fromInt n))]
   | Assign(x, e) ->
           let tr1 = new_reg() in
           let v = "prefix_"^x in
           let offset = get_offset v !variables in
           let res_inst = (compile_expr e) @  [Add(tr1, R0, Immed(fromInt offset)); Sub(tr1, R30, tr1);Sw(R2, tr1, (fromInt 0))] in
           release_reg tr1;
           res_inst
   | Var(vn) ->
         let v = "prefix_"^vn in
         let offset = get_offset v !variables in
         let tr1 = new_reg() in
         (*copy the value of this var to R2*)
         let res_inst = [Add(tr1, R0, Immed(fromInt offset)); Sub(tr1, R30, tr1); Lw(R2, tr1, (fromInt 0))] in
         release_reg tr1;
         res_inst
   | Call(var, list_expr) ->
        (*let tr1 = new_reg() in*)
        (*get arg names of this function, iterate the list_expr, store every result to the corresponding variable location, hopefully *)
        let list_varnames = get_args var !functions in
        let var_size = get_size !variables in
       (* let var_log = string_of_variables !variables in
            failwith ("var_size is "^(string_of_int var_size)^ "content are : "^var_log);*)
        let len_expr = List.length list_expr in
            let len_varnames = List.length list_varnames in
            let log_msg = string_of_explist list_expr in
            if len_expr == len_varnames then
                let compiled_args = List.concat (List.map2 (fun (rexp,pos) var_name ->
                    let compiled_res = compile_expr (rexp, pos) in
                    let tr1 = new_reg() in
                       let v = "prefix_"^var_name in
                       let offset = get_offset v !variables in
                       let store_inst = [Add(tr1, R0, Immed(fromInt offset)); Sub(tr1, R29, tr1);Sw(R2, tr1, (fromInt 0))] in
                       release_reg tr1;
                       compiled_res @ store_inst
                    )  list_expr list_varnames) in
                let res_inst =  compiled_args @ (prologue var_size) @ [Add(R4, R2, Immed((fromInt 0))); Jal(var)] @ (epilogue var_size) in
                (*release_reg tr1;*)
                res_inst

            else
                let compiled_args = List.concat (List.map (fun (rexp,pos) -> compile_expr (rexp, pos)) list_expr) in
                let res_inst = compiled_args @ (prologue var_size)  @ [Add(R4, R2, Immed((fromInt 0))); Jal(var)] @ (epilogue var_size) in
                 (*release_reg tr1;*)
                res_inst
  | And(r1, r2) ->
      let tr1 = new_reg() in
      let tr2 = new_reg() in
      let res_inst = (compile_expr r1) @ [Add(tr1, R2, Immed((fromInt 0)))] @ (*move R2 to tr1*)
      (compile_expr r2) @ [Add(tr2, R2, Immed((fromInt 0)))] @ (*move R2 to tr2*)
      [And(R2, tr1, Reg(tr2))] in
      release_reg tr1;
      release_reg tr2;
      res_inst
  | Or(r1, r2) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let tr3 = new_reg() in
       let res_inst = (compile_expr r1) @ [Add(tr1, R2, Immed((fromInt 0)))] @ (*move R2 to R3*)
      (compile_expr r2) @ [Add(tr2, R2, Immed((fromInt 0)))] @ (*move R2 to R4*)
      [Li(tr3, (fromInt 0)); Sne(tr1, tr1, tr3); Sne(tr2, tr2, tr3)] @ [Or(R2, tr1, Reg(tr2))] in (*chekc (r1 != 0) || (r2 != 0)*)
      release_reg tr1;
      release_reg tr2;
        release_reg tr3;
        res_inst
  | Not(r) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let res_inst = (compile_expr r) @ [Add(tr1, R2, Immed((fromInt 0))); Li(tr2, (fromInt 0)); Seq(R2, tr1, tr2)] in (*move R2 to R3, R2 = R3 xor 1; should I consider conflict between registers?*)
       release_reg tr1;
       release_reg tr2;
       res_inst
  | Binop(e1, op, e2) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let res_inst = (compile_expr e1) @ [Add(tr1, R2, Immed((fromInt 0))) ] @
      (compile_expr e2) @ [Add(tr2, R2, Immed((fromInt 0))) ] @
      (match op with
          | Plus -> [ Add(R2, tr1, Reg(tr2)) ]
          | Minus -> [ Sub(R2, tr1, tr2) ]
          | Times -> [ Mul(R2, tr1, tr2) ]
          | Div -> [ Div(R2, tr1, tr2) ]
          | Eq -> [ Seq(R2, tr1, tr2) ]
          | Neq -> [ Sne(R2, tr1, tr2) ]
          | Lt -> [ Slt(R2, tr1, Reg(tr2)) ]
          | Lte -> [ Sle(R2, tr1, tr2) ]
          | Gt -> [ Sgt(R2, tr1, tr2) ]
          | Gte -> [ Sge(R2, tr1, tr2) ]) in
      release_reg tr1;
      release_reg tr2;
      res_inst


let rec compile_stmt ((s,_):Ast.stmt): inst list =
   match s with
   | Exp(rexp) ->
     compile_expr rexp
   | Seq(stmt1, stmt2) ->
     let insts1 = compile_stmt stmt1 in
     let insts2 = compile_stmt stmt2 in
     insts1 @ insts2
   | Let(var, expr, stmt1) ->
     let insts1 = compile_expr expr in
     let insts2 = compile_stmt stmt1 in
     let tr1 = new_reg() in
     (*store the result of expr to var*)
     let v = "prefix_"^var in
     let offset = get_offset v !variables in
     let res_inst = insts1 @ [Add(tr1, R0, Immed(fromInt offset)); Sub(tr1, R30, tr1);Sw(R2, tr1, (fromInt 0))] @ insts2 in
     release_reg tr1;
     res_inst
   | If(rexp, stmt1, stmt2) ->
        let insts1 = compile_stmt stmt1 in
        let insts2 = compile_stmt stmt2 in
        let else_label = new_label () in
        let end_label = new_label () in
        let cond_insts = compile_expr rexp in
        cond_insts @ [Blez(R2, else_label)] @ insts1 @ [J(end_label)] @ [Label(else_label)] @ insts2 @ [Label(end_label)]
   | While(rexp, stmt) ->
        let loop_label = new_label () in
        let end_label = new_label () in
        let cond_insts = compile_expr rexp in
        let body_insts = compile_stmt stmt in
        [Label(loop_label)] @ cond_insts @ [Blez(R2, end_label)] @ body_insts @ [J(loop_label)] @ [Label(end_label)]
   | For(rexp1, rexp2, rexp3, stmt) ->
        let insts1 = compile_expr rexp1 in
        let loop_label = new_label () in
        let end_label = new_label () in
        let cond_insts = compile_expr rexp2 in
        let body_insts = compile_stmt stmt in
        let step_insts = compile_expr rexp3 in
        insts1 @ [Label(loop_label)] @ cond_insts @ [Blez(R2, end_label)] @ body_insts @ step_insts @ [J(loop_label)] @ [Label(end_label)]
   | Return(rexp) ->
     let insts = compile_expr rexp in
     insts @ [Add(R2, R2, Immed((fromInt 0))); Jr(R31)]

let rec compile (p:Ast.program) : result =
    (*add printInt to map*)
  let rec compile_func (Ast.Fn {name; args; body; _}) : inst list =
    (*collect all variables, set offset*)
    (*reset variables*)
    variables = ref { var_map = VarMap.empty; offset = 0 };
    collect_vars body;
    List.iter collect_single_var args;
    (*store args list of this function to map*)
    functions := add_fun name args !functions;
    (*
    let list_varnames = get_args name !functions in
    let len_varnames = List.length list_varnames in
    let log_mesg = functions_to_string() in
    failwith ("the functions is: " ^ log_mesg ^" but the len is" ^(string_of_int len_varnames));*)
    let body_insts = compile_stmt body in
    [Label(name)] @ body_insts in
   let func_insts = List.map compile_func p in
   let func_labels = List.map (fun (Ast.Fn fs) -> fs.name) p in
  {code = List.concat func_insts; data = []}


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
