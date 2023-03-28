(* Compile Fish AST to MIPS AST *)
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

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)
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
    let r = Random.int 30 in
    if r <> 0 && r <> 1 && r <> 2 && r <> 27 && not (check_set r) then r
    else loop ()
  in loop ()



(* a table of variables that we need for the code segment *)


(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

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


(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

(* find all of the variables in a program and add them to
 * the set variables *)

let get_rstmt ((s:rstmt),(pos:int)) : rstmt =
    s

let rec collect_vars2 ((e:rexp),(pos:int)) : unit =
  match e with
  | Var(v) ->
        let var = "prefix_"^v in
        variables := VarSet.add var !variables
  | Assign(v, expr) ->
        let var = "prefix_"^v in
        variables := VarSet.add var !variables;
        collect_vars2 expr
  | _ -> ()
let rec collect_vars (p : Ast.program) : unit =
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
  | For(expr1, _, _, s4) ->
    let s1 = (Exp(expr1),0) in
    collect_vars s1;
    collect_vars s4
  | _ ->
    ()




(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)

 let rec compile_expr ((re:rexp),(pos:int)) : inst list =
   match re with
   | Int(n) ->
       [Li(R2, (fromInt n))]
   | Var(vn) ->
        let v = "prefix_"^vn in
        [La(R2, v); Lw(R2, R2, (fromInt 0))]
   | And(r1, r2) ->
       let tr1 = new_reg() in
       let tr2 = new_reg() in
       let res_list = (compile_expr r1) @ [Add(tr1, R2, Immed((fromInt 0)))] @ (*move R2 to tr1*)
                             (compile_expr r2) @ [Add(tr2, R2, Immed((fromInt 0)))] @ (*move R2 to tr2*)
                             [And(R2, tr1, Reg(tr2))] in
       release_reg tr1;
       release_reg tr2;
       res_list
   | Or(r1, r2) ->
        let tr1 = new_reg() in
        let tr2 = new_reg() in
        let res_list = (compile_expr r1) @ [Add(tr1, R2, Immed((fromInt 0)))] @ (*move R2 to R3*)
                              (compile_expr r2) @ [Add(tr2, R2, Immed((fromInt 0)))] @ (*move R2 to R4*)
                              [Sne(tr1, tr1, R0); Sne(tr2, tr2, R0)] @ [Or(R2, tr1, Reg(tr2))] (*chekc (r1 != 0) || (r2 != 0)*) in
        release_reg tr1;
        release_reg tr2;
        res_list
        (*let tr3 = new_reg() in*)

   | Not(r) ->
        let tr1 = new_reg() in
        let tr2 = new_reg() in
        let res_list = (compile_expr r) @ [Add(tr1, R2, Immed((fromInt 0))); Li(tr2, (fromInt 0)); Seq(R2, tr1, tr2)] in (*move R2 to R3, R2 = R3 xor 1; should I consider conflict between registers?*)
        release_reg tr1;
        release_reg tr2;
       res_list
   | Binop(e1, op, e2) ->
        let tr1 = new_reg() in
        let tr2 = new_reg() in
        let res_list = (compile_expr e1) @ [Add(tr1, R2, Immed((fromInt 0))) ] @
                              (compile_expr e2) @ [Add(tr2, R2, Immed((fromInt 0))) ] @
                              (match op with
                                  | Plus -> [ Add(R2, tr1, Reg(tr2)) ]
                                  | Minus -> [ Sub(R2, tr1, tr2) ]
                                  | Times -> [ Mul(R2, tr1, tr2) ]
                                  | Div -> [ Div(R2, tr1, tr2) ]
                                  | Eq -> [ Seq(R2, tr1, tr2) ]
                                  | Neq -> [ Sne(R2, tr1, tr2) ]
                                  | Lt -> [ Slt(R2, tr1, tr2) ]
                                  | Lte -> [ Sle(R2, tr1, tr2) ]
                                  | Gt -> [ Sgt(R2, tr1, tr2) ]
                                  | Gte -> [ Sge(R2, tr1, tr2) ])
                                  in
        release_reg tr1;
        release_reg tr2;
       res_list
   | Assign(x, e) ->
        let tr1 = new_reg() in
        let v = "prefix_"^x in
        (compile_expr e) @ [La(tr1, v); Sw(R2, tr1, (fromInt 0))]

 let rec compile_stmt ((s,_):Ast.stmt) : inst list =
   match s with
   | Exp(rexp) ->
     compile_expr rexp
   | Seq(stmt1, stmt2) ->
     let insts1 = compile_stmt stmt1 in
     let insts2 = compile_stmt stmt2 in
     insts1 @ insts2
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



(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "fish_main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl printInt\n" ^
    "\t.globl fish_main\n" ^
    "\t.globl main\n\n" ^
    "main:\n" ^
    "\tmove $s8, $31\n" ^
    "\tjal fish_main\n" ^
    "\tmove $31, $s8\n" ^
    "\tmove $a0, $2\n" ^
    "\tj printInt\n\n" ^
    "printInt:\n" ^
    "\tadd $t0, $v0, $zero\n"^
    "\tli $v0, 1\n"^
    "\tsyscall\n"^
    "\tadd $v0, $t0, $zero\n"^
    "\tjr $ra\n\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
