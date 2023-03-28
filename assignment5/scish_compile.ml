(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)
(*
exception Unimplemented

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = raise Unimplemented
*)
 open Scish_ast
    open Cish_ast
let result = "result"
(* A closure is a function and its environment *)
type closure = { body : (Scish_ast.var * Scish_ast.exp); env : env }
(* Values are either integers, closures, or pairs *)
and value =
  Int_v of int
| Cons_v of value * value
| Closure_v of closure
(* an environment provides values for the variables in scope.
 * It's essentially a list of frames, corresponding to each
 * nested lambda. *)
and env =
  Empty
| Frame of (Scish_ast.var * value) * env

(* Lookup a variable in the environment -- return the 1st binding
 * corresponding to the variable. *)
let rec lookup (x:Scish_ast.var) (env:env) : value =
    match env with
      Empty -> failwith ("Unbound variable "^x)
    | Frame ((y,v),rest) -> if (y = x) then v else lookup x rest

(*type closure = { code : stmt ptr; env : (string * value ptr) list ptr }*)


let fresh_var =
  let c = ref 0 in
  fun () ->
    let v = "v" ^ string_of_int !c in
    c := !c + 1;
    v


let rstmt_of_stmt stmt =
  match stmt with
  | (rstmt, _) -> rstmt

let rexp_to_stmt rexp =
    (Cish_ast.Exp((rexp,0)), 0)
let exp_of_stmt stmt =
  let rstmt = rstmt_of_stmt stmt in
  match rstmt with
  | (Cish_ast.Exp (exp)) -> exp
  | _ ->
    let msg = Cish_ast.stmt2string stmt in
    failwith ("stmt is not an Exp, "^msg)




let body_of_func func =
  match func with
  | Cish_ast.Fn { body = body_stmt; _ } -> body_stmt


(*generate a function for lambda *)
let rec generate_func (x : Scish_ast.var) (e:Scish_ast.exp) : Cish_ast.program =
    let statement =
      match e with
      | Scish_ast.Int i -> (Cish_ast.Exp (Cish_ast.Int i, 0), 0)
      | Scish_ast.PrimApp (op, es) -> compile_primop op es
    in
    [(Cish_ast.Fn { Cish_ast.name = x; Cish_ast.args = []; Cish_ast.body = statement; Cish_ast.pos = 0 })]


(*generate old function *)
and compile_exp2 (e:Scish_ast.exp) : Cish_ast.program =
    let fresh = fresh_var () in
    let rec compile_exp3 (ex:Scish_ast.exp) : Cish_ast.stmt =
        match ex with
              | Scish_ast.Int i -> (Cish_ast.Exp (Cish_ast.Int i, 0), 0)
              | Scish_ast.PrimApp (op, es) -> compile_primop op es
              | Scish_ast.Lambda (x, es) ->
                failwith ("Found you!!! Lambda: x is : " ^x^"\n es is : " ^(Scish_ast.exp2string es))
                    (*add function name to hashmap*)
                  (* (Let(x, exp_of_stmt (compile_exp3 es), (skip, 0)), 0)*)
              | Scish_ast.App(e1, e2) ->
                    failwith ("\n App : e1 is : " ^(Scish_ast.exp2string e1)^"\n e2 is : " ^(Scish_ast.exp2string e2));
                    let stmt1 = compile_exp3 e1 in
                    let stmt2 = compile_exp3 e2 in
                    (Cish_ast.Seq(stmt1, stmt2), 0)
              | _ -> failwith ("Match fail with : " ^ (Scish_ast.exp2string ex))
    in
    let statement = compile_exp3 e in
    let return_stmt = (Cish_ast.Return( (Cish_ast.Var(result), 0) ), 0)in
    let final_stmt = (Cish_ast.Seq(statement, return_stmt), 0) in
    [(Cish_ast.Fn { Cish_ast.name = fresh; Cish_ast.args = []; Cish_ast.body = final_stmt; Cish_ast.pos = 0 })]


(* generate main function*)
and  compile_exp (e:Scish_ast.exp) : Cish_ast.program =
    let main_name = "main" in
    let fresh = fresh_var() in
    let functions = compile_exp2 e in (*compile_exp2 will return a function like oldmain, and main will call and return the result of oldmain*)
    let f_name = match (List.hd functions) with
                            | Cish_ast.Fn { name; args; body; pos } -> name in
    let var_list = match (List.hd functions) with
                                | Cish_ast.Fn { name; args; body; pos } -> args in
    let f_args = List.map (fun v -> Cish_ast.Var(v), 0) var_list in

    let call_exp = (Cish_ast.Call((Cish_ast.Var(f_name), 0), f_args), 0) in
    (*let assign_stmt = (Cish_ast.Exp(Cish_ast.Assign(fresh, call_exp), 0), 0) in *)
    let return_stmt = (Cish_ast.Return(call_exp), 0)in
    let statement = return_stmt in
    let let_result_stmt =  (Cish_ast.Let(result, (Cish_ast.Int(0), 0), statement), 0) in
    functions @ [(Cish_ast.Fn { Cish_ast.name = main_name; Cish_ast.args = []; Cish_ast.body = let_result_stmt; Cish_ast.pos = 0 })]

and compile_primop op es : Cish_ast.stmt =
    let rexpr =
    match (op,es) with
      (Plus,[Int(i);Int(j)]) -> Assign(result, (Int(i+j), 0))
    | (Minus,[Int(i);Int(j)]) -> Int(i-j)
    | (Times,[Int(i);Int(j)]) -> Int(i*j)
    | (Div,[Int(i);Int(j)]) -> Int(i / j)
    | (Eq,[Int(i);Int(j)]) ->
      if (i = j) then Int(1) else Int(0)
    | (Lt,[Int(i);Int(j)]) ->
      if (i < j) then Int(1) else Int(0)
    | (_,vs) ->
      failwith ("failed")
    in
    rexp_to_stmt rexpr
(*
and compile_primop op es : Cish_ast.stmt =
  match es with
  | [exp1; exp2] ->
      let pro1 = compile_exp2 exp1 in
      let pro2 = compile_exp2 exp2 in
      let e1 = exp_of_stmt  (body_of_func (List.hd pro1)) in
      let e2 = exp_of_stmt  (body_of_func (List.hd pro2)) in
      match op with
      | Scish_ast.Plus ->
            let assign_exp = Cish_ast.Assign(result, (Cish_ast.Binop (e1, Cish_ast.Plus, e2), 0)) in
            rexp_to_stmt assign_exp
*)
(*
let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program =
  let fresh = fresh_var () in
  match e with
  | Scish_ast.Int i -> [(Cish_ast.Fn { Cish_ast.name = fresh; Cish_ast.args = []; Cish_ast.body = (Cish_ast.Exp (Cish_ast.Int i, 0), 0); Cish_ast.pos = 0 })]
  | Scish_ast.Var x -> [(Cish_ast.Fn { Cish_ast.name = fresh; Cish_ast.args = []; Cish_ast.body = (Cish_ast.Exp ((Cish_ast.Load (Cish_ast.Var x, 0), 0)), 0); Cish_ast.pos = 0 })]
  | Scish_ast.PrimApp (op, es) -> compile_primop op es fresh
  | Scish_ast.Lambda (x, e') ->
        let subprogram = compile_exp e' in
        subprogram @ [(Cish_ast.Fn { Cish_ast.name = fresh; Cish_ast.args = [x]; Cish_ast.body = (Cish_ast.Return (Cish_ast.Var x, 0), 0); Cish_ast.pos = 0 })]
  | Scish_ast.App (e1, e2) ->
      let fe1 = fresh_var () in
      let fe2 = fresh_var () in
      let subprogram1 = compile_exp e1 in
      let subprogram2 = compile_exp e2  in
      subprogram1 @ subprogram2 @ [(Cish_ast.Fn { Cish_ast.name = fe1; Cish_ast.args = [fe2]; Cish_ast.body = (Cish_ast.Call (Cish_ast.Var fe1, [Cish_ast.Var fe2], 0), 0) })] @
      [(Cish_ast.Call (Cish_ast.Var fe1, [compile_exp e2; (Cish_ast.Int 0, 0)], 0), 0)]
  | Scish_ast.If (e1, e2, e3) ->
      let fe1 = fresh_var () in
      let fe2 = fresh_var () in
      let fe3 = fresh_var () in
      let fe4 = fresh_var () in
      compile_exp e1 @
      [(Cish_ast.Fn { Cish_ast.name = fe1; Cish_ast.args = [fe2; fe3]; Cish_ast.body = [(Cish_ast.If ((Cish_ast.Var fe2, 0), compile_exp e2 @ [(Cish_ast.Return (Cish_ast.Var fe4, 0), 0)], compile_exp e3 @ [(Cish_ast.Return (Cish_ast.Var fe4, 0), 0)]), 0)] })] @
      compile_exp e2 @
      compile_exp e3 @
      [(Cish_ast.Fn { Cish_ast.name = fresh; Cish_ast.args = []; Cish_ast.body = [(Cish_ast.Call (Cish_ast.Var fe1, [compile_exp e2; compile_exp e3], 0), 0); (Cish_ast.Return (Cish_ast.Var fe4, 0), 0)] })]
and compile_primop op es fresh =
  match op with
  | Scish_ast.Plus -> [(Cish_ast.Fn { Cish_ast.name = fresh; Cish_ast.args = []; Cish_ast.body = (compile_binop Cish_ast.Plus es, 0) })]

*)