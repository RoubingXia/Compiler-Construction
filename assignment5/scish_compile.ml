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
let var_stack = "var_stack"
let stack1 = "stack1"
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

let str2exp str =
    (Var(str),0)

(* for variables *)

module VarMap = Map.Make(struct
                           type t = Cish_ast.var
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

(* end for variables*)

let offset = 10000 (*used for pair*)

let body_of_func func =
  match func with
  | Cish_ast.Fn { body = body_stmt; _ } -> body_stmt

 let push_stack_stmt (stack_name: string) : Cish_ast.stmt =
                (*push result to stack*)
               let push_stmt = rexp_to_stmt (Store((Var(stack_name), 0), (Var(result), 0))) in
               let move_stmt = rexp_to_stmt (Assign(stack_name, (Binop( (str2exp stack_name), Plus, (Int 4, 0)), 0))) in
               (Seq(move_stmt, push_stmt), 0)

 let pop_stack_stmt (stack_name: string) ( v : Cish_ast.var) : Cish_ast.stmt =
                (*v = stack.pop()*)
                let pop_stmt = rexp_to_stmt ( Assign( v, (Load((Var(stack_name), 0)), 0) ) ) in
                let move_stmt = rexp_to_stmt (Assign(stack_name, (Binop( (str2exp stack_name), Minus, (Int 4, 0)), 0))) in
                (Seq(pop_stmt, move_stmt), 0)

let rec compile_exp3 (ex:Scish_ast.exp) : Cish_ast.stmt =
        match ex with
              | Scish_ast.Int i -> rexp_to_stmt (Assign(result, ((Int i), 0)))
              | Scish_ast.Var v -> (*pop from stack*)  rexp_to_stmt (Assign(result, ((Var(v), 0))))
              | Scish_ast.PrimApp (op, es) ->
                compile_primop op es
              | Scish_ast.Lambda (x, es) ->
                print_string ("\n Match Lambda \n x is : " ^x^"\n es is : " ^(Scish_ast.exp2string es));
                (*x = pop of stack; *)
                let stmt1 = pop_stack_stmt var_stack x in
                let stmt2 = compile_exp3 es in
                let inside_stmt = (Seq(stmt1, stmt2), 0) in
                let declare_stmt1 =  (Let(x, (Int(0), 0), inside_stmt ), 0) in
                (*let stmt1 = pop_stack_stmt x in*)
                (*compile es*)

                (*(Seq(stmt1, stmt2), 0)*)
                declare_stmt1


                    (*add function name to hashmap*)
                  (* (Let(x, exp_of_stmt (compile_exp3 es), (skip, 0)), 0)*)
              | Scish_ast.App(e1, e2) ->
                   print_string ("\n Match App \n e1 is : " ^(Scish_ast.exp2string e1)^"\n e2 is : " ^(Scish_ast.exp2string e2));
                    let stmt2 = compile_exp3 e2 in (*compile of e2 should be a value, or it could be a function *)

                    (*Push the result of e2 to stack*)
                    let push_stmt = push_stack_stmt var_stack in
                    let stmt1 = compile_exp3 e1 in
                    (Cish_ast.Seq((Cish_ast.Seq(stmt2, push_stmt), 0), stmt1), 0)
              | Scish_ast.If(e1, e2, e3) ->
                (*failwith ("Mathc If statement, e1 :  " ^ (Scish_ast.exp2string e1) ^"\n e2 : "^(Scish_ast.exp2string e1)^"\n e3: "^(Scish_ast.exp2string e1))*)
                let stmt1 = compile_exp3 e1 in
                let stmt2 = compile_exp3 e2 in
                let stmt3 = compile_exp3 e3 in
                (Seq(stmt1, (If(str2exp result, stmt2, stmt3), 0)), 0)
              | _ -> failwith ("Match fail with : " ^ (Scish_ast.exp2string ex))
(*generate old function *)
and compile_exp2 (e:Scish_ast.exp) : Cish_ast.program =
    let fresh = fresh_var () in


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
    (*declare a stack on memory *)
    let let_stack_stmt =  (Cish_ast.Let(var_stack, (Cish_ast.Malloc((Cish_ast.Int(64), 0)), 0), let_result_stmt), 0) in
    let let_stack_stmt2 =  (Cish_ast.Let(stack1, (Cish_ast.Malloc((Cish_ast.Int(64), 0)), 0), let_stack_stmt), 0) in
    functions @ [(Cish_ast.Fn { Cish_ast.name = main_name; Cish_ast.args = []; Cish_ast.body = let_stack_stmt2; Cish_ast.pos = 0 })]

and compile_primop op es : Cish_ast.stmt =
    let tv1 = fresh_var() in
    let tv2 = fresh_var() in
    match (op,es) with
     (Plus,[e1; e2]) ->
        let stmt1 = compile_exp3 e1 in
        let stmt2 = compile_exp3 e2 in
        let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
        let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
        let add_stmt = rexp_to_stmt (Assign(result, (Binop(str2exp tv1, Plus, str2exp tv2), 0)) ) in
        let inside_stmt = (Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), add_stmt), 0) in
        let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
        let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
        declare_stmt1
     | (Minus,[e1; e2]) ->
         let stmt1 = compile_exp3 e1 in
         let stmt2 = compile_exp3 e2 in
         let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
         let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
         let sub_stmt = rexp_to_stmt (Assign(result, (Binop(str2exp tv1, Minus, str2exp tv2), 0)) ) in
         let inside_stmt = (Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), sub_stmt), 0) in
         let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
         let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
         declare_stmt1
     | (Times,[e1; e2]) ->
         let stmt1 = compile_exp3 e1 in
         let stmt2 = compile_exp3 e2 in
         let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
         let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
         let mul_stmt = rexp_to_stmt (Assign(result, (Binop(str2exp tv1, Times, str2exp tv2), 0)) ) in
         let inside_stmt = (Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), mul_stmt), 0) in
         let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
         let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
         declare_stmt1
    | (Div,[e1; e2]) ->
        let stmt1 = compile_exp3 e1 in
         let stmt2 = compile_exp3 e2 in
         let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
         let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
         let div_stmt = rexp_to_stmt (Assign(result, (Binop(str2exp tv1, Div, str2exp tv2), 0)) ) in
         let inside_stmt = (Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), div_stmt), 0) in
         let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
         let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
         declare_stmt1
    | (Cons, [e1; e2]) ->   (* create a pair, result = key * 10^4 + value; assign key to high half of an integer 10^4 *)
        let stmt1 = compile_exp3 e1 in
         let stmt2 = compile_exp3 e2 in
         let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
         let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
         (*mallocate 8 bytes to start key and value*)
         let pair_ptr = fresh_var() in
         (* store tv1 to *pair_ptr, store tv2 to *(pair_ptr + 4)  *)
         let store_stmt1 = rexp_to_stmt (Store(str2exp pair_ptr, str2exp tv1)) in
         (*let move_ptr_stmt1 = rexp_to_stmt (Assign(pair_ptr, (Binop( (str2exp pair_ptr), Plus, (Int 4, 0)), 0))) in
         let combin_store_stmt1 = Seq(store_stmt1 , move_ptr_stmt1)  in *)

         let store_stmt2 = rexp_to_stmt (Store( (Binop((str2exp pair_ptr), Plus, (Int 4, 0)), 0), str2exp tv2)) in
         (*
         let move_ptr_stmt2 = rexp_to_stmt (Assign(pair_ptr, (Binop( (str2exp pair_ptr), Plus, (Int 4, 0)), 0))) in
         let combin_store_stmt2 = Seq(store_stmt2 , move_ptr_stmt2)  in *)

         let assigne_stmt =  rexp_to_stmt (Assign(result, str2exp pair_ptr)) in

         let store_stmts = (Seq(store_stmt1, store_stmt2), 0) in
         let merge_stmt = rexp_to_stmt (Assign(result,(Binop( (Binop(str2exp tv1, Times, (Int(offset), 0)), 0), Plus, str2exp tv2), 0) )) in
         let inside_stmt = (Seq((Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), store_stmts), 0) , assigne_stmt), 0)  in
         let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
         let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
         let pair_ptr_stmt1 =  (Let(pair_ptr, (Malloc((Int(8), 0)), 0), declare_stmt1), 0) in
         pair_ptr_stmt1

    | (Fst, [e1]) ->  (* fetch the 1st component of a pair *)
        let stmt1 = compile_exp3 e1 in
         let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
         (*first element = load(e1) *)
         let get_stmt = rexp_to_stmt (Assign(result, (Load((Var(tv1), 0)), 0) )  ) in
         let inside_stmt = (Seq( (Seq(stmt1, move_stmt1), 0), get_stmt), 0) in
         let declare_stmt1 =  (Let(tv1, (Int(0), 0), inside_stmt ), 0) in
         declare_stmt1

    | (Snd, [e1]) ->  (* fetch the 2nd component of a pair *)
      let stmt1 = compile_exp3 e1 in
       let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
       (*second element = load(e1 + 4) *)
       let expr1 = (Binop(str2exp tv1, Plus, (Int(4), 0)), 0) in
       let get_stmt = rexp_to_stmt (Assign(result, (Load(expr1), 0) ))  in
       let inside_stmt = (Seq( (Seq(stmt1, move_stmt1), 0), get_stmt), 0) in
       let declare_stmt1 =  (Let(tv1, (Int(0), 0), inside_stmt ), 0) in
       declare_stmt1    (* fetch the 2nd component of a pair *)

    | (Eq, [e1; e2])  ->     (* compare two ints for equality *)
        let stmt1 = compile_exp3 e1 in
        let stmt2 = compile_exp3 e2 in
        let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
        let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
        let eq_stmt = rexp_to_stmt (Assign(result, (Binop(str2exp tv1, Eq, str2exp tv2), 0)) ) in
        let inside_stmt = (Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), eq_stmt), 0) in
        let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
        let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
        declare_stmt1
    | (Lt, [e1; e2])  ->    (* compare two ints for inequality *)
        let stmt1 = compile_exp3 e1 in
        let stmt2 = compile_exp3 e2 in
        let move_stmt1 = rexp_to_stmt (Assign(tv1, str2exp result)) in
        let move_stmt2 = rexp_to_stmt (Assign(tv2, str2exp result)) in
        let lt_stmt = rexp_to_stmt (Assign(result, (Binop(str2exp tv1, Lt, str2exp tv2), 0)) ) in
        let inside_stmt = (Seq( (Seq( (Seq(stmt1, move_stmt1), 0), (Seq(stmt2, move_stmt2), 0) ), 0), lt_stmt), 0) in
        let declare_stmt2 = (Let(tv2, (Int(0), 0), inside_stmt ), 0) in
        let declare_stmt1 =  (Let(tv1, (Int(0), 0), declare_stmt2 ), 0) in
        declare_stmt1


    | (_,vs) ->
      failwith ("\n Failed to match primop, op is : "^(primop2string op)^"\n es is: "^(Scish_ast.exps2string es) )
