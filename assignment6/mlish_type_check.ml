open Mlish_ast

exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)
(*
let type_check_exp (e:Mlish_ast.exp) : tipe = raise TypeError
*)
(*
type substitution = (tvar * tipe) list

let empty_subst : substitution = []

let subst_apply (s : substitution) (t : tipe) : tipe =
  let rec subst_apply' s' = function
    | Tvar_t tv as ty ->
      (match List.assoc_opt tv s' with
        | Some ty' -> subst_apply' s' ty'
        | None -> ty)
    | Int_t | Bool_t | Unit_t -> t
    | Fn_t (t1, t2) -> Fn_t (subst_apply' s' t1, subst_apply' s' t2)
    | Pair_t (t1, t2) -> Pair_t (subst_apply' s' t1, subst_apply' s' t2)
    | List_t t' -> List_t (subst_apply' s' t')
    | Guess_t r ->
      (match !r with
        | None ->
          let fresh_tvar = Tvar_t (ppfreshtvar ()) in
          r := Some fresh_tvar;
          subst_apply' s' fresh_tvar
        | Some ty -> subst_apply' s' ty)
  in
  subst_apply' s t

let rec subst_vars (vars : tvar list) (s : substitution)  : tipe list =
  match vars with
  | [] -> []
  | v::vs ->
    let t = subst_apply s (Tvar_t v) in
    t :: (subst_vars vs s)
*)

let rec string_of_tipe_scheme (ts : tipe_scheme) : string =
  match ts with
  | Forall ([], t) -> "Forall([], t): [], t is :"^string_of_tipe t
  | Forall (vars, t) ->
      "∀" ^ String.concat " " vars ^ "." ^ string_of_tipe t
and string_of_tipe (t : tipe) : string =
  match t with
  | Tvar_t tv -> "'" ^ tv
  | Int_t -> "int"
  | Bool_t -> "bool"
  | Unit_t -> "unit"
  | Fn_t (t1, t2) ->
      "(" ^ string_of_tipe t1 ^ " -> " ^ string_of_tipe t2 ^ ")"
  | Pair_t (t1, t2) ->
      "(" ^ string_of_tipe t1 ^ " * " ^ string_of_tipe t2 ^ ")"
  | List_t t' -> "(" ^ string_of_tipe t' ^ " list)"
  |  Guess_t r when !r = None ->
                "Empty Guess"
  | Guess_t ({ contents = Some t2' }) ->
           "Guess[" ^ string_of_tipe t2'^ "]"


let rec type_eq (t1 : tipe) (t2 : tipe) : bool =
print_string("\n Inside type_eq  branch, t1 is : "^tipe2string(t1)^"\n t2 is :"^tipe2string(t2)^"\n ");
  match (t1, t2) with
  | Tvar_t tv1, Tvar_t tv2 -> tv1 = tv2
  | Int_t, Int_t -> true
  | Bool_t, Bool_t -> true
  | Unit_t, Unit_t -> true
  | Fn_t (t1a, t1b), Fn_t (t2a, t2b) ->
      type_eq t1a t2a && type_eq t1b t2b
  | Pair_t (t1a, t1b), Pair_t (t2a, t2b) ->
      type_eq t1a t2a && type_eq t1b t2b
  | List_t t1a, List_t t2a -> type_eq t1a t2a
  | Guess_t r1, Guess_t r2 ->
      match (r1, r2) with
      (*| None, None -> true
      | Some t1, Some t2 -> type_eq t1 t2*)
      | _ -> false
  | other -> print_string("\n Miss match here??? \n"); false

let rec unify (t1 : tipe) (t2 : tipe) : bool =
   print_string("\n Inside unify, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n");
  if t1 == t2 then (print_string("\n Inside unify then branch \n"); true)
  else
    (print_string("\n Inside unify else branch \n");
    match (t1, t2) with
    | Guess_t ({ contents = Some t1' }), _ ->
         print_string("\n Inside Unify Match a guess, _ \n");
        unify t1' t2
    |  Guess_t r, t2 when !r = None ->
              print_string("\n Inside Unify Match a empty guess, _ \n");
              r := Some t2;
              true
    |  t1, Guess_t r when !r = None ->
                  print_string("\n Inside Unify Match a empty guess, _  2, will this lead some bugs??\n");
                  r := Some t1;
                  true
    | _, Guess_t ({ contents = Some t2' }) ->
        print_string("\n Inside Unify Match a _ guess \n");
        unify t2' t1
    | Int_t, Int_t ->
        true
    | Pair_t(t1a, t1b), Pair_t(t2a, t2b) ->
             unify t1a t2a && unify t1b t2b
    | Fn_t (t1a, t1b), Fn_t (t2a, t2b) ->
        unify t1a t2a && unify t1b t2b
    | _ ->
        print_string("\nUnify return false, t1 is : "^(string_of_tipe t1)^"\n t2 is :"^(string_of_tipe t2)^"\n" );
        false)


let rec tipe_list_to_string (tl : tipe list) : string =
  match tl with
  | [] -> ""
  | [t] -> tipe2string t
  | t :: tl' -> tipe2string t ^ ", " ^ tipe_list_to_string tl'


let string_of_substitution (s: (tvar * tipe) list) : string =
  let string_of_pair (tv, t) =
    tv ^ " -> " ^ string_of_tipe t
  in
  String.concat ", " (List.map string_of_pair s)



let rec substitute (s: (tvar * tipe) list) (t: tipe) : tipe =
  print_string("\n Inside substitue, s is : "^string_of_substitution(s)^"\n t is : "^tipe2string(t)^"\n");
  match t with
  | Tvar_t a -> (try List.assoc a s with Not_found -> t)
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t (t1, t2) -> Fn_t (substitute s t1, substitute s t2)
  | Pair_t (t1, t2) -> Pair_t (substitute s t1, substitute s t2)
  | List_t t' -> List_t (substitute s t')
  | Guess_t r -> print_string("\n I am guess type inside substitue\n"); Guess_t r (* no substitution for guess types *)


let instantiate (s: tipe_scheme): tipe =
  match s with
  | Forall (vs, t) ->
    print_string("\n in instantiate, the input scheme is : "^string_of_tipe_scheme(s)^"\n");
    print_string("\n in instantiate, the input t is : "^string_of_tipe(t)^"\n");
    let b = List.map (fun a -> (a, Guess_t (ref None))) vs in
    print_string("\n in instantiate, after map the input t is : "^string_of_tipe(t)^"\n");
    substitute b t


let rec lookup (x : var) (env : (var * tipe_scheme) list) : tipe_scheme =
  match env with
  | [] ->
      failwith ("Variable " ^ x ^ " not found in environment")
  | (y, scheme) :: rest ->
      if x = y then
        (print_string("in lookup, scheme for "^y^" is :"^string_of_tipe_scheme(scheme));
        scheme) else lookup x rest

(*update scheme of var in env from Forall(list, g) to Forall(list, t)*)
let rec update_env (x : var) (env : (var * tipe_scheme) list) (t : tipe) : unit =
  match env with
  | [] ->
      failwith ("In update env, variable " ^ x ^ " not found in environment")
  | (y, scheme) :: rest ->
      if x = y then
        (match scheme with
        | Forall (vs, t') ->
            (match t' with
            | Guess_t r when !r = None ->
                r := Some t
            | _ -> type_error("In update_env, meet a non-empty guess1"))
        | _ -> type_error("In update_env, meet a non-empty guess2"))
      else update_env x rest t


let prim_to_string = function
  | Int i -> string_of_int i
  | Bool b -> if b then "true" else "false"
  | Unit -> "()"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Div -> "Div"
  | Eq -> "Eq"
  | Lt -> "Lt"
  | Pair -> "Pair"
  | Fst -> "Fst"
  | Snd -> "Snd"
  | Nil -> "Nil"
  | Cons -> "Cons"
  | IsNil -> "IsNil"
  | Hd -> "Hd"
  | Tl -> "Tl"


let rec print_exp (e: exp) : unit =
  match e with
  | Var x, _ -> print_string x
  | PrimApp (p, es), _ ->
      print_string (prim_to_string p);
      print_string "(";
      List.iter (fun e -> print_exp e; print_string ", ") es;
      print_string ")"
  | Fn (x, e), _ ->
      print_string "fn ";
      print_string x;
      print_string " => ";
      print_exp e
  | App (e1, e2), _ ->
      print_exp e1;
      print_string " ";
      print_exp e2
  | If (e1, e2, e3), _ ->
      print_string "if ";
      print_exp e1;
      print_string " then ";
      print_exp e2;
      print_string " else ";
      print_exp e3
  | Let (x, e1, e2), _ ->
      print_string "let ";
      print_string x;
      print_string " = ";
      print_exp e1;
      print_string " in ";
      print_exp e2


let rec print_exp_list = function
  | [] -> ()
  | e :: es ->
      let _ = print_exp e in
      let _ = print_string ";\n" in
      print_exp_list es


let prim_to_tipe p ts =
  match p with
  | Int _ -> Int_t
  | Bool _ -> Bool_t
  | Unit -> Unit_t
  | Cons -> List_t(List.hd ts)
  | Plus | Minus | Times | Div ->
     (match ts with
     | [t1; t2] -> if t1 = t2 then t1 else type_error("Failed location 001")
     | _ -> type_error("Failed location 002"))
  | Nil -> Int_t
  | Eq | Lt -> Int_t
  | Pair -> (match ts with
                 | [t1; t2] -> Pair_t(t1, t2)
                 | _ -> type_error("Failed location 003"))

  | Fst ->
        (match (List.hd ts) with
             | Pair_t(t1, t2) -> t1
             | Guess_t r when !r = None ->
                 print_string("\n Match None guess in prim_to_tipe ");
                 Bool_t;
              | Tvar_t t ->
                              print_string("\n Match Tvar in prim_to_tipe ");
                              Bool_t;
                | Int_t ->  print_string("\n Match Int in prim_to_tipe ");
                                                         Bool_t;
                | Bool_t ->  print_string("\n Match Bool in prim_to_tipe ");
                                                          Bool_t;
                | Unit_t ->  print_string("\n Match Unin in prim_to_tipe ");
                                                          Bool_t;
                | Fn_t (t1, t2) ->  print_string("\n Match Fn in prim_to_tipe ");
                                                                 Bool_t;
                | Pair_t (t1, t2) ->  print_string("\n Match Pair in prim_to_tipe ");
                                                                   Bool_t;
                | List_t t ->  print_string("\n Match List in prim_to_tipe ");
                                                            Bool_t;
                | Guess_t ({ contents = Some t2' }) ->
                    print_string("\n Match Some Guess in prim_to_tipe the content of guess is :"^string_of_tipe(t2'));
                                                 Bool_t;
             | _ -> type_error("\n Failed location 004"^(tipe2string (List.hd ts))))
  | Snd ->
        (match (List.hd ts) with
            | Pair_t(t1, t2) -> t2
            | _ -> type_error("Failed location 005"))
  | IsNil ->
        (match (List.hd ts)  with
            | List_t(t1) -> Bool_t
            | _ -> type_error("Failed location 006"))
  | Hd ->
        (match (List.hd ts)  with
          | List_t(t1) -> Bool_t
          | _ -> type_error("Failed location 007"))
  | Tl ->
        (match (List.hd ts)  with
          | List_t(t1) -> Bool_t
          | _ -> type_error("Failed location 008"))


let type_check_exp (e : exp) : tipe =
  let rec type_check_exp' (e : exp) (env : (var * tipe_scheme) list) : tipe =
    match e with
    | (Var x, _) ->
      print_string "\n Match Var\n";
      let res = instantiate(lookup x env) in
      print_string ("res is :"^string_of_tipe(res));
      res
    | (PrimApp (p, es), _) ->
        print_string ("\n Match PrimApp (p, es)\n    p is : "^(prim_to_string p)^"\n    es is : ");
        print_exp_list es;
        (match p with
          | Int _ -> Int_t
          | Bool _ -> Bool_t
          | Unit -> Unit_t
          (*| Cons -> List_t(List.hd ts)*)
          | Plus | Minus | Times | Div ->
             let ts = List.map (fun e -> type_check_exp' e env) es in
             (match ts with
             | [t1; t2] -> if t1 = t2 then t1 else type_error("Failed location 001")
             | _ -> type_error("Failed location 002"))
          | Nil -> Int_t
          | Eq | Lt -> Int_t
          | Pair ->
             let ts = List.map (fun e -> type_check_exp' e env) es in
             (match ts with
             | [t1; t2] -> Pair_t(t1, t2)
             | _ -> type_error("Failed location 003"))
          | Fst ->
          (*
          by now, I know p is Fst  and es is x, so I know x should be pair type, then I update my env  (x, ForAll([], g1 ) ) becomes  (x, ForAll([], Pair(g3, g4) ) )
                 and in the PrimApp(p, es) case, I should return a Fn_t( Pair(g1, g2), g2 )
          *)
             let g1 = Guess_t(ref None) in
             let g2 = Guess_t(ref None) in
             let get_var_from_exp_list (exps : exp list) : var =
                   match exps with
                   | [Var x, _] -> x
                   | _ -> type_error("Failed location 009")
             in
             let vname = get_var_from_exp_list es in
             update_env vname env (Pair_t(g1, g2));
             g1;)

            (* Fn_t(Pair_t(g1, g2), g1);)*)
              (*  (match (List.hd ts) with
                     | Pair_t(t1, t2) -> t1
                     | Guess_t r when !r = None ->
                         print_string("\n Match None guess in prim_to_tipe ");
                         Bool_t;
                      | Tvar_t t ->
                                      print_string("\n Match Tvar in prim_to_tipe ");
                                      Bool_t;
                        | Int_t ->  print_string("\n Match Int in prim_to_tipe ");
                                                                 Bool_t;
                        | Bool_t ->  print_string("\n Match Bool in prim_to_tipe ");
                                                                  Bool_t;
                        | Unit_t ->  print_string("\n Match Unin in prim_to_tipe ");
                                                                  Bool_t;
                        | Fn_t (t1, t2) ->  print_string("\n Match Fn in prim_to_tipe ");
                                                                         Bool_t;
                        | Pair_t (t1, t2) ->  print_string("\n Match Pair in prim_to_tipe ");
                                                                           Bool_t;
                        | List_t t ->  print_string("\n Match List in prim_to_tipe ");
                                                                    Bool_t;
                        | Guess_t ({ contents = Some t2' }) ->
                            print_string("\n Match Some Guess in prim_to_tipe the content of guess is :"^string_of_tipe(t2'));
                                                         Bool_t;
                     | _ -> type_error("\n Failed location 004"^(tipe2string (List.hd ts))))
          | Snd ->
                (match (List.hd ts) with
                    | Pair_t(t1, t2) -> t2
                    | _ -> type_error("Failed location 005"))
          | IsNil ->
                (match (List.hd ts)  with
                    | List_t(t1) -> Bool_t
                    | _ -> type_error("Failed location 006"))
          | Hd ->
                (match (List.hd ts)  with
                  | List_t(t1) -> Bool_t
                  | _ -> type_error("Failed location 007"))
          | Tl ->
                (match (List.hd ts)  with
                  | List_t(t1) -> Bool_t
                  | _ -> type_error("Failed location 008"))
                   *)
        (*get the type of es, if it's a list, I should use it*)
        (*let t1 = List.map (fun e -> type_check_exp' e env) es in
        print_string("\n Before call prim_to_tipe, the p is : "^(prim_to_string p)^"\n t1 is : "^tipe_list_to_string(t1)^"\n");
        prim_to_tipe p t1 *)
    | (Fn (x, e), _) ->
        print_string "\n Match Fn (x, e) \n";
        print_string("    x is : "^x);
                 print_string("\n    e is : ");
                 print_exp e;
          let g = Guess_t(ref None) in
         (* let tvar = Tvar_t (ppfreshtvar ()) in*)
          let env' = (x, Forall ([], g)) :: env in
          let body_tipe = type_check_exp' e env' in
          Fn_t (g, body_tipe);
    | (App (e1, e2), _) ->
        print_string "\n Match App (e1, e2)\n";
         print_string("    e1 is : ");
         print_exp e1;
         print_string("\n    e2 is : ");
         print_exp e2;
         let t1 = type_check_exp' e1 env in
         print_string("\n after check e1, I got t1 is :"^string_of_tipe(t1)^"\n");
         let t2 = type_check_exp' e2 env in
         print_string("\n after check e2, I got t2 is :"^string_of_tipe(t2)^"\n");
         print_string("\n Before unify t1 is : "^string_of_tipe(t1)^" and t2 is : "^string_of_tipe(t2));
         let t = Guess_t(ref None)
         in
         if unify t1 (Fn_t(t2,t)) then t
         else type_error("Fail with App match t1 with guess")
    | (If (e1, e2, e3), _) ->
        print_string "\n match If\n";
            let t1 = type_check_exp' e1 env in
            let t2 = type_check_exp' e2 env in
            let t3 = type_check_exp' e3 env in
            let t = Guess_t(ref None) in
            if unify t2 t3 then t2
            else type_error "Type error: Invalid argument types for If expression"
    | (Let (x, e1, e2), _) ->
        print_string "\n match Let\n";
                   Tvar_t("")
  in
  let t = type_check_exp' e [] in
  t
