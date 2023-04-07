open Mlish_ast

exception TypeError
let type_error(s:string) = ((*print_string s;*) raise TypeError)
let log_open = false
let m_print(s:string) =
    if (log_open) then print_string(s)
(*
    ChapGPT and I wrote this code together,
    and now both of us don't understand WTF this code is doing, but somehow it still works
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

let rec string_of_exp ((rexp, _): exp) : string =
  match rexp with
  | Var v -> v
  | PrimApp (p, es) ->
    let es_str = List.map string_of_exp es in
    let es_str' = String.concat ", " es_str in
    (match p with
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Unit -> "()"
    | Plus -> "(" ^ es_str' ^ ")" ^ " + "
    | Minus -> "(" ^ es_str' ^ ")" ^ " - "
    | Times -> "(" ^ es_str' ^ ")" ^ " * "
    | Div -> "(" ^ es_str' ^ ")" ^ " / "
    | Eq -> "(" ^ es_str' ^ ")" ^ " = "
    | Lt -> "(" ^ es_str' ^ ")" ^ " < "
    | Pair -> "(" ^ es_str' ^ ")"
    | Fst -> "fst (" ^ es_str' ^ ")"
    | Snd -> "snd (" ^ es_str' ^ ")"
    | Nil -> "[]"
    | Cons -> "[" ^ es_str' ^ "]"
    | IsNil -> "is_nil (" ^ es_str' ^ ")"
    | Hd -> "hd (" ^ es_str' ^ ")"
    | Tl -> "tl (" ^ es_str' ^ ")")
  | Fn (v, e) -> "fn " ^ v ^ " => " ^ string_of_exp e
  | App (e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | If (e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2

let string_of_exps (es: exp list) : string =
  let es_str = List.map string_of_exp es in
  String.concat ";\n" es_str


let rec type_eq (t1 : tipe) (t2 : tipe) : bool =
  m_print("\n Inside type_eq  branch, t1 is : "^tipe2string(t1)^"\n t2 is :"^tipe2string(t2)^"\n ");
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
      match (!r1, !r2) with
      | None, None -> true
      | Some t1, Some t2 -> type_eq t1 t2
      | _ -> false
  | _ -> false


let rec unify (t1 : tipe) (t2 : tipe) : bool =
   m_print("\n Inside unify, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n");
  if t1 == t2 then (m_print("\n Inside unify then branch \n"); true)
  else
    (m_print("\n Inside unify else branch \n");
    match (t1, t2) with
    | Guess_t ({ contents = Some t1' }), _ ->
         m_print("\n Inside Unify Match a guess, _ \n");
        unify t1' t2
    |  Guess_t r, t2 when !r = None ->
              m_print("\n Inside Unify Match a empty guess, _ \n");
              r := Some t2;
              true
    |  t1, Guess_t r when !r = None ->
                  m_print("\n Inside Unify Match a empty guess, _  2, will this lead some bugs??\n");
                  r := Some t1;
                  true
    | _, Guess_t ({ contents = Some t2' }) ->
        m_print("\n Inside Unify Match a _ guess \n");
        unify t2' t1
    | Int_t, Int_t ->
        true
    | Bool_t, Bool_t ->
        true
    | Pair_t(t1a, t1b), Pair_t(t2a, t2b) ->
             unify t1a t2a && unify t1b t2b
    | Fn_t (t1a, t1b), Fn_t (t2a, t2b) ->
        unify t1a t2a && unify t1b t2b
    | List_t (t1a), List_t (t2a) ->
        unify t1a t2a
    | _ ->
        m_print("\nUnify return false, t1 is : "^(string_of_tipe t1)^"\n t2 is :"^(string_of_tipe t2)^"\n" );
        false)



let rec tipe_list_to_string (tl : tipe list) : string =
  match tl with
  | [] -> ""
  | [t] -> string_of_tipe t
  | t :: tl' -> string_of_tipe t ^ ", " ^ tipe_list_to_string tl'


let string_of_substitution (s: (tvar * tipe) list) : string =
  let string_of_pair (tv, t) =
    tv ^ " -> " ^ string_of_tipe t
  in
  String.concat ", " (List.map string_of_pair s)



let rec substitute (s: (tvar * tipe) list) (t: tipe) : tipe =
  m_print("\n Inside substitue, s is : "^string_of_substitution(s)^"\n t is : "^string_of_tipe(t)^"\n");
  match t with
  | Tvar_t a -> (try List.assoc a s with Not_found -> t)
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t (t1, t2) -> Fn_t (substitute s t1, substitute s t2)
  | Pair_t (t1, t2) -> Pair_t (substitute s t1, substitute s t2)
  | List_t t' -> List_t (substitute s t')
  | Guess_t r -> m_print("\n I am guess type inside substitue\n"); Guess_t r (* no substitution for guess types *)


let instantiate (s: tipe_scheme): tipe =
  match s with
  | Forall (vs, t) ->
    m_print("\n in instantiate, the input scheme is : "^string_of_tipe_scheme(s)^"\n");
    m_print("\n in instantiate, the input t is : "^string_of_tipe(t)^"\n");
    let b = List.map (fun a -> (a, Guess_t (ref None))) vs in
    m_print("\n in instantiate, after map the input t is : "^string_of_tipe(t)^"\n");
    substitute b t


let rec lookup (x : var) (env : (var * tipe_scheme) list) : tipe_scheme =
  match env with
  | [] ->
      failwith ("Variable " ^ x ^ " not found in environment")
  | (y, scheme) :: rest ->
      if x = y then
        (m_print("in lookup, scheme for "^y^" is :"^string_of_tipe_scheme(scheme));
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
                 m_print("\n Match None guess in prim_to_tipe ");
                 Bool_t;
              | Tvar_t t ->
                              m_print("\n Match Tvar in prim_to_tipe ");
                              Bool_t;
                | Int_t ->  m_print("\n Match Int in prim_to_tipe ");
                                                         Bool_t;
                | Bool_t ->  m_print("\n Match Bool in prim_to_tipe ");
                                                          Bool_t;
                | Unit_t ->  m_print("\n Match Unin in prim_to_tipe ");
                                                          Bool_t;
                | Fn_t (t1, t2) ->  m_print("\n Match Fn in prim_to_tipe ");
                                                                 Bool_t;
                | Pair_t (t1, t2) ->  m_print("\n Match Pair in prim_to_tipe ");
                                                                   Bool_t;
                | List_t t ->  m_print("\n Match List in prim_to_tipe ");
                                                            Bool_t;
                | Guess_t ({ contents = Some t2' }) ->
                    m_print("\n Match Some Guess in prim_to_tipe the content of guess is :"^string_of_tipe(t2'));
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
      m_print "\n Match Var\n";
      let res = instantiate(lookup x env) in
      m_print ("res is :"^string_of_tipe(res));
      res
    | (PrimApp (p, es), _) ->
        m_print ("\n Match PrimApp (p, es)\n    p is : "^(prim_to_string p)^"\n    es is : "^string_of_exps(es));
        (match p with
          | Int _ -> Int_t
          | Bool _ -> Bool_t
          | Unit -> Unit_t
          | Cons ->
             let ts = List.map (fun e -> type_check_exp' e env) es in
             let rec unwrap_list_t t =
               match t with
               | List_t t' -> unwrap_list_t t'
               | Guess_t ({ contents = Some t' })  -> unwrap_list_t t'
               | _ -> t
             in
             (*check if the last element is "Nil" or List_t,
             if not, type check should fail;
             Every element except Nil should be the same type*)
             let all_same_unwrapped_type ts =
               let unwrapped_ts = List.map unwrap_list_t ts in
               m_print("\n The flattened list is : "^tipe_list_to_string(unwrapped_ts)^"\n");
                let last = List.hd (List.rev unwrapped_ts) in
               let ts = (match last with
               | Tvar_t(v) -> List.tl (List.rev unwrapped_ts)
               | _ -> unwrapped_ts) in
               match ts with
               | [] -> true
               | h::t -> List.for_all (fun t' ->
                (*every t should
                    either:  be the same type with h
                    or:   one of t and h should be empty guess, do association here*)
                    unify t' h
                                      ) t
             in
             let has_nil ts =
                let last = List.hd (List.rev ts) in
                let tail = (match last with
                | Tvar_t(v) -> v
                | List_t(t) -> "List"
                | Guess_t ({ contents = Some t' })  -> "Some Guess"
                | _ -> "error") in
                m_print("\n The tail is : "^tail^"\n");
                match tail with
                | "Nil" -> true
                | "List" -> true
                | "Some Guess" -> true
                | _ -> false
              in
             m_print("\n the list is : "^tipe_list_to_string(ts)^"\n");
             if (all_same_unwrapped_type ts) && (has_nil ts) then List_t(List.hd ts) else type_error("\n Failed location 012")
          | Plus | Minus | Times | Div ->
             let ts = List.map (fun e -> type_check_exp' e env) es in
             (match ts with
             | [t1; t2] ->
                (match (t1, t2) with
                (*The result must be int, and input must be int *)
                | Guess_t r1, Guess_t r2 when !r1 = None && !r2 = None ->
                                     m_print("\n Inside PrimApp  Match two empty guesses \n");
                                     r1:= Some Int_t;
                                     r2:= Some Int_t;
                                     t1;
                | Guess_t r, t2 when !r = None ->
                    m_print("\n Inside PrimApp  Match a guess and _ \n the guess is : "^string_of_tipe(t1)^"\n and the t2 is : "^string_of_tipe(t2)^"\n");
                    r:= Some Int_t;
                    if (unify t1 t2) then t2 else type_error("\n Failed location 014, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n")
                | t1, Guess_t r when !r = None ->
                     m_print("\n Inside PrimApp  Match a _ and guess \n the t1 is : "^string_of_tipe(t1)^"\n and the guess is : "^string_of_tipe(t2)^"\n");
                     r:= Some Int_t;
                     if (unify t1 t2) then t1 else type_error("\n Failed location 015, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n")
                | t1, t2 ->
                    m_print("\n Inside PrimApp  Match t1 , t2 \n the t1 is : "^string_of_tipe(t1)^"\n and the t2 is : "^string_of_tipe(t2)^"\n");
                    if t1 = t2 then t1 else type_error("\n Failed location 013, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n")
                )
             | _ -> type_error("Failed location 002"))
          | Nil -> Tvar_t("Nil")
          | Eq | Lt -> Bool_t
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
             g1;
          | Snd ->
            (*
            by now, I know p is Fst  and es is x, so I know x should be pair type, then I update my env  (x, ForAll([], g1 ) ) becomes  (x, ForAll([], Pair(g3, g4) ) )
                   and in the PrimApp(p, es) case, I should return a Fn_t( Pair(g1, g2), g2 )
            *)
               let g1 = Guess_t(ref None) in
               let g2 = Guess_t(ref None) in
               let get_var_from_exp_list (exps : exp list) : var =
                     match exps with
                     | [Var x, _] -> x
                     | _ -> type_error("Failed location 010")
               in
               let vname = get_var_from_exp_list es in
               update_env vname env (Pair_t(g1, g2));
               g2;
          | IsNil -> (*Dont care true of false*)Bool_t
          | Hd ->
                   let g1 = Guess_t(ref None) in
                   let get_var_from_exp_list (exps : exp list) : var =
                         match exps with
                         | [Var x, _] -> x
                         | _ -> type_error("Failed location 011")
                   in
                   let vname = get_var_from_exp_list es in
                   update_env vname env (List_t(g1));
                   g1;


          | Tl -> let g1 = Guess_t(ref None) in
                 let get_var_from_exp_list (exps : exp list) : var =
                       match exps with
                       | [Var x, _] -> x
                       | _ -> type_error("Failed location 016")
                 in
                 let vname = get_var_from_exp_list es in
                 update_env vname env (List_t(g1));
                 List_t(g1);
          )
    | (Fn (x, e), _) ->
        m_print "\n Match Fn (x, e) \n";
        m_print("    x is : "^x);
                 m_print("\n    e is : "^string_of_exp(e));
          let g = Guess_t(ref None) in
         (* let tvar = Tvar_t (ppfreshtvar ()) in*)
          let env' = (x, Forall ([], g)) :: env in
          let body_tipe = type_check_exp' e env' in
          Fn_t (g, body_tipe);
    | (App (e1, e2), _) ->
        m_print "\n Match App (e1, e2)\n";
         m_print("    e1 is : "^string_of_exp(e1)^"\n");
         m_print("\n    e2 is : "^string_of_exp(e2)^"\n");
         let t1 = type_check_exp' e1 env in
         m_print("\n after check e1, I got t1 is :"^string_of_tipe(t1)^"\n");
         let t2 = type_check_exp' e2 env in
         m_print("\n after check e2, I got t2 is :"^string_of_tipe(t2)^"\n");
         m_print("\n Before unify t1 is : "^string_of_tipe(t1)^" and t2 is : "^string_of_tipe(t2));
         let t = Guess_t(ref None)
         in
         if unify t1 (Fn_t(t2,t)) then t
         else type_error("Fail with App match t1 with guess")
    | (If (e1, e2, e3), _) ->
        m_print ("\n match If, e1 is : "^string_of_exp(e1)^"\n e2 is : "^string_of_exp(e2)^"\n e3 is : "^string_of_exp(e3)^"\n");
            let t1 = type_check_exp' e1 env in
            let t2 = type_check_exp' e2 env in
            let t3 = type_check_exp' e3 env in
        m_print("\n match If, t1 is : "^string_of_tipe(t1)^"\n t2 is : "^string_of_tipe(t2)^"\n t3 is : "^string_of_tipe(t3)^"\n");
            let t = Guess_t(ref None) in
            (*t1 must be a bool*)
            (match t1 with
            | Guess_t r when !r = None ->
                r:= Some Bool_t
            | Bool_t -> ();
            | _ -> type_error "Type error: Invalid argument types for If expression 015");
            if unify t2 t3 then t2
            else type_error "Type error: Invalid argument types for If expression"
    | (Let (x, e1, e2), _) ->
        m_print "\n match Let\n";
                   Tvar_t("")
  in
  let t = type_check_exp' e [] in
  t
