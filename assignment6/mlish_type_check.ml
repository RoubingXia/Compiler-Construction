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


let subst_compose (s2 : substitution) (s1 : substitution) : substitution =
  let s1' = List.map (fun (tv, t) -> (tv, subst_apply s2 t)) s1 in
  s2 @ s1'

let subst_domain (s : substitution) : tvar list =
  List.map fst s

let instantiate (ts:tipe_scheme) : tipe =
  let rec instantiate' (subst:(tvar*tipe) list) (t:tipe) : tipe =
    match t with
    | Tvar_t tv ->
        begin
          try List.assoc tv subst
          with Not_found -> Tvar_t tv
        end
    | Int_t -> Int_t
    | Bool_t -> Bool_t
    | Unit_t -> Unit_t
    | Fn_t (t1, t2) ->
        Fn_t (instantiate' subst t1, instantiate' subst t2)
    | Pair_t (t1, t2) ->
        Pair_t (instantiate' subst t1, instantiate' subst t2)
    | List_t t' ->
        List_t (instantiate' subst t')
    | Guess_t r ->
        match !r with
        | None ->
            let tv = ppfreshtvar () in
            let t' = Tvar_t tv in
            r := Some t';
            t'
        | Some t' -> instantiate' subst t'
  in
  match ts with
  | Forall (tvars, t) ->
      let subst = List.map (fun tv -> (tv, Tvar_t (ppfreshtvar ()))) tvars in
      instantiate' subst t


let rec occurs_in (a:tvar) (t:tipe) : bool =
  match t with
  | Tvar_t b -> a = b
  | Int_t | Bool_t | Unit_t -> false
  | Fn_t (t1, t2) -> occurs_in a t1 || occurs_in a t2
  | Pair_t (t1, t2) -> occurs_in a t1 || occurs_in a t2
  | List_t t' -> occurs_in a t'
  | Guess_t tr ->
    match !tr with
    | None -> false
    | Some t' -> occurs_in a t'


let rec unify (t1 : tipe) (t2 : tipe) : substitution =
  match (t1, t2) with
  | (Int_t, Int_t) -> []
  | (Bool_t, Bool_t) -> []
  | (Unit_t, Unit_t) -> []
  | (Tvar_t v, t) | (t, Tvar_t v) ->
    if not (occurs_in v t) then
      [(v, t)]
    else
      type_error ( "recursive unification")
  | (Fn_t (a1, r1), Fn_t (a2, r2)) ->
    let s1 = unify a1 a2 in
    let s2 = unify (subst_apply s1 r1) (subst_apply s1 r2) in
    subst_compose s2 s1
  | (Pair_t (l1, r1), Pair_t (l2, r2)) ->
    let s1 = unify l1 l2 in
    let s2 = unify (subst_apply s1 r1) (subst_apply s1 r2) in
    subst_compose s2 s1
  | (List_t t1, List_t t2) ->
    unify t1 t2
  | _ ->
    type_error ("type mismatch")


let rec unify_all (ts : (tipe * tipe) list) : substitution =
  match ts with
  | [] -> []
  | (t1, t2) :: ts' ->
    let subst = unify t1 t2 in
    let subst' = unify_all (apply_subst_to_pairs subst ts') in
    compose_subst subst' subst
  (* helper function to apply a substitution to a list of type pairs *)
  and apply_subst_to_pairs (subst : substitution) (ts : (tipe * tipe) list) : (tipe * tipe) list =
    List.map (fun (t1, t2) -> (subst_apply subst t1, subst_apply subst t2)) ts
  (* helper function to compose two substitutions *)
  and compose_subst (subst1 : substitution) (subst2 : substitution) : substitution =
    let subst2' = List.map (fun (x, t) -> (x, subst_apply subst1 t)) subst2 in
    subst2' @ subst1


let prim_to_tipe = function
  | Int _ -> Int_t
  | Bool _ -> Bool_t
  | Unit -> Unit_t
  | Plus | Minus | Times | Div -> Fn_t (Int_t, Fn_t (Int_t, Int_t))
  | Eq | Lt -> Fn_t (Int_t, Fn_t (Int_t, Bool_t))
  | Pair -> Fn_t (Guess_t (ref None), Fn_t (Guess_t (ref None), Pair_t (Tvar_t (ppfreshtvar ()), Tvar_t (ppfreshtvar ()))))
  | Fst -> Fn_t (Pair_t (Tvar_t (ppfreshtvar ()), Tvar_t (ppfreshtvar ())), Tvar_t (ppfreshtvar ()))
  | Snd -> Fn_t (Pair_t (Tvar_t (ppfreshtvar ()), Tvar_t (ppfreshtvar ())), Tvar_t (ppfreshtvar ()))
  | Nil -> List_t (Tvar_t (ppfreshtvar ()))
  | Cons -> Fn_t (Tvar_t (ppfreshtvar ()), Fn_t (List_t (Tvar_t (ppfreshtvar ())), List_t (Tvar_t (ppfreshtvar ()))))
  | IsNil -> Fn_t (List_t (Tvar_t (ppfreshtvar ())), Bool_t)
  | Hd -> Fn_t (List_t (Tvar_t (ppfreshtvar ())), Tvar_t (ppfreshtvar ()))
  | Tl -> Fn_t (List_t (Tvar_t (ppfreshtvar ())), List_t (Tvar_t (ppfreshtvar ())))
*)

let type_check_exp (e : exp) : tipe =
  let rec type_check_exp' (e : exp) (env : (var * tipe_scheme) list) : tipe =
    match e with
    | (Var x, _) ->
      print_string "match Var";
      Tvar_t("")
    | (PrimApp (p, es), _) ->
        print_string "match PrimApp";
           Tvar_t("")
    | (Fn (x, e), _)  ->
        print_string "match Fn";
                   Tvar_t("")
    | (App (e1, e2), _) ->
        print_string "match App";
                   Tvar_t("")
    | (If (e1, e2, e3), _) ->
        print_string "match If";
                   Tvar_t("")
    | (Let (x, e1, e2), _) ->
        print_string "match Let";
                   Tvar_t("")
      (*
    | (PrimApp (p, es), _) ->
      let (arg_tipes, arg_vars) = List.split (List.map (fun e -> type_check_exp' e env) es) in
      let ret_tipe = prim_to_tipe p in
      (*unify_all (List.map2 (fun a b -> (a, b)) arg_tipes (prim_arg_tipes p)) (subst_vars arg_vars []), ret_tipe)
      *)
      let prim_arg_tipes = function
        | Plus | Minus | Times | Div -> [Int; Int]  in
       let arg_pairs = List.map2 (fun a b -> (a, b)) arg_tipes (prim_arg_tipes p) in
            let s1 = unify_all arg_pairs empty_subst in
            let s2 = subst_vars arg_vars s1 in
            let s3 = unify (subst s2 ret_tipe) (type_apply_subst s2 (fresh_tvar ())) s2 in
            s3, subst s3 ret_tipe
    | (Fn (x, e), _) ->
      let tvar = Tvar_t (ppfreshtvar ()) in
      let env' = (x, Forall ([], tvar)) :: env in
      let (body_tipe, body_vars) = type_check_exp' e env' in
      (Fn_t (subst_vars body_vars [], tvar), [])
    | (App (e1, e2), _) ->
      let (t1, v1) = type_check_exp' e1 env in
      let (t2, v2) = type_check_exp' e2 env in
      let tvar = Tvar_t (ppfreshtvar ()) in
      let u = Tvar_t (ppfreshtvar ()) in
      let sub = unify (subst_vars t1 v1) (Fn_t (subst_vars t2 v2, u)) in
      let ret_tipe = subst_vars u sub in
      (subst_vars ret_tipe sub, [])
    | (If (e1, e2, e3), _) ->
      let (t1, v1) = type_check_exp' e1 env in
      let (t2, v2) = type_check_exp' e2 env in
      let (t3, v3) = type_check_exp' e3 env in
      let sub1 = unify t1 Bool_t in
      let sub2 = unify (subst_vars t2 v2) (subst_vars t3 v3) in
      (subst_vars (subst_vars t2 v2) (compose_subs sub2 sub1), [])
    | (Let (x, e1, e2), _) ->
      let (t1, v1) = type_check_exp' e1 env in
      let tvar_list = free_tvars t1 in
      let scheme = Forall (tvar_list, t1) in
      let env' = (x, scheme) :: env in
      let (t2, v2) = type_check_exp' e2 env' in
      (t2, []) *)
  in
  let t = type_check_exp' e [] in
  t

(*
 let rec tc (env:var->tipe) (e:exp) =
  match e with
  | Var x -> env x
  | Int _ -> Int_t
  | Plus_i(e1,e2) ->
  (match tc env e1, tc env e with
  | Int_t, Int_t -> Int_t
  | _,_ => error())
  | Lambda(x,t,e) -> Arrow_t(t,tc (extend env x t) e)
  | App(e1,e2) ->
  (match (tc env e1, tc env e2) with
  | Arrow_t(t1,t2), t ->
  if (t1 != t) then error() else t2
  | _,_ -> error())
  *)