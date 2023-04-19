open Mlish_ast
exception TypeError
let type_error(s:string) = ((*print_string s;*) raise TypeError)
let log_open = false
let m_print(s:string) =
    if (log_open) then print_string(s)

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)
(*
module GuessSet = struct
  module H = Hashtbl.Make(struct
    type t = tipe option ref
    let equal r1 r2 = r1 == r2
    let hash r = Hashtbl.hash (Obj.magic r)
  end)

  type t = H.t

  let create ?(size=16) () = H.create size

  let mem set r = H.mem set r

  let add set r = H.add set r ()

  let remove set r = H.remove set r

  let clear set = H.clear set

  let iter f set = H.iter (fun r () -> f r) set

  let fold f set acc = H.fold (fun r () acc -> f r acc) set acc
end
*)
(*
module GuessSet = Hashtbl.Make(struct
    type t = tipe option ref
    let equal r1 r2 = r1 == r2
    let hash r = Hashtbl.hash (Obj.magic r)
end)
*)
module GuessSet = Set.Make(struct
    type t = tipe option ref
    let compare r1 r2 = if r1 == r2 then 0 else 1
    end)


(**********************************************************)
(* Pretty printing                                        *)
(**********************************************************)
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

let string_of_tipes (ts: tipe list) : string =
  let ts_str = List.map string_of_tipe ts in
  String.concat ";\n" ts_str

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

let string_of_env (env : (var * tipe_scheme) list) : string =
  let string_of_binding ((x, t) : var * tipe_scheme) : string =
    x ^ " : " ^ (string_of_tipe_scheme t)
  in
  String.concat ", " (List.map string_of_binding env)

let tvar_tipe_pair_to_string (tv, t) =
  tv ^ " : " ^ string_of_tipe t

let rec string_of_tvar_tipe_list (lst: (tvar * tipe) list) =
  match lst with
  | [] -> ""
  | [(tv, t)] -> tvar_tipe_pair_to_string (tv, t)
  | (tv, t) :: rest ->
      tvar_tipe_pair_to_string (tv, t) ^
      ", " ^
      string_of_tvar_tipe_list rest

let string_of_guess_pairs guess_pairs =
  let string_of_guess_pair (guess, _) =
    let guess_str = match guess with
      | Some t -> "Some " ^ string_of_tipe t
      | None -> "None"
    in
    "(" ^ guess_str ^ ", let ppfreshtvar ())"
  in
  "[" ^ String.concat "; " (List.map string_of_guess_pair guess_pairs) ^ "]"

let string_of_guess_pairs_add guess_pairs =
  let string_of_guess_pair (guess, var) =
    let guess_str =
        string_of_int(Obj.magic guess)
    in
    "(" ^ guess_str ^", "^var ^")"
  in
  "[" ^ String.concat "; " (List.map string_of_guess_pair guess_pairs) ^ "]"

let guesses_addr_to_string (guesses : (tipe option ref) list) : string =
  let guess_to_string (guess : tipe option ref) : string =
         string_of_int(Obj.magic guess)
  in
  String.concat ", " (List.map guess_to_string guesses)

let guesses_to_string (guesses : (tipe option ref) list) : string =
  let guess_to_string (guess : tipe option ref) : string =
    match !guess with
    | None -> "_"
    | Some t -> string_of_tipe t
  in
  String.concat ", " (List.map guess_to_string guesses)

let string_of_int_set s =
  "{" ^ (String.concat ", " (List.map string_of_int (IntSet.elements s))) ^ "}"

(**********************************************************)
(* Pretty printing end                                    *)
(**********************************************************)


let rec type_eq (t1 : tipe) (t2 : tipe) : bool =
  (*m_print("\n Inside type_eq  branch, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n ");*)
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
  | Guess_t r1, Guess_t r2 -> false
  | _ -> false


let rec unify (t1 : tipe) (t2 : tipe) : bool =
   m_print("\n Inside unify, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n");
  if (type_eq t1 t2) then (m_print("\n Inside unify then branch \n"); true)
  else
    (m_print("\n Inside unify else branch \n");
    (*
     Before unify t1 is : (Guess[(Empty Guess * Empty Guess)] -> Empty Guess) and t2 is : Empty Guess
     Inside unify, t1 is : (Guess[(Empty Guess * Empty Guess)] -> Empty Guess)
     t2 is :(Empty Guess -> Empty Guess)
    *)
    match (t1, t2) with
    |  Guess_t r1, Guess_t r2 when !r1 = None && !r2 = None && r1 == r2  ->
                  m_print("\n Inside Unify Match two empty guesses, return true and do nothing \n");
                  true
    |  Guess_t r, t2 when !r = None ->
                  m_print("\n Inside Unify Match a empty guess, _ \n");
                  r := Some t2;
                  true
    |  t1, Guess_t r when !r = None ->
                  m_print("\n Inside Unify Match a  _, empty guess \n");
                  r := Some t1;
                  true
    | Guess_t ({ contents = Some t1' }), _ ->
         m_print("\n Inside Unify Match a guess, _ \n");
        unify t1' t2
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


let rec substitute (s: (tvar * tipe) list) (t: tipe) : tipe =
  (*m_print("\n Inside substitue, s is : "^string_of_substitution(s)^"\n t is : "^string_of_tipe(t)^"\n");*)
  match t with
  | Tvar_t a -> (try List.assoc a s with Not_found -> t)
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t (t1, t2) -> Fn_t (substitute s t1, substitute s t2)
  | Pair_t (t1, t2) -> Pair_t (substitute s t1, substitute s t2)
  | List_t t' -> List_t (substitute s t')
  | Guess_t r -> (*m_print("\n I am guess type inside substitue\n");*) Guess_t r (* no substitution for guess types *)

(*
pair use with generalize, for a input Forall this function will replace all type variables with empty guesses, e.g.
for input like Forall([tv1], Fn_t(tv1, tv1)), this function should return Fn_t(g1, g1);
*)
let instantiate (s: tipe_scheme): tipe =
  match s with
  | Forall (vs, t) ->
    m_print("\n in instantiate, the input scheme is : "^string_of_tipe_scheme(s)^"\n");
    let b = List.map (fun a -> (a, Guess_t (ref None))) vs in
    m_print("\n in instantiate, after map the b is : "^string_of_tvar_tipe_list(b)^"\n");
    substitute b t


let subst_tvars (pairs : (tipe option ref * tvar) list) (t : tipe) : tipe =
  let rec loop t =
    match t with
    | Tvar_t _ -> t
    | Int_t -> t
    | Bool_t -> t
    | Unit_t -> t
    | Fn_t (t1, t2) -> Fn_t (loop t1, loop t2)
    | Pair_t (t1, t2) -> Pair_t (loop t1, loop t2)
    | List_t t' -> List_t (loop t')
    | Guess_t {contents = Some t'} ->
      loop t'
    | Guess_t r when !r = None ->
        (*search the gr in pairs(key,value), if gr == key return value*)
         (*(match List.assoc_opt r pairs with
                | Some tv -> Tvar_t tv
                | None -> m_print("\n Ah!!!!!!!!! This should never happen!!!"); Guess_t r)*)
    (*     (try List.assoc a s with Not_found -> t)*)
         let (_, tv) = (try List.find (fun (tipe_opt_ref, tvar_name) ->
                  tipe_opt_ref == r) pairs with Not_found ->  (r, "Not_Found")) in
         (match tv with
         | "Not_Found" -> t
         | _ -> Tvar_t(tv))


  in
  loop t
(*
module IntSet = struct
  type t = int
  let compare = compare
end

module Set = struct
  include Set.Make(IntSet)
  let mem x s = try ignore (find x s); true with Not_found -> false
end*)



let uniq_list lst =
  let rec loop acc set lst =
    (match lst with
    | [] ->  acc
    | x :: xs ->
     m_print("\n this set :"^string_of_int_set(set)^"\n");
      if IntSet.mem (Obj.magic x) set then
        loop acc set xs
      else
        let set' = IntSet.add (Obj.magic x) set in
        loop (x :: acc) set' xs)
  in
  loop [] IntSet.empty lst
(*
let uniq_list l =
  let set = ref IntSet.empty in
  List.fold_left (fun acc x ->
    if IntSet.mem x !set then
      acc
    else
      begin
        set := IntSet.add x !set;
        x :: acc
      end
  ) [] (List.rev l)
*)
let get_all_guesses_in_type (t : tipe) : (tipe option ref) list =
  let rec loop t acc =
    match t with
    | Guess_t {contents = Some t'} -> loop t' acc
    | Guess_t gr when !gr = None -> gr :: acc
    | Fn_t (t1, t2) -> loop t1 (loop t2 acc)
    | Pair_t (t1, t2) -> loop t1 (loop t2 acc)
    | List_t t' -> loop t' acc
    | _ -> acc
  in
  loop t []

let unique_refs (lst : (tipe option ref) list) =
  let guess_set = ref (GuessSet.empty)in
  let add_to_set ele =
    guess_set := GuessSet.add ele !guess_set
  in
  List.iter add_to_set lst;
  !guess_set


(*
let get_all_guesses_in_type (t : tipe) : (tipe option ref) list =
  let rec loop t set acc =
    match t with
    | Guess_t {contents = Some t'} -> loop t' set acc
    | Guess_t gr when !gr = None ->
      if Set.mem (Obj.magic gr) set then
        acc
      else
        let set' = Set.add (Obj.magic gr) set in
        gr :: acc
    | Fn_t (t1, t2) -> loop t1 set (loop t2 set acc)
    | Pair_t (t1, t2) -> loop t1 set (loop t2 set acc)
    | List_t t' -> loop t' set acc
    | _ -> acc
  in
  loop t Set.empty []
*)

let get_all_guesses_in_env (env : (var * tipe_scheme) list) : (tipe option ref) list =
  let rec loop t acc =
    match t with
    | Guess_t {contents = Some t'} -> loop t' acc
    | Guess_t gr when !gr = None -> gr :: acc
    | Fn_t (t1, t2) -> loop t1 (loop t2 acc)
    | Pair_t (t1, t2) -> loop t1 (loop t2 acc)
    | List_t t' -> loop t' acc
    | _ -> acc
  in
  List.fold_left (fun acc (_, Forall (_, tipe)) -> loop tipe acc) [] env


(*
generalize find guesses in t as gs1 and guesses in env as gs2, then find the guesses that only
show in t not in env, let's call this set diff_gs. Then create a new_t which is simliar to t but replace all guess in diff_gs
to a type variable, then return Forall([type variables], new_t), e.g. when call generalize env t, where env has g1, and t
is Fn_t(g2, g2), the function should return a Forall([tv1], Fn_t(tv1, tv1))
*)
let generalize (env : (var * tipe_scheme) list) (t : tipe) : tipe_scheme =
  (*Get all guesses from env*)
  let gs_of_env = get_all_guesses_in_env env in
  (*Get all guesses from t*)
  let gs_list = get_all_guesses_in_type t in
   m_print("\n gs_list : "^guesses_addr_to_string(gs_list)^"");
  let gs_of_env_set =  unique_refs gs_of_env in
  let gs_of_t_set =  unique_refs gs_list in
  (*Get guesses only in t*)
  m_print("\n Still alive 01");
  let diff_gs = GuessSet.elements(GuessSet.diff gs_of_t_set gs_of_env_set) in
  m_print("\n Still alive 02");
  m_print("\n diff_gs : "^guesses_addr_to_string(diff_gs)^"");
  m_print("\n Still alive 03");
  let make_guess_pairs guesses =
    List.map (fun guess -> (guess, ppfreshtvar())) guesses
  in
  m_print("\n Still alive 04");
  let gs_pair = make_guess_pairs diff_gs in
  m_print("\n Still alive 05");
  let tc = subst_tvars gs_pair t in
  m_print("\n Still alive 06");
 (* m_print("\n In generalize the guess in env is : "^guesses_to_string(gs_of_env)^"\n\t the guess in t is : "^guesses_to_string(gs_of_t)^"\n\t guess in diff is "^guesses_to_string(diff_gs)^"\n");
  *)m_print("\n\t gs_pair : "^string_of_guess_pairs_add(gs_pair)^"\n");
  m_print("\n Still alive 07");
  Forall(List.map snd gs_pair, tc)
  (*
  let tv1 = ppfreshtvar() in
  Forall ([tv1], Fn_t(Tvar_t(tv1), Tvar_t(tv1)))*)

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


let type_check_exp (e : exp) : tipe =
  let rec type_check_exp' (e : exp) (env : (var * tipe_scheme) list) : tipe =
    match e with
    | (Var x, _) ->
      (*m_print("\n Match Var "^x^"\n");*)
      let res = instantiate(lookup x env) in
     (* m_print ("\n Res is :"^string_of_tipe(res));*)
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
             m_print("\n Match PrimApp(p, es) \n\t p : "^prim_to_string(p)^" \n\t es : "^string_of_exps(es)^"\n\t ts : "^string_of_tipes(ts)^"\n");
             m_print("\n Current env is : "^string_of_env(env)^"\n");
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
                    m_print("\n Inside PrimApp  Match a empty guess and _ \n the guess is : "^string_of_tipe(t1)^"\n and the t2 is : "^string_of_tipe(t2)^"\n");
                    m_print("\n Current env is : "^string_of_env(env)^"\n");
                    r:= Some Int_t;
                    if (unify t1 t2) then (
                    m_print("\n Current env is : "^string_of_env(env)^"\n");
                    t2) else type_error("\n Failed location 014, t1 is : "^string_of_tipe(t1)^"\n t2 is :"^string_of_tipe(t2)^"\n")
                | t1, Guess_t r when !r = None ->
                     m_print("\n Inside PrimApp  Match a _ and empty guess \n the t1 is : "^string_of_tipe(t1)^"\n and the guess is : "^string_of_tipe(t2)^"\n");
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
        let res = Fn_t (g, body_tipe) in
        m_print ("\n Match Fn (x, e) \n\t x : "^x^"\n\t e :"^string_of_exp(e)^"\n\t return: "^string_of_tipe(res)^"\n");
        m_print("\n Current env is : "^string_of_env(env)^"\n");
        res
    | (App (e1, e2), _) ->
         m_print ("\n Match App (e1, e2)\n\t e1 : "^string_of_exp(e1)^"\n\t e2 : "^string_of_exp(e2)^"\n");
         let t1 = type_check_exp' e1 env in
         let t2 = type_check_exp' e2 env in
         let t = Guess_t(ref None)
         in
         m_print("\n Before unify t1 is : "^string_of_tipe(t1)^" and t2 is : "^string_of_tipe(t2));
         if unify t1 (Fn_t(t2,t)) then (
            m_print "\n Match App (e1, e2)\n";
             m_print("\n e1 is : "^string_of_exp(e1)^"\n after check e1, I got t1 is :"^string_of_tipe(t1)^"\n");
             m_print("\n e2 is : "^string_of_exp(e2)^"\n after check e2, I got t2 is :"^string_of_tipe(t2)^"\n");
            m_print("\n returns : "^string_of_tipe(t)^"\n");
            m_print("\n Current env is : "^string_of_env(env)^"\n");
            t)
         else type_error("Fail with App match t1 with guess")
    | (If (e1, e2, e3), _) ->
        m_print ("\n match If, e1 is : "^string_of_exp(e1)^"\n e2 is : "^string_of_exp(e2)^"\n e3 is : "^string_of_exp(e3)^"\n");
            let t1 = type_check_exp' e1 env in
            let t2 = type_check_exp' e2 env in
            let t3 = type_check_exp' e3 env in
        m_print("\n match If, t1 is : "^string_of_tipe(t1)^"\n t2 is : "^string_of_tipe(t2)^"\n t3 is : "^string_of_tipe(t3)^"\n");
            let t = Guess_t(ref None) in
            (*t1 must be a bool*)
            if unify t1 Bool_t then () else type_error "Type error: Invalid argument types for If expression 015";
            (*(match t1 with
            | Guess_t r when !r = None ->
                r:= Some Bool_t
            | Bool_t -> ();
            | _ -> type_error "Type error: Invalid argument types for If expression 015");*)
            if unify t2 t3 then t2
            else type_error "Type error: Invalid argument types for If expression"
    | (Let (x, e1, e2), _) ->
        m_print ("\n Match Let \n\t x is : "^x^"\n\t e1 is :"^string_of_exp(e1)^"\n\t e2 is : "^string_of_exp(e2)^"\n");
        let t1 = type_check_exp' e1 env in
        m_print ("\n\t The t1 looks like : \n\t "^string_of_tipe(t1)^"\n");
        m_print ("\n\t The env before generalize looks like : \n\t "^string_of_env(env)^"\n");
        let s = generalize env t1 in
        m_print ("\n\t The s looks like : \n\t "^string_of_tipe_scheme(s)^"\n");
        let env' = (x, s) :: env in
        m_print ("\n\t The env after generalize looks like : \n\t "^string_of_env(env')^"\n");
        type_check_exp' e2 env'
  in
  let t = type_check_exp' e [] in
  t
