(* file: main.ml
   author: Robert Muller, J. Tassarotti

  Problem set 0: exercises for coming up to speed in OCaml.

  Usage from parent directory:

   > dune exec bin/main.exe test

   Note that omitting "test" in the above won't run the tests.

   There are 10 problems worth 6 points total.

   1. isDigit (.25)
   2. newFileName (.25)
   3. homePath (.25)
   4. atoi (.25)
   5. tokenize (1)
   6. formatTree (1)
   7. subpalindrome (.75)
   8. listComparisons (.5)
   9. patience (1)
   10. FilteredSet module (.75)
*)

(* Problem 1: .25 points

   isDigit : char -> bool
*)
let isDigit c = match c with
'0'..'9' -> true
| _ -> false

(* Problem 2: .25 points

   newFileName : string -> string -> string
*)

let newFileName oldFileName newExtension =
    let len = String.rindex_opt oldFileName '.' in
    let finalFileName = match len with
    | None -> oldFileName
    | Some len -> let baseFileName = String.sub oldFileName 0 len in
    let prefix = String.cat baseFileName "." in
    String.cat prefix newExtension
    in finalFileName


(* Problem 3: .25 points

   homePath : unit -> string list
*)

let homePath () =
    let rawStr = Unix.getenv "HOME" in
    (* remove the first / char*)
    let homePathStr = String.sub rawStr 1 ((String.length rawStr) - 1) in
    String.split_on_char '/' homePathStr


(* Problem 4: .25 points

   atoi : string -> int
*)
 let listToInt = List.fold_left (fun x y -> x * 10 + y) 0
 let getIntList = List.map (fun x -> Char.code x - Char.code '0')

 let atoi num =
    let digits = Lib.explode num in
    let valList = getIntList digits in
    listToInt valList





type token = If | And | Or

(* Problem 5: 1 points

   tokenize : string -> token list
*)

let mapping x =
    match x with
     | "&&" -> And
     | "if" -> If
     | _ -> Or


let tokenize str =
    let strList = String.split_on_char ' ' str in
    let list1 = List.filter_map (fun x -> if x = "" then None else Some(x)) strList in
    List.map (mapping) list1



(* Two problems related to trees. We have arrows as interior nodes
   and leaves that include a unique constant C and variables
   {v1, v2, ... }

               ->             ->
              /  \           /  \
            v0   ->         C    v1
                /  \
               C    C
 *)

(* Note that the 'too' field should probably be called 'to' (that is the names would be from and to), but 'to'
   is already a reserved keyword in OCaml, so we cannot use it for a field name. *)

type t = C
       | Var of int
       | Arrow of { from : t
                  ; too  : t
                  }

(* Problem 6: 1 point

   formatTree : t -> string
*)
let rec getTree t = match t with
| C -> "C"
| Var(v) ->  Printf.sprintf "v%d" v
| Arrow(arr) -> let fStr = getTree arr.from in let tStr = getTree arr.too in Printf.sprintf "(%s -> %s)" fStr tStr
let formatTree t =
 getTree t

(* Problem 7: .75 point 

   subplaindrome : string -> string
*)
let rec isPalindrome str left right =
    if left >= right then true else
    let firstChar = String.get str left in
    let lastChar = String.get str right in
    if firstChar = lastChar && isPalindrome str (left + 1) (right - 1) then true else false

let rec helper str left right =
    if left >= right then String.sub str left 1 else
    if isPalindrome str left right then String.sub str left (right - left + 1) else
    helper str (left + 1) (right - 1)

let subpalindrome str =
    let right = String.length str in
    if right = 0 then "" else
    helper str 0 (right - 1)


(* Problem 8: .5 point

   list_comparisons : int list -> comparison list
*)
type comparison = GEQ | LT

let listComparisons inputList =
    let reversedList1 = List.rev inputList in
    let reversedList2 = List.tl reversedList1 in
    let list1 = List.rev reversedList2 in (* inputList without last element*)
    let list2 = List.tl inputList in (* inputList without first element*)
    let list3 = List.map2 (fun x y -> if y >= x then GEQ else LT) list1 list2 in
    List.append [GEQ] list3

(* Problem 9: 1 point

   patience : (int list) list -> int -> (int list) list
*)
let rec findIndex list n =
    match list with
    | [] -> -1;
    | pile::t -> let head = List.hd pile in if head >= n then 0 else 1 + findIndex t n
(* check if n should be add as a new pile*)
let check list n =
    List.exists (fun pile -> let head = List.hd pile in if head >= n then true else false) list
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;
let patience piles n =
    if check piles n then
    let index = findIndex piles n in
    let originalPile = List.nth piles index in
    let newPile = List.append [n] originalPile in
    replace piles index newPile
    else List.append piles [[n]]

(* Problem 10 : .75 points *)

module type FilteredSetType = sig
  type t
  val newSet : (int -> bool) -> t
  val insert : int -> t -> t
  val member : int -> t -> bool
  val mapAndFilter : (int -> int) -> t -> t
end
module FilteredSet : FilteredSetType =
struct
  type t = {fc : int -> bool; elements : int list}
  let newSet f = {fc = f; elements = []}
  let insert x set =
          let filterFunc = set.fc in
          let list = set.elements in
          let list2 = if filterFunc x then x :: list else list in
          {fc = filterFunc; elements = list2}
  let rec helper x s =
           match s with
              | [] -> false
              | elem::l -> if elem = x then true else helper x l
      let member x set =
          helper x set.elements
  let mapAndFilter f set =
          let list1 = List.map f set.elements in
          let list2 = List.filter set.fc list1 in
          {fc = set.fc; elements = list2}
end


(* TESTING **********************************************************)

type parts = One       (* isDigit *)

           | Two       (* newFileName *)
           | Three     (* homePath *)
           | Four      (* atoi *)
           | Five      (* tokenize *)
           | Six       (* formatTree *)
           | Seven       (* subpalindrome *)
           | Eight       (* listComparisons *)
           | Nine       (* patience *)
           | Ten       (* FilteredSet module *)


(* Some simple test data *)

let v0 = Lib.fresh()
let v1 = Lib.fresh()
let v2 = Lib.fresh()

(*            t0 = ->        t1 = ->
                  /  \           /  \
                v0   ->         C    v1
                    /  \
                   C    C
*)
let t0 = Arrow { from = Var v0
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }

let t1 = Arrow { from = C
               ; too  = Var v1
               }

let t2 = Arrow { from = t1
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }
let t3 = Arrow { from = C
               ; too  = t0
               }

(********************************************************************)

(* Test isDigit
*)
let isDigitTest1 () = isDigit '0'
let isDigitTest2 () = isDigit '9'
let isDigitTest3 () = not (isDigit 'A')
let isDigitTests () =
  Lib.run_test "isDigit test1" isDigitTest1 ;
  Lib.run_test "isDigit test2" isDigitTest2 ;
  Lib.run_test "isDigit test3" isDigitTest3

(* Test newFileName *)

let newFileNameTest1 () = newFileName "A.java" "class" = "A.class"
let newFileNameTest2 () = newFileName "A" "class" = "A"
let newFileNameTest3 () = newFileName "A.B.java" "class" = "A.B.class"
let newFileNameTest4 () = newFileName "A." "class" = "A.class"
let newFileNameTests () =
  Lib.run_test "newFileName test1" newFileNameTest1 ;
  Lib.run_test "newFileName test2" newFileNameTest2 ;
  Lib.run_test "newFileName test3" newFileNameTest3 ;
  Lib.run_test "newFileName test4" newFileNameTest4

(* Test homePath *)

let homePathTest () =
  let answer = homePath () in
  let combiner name1 name2 = Lib.fmt "%s/%s" name1 name2 in
  let path = List.fold_left combiner "" answer
  in
  try
    Unix.chdir path = ()
  with
    Unix.Unix_error _ -> failwith "homeTest failed"
let homePathTests () = Lib.run_test "home test" homePathTest

(* Test atoi *)
let atoiTest1 () = atoi "345" = 345
let atoiTest2 () = atoi "0" = 0
let atoiTests () =
  Lib.run_test "atoi test1" atoiTest1 ;
  Lib.run_test "atoi test2" atoiTest2

(* Test tokenize
*)
let tokenizeTest1 () =
  (tokenize "|| if && if && ") = [Or; If; And; If; And]
let tokenizeTest2 () = (tokenize "||") = [Or]
let tokenizeTest3 () = (tokenize "       ") = []
let tokenizeTests () =
  Lib.run_test "tokenize test1" tokenizeTest1 ;
  Lib.run_test "tokenize test2" tokenizeTest2 ;
  Lib.run_test "tokenize test3" tokenizeTest3

(* Test formatTree
*)
let formatTreeTest1 () = (formatTree t0) = "(v0 -> (C -> C))"
let formatTreeTest2 () = (formatTree t1) = "(C -> v1)"
let formatTreeTest3 () = (formatTree (Var v2)) = "v2"
let formatTreeTests () =
  Lib.run_test "formatTree test1" formatTreeTest1 ;
  Lib.run_test "formatTree test2" formatTreeTest2 ;
  Lib.run_test "formatTree test3" formatTreeTest3

(* Test subpalindrome
*)
let subpalindromeTest1 () = subpalindrome "aba" = "aba"
let subpalindromeTest2 () = subpalindrome "dabac" = "aba"
let subpalindromeTest3 () = subpalindrome "xx" = "xx"
let subpalindromeTest4 () = subpalindrome "x1amanaplanacanalpanamax1" = "amanaplanacanalpanama"
let subpalindromeTest5 () = subpalindrome "civic" = "civic"
let subpalindromeTest6 () = subpalindrome "deified" = "deified"
let subpalindromeTest7 () = subpalindrome "2eifie+" = "eifie"
let subpalindromeTest8 () = subpalindrome "xyz" = "y"
let subpalindromeTest9 () = subpalindrome "" = ""
let subpalindromeTests () =
  Lib.run_test "subpalindrome test1" subpalindromeTest1 ;
  Lib.run_test "subpalindrome test2" subpalindromeTest2 ;
  Lib.run_test "subpalindrome test3" subpalindromeTest3 ;
  Lib.run_test "subpalindrome test4" subpalindromeTest4 ;
  Lib.run_test "subpalindrome test5" subpalindromeTest5 ;
  Lib.run_test "subpalindrome test6" subpalindromeTest6 ;
  Lib.run_test "subpalindrome test7" subpalindromeTest7 ;
  Lib.run_test "subpalindrome test8" subpalindromeTest8 ;
  Lib.run_test "subpalindrome test9" subpalindromeTest9

let listComparisonsTest1 () = listComparisons [3] = [GEQ]
let listComparisonsTest2 () = listComparisons [3;4;5] = [GEQ; GEQ; GEQ]
let listComparisonsTest3 () = listComparisons [1;-1;1] = [GEQ; LT; GEQ]
let listComparisonsTest4 () = listComparisons [-1;-1;1] = [GEQ; GEQ; GEQ]
let listComparisonsTest5 () = listComparisons [9;8;7] = [GEQ; LT; LT]
let listComparisonsTest6 () = listComparisons [9;8;7;10] = [GEQ; LT; LT; GEQ]
let listComparisonsTests () =
  Lib.run_test "listComparisons test1" listComparisonsTest1 ;
  Lib.run_test "listComparisons test2" listComparisonsTest2 ;
  Lib.run_test "listComparisons test3" listComparisonsTest3 ;
  Lib.run_test "listComparisons test4" listComparisonsTest4 ;
  Lib.run_test "listComparisons test5" listComparisonsTest5 ;
  Lib.run_test "listComparisons test6" listComparisonsTest6 

let patienceTest1 () = patience [[3]] 4 = [[3]; [4]]
let patienceTest2 () = patience [] 3 = [[3]]
let patienceTest3 () = patience [[4]; [5]] 3 = [[3;4]; [5]]
let patienceTest4 () = patience [[2]; [6]] 4 = [[2]; [4;6]]
let patienceTest5 () = patience [[2]; [6]; [10]] 8 = [[2]; [6]; [8; 10]]
let patienceTest6 () = patience [[2]; [6]; [10]] 12 = [[2]; [6]; [10]; [12]]
let patienceTest7 () = patience [[2]; [3;6]; [10]] 3 = [[2]; [3;3;6]; [10]]
let patienceTest8 () = patience [[2]; [3]; [4]; [5]; [6]] 4 = [[2]; [3]; [4;4]; [5]; [6]]
let patienceTests () =
  Lib.run_test "patience test1" patienceTest1 ;
  Lib.run_test "patience test2" patienceTest2 ;
  Lib.run_test "patience test3" patienceTest3 ;
  Lib.run_test "patience test4" patienceTest4 ;
  Lib.run_test "patience test5" patienceTest5 ;
  Lib.run_test "patience test6" patienceTest6 ;
  Lib.run_test "patience test7" patienceTest7 ;
  Lib.run_test "patience test8" patienceTest8


let isEven n = (n mod 2 = 0)

open FilteredSet

let rec insert_list xs s =
  match xs with
  | [] -> s
  | x :: xs -> insert_list xs (insert x s)

let filteredSetTests_wrapper () =
  let evenSet_empty = newSet isEven in

  let evenSet_1 = insert_list [1;2;3;4;5;6] evenSet_empty in
  let evenSet_2 = insert_list [10;12;13] evenSet_empty in

  let lt5Set_empty = newSet ((>) 5) in

  let lt5Set_1 = insert_list [1;2;3;4;5;6] lt5Set_empty in
  let lt5Set_2 = mapAndFilter ((+) 2) lt5Set_1 in
  let filteredSetTest1 () = member 5 evenSet_1 = false in
  let filteredSetTest2 () = member 2 evenSet_1 = true in
  let filteredSetTest3 () = member 2 evenSet_2 = false in
  let filteredSetTest4 () = member 12 evenSet_2 = true in
  let filteredSetTest5 () = member 4 lt5Set_1 = true in
  let filteredSetTest6 () = member 5 lt5Set_1 = false in
  let filteredSetTest7 () = member 6 lt5Set_2 = false in
  let filteredSetTest8 () = member 4 lt5Set_2 = true in
  let filteredSetTest9 () = member 1 lt5Set_2 = false in
  Lib.run_test "filteredSet test1" filteredSetTest1 ;
  Lib.run_test "filteredSet test2" filteredSetTest2 ;
  Lib.run_test "filteredSet test3" filteredSetTest3 ;
  Lib.run_test "filteredSet test4" filteredSetTest4 ;
  Lib.run_test "filteredSet test5" filteredSetTest5 ;
  Lib.run_test "filteredSet test6" filteredSetTest6 ;
  Lib.run_test "filteredSet test7" filteredSetTest7 ;
  Lib.run_test "filteredSet test8" filteredSetTest8 ;
  Lib.run_test "filteredSet test9" filteredSetTest9

let filteredSetTests () =
  try filteredSetTests_wrapper () with
  | Failure s -> print_endline ("filteredSet tests error: `" ^ s ^ "`\n")
  | e -> print_endline ("filteredSet tests error: `" ^ Printexc.to_string e ^ "`\n")


(******************************************************************)

(******************************************************************)

let test part =
  match part with
  | One   -> isDigitTests()
  | Two   -> newFileNameTests()
  | Three -> homePathTests()
  | Four  -> atoiTests()
  | Five  -> tokenizeTests()
  | Six   -> formatTreeTests()
  | Seven   -> subpalindromeTests ()
  | Eight   -> listComparisonsTests ()
  | Nine -> patienceTests ()
  | Ten  -> filteredSetTests ()


let run () =
  let () = test One in
  let () = test Two in
  let () = test Three in
  let () = test Four in
  let () = test Five in
  let () = test Six in
  let () = test Seven in
  let () = test Eight in
  let () = test Nine in
  let () = test Ten in
  ()

let () =
  if (Array.length Sys.argv = 2 && Sys.argv.(1) = "test") then
    run ()
  else
    ()
