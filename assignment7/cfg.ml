open Cfg_ast
exception Implement_Me
exception FatalError


type igraph_node = RegNode of Mips.reg | VarNode of var

let string_of_node (n: igraph_node) : string =
  match n with
  | RegNode r -> Mips.reg2string r
  | VarNode v -> v
;;

module IGraphNode =
  struct
    type t = igraph_node
    let compare = compare
  end

module NodeSet = Set.Make(IGraphNode)                                                   
(* These are the registers that must be generated / killed as part of
   liveness analysis for call instructions to reflect MIPS calling
   conventions *)

let call_gen_list = ["$4";"$5";"$6";"$7"]
let call_kill_list = ["$1";"$2";"$3";"$4";"$5";"$6";"$7";"$8";"$9";"$10";
                      "$11";"$12";"$13";"$14";"$15";"$24";"$25";"$31"]

(* Undirected graphs where nodes are identified by igraph_node type above. Look at
   graph.ml for the interface description.  *)

module IUGraph = Graph.UndirectedGraph(IGraphNode)

(* this is a wrapper to addEdge that prevents adding self edges.
   to do all sorts of other complicated stuff for eg coloring *)
let specialAddEdge u v g =
  if (u = v) then
    g
  else
    IUGraph.addEdge u v g

(* An interference graph is an SUGraph where a node is temp variable
   or a register (to be able to handle pre-colored nodes)

   The adjacency set of variable x should be the set of variables
   y such that x and y are live at the same point in time. *)
type interfere_graph = IUGraph.graph

(* To help you printing an igraph for debugging *)
let string_of_igraph (g: interfere_graph) : string =
  let rec string_of_row (n: IUGraph.node) =
    let ns = IUGraph.adj n g in
    Printf.sprintf "%s : {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IUGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}" rows
  

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)
let clear_file =
  let oc = open_out "log.txt" in
  close_out oc

let write_to_file (str: string) : unit =
  let oc = open_out_gen [Open_wronly; Open_creat; Open_append] 0o666 "log.txt" in
  output_string oc str;
  close_out oc

let reg_node v =
  match v with
  | 0 -> RegNode(Mips.R0)
  | 1 -> RegNode(Mips.R1)
  | 2 -> RegNode(Mips.R2)
  | 3 -> RegNode(Mips.R3)
  | 4 -> RegNode(Mips.R4)
  | 5 -> RegNode(Mips.R5)
  | 6 -> RegNode(Mips.R6)
  | 7 -> RegNode(Mips.R7)
  | 8 -> RegNode(Mips.R8)
  | 9 -> RegNode(Mips.R9)
  | 10 -> RegNode(Mips.R10)
  | 11 -> RegNode(Mips.R11)
  | 12 -> RegNode(Mips.R12)
  | 13 -> RegNode(Mips.R13)
  | 14 -> RegNode(Mips.R14)
  | 15 -> RegNode(Mips.R15)
  | 16 -> RegNode(Mips.R16)
  | 17 -> RegNode(Mips.R17)
  | 18 -> RegNode(Mips.R18)
  | 19 -> RegNode(Mips.R19)
  | 20 -> RegNode(Mips.R20)
  | 21 -> RegNode(Mips.R21)
  | 22 -> RegNode(Mips.R22)
  | 23 -> RegNode(Mips.R23)
  | 24 -> RegNode(Mips.R24)
  | 25 -> RegNode(Mips.R25)
  | 26 -> RegNode(Mips.R26)
  | 27 -> RegNode(Mips.R27)
  | 28 -> RegNode(Mips.R28)
  | 29 -> RegNode(Mips.R29)
  | 30 -> RegNode(Mips.R30)
  | 31 -> RegNode(Mips.R31)
  | _ -> raise Implement_Me

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph = 
    clear_file;
    let print_block block =
      let str = block2string block in
      write_to_file (str^"\n") in

    List.iter print_block f;
    let result_graph = IUGraph.empty in
    let node = reg_node 2 in
    let result_graph = IUGraph.addNode node result_graph in
    result_graph
