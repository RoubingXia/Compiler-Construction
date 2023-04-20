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
(*graph to represent the connections between blocks*)
module IDGraph = Graph.DirectedGraph(IGraphNode)
type block_graph = IDGraph.graph

module IGraphNode = struct
  type t = igraph_node
  let compare = compare
end

module NodeSet = Set.Make(IGraphNode)

let node_sets_equal set1 set2 =
  let sorted_list1 = List.sort compare (NodeSet.elements set1) in
  let sorted_list2 = List.sort compare (NodeSet.elements set2) in
  List.compare_lengths sorted_list1 sorted_list2 = 0 &&
  List.for_all2 (fun n1 n2 -> compare n1 n2 = 0) sorted_list1 sorted_list2

let string_of_igraph2 (g: block_graph) : string =
  let rec string_of_row (n: IDGraph.node) =
    let ns = IDGraph.succ n g in
    Printf.sprintf "%s : {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IDGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}" rows

let find_label (block: block) : label =
  let rec helper block =
    match block with
    | Label l :: _ -> l
    | [] -> raise Implement_Me
    | _ :: rest -> helper rest
  in
  helper block

let rec find_jump_labels block =
  match block with
  | [] -> []
  | Jump(l) :: rest -> l :: find_jump_labels rest
  | If(_, _, _, l1, l2) :: rest -> l1 :: l2 :: find_jump_labels rest
  | _ :: rest -> find_jump_labels rest


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
    (*Print blocks to log file*)
    clear_file;
    let print_block block =
      let str = block2string block in
      write_to_file (str^"\n") in
    List.iter print_block f;

    (*Build direct graph of blocks*)
    let build_block_graph (f : func) : block_graph =
        (*Get a list of label*)
        let result_block_graph = IDGraph.empty in
        let label_list = List.map find_label f in
        (*adj_label_list is a list of list, i th element is a
          list of labels which can be jump to from the i th label in label_list
        *)
        let adj_label_list = List.map find_jump_labels f in
        (*List.iter (fun lst -> List.iter write_to_file lst) adj_label_list;*)
        (*List.iter write_to_file label_list;*)
        let ref_g = ref result_block_graph in
        (*add all nodes to graph*)
        List.iter (fun label ->
            let new_node = VarNode(label) in
            ref_g := IDGraph.addNode new_node !ref_g
            ) label_list;
        (*build connections *)
        List.iter2 (fun label adj_label ->
            let src_node = VarNode(label) in
            List.iter (fun dst_label ->
                let dst_node = VarNode(dst_label) in
                            ref_g := IDGraph.addEdge src_node dst_node !ref_g
                ) adj_label;
         ) label_list adj_label_list;
        !ref_g
    in
    let result_graph = IUGraph.empty in
    let result_block_graph = build_block_graph f in
    let log_msg = string_of_igraph2 result_block_graph in
    write_to_file ("\n block graph starts: \n"^log_msg^"\n");
    (*Choose the block which has lowest out degree as starting point*)
    let get_start_block (g : block_graph) : igraph_node =
        let nodes = (NodeSet.elements (IDGraph.nodes g)) in
        let node_with_least_succ (g : block_graph)  nodes =
          let succ_counts = List.map (fun n -> List.length (NodeSet.elements (IDGraph.succ n g))) nodes in
          let min_succ_count = List.fold_left min (List.hd succ_counts) succ_counts in
          List.find (fun n -> List.length (NodeSet.elements (IDGraph.succ n g)) = min_succ_count) nodes
        in
        node_with_least_succ g nodes;
    in
    let start_node = get_start_block result_block_graph in
     write_to_file("\n I am so high: I am the starting node block: "^ (string_of_node start_node));
(* So that's why

initialize LiveIn[L] := Gen[L].
initialize LiveOut[L] := { }.
loop until no change {
for each L:
Out := LiveIn[L1]  ...  LiveIn[Ln]
where succ[L] = {L1,...,Ln}
if Out == LiveOut[L] then continue to next block.
LiveOut[L] := Out.
LiveIn[L] := Gen[L]  (LiveOut[L] - Kill[L]).
}

*)
(*
?????
get the stabled liveOut first
use this stabled liveOut to traverse the block graph to build the interference graph
*)
    let initLiveOut = ref NodeSet.empty in
    let liveOut = ref NodeSet.empty in
    liveOut := (*some code to get the new OutSet after one DFS of whole function*)
    while !(node_sets_equal !initLiveOut !liveOut) do
        initLiveOut := !liveOut in
        liveOut = (*some code to get the new OutSet after one DFS of whole function*) in
    done

    
    let node1 = reg_node 2 in
    let result_graph = IUGraph.empty in
    (* Add all nodes to the graph *)
    let result_graph = List.fold_left (fun g node -> IUGraph.addNode node g) result_graph [node1]
    in
    (* Connect all nodes with edges *)
    (*
     x10	: {x0,x1,x11,x2,x3,x4,x5,x6,x7,x8,x9}
      x11	: {x0,x1,x10,x2,x3,x4,x5,x6,x7,x8,x9}
      x12	: {x0,x1,x2,x3,x4,x5,x6,x7,x8,x9}

    *)

    let ref_graph = ref result_graph in
    (*
    let num_nodes = 9 in
    for i = 0 to num_nodes do
      for j = 0 to 12 do
        let src_node = VarNode("x"^(string_of_int i) ) in
        let dst_node = VarNode("x"^(string_of_int j)) in
        ref_graph := specialAddEdge src_node dst_node !ref_graph;
        ()
      done
    done;
    let limit = 11 in
    for i = 10 to limit do
        for j = 0 to 11 do
                let src_node = VarNode("x"^(string_of_int i) ) in
                let dst_node = VarNode("x"^(string_of_int j)) in
                ref_graph := specialAddEdge src_node dst_node !ref_graph;
                ()
              done
    done;
    *)
    !ref_graph
