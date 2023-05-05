open Mips
open Cfg_ast
open Cfg

exception AllocError of string
exception Implement_Me

module NodeMap = Map.Make(Cfg.IGraphNode)                                                   

module RegSet = Set.Make(struct
                  type t = reg
                  let compare = fun x y -> (String.compare (reg2string x) (reg2string y))
                end)
let reglist2sset ls =
  List.fold_left (fun ls var -> RegSet.add var ls) RegSet.empty ls

let validreglist = 
  R2 :: R3  :: R4 :: R5 ::   R6 ::  R7  ::  R8 :: R9 ::  R10 :: 
  R11 :: R12 :: R13 :: R14 :: R15 :: R16 :: R17 :: R18 :: R19 :: R20 ::
  R21 :: R22 :: R23 (*R24,R25 reserved*) :: R30 :: R31 :: []
let regcount = List.length validreglist
let validregset = reglist2sset validreglist 

let string_of_colormap colormap =
  let buffer = Buffer.create 100 in
  NodeMap.iter (fun node regset ->
    match regset with
    | None -> Printf.bprintf buffer "%s -> unallocated\n" (Cfg.string_of_node node)
    | Some reg -> Printf.bprintf buffer "%s -> %s\n" (Cfg.string_of_node node) (reg2string reg)
  ) colormap;
  Buffer.contents buffer


let string_of_stack stack =
  let buffer = Buffer.create 100 in
  Stack.iter (fun node ->
    Printf.bprintf buffer "Node: %s\n" (string_of_node node);
  ) stack;
  Buffer.contents buffer


let rewriteOp colormap o =
  match o with
    | Var x -> 
        (match NodeMap.find (Cfg.VarNode x) colormap with
          | None -> raise (AllocError "No color assigned for that variable.") 
          | Some r -> Cfg_ast.Reg r)
    | _ -> o

let rewriteInst colormap (i: Cfg_ast.inst) : Cfg_ast.inst = 
  let rewrite = rewriteOp colormap in
    match i with
      | Label x -> Label x
      | Move (x, y) -> Move (rewrite x, rewrite y)
      | Arith (x, y, p, z) -> Arith (rewrite x, rewrite y, p, rewrite z)
      | Load (x, y, i) -> Load (rewrite x, rewrite y, i)
      | Store (x, i, y) -> Store (rewrite x, i, rewrite y)
      | Call x -> Call (rewrite x)
      | Jump x -> Jump x
      | If (x,b,y,t,f) -> If (rewrite x, b, rewrite y, t, f)
      | Return -> Return


type colormap = ((RegSet.elt option) NodeMap.t)

type assign_result = 
    | Success of colormap
    | Fail of var list

(*******************************************************************)
(* PS8 TODO:  graph coloring *)

(* given an inteference graph, return an assign_result, which consists of either
   (1) Success cm, where cm is a coloring, i.e. a map from variables to registers
   (2) Fail vs, where vs is a list of variables to spill, when a graph coloring could not be found

 *)

let is_register_node node =
    match node with
    | RegNode(reg) -> true
    | VarNode(var_name) -> false

let assign_colors (ig: Cfg.interfere_graph) f : assign_result =
    let ig_ref = ref ig in
    let stack =  Stack.create() in
    let bfs (ig: interfere_graph) (node : igraph_node) : igraph_node =
        (* Breadth first search to find a node with degree less than regcount*)
        let visited = ref NodeSet.empty in
        let queue = Queue.create() in
        let res_node = ref (VarNode("init")) in
        Queue.add node queue;
        visited := NodeSet.add node !visited;
        while not (Queue.is_empty queue) do
            let curNode = Queue.take queue in
            let degree = IUGraph.degree curNode ig in
            if degree < regcount then
                (res_node := curNode);
            List.map (fun neiNode ->
                if not (NodeSet.mem neiNode !visited) then
                    Queue.add neiNode queue;
                    visited := NodeSet.add neiNode !visited;
                    ) (NodeSet.elements (IUGraph.adj curNode ig));
            ()
        done;
        !res_node;
    in
    let is_empty (ig: interfere_graph) =
        if (List.length (NodeSet.elements (IUGraph.nodes ig))) = 0 then
            true
        else
            false
    in
    let need_spill (ig: interfere_graph) =
        let start_node = List.hd (NodeSet.elements (IUGraph.nodes ig)) in
        let next_valid_node = bfs ig start_node in
        match next_valid_node with
            | VarNode("init") ->
                true
            | _ ->
                false
    in
    let get_var_list (ig: Cfg.interfere_graph) : var list =
        (*Find the node with largest degree, and return a var list*)
        let node_ref = ref (List.hd (NodeSet.elements (IUGraph.nodes ig))) in
        List.iter (fun node ->
                let cur_degree = IUGraph.degree node ig in
                let max_degree = IUGraph.degree !node_ref ig in
                if (max_degree < cur_degree) then
                    node_ref := node;
            ) (NodeSet.elements (IUGraph.nodes ig));
        let node_list = !node_ref::(NodeSet.elements (IUGraph.adj !node_ref ig)) in
        let res_list =
            List.map (fun node ->
                    match node with
                    | VarNode(var_name) -> var_name;
                    | _ -> (* write_to_file("\n Fatal error 0001\n"); *) raise Implement_Me
                ) node_list in
        res_list
    in
    let build_colormap (ig: Cfg.interfere_graph) : colormap =
      (*  write_to_file("inside build colormap stack is : "^string_of_stack(stack)^"\n");*)
        let res_map = ref (NodeMap.empty : colormap) in
        while not (Stack.is_empty stack) do
            let cur_node = Stack.pop stack in
            let available_reg = ref (reglist2sset validreglist) in
            (*iterate all neighbor nodes of cur_node, and try to find in res_map, see if this neighbor node already assigned a color
                if yes, mark that color as used, when finished all neighbor nodes, iterate validreglist to get a color that
                has not been used
            *)
            List.iter (fun nei_node ->
                    if NodeMap.mem nei_node !res_map  then
                        (
                            let rm_reg = (
                                match NodeMap.find nei_node !res_map with
                                | Some r -> r
                            ) in
                            available_reg := RegSet.remove rm_reg !available_reg;
                        );
                    if is_register_node nei_node then (
                        let rm_reg = (
                            match nei_node with
                            | RegNode r -> r
                        ) in
                        available_reg := RegSet.remove rm_reg !available_reg;
                    )
                ) (NodeSet.elements (IUGraph.adj cur_node ig));
            res_map := NodeMap.add cur_node (Some (List.hd (RegSet.elements !available_reg))) !res_map;

        ()
        done;
        let msg_str = string_of_colormap !res_map in
       (* write_to_file("In build colormap the map is :"^msg_str^"\n");*)
        !res_map
    in
    let result = ref (Success((NodeMap.empty : colormap))) in

    while (not (is_empty !ig_ref)) && (not (need_spill !ig_ref)) do
        let start_node = List.hd (NodeSet.elements (IUGraph.nodes !ig_ref)) in
        let next_valid_node = bfs !ig_ref start_node in
        (*Push current node to stack*)
        if not (is_register_node next_valid_node) then
            Stack.push next_valid_node stack;
        (*Remove current node from graph*)
        ig_ref := IUGraph.rmNode next_valid_node !ig_ref;
    done;
    (* write_to_file("Reg alloc still alive 0002\n");*)
    if (not (is_empty !ig_ref)) then
        (*Need to return a fail var list*)
        let var_list = get_var_list !ig_ref in
        result := Fail(var_list);
        (* write_to_file("Reg alloc still alive 0003\n");*)
        !result
    else
        (*Need to build a color map*)
        ((*write_to_file("before build colormap stack is : "^string_of_stack(stack)^"\n");*)
        let res_map = build_colormap ig in
        result := Success(res_map);
         (* write_to_file("Reg alloc still alive 0004\n"); *)
         let msg_str = string_of_colormap res_map in
        write_to_file("The res_map is "^msg_str^"\n");
        !result)


let rec reg_alloc_spill (fraw : func) (sl: var list): func = 
  (*First spill all of the vars in sl by adding loads/stores to fraw*)
  let f = Spill.spill fraw sl in
  let ig = Cfg.build_interfere_graph f in
  let colormapopt = assign_colors ig f in
    match colormapopt with
      | Success colormap ->
          let allocatedf = List.map (fun x -> List.map (rewriteInst colormap) x) f in
            (*Get rid of trivial moves*)
            List.map 
              (fun b -> List.filter
                          (fun i ->
                             match i with
                               | Move (o1,o2) -> (if(o1=o2) then false else true)
                               | _ -> true
                          ) b
              )
              allocatedf
      | Fail spilllist ->
            reg_alloc_spill fraw (List.append spilllist sl)

let reg_alloc (f:func) : func =
  reg_alloc_spill f []
