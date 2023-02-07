open Mips_ast
open Mips_assem
open Byte

exception TODO
exception FatalError



(* Take a look at the definition of the Mips AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the Mips machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
 *)
let execute (init_state : state) (instruction : inst) : state =
    match instruction with
      | Add (rd, rs, rt) ->
          (*rd = rs + rt*)
          Printf.printf "process Add inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rd)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int (reg2ind32 rt));
          let rt_val = rf_lookup (reg2ind rt)  init_state.r in
          let rs_val = rf_lookup (reg2ind rs)  init_state.r in
          let res_val = Int32.add rt_val rs_val in
          let regfile = rf_update (reg2ind rd) res_val init_state.r in
          let updated_state = { r = regfile; pc = init_state.pc; m = init_state.m } in
          updated_state
      | Beq (rs, rt, offset) ->
          Printf.printf "process Beq inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rs)) (Int32.to_int (reg2ind32 rt)) (Int32.to_int offset);
                    init_state
      | Jr rs ->
          Printf.printf "process Jr inst with parameters (%d) \n" (Int32.to_int (reg2ind32 rs));
                    init_state
      | Jal addr ->
          Printf.printf "process Jal inst with parameters (%d) \n" (Int32.to_int addr);
                    init_state
      | Li (rd, imm) -> raise (PError "Li encountered")
      | Lui (rt, imm) ->
           (* shift left 16 bits of imm, and load it to rt*)
          Printf.printf "process Lui inst with parameters (%d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int imm);
          let upper_imm = Int32.shift_left imm 16 in
          let regfile = rf_update (reg2ind rt) upper_imm init_state.r in
          let updated_state = { r = regfile; pc = init_state.pc; m = init_state.m } in
          updated_state
      | Ori (rt, rs, imm) ->
          (* rt = rs | imm *)
          Printf.printf "process Ori inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int imm);
          let rs_val = rf_lookup (reg2ind rs) init_state.r in
          let res_val = Int32.logor rs_val imm in
          let regfile = rf_update (reg2ind rt) res_val init_state.r in
          let updated_state = { r = regfile; pc = init_state.pc; m = init_state.m } in
          updated_state
      | Lw (rt, rs, imm) ->
          Printf.printf "process Lw inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int imm);
                    init_state
      | Sw (rt, rs, imm) ->
          Printf.printf "process Sw inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int imm);
                    init_state
let rec interp (init_state : state) : state =
    let res_state = ref init_state in
    let w = read_word2 init_state.m init_state.pc in
    let wordRef = ref w in
    let pc = ref init_state.pc in
    Printf.printf "stat memory is : %s\n" (string_of_mem init_state.m);
    Printf.printf "stat register is : %s\n" (string_of_rf init_state.r);
    Printf.printf "stat pc is : %d\n" (Int32.to_int init_state.pc);
    while !wordRef <> Int32.zero do
        let word_str = Int32.to_string !wordRef in
        Printf.printf "the word read from memory is : %x\n" (Int32.to_int !wordRef);
        let inst = word2ins !wordRef in
        let inst_str = inst2str inst in
        Printf.printf "the instruction is : %s\n" inst_str;
        let next_state = execute !res_state inst in
        res_state := next_state;
        pc :=  Int32.add !pc 0x4l;
        Printf.printf "the pc is : %s\n" (Int32.to_string !pc);
        let w = read_word2 init_state.m !pc in
            (*parse word to instruction*)
        wordRef := w;
    done;
    let updated_state = { r = !res_state.r; pc = !pc; m = !res_state.m } in
    res_state := updated_state;
    Printf.printf "res_stat register is : %s\n" (string_of_rf !res_state.r);
    Printf.printf "res_stat pc is : %d\n"  (Int32.to_int !res_state.pc);
    !res_state

(*
(1) a function to load the next 4 bytes from memory pointed to by the PC and assemble them into a word
  (2) a function to decode a word into an instruciton, as represented in Mips_ast
  (3) separate functions to interpret each possible instruction
      e.g. in our reference solution we have step_add, step_beq, etc.
*)

(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Big Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Stack grows downward starting at 0x7ffffffc
  * > GP points to 30000000
  * > The assembler uses register 1 as temp storage for encoding Li
  * > We don't implement delay slots in either assembly or bitcode semantics
  * > As stated in lecture, we shift jump and break immediates left by 2
  * > The PC is always incremented before executing an instruction
  * > Beq subtracts 4 from the PC before adding its offset
  * > We preserve the top 4 bits of the PC when executing a jal
*)
