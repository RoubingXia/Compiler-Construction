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
          let updated_state = { r = regfile; pc = (Int32.add init_state.pc 0x4l); m = init_state.m } in
          updated_state
      | Beq (rs, rt, offset) ->
           (*if rs == rt then pc = offset << 2*)
          Printf.printf "process Beq inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rs)) (Int32.to_int (reg2ind32 rt)) (Int32.to_int offset);
          let rs_val = rf_lookup (reg2ind rs) init_state.r in
          let rt_val = rf_lookup (reg2ind rt) init_state.r in
          if Int32.equal rs_val rt_val then
            let updated_state = { r = init_state.r; pc = Int32.add (Int32.shift_left offset 2) init_state.pc; m = init_state.m } in
            updated_state
          else
            let updated_state = { r = init_state.r; pc = (Int32.add init_state.pc 0x4l); m = init_state.m } in
            updated_state
      | Jr rs ->
          (*pc = rs*)
          Printf.printf "process Jr inst with parameters (%d) \n" (Int32.to_int (reg2ind32 rs));
          let updated_state = { r = init_state.r; pc = (rf_lookup (reg2ind rs) init_state.r); m = init_state.m } in
          updated_state
      | Jal addr ->
           (*$31 = current pc + 4, pc = addr   (PC & 0xf0000000) | (target << 2)*)
          Printf.printf "process Jal inst with parameters (%d) \n" (Int32.to_int addr);
          let target_addr = Int32.logor (Int32.logand init_state.pc 0xf0000000l) (Int32.shift_left addr 2) in
          let regfile = rf_update 31 (Int32.add 0x4l init_state.pc) init_state.r in
          let updated_state = { r = regfile; pc = target_addr; m = init_state.m } in
          updated_state
      | Li (rd, imm) -> raise (PError "Li encountered")
      | Lui (rt, imm) ->
           (* shift left 16 bits of imm, and load it to rt*)
          Printf.printf "process Lui inst with parameters (%d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int imm);
          let upper_imm = Int32.shift_left imm 16 in
          let regfile = rf_update (reg2ind rt) upper_imm init_state.r in
          let updated_state = { r = regfile; pc = (Int32.add init_state.pc 0x4l); m = init_state.m } in
          updated_state
      | Ori (rt, rs, imm) ->
          (* rt = rs | imm *)
          Printf.printf "process Ori inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int imm);
          let rs_val = rf_lookup (reg2ind rs) init_state.r in
          let res_val = Int32.logor rs_val imm in
          let regfile = rf_update (reg2ind rt) res_val init_state.r in
          let updated_state = { r = regfile; pc = (Int32.add init_state.pc 0x4l); m = init_state.m } in
          updated_state
      | Lw (rt, rs, imm) ->
            (*load mem[rs + imm] to rt*)
          Printf.printf "process Lw inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int imm);
          let target_addr = Int32.add (rf_lookup (reg2ind rs) init_state.r) imm  in
          let reg_val = read_word2 init_state.m target_addr in
          let regfile = rf_update (reg2ind rt)  reg_val init_state.r in
          let updated_state = { r = regfile; pc = (Int32.add init_state.pc 0x4l); m = init_state.m } in
          updated_state
      | Sw (rt, rs, imm) ->
        (*store rt to mem[rs + imm]t*)
          Printf.printf "process Sw inst with parameters (%d, %d, %d) \n" (Int32.to_int (reg2ind32 rt)) (Int32.to_int (reg2ind32 rs)) (Int32.to_int imm);
          let target_addr = Int32.add (rf_lookup (reg2ind rs) init_state.r) imm  in
          let updated_mem =
            (*mem_update target_addr (getByte (rf_lookup (reg2ind rt)) 0)*)
            mem_update target_addr (getByte (rf_lookup (reg2ind rt) init_state.r)  3) (
                  mem_update (Int32.add target_addr 1l) (getByte (rf_lookup (reg2ind rt) init_state.r) 2) (
                  mem_update (Int32.add target_addr 2l) (getByte (rf_lookup (reg2ind rt) init_state.r) 1) (
                  mem_update (Int32.add target_addr 3l) (getByte (rf_lookup (reg2ind rt) init_state.r) 0) (
                    init_state.m))))
           in
          let updated_state = { r = init_state.r; pc = (Int32.add init_state.pc 0x4l); m = updated_mem } in
          updated_state
let rec interp (init_state : state) : state =
    let res_state = ref init_state in
    let w = read_word2 init_state.m init_state.pc in
    let wordRef = ref w in
    let pc = ref init_state.pc in
    (*
    Printf.printf "stat memory is : %s\n" (string_of_mem init_state.m);
    Printf.printf "stat register is : %s\n" (string_of_rf init_state.r);
    Printf.printf "stat pc is : %d\n" (Int32.to_int init_state.pc);
    *)
    while !wordRef <> Int32.zero do
        let word_str = Int32.to_string !wordRef in
        (*Printf.printf "current memory is : %s\n" (string_of_mem init_state.m);
        Printf.printf "current pc is : %d\n" (Int32.to_int !pc);
        Printf.printf "the word read from memory is : %x\n" (Int32.to_int !wordRef);
        *)
        let inst = word2ins !wordRef in
        let inst_str = inst2str inst in
       (* Printf.printf "the instruction is : %s\n" inst_str; *)
        let next_state = execute !res_state inst in
        res_state := next_state;
        pc :=  Int32.add !pc 0x4l;
       (* Printf.printf "the pc is : %s\n" (Int32.to_string !pc); *)
        let w = read_word2 init_state.m !pc in
            (*parse word to instruction*)
        wordRef := w;
    done;
    (*
    Printf.printf "res_stat register is : %s\n" (string_of_rf !res_state.r);
    Printf.printf "res_stat pc is : %d\n"  (Int32.to_int !res_state.pc);
    *)
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
