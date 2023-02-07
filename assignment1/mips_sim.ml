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
let rec interp (init_state : state) : state =
    let w = read_word init_state.m init_state.pc in
    let wordRef = ref w in
    let pc = ref init_state.pc in
    Printf.printf "stat memory is : %s\n" (string_of_mem init_state.m);
    Printf.printf "stat pc is : %d\n" (Int32.to_int init_state.pc);
    while !wordRef <> Int32.zero do
        let word_str = Int32.to_string !wordRef in
        Printf.printf "the word read from memory is : %d\n" (Int32.to_int !wordRef);
       (* let inst = word2ins w in*)
       (* let inst_str = inst2str inst in
        Printf.printf "the instruction is : %s\n" inst_str;*)
        pc :=  Int32.add !pc 0x4l;
        Printf.printf "the pc is : %s\n" (Int32.to_string !pc);
        let w = read_word init_state.m !pc in
            (*parse word to instruction*)
        wordRef := w;
    done;

    (*
    let quit_loop = ref false in
      while not !quit_loop do
        print_string "Have you had enough yet? (y/n) ";
        let str = read_line () in
          if str.[0] = 'y' then quit_loop := true
      done;;
    *)
   (* Printf.printf "I am the interp function, the input register is %s, memory is %s, pr is %s, word is %s\n" re_str mem_str pc_str word_str;*)

     init_state

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
