(* Takes Translate.statement's and turns them into a list of Int32.t opcodes.
   Fuck yeah.
*)

open Translate

(* Useful constants for composing and decomposing instructions
   of different formats.
   Lifted wholesale from Havoc2/include/cpu.h
*)
let o3destmask = 0x001f0000l
and o3destshift = 16
and o3src1mask = 0x0000f800l
and o3src1shift = 11
and o3src2mask = 0x000007ffl
and o3src2length = 11

and o2destmask = 0x001f0000l
and o2destshift = 16
and o2mask = 0x0000ffffl
and o2length = 16

and o1mask = 0x001fffffl
and o1length = 21

and conditionmask = 0xf0000000l
and opcodemask = 0x0fe00000l
and conditionshift = 28
and opcodeshift = 21
;;

let lor32 = Int32.logor;;
let lsl32 = Int32.shift_left;;
let lsr32 = Int32.shift_right;;
let and32 = Int32.logand;;

let lorMany = List.fold_left lor32 0l;;

(* XXX: We MUST mask these!!!  Some fo them are negative numbers! *)
let makeO3 cond op o1 o2 o3 =
  let cond = Int32.of_int cond
  and op = Int32.of_int op
  and o1 = Int32.of_int o1
  and o2 = Int32.of_int o2
  and o3 = Int32.of_int o3
  in
  let condpart = and32 conditionmask (lsl32 cond conditionshift)
  and oppart = and32 opcodemask (lsl32 op opcodeshift)
  and arg1 = and32 o3destmask (lsl32 o1 o3destshift)
  and arg2 = and32 o3src1mask (lsl32 o2 o3src1shift)
  and arg3 = and32 o3src2mask o3 
  in
(*  Printf.printf "O3: %lX %lX %lX %lX %lX\n" condpart oppart arg1 arg2 arg3; *)
  lorMany [condpart; oppart; arg1; arg2; arg3]
;;

let makeO2 cond op o1 o2 =
(*  Printf.printf "O2 source: %X %X %X %X\n" cond op o1 o2; *)
  let cond = Int32.of_int cond
  and op = Int32.of_int op
  and o1 = Int32.of_int o1
  and o2 = Int32.of_int o2
  in
  let condpart = (lsl32 cond conditionshift)
  and oppart = (lsl32 op opcodeshift)
  and arg1 = (lsl32 o1 o2destshift)
  and arg2 = and32 o2mask o2
  in
(*  Printf.printf "O2 result: %lX %lX %lX %lX\n" condpart oppart arg1 arg2; *)
  lorMany [condpart; oppart; arg1; arg2]
;;

let makeO1 cond op o1 =
  let cond = Int32.of_int cond
  and op = Int32.of_int op
  and o1 = Int32.of_int o1
  in
  let condpart = and32 conditionmask (lsl32 cond conditionshift)
  and oppart = and32 opcodemask (lsl32 op opcodeshift)
  and arg1 = and32 o1mask o1
  in
(*  Printf.printf "O1: %lX %lX %lX\n" condpart oppart arg1; *)
  lorMany [condpart; oppart; arg1]
;;

let encodeInstruction = function
  | Reg3(cond, op, r1, r2, r3) ->
    makeO3 cond op r1 r2 r3
  | Reg2Val(cond, op, r1, r2, Literal(vl)) ->
    makeO3 cond op r1 r2 vl
  | Reg2(cond, op, r1, r2) ->
    makeO2 cond op r1 r2
  | Val1(cond, op, Literal(vl)) ->
    makeO1 cond op vl
  | Reg1Val(cond, op, r1, Literal(vl)) ->
    makeO2 cond op r1 vl
  | NoArg(cond, op) ->
    makeO1 cond op 0
  | _ -> Error.errorAndDie "encodeInstruction: Impossible instruction!  Did a label sneak in somehow?"
;;


(* I would name this function fuckMeWithARake but that wouldn't make it shorter...
   So, Fuck My Life seems appropriate.
*)
let fml = Int32.shift_right;;

let cockmongleInt32 f =
  let mask = 0xFFl in
  let byte1 = (Int32.to_int (Int32.logand f mask))
  and byte2 = (Int32.to_int (Int32.logand (fml f 8) mask))
  and byte3 = (Int32.to_int (Int32.logand (fml f 16) mask))
  and byte4 = (Int32.to_int (Int32.logand (fml f 24) mask))
  in
  (* LITTLE ENDIAN GODS DAMMIT *)
  [byte1; byte2; byte3; byte4]
;;

let encodeStatements stms =
  let rec loop stms accm =
    match stms with
	[] -> List.rev accm
      | hd::tl -> 
	match hd with
	  | Instr(x) -> loop tl ((encodeInstruction x) :: accm)
	  | x -> loop tl accm
  in
  let ints = loop stms [] in
  List.flatten (List.map cockmongleInt32 ints)
;;

