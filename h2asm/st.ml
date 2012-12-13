(* Syntax tree and verification code. *)

open Error

type condition =
    ALWAYS
  | GT
  | GE
  | LT
  | LE
  | EQ
  | NE
  | AB
  | BL
  | AE
  | BE
  | CS
  | CC
  | N
  | FL
;;

let conditionTable = [
  (ALWAYS, "al");
  (GT, "gt");
  (GE, "ge");
  (LT, "lt");
  (LE, "le");
  (EQ, "eq");
  (NE, "ne");
  (AB, "ab");
  (BL, "bl");
  (AE, "ae");
  (BE, "be");
  (CS, "cs");
  (CC, "cc");
  (N, "n");
  (FL, "fl")
];;

let table2Op (op, _) = op;;
let table2String (_, str) = str;;

let table3Op (op, _, _) = op;;
let table3String (_, str, _) = str;;
let table3Types (_, _, t) = t;;

let condition2string cond =
  try
    let o = List.find (fun x -> (table2Op x) = cond) conditionTable in
    table2String o
  with
      Not_found ->
	errorAndDie ("Unknown condition code: This should be impossible!")
;;

let string2condition str =
  try
    let o = List.find (fun x -> (table2String x) = str) conditionTable in
    table2Op o
  with
      Not_found ->
	errorAndDie ("Could not turn string '" ^ str ^ "' into a condition code!")
;;

type reg =
    Reg of int
  | SReg of int
;;

let reg2string = function
    Reg(i) -> "r" ^ (string_of_int i)
  | SReg(i) ->  "s" ^ (string_of_int i)
;;

type vl =
  | Lit of int
  | Label of string
  | LocalLabel of string
;;

let vl2string = function
  | Lit(i) -> Printf.sprintf "0x%X" i
  | Label(s) -> s
  | LocalLabel(s) -> "!" ^ s

type optype =
    TReg3
  | TReg2Val
  | TReg2
  | TVal1
  | TReg1Val
  | TNoArg
;;

let optype2string = function
    TReg3 -> "Reg3"
  | TReg2Val -> "Reg2Val"
  | TReg2 -> "Reg2"
  | TVal1 -> "Val1"
  | TReg1Val -> "Reg1Val"
  | TNoArg -> "NoArg"
;;

type op =
    Add
  | Adc
  | Sub
  | Sbb
  | Mul
  | Umul
  | Div
  | Udiv

  | And
  | Or
  | Xor
  | Sl
  | Sr
  | Sru
  | Rl
  | Rr

  | Nand
  | Nor
  | Nxor
  | B
  | Bl
  | Br
  | Brl
  | Bf
  | Bfl
  | Get
  | Set
  | Int
  | Reti

  | Lo
  | Lor
  | Lof
  | Lt
  | Ltr
  | Ltf
  | Lw
  | Lwr
  | Lwf
  | Lb
  | Lbr
  | Lbf

  | So
  | Sor
  | Sof
  | St
  | Str
  | Stf
  | Sw
  | Swr
  | Swf
  | Sb
  | Sbr
  | Sbf

  | Liu
  | Lium
  | Lilm
  | Lil
  | Li 
  | Cmp
  | Extb
  | Extw
  | Extt
  | Cmpsz
  | Halt

(* Assembler implemented instructions *)
  | Not
  | M
  | Neg
;;

let opTable = [
  (Add, "add", [TReg3; TReg2Val]);
  (Adc, "adc", [TReg3; TReg2Val]);
  (Sub, "sub", [TReg3; TReg2Val]);
  (Sbb, "sbb", [TReg3; TReg2Val]);
  (Mul, "mul", [TReg3; TReg2Val]);
  (Umul, "umul", [TReg3; TReg2Val]);
  (Div, "div", [TReg3; TReg2Val]);
  (Udiv, "udiv", [TReg3; TReg2Val]);

  (And, "and", [TReg3; TReg2Val]);
  (Or, "or", [TReg3; TReg2Val]);
  (Xor , "xor", [TReg3; TReg2Val]);
  (Sl, "sl", [TReg3; TReg2Val]);
  (Sr, "sr", [TReg3; TReg2Val]);
  (Sru, "sru", [TReg3; TReg2Val]);
  (Rl, "rl", [TReg3; TReg2Val]);
  (Rr, "rr", [TReg3; TReg2Val]);

  (Nand, "nand", [TReg3; TReg2Val]);
  (Nor, "nor", [TReg3; TReg2Val]);
  (Nxor, "nxor", [TReg3; TReg2Val]);
  (B, "b", [TReg1Val]);
  (Bl, "bl", [TReg1Val]);
  (Br, "br", [TReg1Val]);
  (Brl, "brl", [TReg1Val]);
  (Bf, "bf", [TVal1]);
  (Bfl, "bfl", [TVal1]);
(* XXX: Get/Set miiiight need to be some sort of special case for system 
   registers...
   Oh well, for the moment we can just refer to them as numbers.
*)
  (Get, "get", [TReg1Val]);
  (Set, "set", [TReg1Val]);
  (Int, "int", [TVal1]);
  (Reti, "reti", [TReg1Val]);

  (Lo, "lo", [TReg2Val]);
  (Lor, "lor", [TReg2Val]);
  (Lof, "lof", [TReg1Val]);
  (Lt, "lt", [TReg2Val]);
  (Ltr, "ltr", [TReg2Val]);
  (Ltf, "ltf", [TReg1Val]);
  (Lw, "lw", [TReg2Val]);
  (Lwr, "lwr", [TReg2Val]);
  (Lwf, "lwf", [TReg1Val]);
  (Lb, "lb", [TReg2Val]);
  (Lbr, "lbr", [TReg2Val]);
  (Lbf, "lbf", [TReg1Val]);

  (So, "so", [TReg2Val]);
  (Sor, "sor", [TReg2Val]);
  (Sof, "sof", [TReg1Val]);
  (St, "st", [TReg2Val]);
  (Str, "str", [TReg2Val]);
  (Stf, "stf", [TReg1Val]);
  (Sw, "sw", [TReg2Val]);
  (Swr, "swr", [TReg2Val]);
  (Swf, "swf", [TReg1Val]);
  (Sb, "sb", [TReg2Val]);
  (Sbr, "sbr", [TReg2Val]);
  (Sbf, "sbf", [TReg1Val]);

  (Liu, "liu", [TReg1Val]);
  (Lium, "lium", [TReg1Val]);
  (Lilm, "lilm", [TReg1Val]);
  (Lil, "lil", [TReg1Val]);
  (Li , "li", [TReg1Val]);
  (Cmp, "cmp", [TReg2]);
  (Extb, "extb", [TReg2]);
  (Extw, "extw", [TReg2]);
  (Extt, "extt", [TReg2]);
  (Cmpsz, "cmpsz", [TReg3]);
  (Halt, "halt", [TNoArg]);

  (Not, "not", [TReg2]);
  (M, "m", [TReg2]);
  (Neg, "neg", [TReg2]);
];;

let op2string op =
  try
    let o = List.find (fun x -> (table3Op x) = op) opTable in
    table3String o
  with
      Not_found ->
	errorAndDie ("Unknown op type: This should be impossible!")
;;

let string2op str =
  try
    let o = List.find (fun x -> (table3String x) = str) opTable in
    table3Op o
  with 
      Not_found ->
	errorAndDie ("Could not turn string '" ^ str ^ "' into an opcode!")
;;

let getTypes op =
  try
    let o = List.find (fun x -> (table3Op x) = op) opTable in
    table3Types o
  with 
      Not_found ->
	errorAndDie ("Tried to get op type of unknown opcode: '" ^ (op2string op) ^ "'!")
;;


type instruction =
    Reg3 of condition * op * reg * reg * reg
  | Reg2Val of condition * op * reg * reg * vl
  | Reg2 of condition * op * reg * reg
  | Val1 of condition * op * vl
  | Reg1Val of condition * op * reg * vl
  | NoArg of condition * op
;;

let instruction2string = function
    Reg3(cond, op, reg1, reg2, reg3) -> 
      Printf.sprintf "  %s.%s %s, %s, %s"
      (op2string op) (condition2string cond) (reg2string reg1)
      (reg2string reg2) (reg2string reg3)

  | Reg2Val(cond, op, reg1, reg2, vl) ->
    Printf.sprintf "  %s.%s %s, %s, %s"
      (op2string op) (condition2string cond) (reg2string reg1)
      (reg2string reg2) (vl2string vl)

  | Reg2(cond, op, reg1, reg2) ->
    Printf.sprintf "  %s.%s %s, %s"
      (op2string op) (condition2string cond) (reg2string reg1)
      (reg2string reg2)

  | Val1(cond, op, vl) ->
    Printf.sprintf "  %s.%s %s"
      (op2string op) (condition2string cond) (vl2string vl)

  | Reg1Val(cond, op, reg, vl) ->
    Printf.sprintf "  %s.%s %s, %s"
      (op2string op) (condition2string cond) (reg2string reg)
      (vl2string vl)

  | NoArg(cond, op) ->
    Printf.sprintf "  %s.%s"
      (op2string op) (condition2string cond)
;;

(* First int is line number, for error messages *)
type statement =
    Decl of int * int * Int64.t
  | Const of int * string * Int64.t
  | Instr of int * instruction
  | LabelStm of int * string
  | LocalLabelStm of int * string
  | Segment of int * string
  | NullStm


let statement2string = function
    Decl(_, i, i2) -> Printf.sprintf "decl %d, %Ld" i i2
  | Const(_, str, i) -> Printf.sprintf "const %s 0x%LX" str i
  | Instr(_, i) -> instruction2string i
  | LabelStm(_, s) -> s ^ ":"
  | LocalLabelStm(_, s) -> "!" ^ s ^ ":"
  | Segment(_, s) -> "segment " ^ s
  | NullStm -> ""
;;

let statements2string stmlist =
  List.fold_left (fun x y -> x ^ "\n" ^ (statement2string y)) "" stmlist
;;


(* Okay, so first we need to verify the existing code and make sure
   it is all valid and good.
   While we are at it, we will translate local labels into globals.
*)


(* Symbol table type and generally useful functions *)
type symtbl = {
  st_table : string list;
};;

let newSymtbl () = {
  st_table = [];
};;

let symExists st label = List.mem label st.st_table;;
(* XXX: Fix line numbers! 
*)
let symtblAdd st label =
  if symExists st label then
    Error.errorAndDie ("Duplicate label: " ^ label ^ " (oh and don't believe the line number...)")
  else
    {st_table = label :: st.st_table}
;;

(* Step 0: Resolve local symbols into global symbols *)
let createLocalLabel global label = global ^ "!" ^ label;;

let globalifyInstruction lastglobal instr =
  let helper = function
    | Label(s) -> Label(s)
    | LocalLabel(s) -> Label(createLocalLabel lastglobal s)
    | other -> other
  in
  match instr with
    | Reg2Val(cond, op, reg1, reg2, vl) -> 
      Reg2Val(cond, op, reg1, reg2, helper vl)
    | Val1(cond, op, vl) -> 
      Val1(cond, op, helper vl)
    | Reg1Val(cond, op, reg, vl) ->
      Reg1Val(cond, op, reg, helper vl)
    | other -> other
;;

let globalifyLabels stms =
  let rec loop lastglobal stms accm =
    match stms with
	stm::rest -> (
	  match stm with 
	    | LabelStm(l, s) -> 
	      let st = LabelStm(l, s) in
	      loop s rest (st::accm)
	    | LocalLabelStm(l, s) -> 
	      let st = LabelStm(l, createLocalLabel lastglobal s) in
	      loop lastglobal rest (st::accm)
	    | Instr(l, i) ->
	      let st = Instr(l, globalifyInstruction lastglobal i) in
	      loop lastglobal rest (st::accm)
	    | other -> loop lastglobal rest (other::accm)
	)
      | [] -> accm
  in
  List.rev (loop "" stms [])
;;


(* Step 1: Functions to find out what symbols exist.
   We should now have no local labels.
*)
(* Continue fixing line numbers *)
let addLabel symtbl stm =
  match stm with 
  | LabelStm(_, s) -> symtblAdd symtbl s
  | LocalLabelStm(_, s) -> Error.errorAndDie ("addLabel: Got local label " ^ s ^ " even when they should all be gone!")
  | Const(_, s, _) -> symtblAdd symtbl s
  | _ -> symtbl
;;

let addLabels symtbl stms =
  List.fold_left addLabel symtbl stms
;;

(* Step 2: Functions to make sure all referenced labels actually exist. *)
let checkInstrLabel symtbl i =
  let checkValue vl =
    match vl with
	Label(s) ->
	  if symExists symtbl s then
	    ()
	  else
	    Error.errorAndDie ("Unknown label: " ^ s)
      | LocalLabel(s) ->
	Error.errorAndDie ("checkInstrLabel: Got local label " ^ s ^ " even when they should all be gone!")
      | _ -> ()
  in
  match i with
    | Reg2Val(cond, op, reg1, reg2, vl) -> 
      checkValue vl
    | Val1(cond, op, vl) -> checkValue vl
    | Reg1Val(cond, op, reg, vl) -> checkValue vl
    | _ -> ()
;;

let checkLabel symtbl stm =
  match stm with 
  | Instr(_, i) -> checkInstrLabel symtbl i
  | _ -> ()
;;

let checkLabels st x =
  List.iter (checkLabel st) x
;;


(* Step 3: Check instructions to make sure they all line up with the right
   number of arguments.
   XXX: Fix line numbers again!
   You know, one way we could do this is to verify each instruction as valid or not
   DURING the parsing.  We have all the necessary info.  Hmmm.
*)
let opTypeValid line op t =
  let types = getTypes op in
  if not (List.mem t types) then
    Error.errorAndDieAtLine line ("Invalid operation type for op " ^ (op2string op) ^ ": " ^ (optype2string t))
;;


let checkOpType line instr =
  match instr with
    Reg3(_, op, _, _, _) ->
      opTypeValid line op TReg3
  | Reg2Val(_, op, _, _, _) ->
    opTypeValid line op TReg2Val
  | Reg2(_, op, _, _) ->
    opTypeValid line op TReg2
  | Val1(_, op, _) ->
    opTypeValid line op TVal1
  | Reg1Val(_, op, _, _) ->
    opTypeValid line op TReg1Val
  | NoArg(_, op) ->
    opTypeValid line op TNoArg
;;

let checkInstrType stm =
  match stm with 
  | Instr(l, i) -> checkOpType l i
  | _ -> ()
;;

let checkInstrTypes x =
  List.iter checkInstrType x
;;


let verify stms = 
  let st = newSymtbl () in
  let stms = globalifyLabels stms in
  let st = addLabels st stms in
  checkLabels st stms;
  checkInstrTypes stms;

  stms;
;;
