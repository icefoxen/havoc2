(*
  Hokay....
  So we know that the syntax tree is valid, all the labels are
  resolved to globals, and all the instructions are of the
  correct types.  Excellent!
  So now we need to turn St.op's into Translate.opcodes, which involves
  inferring immediate instructions ("add" into either "add" or "addi" 
  depending on arguments) and translating assembler shortcuts into longcuts
  (such as 'm r1, r2' into 'add r1, r2, 0')

  Most of the data structures here look very similar to those in St.
  This is because St is the syntax tree, and here we are actually compiling
  it to its final target code.  But because the final target code is so
  similar to the syntax tree of the source code, the differences are pretty
  subtle.
  
  Then we build the binary layout, and then resolve labels and check to
  make sure all the argument sizes fit.
  Building the binary layout should be pretty easy... we just have a series
  of decl's, each with a size and either a number or instruction as contents.
  Then we go through and figure out where labels point, and we should be
  ready for output.
*)


(*
type opcode =
    Add
  | Addi
  | Adc
  | Adci
  | Sub
  | Subi
  | Sbb
  | Sbbi
  | Mul
  | Muli
  | Umul
  | Umuli
  | Div
  | Divi
  | Udiv
  | Udivi

  | And
  | Andi
  | Or
  | Ori
  | Xor
  | Xori 
  | Sl
  | Sli
  | Sr
  | Sri
  | Sru
  | Srui
  | Rl
  | Rli
  | Rr
  | Rri

  | Nand
  | Nandi
  | Nor
  | Nori
  | Nxor
  | Nxori
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
  | Sto
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
;;
*)

(* First, assembler substitutions, because they're easy *)
let substituteInstruction = function
  | St.Reg2(cond, St.Not, r1, r2) ->
    St.Reg2Val(cond, St.Xor, r1, r2, St.Lit(-1))
  | St.Reg2(cond, St.M, r1, r2) ->
    St.Reg2Val(cond, St.Add, r1, r2, St.Lit(0))
  | St.Reg2(cond, St.Neg, r1, r2) ->
    St.Reg2Val(cond, St.Sub, r1, r2, St.Lit(0))
  | other -> other
;;



(* Opcodes *)
let opAdd = 0x00
and opAddi = 0x01
and opAdc = 0x02
and opAdci = 0x3
and opSub = 0x4
and opSubi = 0x5
and opSbb = 0x6
and opSbbi = 0x7
and opMul = 0x8
and opMuli = 0x9
and opUmul = 0xA
and opUmuli = 0xB
and opDiv = 0xC
and opDivi = 0xD
and opUdiv = 0xE
and opUdivi = 0xF

and opAnd = 0x10
and opAndi = 0x11
and opOr = 0x12
and opOri = 0x13
and opXor = 0x14
and opXori = 0x15
and opSl = 0x16
and opSli = 0x17
and opSr = 0x18
and opSri = 0x19
and opSru = 0x1A
and opSrui = 0x1B
and opRl = 0x1C
and opRli = 0x1D
and opRr = 0x1E
and opRri = 0x1F

and opNand = 0x20
and opNandi = 0x21
and opNor = 0x22
and opNori = 0x23
and opNxor = 0x24
and opNxori = 0x25
and opB = 0x26
and opBl = 0x27
and opBr = 0x28
and opBrl = 0x29
and opBf = 0x2A
and opBfl = 0x2B
and opGet = 0x2C
and opSet = 0x2D
and opInt = 0x2E
and opReti = 0x2F

and opLo = 0x30
and opLor = 0x31
and opLof = 0x32
and opLt = 0x34
and opLtr = 0x35
and opLtf = 0x36
and opLw = 0x38
and opLwr = 0x39
and opLwf = 0x3A
and opLb = 0x3C
and opLbr = 0x3D
and opLbf = 0x3E

and opSo = 0x40
and opSor = 0x41
and opSof = 0x42
and opSt = 0x44
and opStr = 0x45
and opStf = 0x46
and opSw = 0x48
and opSwr = 0x49
and opSwf = 0x4A
and opSb = 0x4C
and opSbr = 0x4D
and opSbf = 0x4E

and opLiu = 0x50
and opLium = 0x51
and opLilm = 0x52
and opLil = 0x53
and opLi = 0x54
and opCmp = 0x55
and opExtb = 0x56
and opExtw = 0x57
and opExtt = 0x58
and opCmpsz = 0x59
and opHalt = 0x5A
;;

(* Conditions are 1-to-1 between source and machine code, so this is
   a little simpler. *)
let condition2code = function
  | St.FL -> 0
  | St.ALWAYS -> 1
  | St.N -> 2
  | St.GT -> 3
  | St.LE -> 4
  | St.LT -> 5
  | St.GE -> 6
  | St.EQ -> 7
  | St.NE -> 8
  | St.AB -> 9
  | St.BE -> 10
  | St.BL -> 11
  | St.AE -> 12
  | St.CS -> 13
  | St.CC -> 14
;;

let reg2code = function
  | St.Reg(i) -> i
  | St.SReg(i) -> i
;;


type value =
    Literal of int
  | Label of string
;;

let vl2value = function
  | St.Lit(i) -> Literal(i)
  | St.Label(s) -> Label(s)
  | St.LocalLabel(_) -> Error.errorAndDie "Local labels should all be gone!"
;;

let value2string = function
  | Literal(i) -> Printf.sprintf "(lit %d)" i
  | Label(s) -> Printf.sprintf "(label %s)" s
;;

(* Similar to, but not the same thing as, St.instruction 
   Namely, more numerical.  First two integers are always condition+opcode,
   next are registers.
*)
type instr =
    Reg3 of int * int * int * int * int
  | Reg2Val of int * int * int * int * value
  | Reg2 of int * int * int * int
  | Val1 of int * int * value
  | Reg1Val of int * int * int * value
  | NoArg of int * int
;;

let instr2string = function
    Reg3(cond, op, r1, r2, r3) -> Printf.sprintf "reg3: 0x%X.%d %d, %d, %d" op cond r1 r2 r3
  | Reg2Val(cond, op, r1, r2, vl) -> Printf.sprintf "reg2val: 0x%X.%d %d, %d, %s" op cond r1 r2 (value2string vl)
  | Reg2(cond, op, r1, r2) -> Printf.sprintf "reg2: 0x%X.%d %d, %d" op cond r1 r2
  | Val1(cond, op, vl) -> Printf.sprintf "val1: 0x%X.%d %s" op cond (value2string vl)
  | Reg1Val(cond, op, r1,vl) -> Printf.sprintf "reg1val: 0x%X.%d %d, %s" op cond r1 (value2string vl)
  | NoArg(cond, op) -> Printf.sprintf "noarg: 0x%X.%d" op cond
;;



let reg3op2code l = function
  | St.Add -> opAdd
  | St.Adc -> opAdc
  | St.Sub -> opSub
  | St.Sbb -> opSbb
  | St.Mul -> opMul
  | St.Umul -> opUmul
  | St.Div -> opDiv
  | St.Udiv -> opUdiv
  | St.And -> opAnd
  | St.Or -> opOr
  | St.Xor -> opXor
  | St.Sl -> opSl
  | St.Sr -> opSr
  | St.Sru -> opSru
  | St.Rl -> opRl
  | St.Rr -> opRr
  | St.Nand -> opNand
  | St.Nor -> opNor
  | St.Nxor -> opNxor
  | St.Cmpsz -> opCmpsz
  | x -> Error.errorAndDieAtLine l ("Invalid op type, expected reg3, got " ^ (St.op2string x))
;;

let reg2valop2code l = function
  | St.Add -> opAddi
  | St.Adc -> opAdci
  | St.Sub -> opSubi
  | St.Sbb -> opSbbi
  | St.Mul -> opMuli
  | St.Umul -> opUmuli
  | St.Div -> opDivi
  | St.Udiv -> opUdivi
  | St.And -> opAndi
  | St.Or -> opOri
  | St.Xor -> opXori
  | St.Sl -> opSli
  | St.Sr -> opSri
  | St.Sru -> opSrui
  | St.Rl -> opRli
  | St.Rr -> opRri
  | St.Nand -> opNandi
  | St.Nor -> opNori
  | St.Nxor -> opNxori

  | St.Lo -> opLo
  | St.Lor -> opLor
  | St.Lt -> opLt
  | St.Ltr -> opLtr
  | St.Lw -> opLw
  | St.Lwr -> opLwr
  | St.Lb -> opLb
  | St.Lbr -> opLbr

  | St.So -> opSo
  | St.Sor -> opSor
  | St.St -> opSt
  | St.Str -> opStr
  | St.Sw -> opSw
  | St.Swr -> opSwr
  | St.Sb -> opSb
  | St.Sbr -> opSbr
  | x -> Error.errorAndDieAtLine l ("Invalid op type, expected reg2val, got " ^ (St.op2string x))
;;

let reg2op2code l = function
  | St.Cmp -> opCmp
  | St.Extb -> opExtb
  | St.Extw -> opExtw
  | St.Extt -> opExtt
  | x -> Error.errorAndDieAtLine l ("Invalid op type, expected reg3, got " ^ (St.op2string x))
;;

let val1op2code l = function
  | St.Bf -> opBf
  | St.Bfl -> opBfl
  | St.Int -> opInt
  | x -> Error.errorAndDieAtLine l ("Invalid op type, expected val1reg, got " ^ (St.op2string x))
;;

let reg1valop2code l = function
  | St.B -> opB
  | St.Bl -> opBl
  | St.Br -> opBr
  | St.Brl -> opBrl
  | St.Get -> opGet
  | St.Set -> opSet
  | St.Reti -> opReti

  | St.Lof -> opLof
  | St.Ltf -> opLtf
  | St.Lwf -> opLwf
  | St.Lbf -> opLbf
  | St.Sof -> opSof
  | St.Stf -> opStf
  | St.Swf -> opSwf
  | St.Sbf -> opSbf
  | St.Liu -> opLiu
  | St.Lium -> opLium
  | St.Lilm -> opLilm
  | St.Lil -> opLil
  | St.Li -> opLi
  | x -> Error.errorAndDieAtLine l ("Invalid op type, expected reg1val, got " ^ (St.op2string x))
;;

let noargop2code l = function
  | St.Halt -> opHalt
  | x -> Error.errorAndDieAtLine l ("Invalid op type, expected noarg, got " ^ (St.op2string x))
;;


(* Turns St.instruction and St.op into Translate.instr and opcodes, as well as condition codes 
   l is the line number, for error reporting.
*)
let translateInstruction l = function
  | St.Reg3(cond, op, r1, r2, r3) ->
    let c = condition2code cond
    and o = reg3op2code l op
    and rc1 = reg2code r1
    and rc2 = reg2code r2
    and rc3 = reg2code r3
    in
    Reg3(c, o, rc1, rc2, rc3)

  | St.Reg2Val(cond, op, r1, r2, vl) ->
    let c = condition2code cond
    and o = reg2valop2code l op
    and rc1 = reg2code r1
    and rc2 = reg2code r2
    and v = vl2value vl
    in
    Reg2Val(c, o, rc1, rc2, v)

  | St.Reg2(cond, op, r1, r2) ->
    let c = condition2code cond
    and o = reg2op2code l op
    and rc1 = reg2code r1
    and rc2 = reg2code r2
    in
    Reg2(c, o, rc1, rc2)

  | St.Val1(cond, op, vl) ->
    let c = condition2code cond
    and o = val1op2code l op
    and v = vl2value vl
    in
    Val1(c, o, v)

  | St.Reg1Val(cond, op, reg, vl) ->
    let c = condition2code cond
    and o = reg1valop2code l op
    and rc1 = reg2code reg
    and v = vl2value vl
    in
    Reg1Val(c, o, rc1, v)

  | St.NoArg(cond, op) ->
    let c = condition2code cond
    and o = noargop2code l op
    in
    NoArg(c, o)
;;

let handleInstruction l instr  =
  let i = substituteInstruction instr in
  translateInstruction  l i
;;

(* Rather like St.statement but not *)
type statement =
    Decl of int * Int64.t
  | Const of string * Int64.t
  | Instr of instr
  | LabelStm of string
  | Segment of string
;;

let statement2string = function
    Decl(i, i64) ->  Printf.sprintf "decl %d 0x%LX" i i64
  | Const(s, i64) -> Printf.sprintf "const %s 0x%LX" s i64
  | Instr(i) -> (instr2string i)
  | LabelStm(s) -> "Label " ^ s
  | Segment(s) -> "Segment " ^ s
;;



(* Step 4: Make memory layout and resolve labels. *)
let statementSize = function
    Decl(i, _) -> i
  | Const(_) -> 0
  | Instr(_) -> 4
  | LabelStm(s) -> 0
  | Segment(s) -> 0
;;


(* This returns a (int, (string, int) list)
   The first int is the total size of the program.
   The second is an assoc list pairing label to the address of that label.
   XXX: Should we be able to specify an address offset in the program?
   Probably...  Well it's not necessary if all code is relocatable, actually.
   Which it is.

   XXX: Int64.to_int silently clobbers int64's that are too large to fit...
*)
let getLabelAddresses stms =
  let rec loop stms i accm =
    match stms with
	[] -> (i, accm)
      | hd :: tl ->
	match hd with
	  | LabelStm(s) -> loop tl i ((s, i) :: accm)
	  | Const(name, i64) -> loop tl i ((name, Int64.to_int i64) :: accm)
	  | x -> loop tl (i + (statementSize x)) accm
  in
  loop stms 0 []
;;

(* Hm.  We actually have a problem.  MOST of the time 
   immediates are a literal number.  But SOME of the time
   they are relative addresses!  Hrmbl.
   They are only relative addresses for the following instructions:
   bf bfl lof ltf lwf lbf sof stf swf sbf

   XXX: Well, right now let us just do the simple-and-dumb
   direct solution...

   XXX: Also, right now, segment declerations are meaningless.

   Also, we could probably do this waaay earlier, perhaps when we
   convert St types to Translate types, thus eliminating a step and a bit of
   complexity (making Translate.instruction only have numbers as arguments.)
   Well no, not quite, we MUST do this on the Translate types because the St
   types don't necessarily have a 1-to-1 correspondance with the machine code.
   For instance, we could make 'li' assemble into multiple loads if the argument
   is longer than 16 bits, etc
*)

let literalizeValue ltbl vl bits =
  let max = ((1 lsl bits) / 2) - 1
  and min = - ((1 lsl bits) / 2)
  in
  let v = 
    match vl with
    | Label(x) -> List.assoc x ltbl
    | Literal(x) -> x
  in
  if (v > max) or (v < min) then
    Error.errorAndDie (Printf.sprintf "Shit, number %d is too big, max %d bits!" v bits)
  else
    Literal(v)
;;

let literalizeInstruction ltbl instr =
  match instr with 
  | Reg2Val(cond, op, r1, r2, vl) ->
  (* vl can be up to 11 bits *)
    let v = literalizeValue ltbl vl 11 in
    Reg2Val(cond, op, r1, r2, v)
  | Val1(cond, op, vl) -> 
  (* vl can be up to 21 bits *)
    let v = literalizeValue ltbl vl 21 in
    Val1(cond, op, v)
  | Reg1Val(cond, op, r1, vl) -> 
  (* vl can be up to 16 bits *)
    let v = literalizeValue ltbl vl 16 in
    Reg1Val(cond, op, r1, v)
  | x -> x
;;

let literalizeStatement ltbl stm =
  match stm with
    | Instr(i) -> Instr(literalizeInstruction ltbl i)
    | x -> x
;;

let literalizeStatements ltbl stms =
  List.map (literalizeStatement ltbl) stms
;;


(* Turn a St.statement list into a Translate.statement 
   A little annoying as we have to groom out the NullStm hacks.
   But, we might as well turn labels into literals in the processes!
*)
let translate stms =
  let rec loop stms accm =
    match stms with
	[] -> List.rev accm
      | hd :: tl ->
	match hd with
	  | St.Decl(_, i, i2) -> loop tl (Decl(i, i2) :: accm)
	  | St.Const(_, str, i) -> loop tl (Const(str, i) :: accm)
	  | St.Instr(l, i) -> loop tl (Instr(handleInstruction l i) :: accm)
	  | St.LabelStm(_, s) -> loop tl (LabelStm(s) :: accm)
	  | St.Segment(_, s) -> loop tl (Segment(s) :: accm)
	  | St.NullStm -> loop tl accm
	  | St.LocalLabelStm(l, s) -> Error.errorAndDieAtLine l ("Local label statement should not exist now!  Got label " ^ s)
  in
  let s = loop stms [] in
  let (bytes, ltbl) = getLabelAddresses s in
  let s = literalizeStatements ltbl s in
  List.iter print_endline (List.map statement2string s);
  Printf.printf "Total program size: %d bytes\n" bytes;
    s
;;
