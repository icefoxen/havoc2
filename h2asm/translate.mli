val substituteInstruction : St.instruction -> St.instruction
val opAdd : int
val opAddi : int
val opAdc : int
val opAdci : int
val opSub : int
val opSubi : int
val opSbb : int
val opSbbi : int
val opMul : int
val opMuli : int
val opUmul : int
val opUmuli : int
val opDiv : int
val opDivi : int
val opUdiv : int
val opUdivi : int
val opAnd : int
val opAndi : int
val opOr : int
val opOri : int
val opXor : int
val opXori : int
val opSl : int
val opSli : int
val opSr : int
val opSri : int
val opSru : int
val opSrui : int
val opRl : int
val opRli : int
val opRr : int
val opRri : int
val opNand : int
val opNandi : int
val opNor : int
val opNori : int
val opNxor : int
val opNxori : int
val opB : int
val opBl : int
val opBr : int
val opBrl : int
val opBf : int
val opBfl : int
val opGet : int
val opSet : int
val opInt : int
val opReti : int
val opLo : int
val opLor : int
val opLof : int
val opLt : int
val opLtr : int
val opLtf : int
val opLw : int
val opLwr : int
val opLwf : int
val opLb : int
val opLbr : int
val opLbf : int
val opSo : int
val opSor : int
val opSof : int
val opSt : int
val opStr : int
val opStf : int
val opSw : int
val opSwr : int
val opSwf : int
val opSb : int
val opSbr : int
val opSbf : int
val opLiu : int
val opLium : int
val opLilm : int
val opLil : int
val opLi : int
val opCmp : int
val opExtb : int
val opExtw : int
val opExtt : int
val opCmpsz : int
val opHalt : int
val condition2code : St.condition -> int
val reg2code : St.reg -> int
type value = Literal of int | Label of string
val vl2value : St.vl -> value
val value2string : value -> string
type instr =
    Reg3 of int * int * int * int * int
  | Reg2Val of int * int * int * int * value
  | Reg2 of int * int * int * int
  | Val1 of int * int * value
  | Reg1Val of int * int * int * value
  | NoArg of int * int
val instr2string : instr -> string
val reg3op2code : int -> St.op -> int
val reg2valop2code : int -> St.op -> int
val reg2op2code : int -> St.op -> int
val val1op2code : int -> St.op -> int
val reg1valop2code : int -> St.op -> int
val noargop2code : int -> St.op -> int
val translateInstruction : int -> St.instruction -> instr
val handleInstruction : int -> St.instruction -> instr
type statement =
    Decl of int * Int64.t
  | Const of string * Int64.t
  | Instr of instr
  | LabelStm of string
  | Segment of string
val statement2string : statement -> string
val statementSize : statement -> int
val getLabelAddresses : statement list -> int * (string * int) list
val literalizeValue : (string * int) list -> value -> int -> value
val literalizeInstruction : (string * int) list -> instr -> instr
val literalizeStatement : (string * int) list -> statement -> statement
val literalizeStatements :
  (string * int) list -> statement list -> statement list
val translate : St.statement list -> statement list
