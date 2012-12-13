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
val conditionTable : (condition * string) list
val table2Op : 'a * 'b -> 'a
val table2String : 'a * 'b -> 'b
val table3Op : 'a * 'b * 'c -> 'a
val table3String : 'a * 'b * 'c -> 'b
val table3Types : 'a * 'b * 'c -> 'c
val condition2string : condition -> string
val string2condition : string -> condition
type reg = Reg of int | SReg of int
val reg2string : reg -> string
type vl = Lit of int | Label of string | LocalLabel of string
val vl2string : vl -> string
type optype = TReg3 | TReg2Val | TReg2 | TVal1 | TReg1Val | TNoArg
val optype2string : optype -> string
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
  | Not
  | M
  | Neg
val opTable : (op * string * optype list) list
val op2string : op -> string
val string2op : string -> op
val getTypes : op -> optype list
type instruction =
    Reg3 of condition * op * reg * reg * reg
  | Reg2Val of condition * op * reg * reg * vl
  | Reg2 of condition * op * reg * reg
  | Val1 of condition * op * vl
  | Reg1Val of condition * op * reg * vl
  | NoArg of condition * op
val instruction2string : instruction -> string
type statement =
    Decl of int * int * Int64.t
  | Const of int * string * Int64.t
  | Instr of int * instruction
  | LabelStm of int * string
  | LocalLabelStm of int * string
  | Segment of int * string
  | NullStm
val statement2string : statement -> string
val statements2string : statement list -> string
type symtbl = { st_table : string list; }
val newSymtbl : unit -> symtbl
val symExists : symtbl -> string -> bool
val symtblAdd : symtbl -> string -> symtbl
val createLocalLabel : string -> string -> string
val globalifyInstruction : string -> instruction -> instruction
val globalifyLabels : statement list -> statement list
val addLabel : symtbl -> statement -> symtbl
val addLabels : symtbl -> statement list -> symtbl
val checkInstrLabel : symtbl -> instruction -> unit
val checkLabel : symtbl -> statement -> unit
val checkLabels : symtbl -> statement list -> unit
val opTypeValid : int -> op -> optype -> unit
val checkOpType : int -> instruction -> unit
val checkInstrType : statement -> unit
val checkInstrTypes : statement list -> unit
val verify : statement list -> statement list
