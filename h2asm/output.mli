val o3destmask : int32
val o3destshift : int
val o3src1mask : int32
val o3src1shift : int
val o3src2mask : int32
val o3src2length : int
val o2destmask : int32
val o2destshift : int
val o2mask : int32
val o2length : int
val o1mask : int32
val o1length : int
val conditionmask : int32
val opcodemask : int32
val conditionshift : int
val opcodeshift : int
val lor32 : int32 -> int32 -> int32
val lsl32 : int32 -> int -> int32
val lsr32 : int32 -> int -> int32
val and32 : int32 -> int32 -> int32
val lorMany : int32 list -> int32
val makeO3 : int -> int -> int -> int -> int -> int32
val makeO2 : int -> int -> int -> int -> int32
val makeO1 : int -> int -> int -> int32
val encodeInstruction : Translate.instr -> int32
val fml : int32 -> int -> int32
val cockmongleInt32 : int32 -> int list
val encodeStatements : Translate.statement list -> int list
