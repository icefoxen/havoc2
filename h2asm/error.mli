val lineNum : int ref
val chrNum : int ref
val fileName : string ref
val nl : unit -> unit
val error : string -> unit
val errorAndDie : string -> 'a
val reset : string -> unit
val setLineNum : int -> unit
val getLineNum : unit -> int
val setLineAndChar : int -> int -> unit
val errorAndDieAtLine : int -> string -> 'a
val makeList : int -> 'a -> 'a list
