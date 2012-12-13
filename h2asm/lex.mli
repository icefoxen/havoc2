exception Eof
exception Lexer_error
val inComment : int ref
val gs : Lexing.lexbuf -> string
val adv : Lexing.lexbuf -> unit
val str2float : string -> float
val str2int : string -> int64
val str2reg : string -> St.reg
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Parse.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Parse.token
val lcomment : Lexing.lexbuf -> Parse.token
val __ocaml_lex_lcomment_rec : Lexing.lexbuf -> int -> Parse.token
