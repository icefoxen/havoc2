type token =
  | BANG
  | COMMA
  | COLON
  | PERIOD
  | SEGMENT
  | CONST
  | REG of (St.reg)
  | NEWLINE
  | EOF
  | FLOAT of (float)
  | INT of (Int64.t)
  | ID of (string)
  | DEC of (int)
  | CONDITION

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> St.statement list
