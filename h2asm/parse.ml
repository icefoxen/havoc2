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

open Parsing;;
# 2 "parse.mly"
open Int64
open St
exception ParseError of string

let parse_error str = 
  Error.errorAndDie str
;;
# 27 "parse.ml"
let yytransl_const = [|
  257 (* BANG *);
  258 (* COMMA *);
  259 (* COLON *);
  260 (* PERIOD *);
  261 (* SEGMENT *);
  262 (* CONST *);
  264 (* NEWLINE *);
    0 (* EOF *);
  269 (* CONDITION *);
    0|]

let yytransl_block = [|
  263 (* REG *);
  265 (* FLOAT *);
  266 (* INT *);
  267 (* ID *);
  268 (* DEC *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\004\000\006\000\007\000\008\000\005\000\005\000\
\005\000\005\000\005\000\005\000\015\000\015\000\015\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\013\000\
\013\000\014\000\016\000\017\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\004\000\002\000\003\000\003\000\002\000\002\000\
\002\000\002\000\002\000\002\000\001\000\001\000\002\000\006\000\
\008\000\006\000\008\000\004\000\006\000\002\000\004\000\004\000\
\006\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\037\000\000\000\003\000\004\000\005\000\006\000\007\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\002\000\015\000\016\000\017\000\
\018\000\019\000\020\000\000\000\000\000\000\000\021\000\022\000\
\030\000\013\000\014\000\000\000\010\000\023\000\036\000\000\000\
\000\000\011\000\000\000\031\000\000\000\032\000\000\000\000\000\
\000\000\033\000\024\000\026\000\000\000\025\000\027\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\041\000\023\000\
\048\000"

let yysindex = "\001\000\
\004\255\000\000\248\254\249\254\251\254\000\000\020\255\017\255\
\000\000\004\255\000\000\000\000\000\000\000\000\000\000\000\000\
\031\255\034\255\038\255\043\255\046\255\047\255\018\255\053\255\
\049\255\035\255\000\000\050\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\039\255\048\255\058\255\000\000\000\000\
\000\000\000\000\000\000\054\255\000\000\000\000\000\000\023\255\
\025\255\000\000\059\255\000\000\061\255\000\000\030\255\037\255\
\062\255\000\000\000\000\000\000\042\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\255\000\000\
\000\000\065\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\060\255\000\000\000\000\000\000\
\063\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\056\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\208\255\000\000\
\000\000"

let yytablesize = 71
let yytable = "\052\000\
\054\000\001\000\024\000\025\000\003\000\026\000\058\000\060\000\
\004\000\005\000\035\000\006\000\063\000\035\000\007\000\008\000\
\035\000\035\000\036\000\035\000\035\000\037\000\027\000\036\000\
\038\000\036\000\028\000\039\000\040\000\051\000\036\000\053\000\
\039\000\040\000\039\000\040\000\057\000\036\000\030\000\039\000\
\040\000\031\000\036\000\059\000\044\000\032\000\039\000\040\000\
\062\000\046\000\033\000\039\000\040\000\034\000\035\000\042\000\
\043\000\045\000\047\000\049\000\055\000\050\000\056\000\061\000\
\001\000\029\000\000\000\028\000\000\000\000\000\029\000"

let yycheck = "\048\000\
\049\000\001\000\011\001\011\001\001\001\011\001\055\000\056\000\
\005\001\006\001\001\001\008\001\061\000\004\001\011\001\012\001\
\007\001\008\001\001\001\010\001\011\001\004\001\003\001\001\001\
\007\001\001\001\010\001\010\001\011\001\007\001\001\001\007\001\
\010\001\011\001\010\001\011\001\007\001\001\001\008\001\010\001\
\011\001\008\001\001\001\007\001\010\001\008\001\010\001\011\001\
\007\001\011\001\008\001\010\001\011\001\008\001\008\001\003\001\
\008\001\008\001\011\001\002\001\002\001\008\001\002\001\002\001\
\000\000\010\000\255\255\008\001\255\255\255\255\008\001"

let yynames_const = "\
  BANG\000\
  COMMA\000\
  COLON\000\
  PERIOD\000\
  SEGMENT\000\
  CONST\000\
  NEWLINE\000\
  EOF\000\
  CONDITION\000\
  "

let yynames_block = "\
  REG\000\
  FLOAT\000\
  INT\000\
  ID\000\
  DEC\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 25 "parse.mly"
      ([_1])
# 152 "parse.ml"
               : St.statement list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : St.statement list) in
    Obj.repr(
# 27 "parse.mly"
      (_1 :: _2)
# 160 "parse.ml"
               : St.statement list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 31 "parse.mly"
    (_1)
# 167 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 33 "parse.mly"
      (_1)
# 174 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 35 "parse.mly"
      (_1)
# 181 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 37 "parse.mly"
      (_1)
# 188 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'locallabel) in
    Obj.repr(
# 39 "parse.mly"
      (_1)
# 195 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'segment) in
    Obj.repr(
# 41 "parse.mly"
      (_1)
# 202 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parse.mly"
      (NullStm)
# 208 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Int64.t) in
    Obj.repr(
# 47 "parse.mly"
      (Decl(_1, _2))
# 216 "parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Int64.t) in
    Obj.repr(
# 51 "parse.mly"
      (Const(_2, _3))
# 224 "parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parse.mly"
      (LabelStm(_1))
# 231 "parse.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 59 "parse.mly"
      (LocalLabelStm(_2))
# 238 "parse.ml"
               : 'locallabel))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 63 "parse.mly"
      (Segment(_2))
# 245 "parse.ml"
               : 'segment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'reg3instr) in
    Obj.repr(
# 66 "parse.mly"
                        (_1)
# 252 "parse.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'reg2valinstr) in
    Obj.repr(
# 67 "parse.mly"
                         (_1)
# 259 "parse.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'reg2instr) in
    Obj.repr(
# 68 "parse.mly"
                      (_1)
# 266 "parse.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'val1instr) in
    Obj.repr(
# 69 "parse.mly"
                      (_1)
# 273 "parse.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'reg1valinstr) in
    Obj.repr(
# 70 "parse.mly"
                         (_1)
# 280 "parse.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'noarginstr) in
    Obj.repr(
# 71 "parse.mly"
                       (_1)
# 287 "parse.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Int64.t) in
    Obj.repr(
# 75 "parse.mly"
      (St.Lit(_1))
# 294 "parse.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parse.mly"
      (St.Label(_1))
# 301 "parse.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parse.mly"
      (St.LocalLabel(_2))
# 308 "parse.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : St.reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : St.reg) in
    Obj.repr(
# 83 "parse.mly"
       (Instr(Reg3(St.ALWAYS, _1, _2, _4, _6)))
# 318 "parse.ml"
               : 'reg3instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : St.reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : St.reg) in
    Obj.repr(
# 86 "parse.mly"
      (Instr(Reg3(_3, _1, _4, _6, _8)))
# 329 "parse.ml"
               : 'reg3instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : St.reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 91 "parse.mly"
      (Instr(Reg2Val(St.ALWAYS, _1, _2, _4, _6)))
# 339 "parse.ml"
               : 'reg2valinstr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : St.reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 94 "parse.mly"
      (Instr(Reg2Val(_3, _1, _4, _6, _8)))
# 350 "parse.ml"
               : 'reg2valinstr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : St.reg) in
    Obj.repr(
# 100 "parse.mly"
      (Instr(Reg2(St.ALWAYS, _1, _2, _4)))
# 359 "parse.ml"
               : 'reg2instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : St.reg) in
    Obj.repr(
# 103 "parse.mly"
      (Instr(Reg2(_3, _1, _4, _6)))
# 369 "parse.ml"
               : 'reg2instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 107 "parse.mly"
      (Instr(Val1(St.ALWAYS, _1, _2)))
# 377 "parse.ml"
               : 'val1instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 110 "parse.mly"
      (Instr(Val1(_3, _1, _4)))
# 386 "parse.ml"
               : 'val1instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 115 "parse.mly"
      (Instr(Reg1Val(St.ALWAYS, _1, _2, _4)))
# 395 "parse.ml"
               : 'reg1valinstr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : St.reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 118 "parse.mly"
      (Instr(Reg1Val(_3, _1, _4, _6)))
# 405 "parse.ml"
               : 'reg1valinstr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 124 "parse.mly"
      (Instr(NoArg(St.ALWAYS, _1)))
# 412 "parse.ml"
               : 'noarginstr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parse.mly"
         (St.string2op _1)
# 419 "parse.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parse.mly"
         (St.string2condition _1)
# 426 "parse.ml"
               : 'cond))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : St.statement list)
;;
# 136 "parse.mly"

# 453 "parse.ml"
