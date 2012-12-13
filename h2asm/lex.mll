{
open St
open Parse
open Error
exception Eof
exception Lexer_error

let inComment = ref 0;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  (*
  let c = (gs lb) in
  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb);
  *)
  chrNum := !chrNum + (String.length (Lexing.lexeme lb))
;;

let str2float x =
   Scanf.sscanf x "%f" (fun x -> x)
;;

let str2int =
  Int64.of_string
;;

(* Go brute force! *)
let str2reg = function
  | "r0" -> Reg(0)
  | "r1" -> Reg(1)
  | "r2" -> Reg(2)
  | "r3" -> Reg(3)
  | "r4" -> Reg(4)
  | "r5" -> Reg(5)
  | "r6" -> Reg(6)
  | "r7" -> Reg(7)
  | "r8" -> Reg(8)
  | "r9" -> Reg(9)
  | "r10" -> Reg(10)
  | "r11" -> Reg(11)
  | "r12" -> Reg(12)
  | "r13" -> Reg(13)
  | "r14" -> Reg(14)
  | "r15" -> Reg(15)
  | "r16" -> Reg(16)
  | "r17" -> Reg(17)
  | "r18" -> Reg(18)
  | "r19" -> Reg(19)
  | "r20" -> Reg(20)
  | "r21" -> Reg(21)
  | "r22" -> Reg(22)
  | "r23" -> Reg(23)
  | "r24" -> Reg(24)
  | "r25" -> Reg(25)
  | "r26" -> Reg(26)
  | "r27" -> Reg(27)
  | "r28" -> Reg(28)
  | "r29" -> Reg(29)
  | "r30" -> Reg(30)
  | "r31" -> Reg(31)
  | "sr" -> Reg(29)
  | "over" -> Reg(30)
  | "lr" -> Reg(31)
  | "s0" -> SReg(0)
  | "s1" -> SReg(1)
  | "s2" -> SReg(2)
  | "s3" -> SReg(3)
  | "s4" -> SReg(4)
  | "sip" -> SReg(0)
  | "sflags" -> SReg(1)
  | "scpu" -> SReg(2)
  | "simask" -> SReg(3)
  | "sidt" -> SReg(4)
  | _ -> errorAndDie "Invalid register!"
;;

(*
let str2char x =
   Scanf.sscanf x "%C" (fun x -> x) 
;;

let str2str x =
   Scanf.sscanf x "%S" (fun x -> x) 
;;

*)


}

let id = 
  ['A'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*
let enum = fnum | fnum ['e' 'E'] inum

let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")

let str = '"'([^'"''\\']|'\\'_)*'"'

let reg = 'r' ['0'-'9']* | 's'['0'-'9']* | "sr" | "over" | "lr" |
    "sip" | "sflags" | "scpu" | "simask" | "sidt"

rule token = parse
   (inum|bnum)		{adv lexbuf; INT(str2int (gs lexbuf))}
 | fnum			{adv lexbuf; FLOAT(str2float (gs lexbuf))}
 | reg                  {adv lexbuf; REG(str2reg (gs lexbuf))}
 | "\n"                 {nl (); NEWLINE}
 | [' ''\t']            {adv lexbuf; token lexbuf}
 | "segment"            {adv lexbuf; SEGMENT}
 | "db"                 {adv lexbuf; DEC(1)}
 | "dw"                 {adv lexbuf; DEC(2)}
 | "dt"                 {adv lexbuf; DEC(4)}
 | "do"                 {adv lexbuf; DEC(8)}
 | "const"              {adv lexbuf; CONST}
 | ","			{adv lexbuf; COMMA}
 | ":"			{adv lexbuf; COLON}
 | "."			{adv lexbuf; PERIOD}
 | "!"                  {adv lexbuf; BANG}
 | ";"                  {adv lexbuf; lcomment lexbuf}
 | id			{adv lexbuf; ID(gs lexbuf)}
 | eof                  {EOF}
 | _                    {errorAndDie "Invalid token!"}

and lcomment = parse
   '\n'                 {nl (); token lexbuf}
  | _                   {adv lexbuf; lcomment lexbuf}
 
