%{
open Int64
open St
exception ParseError of string

let parse_error str = 
  Error.errorAndDie str
;;
%}


%token BANG
%token COMMA COLON PERIOD SEGMENT CONST REG NEWLINE EOF

%token FLOAT INT ID DEC CONDITION
%type <St.reg> REG
%type <int> DEC
%type <float> FLOAT
%type <Int64.t> INT
%type <string> ID
%type <St.statement list> main 
%start main
%%
main: statement 
      {[$1]}
    | statement main
      {$1 :: $2}

statement: 
    decl
    {$1}
  | const
      {$1}
  | instr
      {$1}
  | label
      {$1}
  | locallabel
      {$1}
  | segment
      {$1}
  | NEWLINE
      {NullStm}

decl:
      DEC INT NEWLINE
      {Decl(Error.getLineNum (), $1, $2)}

const:
      CONST ID INT NEWLINE
      {Const(Error.getLineNum (), $2, $3)}

label:
      ID COLON
      {LabelStm(Error.getLineNum (), $1)}

locallabel:
      BANG ID COLON
      {LocalLabelStm(Error.getLineNum (), $2)}

segment:
      SEGMENT ID NEWLINE
      {Segment(Error.getLineNum (), $2)}

instr:
      reg3instr NEWLINE {$1}
  | reg2valinstr NEWLINE {$1}
  | reg2instr NEWLINE {$1}
  | val1instr NEWLINE {$1}
  | reg1valinstr NEWLINE {$1}
  | noarginstr NEWLINE {$1}

value:
    INT
      {St.Lit(Int64.to_int $1)}
  | ID
      {St.Label($1)}
  | BANG ID
      {St.LocalLabel($2)}

reg3instr:
    op REG COMMA REG COMMA REG 
       {Instr(Error.getLineNum (), Reg3(St.ALWAYS, $1, $2, $4, $6))} 

  | op PERIOD cond REG COMMA REG COMMA REG 
      {Instr(Error.getLineNum (), Reg3($3, $1, $4, $6, $8))} 


reg2valinstr:
      op REG COMMA REG COMMA value
      {Instr(Error.getLineNum (), Reg2Val(St.ALWAYS, $1, $2, $4, $6))} 

  | op PERIOD cond REG COMMA REG COMMA value
      {Instr(Error.getLineNum (), Reg2Val($3, $1, $4, $6, $8))}



reg2instr:
      op REG COMMA REG
      {Instr(Error.getLineNum (), Reg2(St.ALWAYS, $1, $2, $4))} 

  | op PERIOD cond REG COMMA REG
      {Instr(Error.getLineNum (), Reg2($3, $1, $4, $6))} 

val1instr:
      op value
      {Instr(Error.getLineNum (), Val1(St.ALWAYS, $1, $2))} 

  | op PERIOD cond value
      {Instr(Error.getLineNum (), Val1($3, $1, $4))} 


reg1valinstr:
      op REG COMMA value
      {Instr(Error.getLineNum (), Reg1Val(St.ALWAYS, $1, $2, $4))} 

  | op PERIOD cond REG COMMA value
      {Instr(Error.getLineNum (), Reg1Val($3, $1, $4, $6))} 



noarginstr:
      op
      {Instr(Error.getLineNum (), NoArg(St.ALWAYS, $1))} 
/*
  | op PERIOD cond
      {Instr(NoArg($3, $1))} 
*/

op:
      ID {St.string2op $1}

cond:
      ID {St.string2condition $1}
%%

