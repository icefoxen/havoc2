open St
open Parse
open Translate

let compileFile fn =
  Error.reset fn;
  try
    let instream = open_in fn in
    let lexbuf = Lexing.from_channel instream in  
    let parsetree = Parse.main Lex.token lexbuf in
    close_in instream;
    print_endline "Parsing succeeded!";
    let stms = St.verify parsetree in
    print_endline (St.statements2string stms);
    let binary = Output.encodeStatements (translate stms) in
    (* HACK HACK HACK *)
    let outputFile = fn ^ ".bin" in
    let outstream = open_out_bin outputFile in
      List.iter (output_byte outstream) binary;
      close_out outstream;
  with
      Sys_error a -> (Printf.eprintf "Could not read file: %s\n" a;
		      exit 1)
;;



let _ =
  let fn = Sys.argv.(1) in
  compileFile fn;
;;
