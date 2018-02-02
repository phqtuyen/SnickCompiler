(*this is the main program for snick compiler
call lexer and parser to create an ast for each snick program
then pretty print it*)
(*Team Name - O'Harry the Caml:
 *            - Wilkins J Leong
 *            - Tuyen Quang Pham
 *            - Jorge Ricardo Pardave Garcia *)
open Printexc
module P = Snick_parse
module CG = Codegen
module A = Analyze
module TC = Type_checking
module F = Format
(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]



let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
  (* Open the input file *)
  let infile = match !infile_name with
  | None -> stdin
  | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
  (* Call the parser *)
      try
      let prog = Snick_parse.program Snick_lex.token lexbuf in  
      match !mode with
        | PrettyPrint -> ()
        | Compile -> 
          begin
            (try
              A.alyz_program prog;
              (*A.show_table ();*)
              TC.check_program prog
             with exn -> output_string stderr (Printexc.to_string exn^"\n"); 
             exit 1); 
            CG.generate_program F.std_formatter prog
          end
    with exn -> 
      begin (**error reporting*)
        let curr = lexbuf.Lexing.lex_curr_p in
          let line = curr.Lexing.pos_lnum in
          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lexbuf in
          begin 
          (*Printf.printf "%d\n" curr;*)
          Printf.eprintf "Syntax error may occur in\n"; 
          (*Printf.eprintf "at line %d, character %d\n" line_num char_num;*)
          Printf.eprintf "line %d to line %d\n" (line - 1) (line);
          Printf.eprintf "Current Token %s\n" tok;
          output_string stderr (Printexc.to_string exn^"\n")
        end
      end
    
  
let _ = main ()
