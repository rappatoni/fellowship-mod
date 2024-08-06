(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
*)

open Format
open Lexing
open Core
open Interpreter
open Interpreter_proof_terms
open Tactics
open Instructions
open Print
open Help

(*Global variables. Use only under adult supervision. For more, see core.ml
and print.ml*)
let prompt () = printf "\n%s @?" prompts (* '@?' flushes the p-printer *)

(*Name of the source file *)
let ifile = ref ""

let set_file ref str = 
  ref := str ;
  keep_ofile := true

(*Compiler options (shown with option --help)*)
let options = 
  [("-t",Arg.Set toplvl,
    " Launch the toplevel");
   ("-c",Arg.Clear toplvl,
    " Launch the compiler");
   ("-o", Arg.String (set_file ofile_pfx), 
    "<file> Keep the external proof traces into <file>.<prover extension> ");
   ("-ascii", Arg.Set ascii,
    " Output symbols using plain ascii (default is unicode)")
  ]
  
let usage = "usage: fsp [ -t | [-c file.fsp] ] [option]"
	      
(*The script evaluator.*)
let eval_script = function
  | Instruction i -> 
      let nc = jack_instruction i !cairn in
      cairn := nc ; 
      echo (pretty_cairn nc)
  | Tactical t ->
      let nc = jack_tactical t !cairn in
      cairn := nc ; 
      echo (pretty_cairn nc)
  | Help h -> 
      jack_help h
  
(*Toplevel*)
let toplevel () = 
  (*If unspecified, the output file name default to "script"*)
  if !ofile_pfx="" then ofile_pfx := "script" ; 
  echo (fun frm ->
    fprintf frm "Welcome! This is FSP version 0.1.0.@\n" ;
    help_hint frm ;
    fprintf frm "%s@\n" (new instruction_msg Logic_switch)#to_string ) ;
  (*Iterate dot by dot*)
  try 
    while true do
      prompt();
      let buf = Lexing.from_channel stdin in
	(try 
	   let ast = Parser.main Lexer.nexttoken buf in eval_script ast
	 with
	     Lexer.Lexing_error s -> 
	       (* Lexical error *)
	       echo (fun frm -> 
	        localization (Lexing.lexeme_start_p buf) frm ;
		fprintf frm "Syntax error: %s@\n" s ;
		help_hint frm )
		 
	   | Parsing.Parse_error -> 
	       (* Syntactic error *)
	       echo (fun frm ->
		localization (Lexing.lexeme_start_p buf) frm ;
		fprintf frm "Parse error@\n" ;
                help_hint frm )
	);
    done;
  with Failure "end" -> 
    echo (pretty_string "EOF!")
      
(*Compiler*)
let compiler () = 
  (*Check the input file was provided*)
  if !ifile="" then (eprintf "./fsp: no input file.@."; 
		     Arg.usage (Arg.align options) usage; exit 1); 
  (*Check the input file has the right extension*)
  if not (Filename.check_suffix !ifile ".fsp") then 
    (eprintf "./fsp: the input file does not have the .fsp extension@.";
     Arg.usage options usage; exit 1); 
  (*If unspecified, the output file name is derived from the input*)
  if !ofile_pfx="" then ofile_pfx := (Filename.chop_suffix !ifile ".fsp");
  
  (*Open the input and output files*)
  let f = open_in !ifile in
    (*Create a lexing buffer*)
  let buf = Lexing.from_channel f in
    echo (fun frm -> fprintf frm "%s@\n" 
      (new instruction_msg Logic_switch)#to_string) ;
    try
      while true do
	(* Parsing: the Parser.prog function transforms the lexical buffer into
	   an abstract syntax tree. *)
	let ast = Parser.main Lexer.nexttoken buf in
	  (eval_script ast ; if isExn !cairn then exit (-1))
      done
    with
	Lexer.Lexing_error s -> 
	  (* Lexical error *)
	  localization (Lexing.lexeme_start_p buf) err_formatter ;
	  eprintf "Parse error: %s@." s ;
          exit (-1)
	    
      | Parsing.Parse_error -> 
	  (* Syntactic error *)
	  localization (Lexing.lexeme_start_p buf) err_formatter ;
	  eprintf "Parse error: syntactical analysis.@." ;
          exit (-1)
      | Failure "end" -> 
          echo (pretty_string "EOF!\n") ;
	  if isOngoing !cairn then exit (-1)
	  else exit 0

(*Go! If something breaks you get to keep both parts...*)
let _ =   
  (*Parse the command line... *)
  Arg.parse (Arg.align options) (set_file ifile) usage;
  (* ...and launch the appropriate program*)
  if !toplvl 
  then toplevel()
  else compiler()

