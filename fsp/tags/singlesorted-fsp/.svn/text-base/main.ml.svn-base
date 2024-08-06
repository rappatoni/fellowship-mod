
open Format
open Lexing
open Ast
open Interpreter
open Commands
open Print

(*Run the compiler or the toplevel*)
let toplvl = ref true
let oki = ref false (* did the user set the flag on the command-line ? *)
let prompt () = printf "\nfsp < @?" (* '@?' flushes the p-printer *)

let set_top ref bool () = 
  ref := bool ;
  oki := true

(*Option : only parse the input file *)
let parse_only = ref false
		   
(*Option : keep the output trace *)
let keep_trace = ref false
		   
(*Name of the source and target files *)
let ifile = ref ""
let ofile_pfx = ref "" (*the output files prefix*)

let set_file ref str = 
  ref := str ;
  keep_trace := true

(*Compiler options (shown with option --help)*)
let options = 
  [("-t",Arg.Unit (set_top toplvl true),
    " Launch the toplevel");
   ("-c",Arg.Unit (set_top toplvl false),
    " Launch the compiler");
   ("-lj", Arg.Set lj,
    " Enable constructive logic (default)");
   ("-lk", Arg.Clear lj,
    " Enable classical logic");
   ("-p",Arg.Set parse_only, 
    " Perform only syntactic analysis (parsing)");
   ("-o", Arg.String (set_file ofile_pfx), 
    "<file> Keep the external proof traces into <file>.<prover extension> ")
  ]
  
let usage = "usage: fsp [ -t | [-c file.fsp] ] [option]"
	      
(*Error localization using line and column info*)
let localization pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
    (eprintf " line %d, character %d:\n" l c) 
    
(*References for the proof state, and the proof trace*)
let cairn = ref (create_cairn ()) 
let trace = ref []

(*Applying a command*)
let apply_command ast = 
  match ast with
    | Deffun(a) -> let nc = (define_terms (funify a) !cairn) in 
	cairn := nc ;
	trace := !trace @ [T_deffun(a)] ;
	echo (sprintf "%s defined." (pretty_symbols a) )
    | Defpred(a) -> let nc = (define_preds (predify a) !cairn) in 
	cairn := nc ;
	trace := !trace @ [T_defpred(a)] ;
	echo (sprintf "%s defined." (pretty_symbols a) )
    | Gl(a) -> let nc = (goal a !cairn) in 
	cairn := nc ;
	if not (isExn nc) then trace := !trace @ [T_goal(get_state nc)] ;
	echo (pretty_cairn nc)
    | Tac(a) -> let nc = (tac a !cairn) in
	cairn := nc ;
	if not (isExn nc) then 
	  trace := !trace @ [T_tac(get_state nc)] ; 
	echo (pretty_cairn nc)
    | Ack(a) -> 
	if isCptScs !cairn then
	  (echo ("Firing up " ^ (pretty_prover a) ^ " for confirmation.\n");
	   launch_prover a !trace !ofile_pfx !keep_trace)
	else
	  echo "You need to complete the proof before checking it out."
    | Disc -> let nc = create_cairn () in 
	cairn := nc ;
	trace := [] ;
	echo "Pulling out."
    | Quit -> 
	echo ("Out." ^ nl);
	exit 0
	  
(*Toplevel*)
let toplevel () = 
  (*If unspecified, the output file name default to "script"*)
  if !ofile_pfx="" then ofile_pfx := "script" ; 
  (*Iterate dot by dot*)
  try 
    while true do
      prompt();
      let buf = Lexing.from_channel stdin in
	(try 
	   let p = Parser.prog Lexer.nexttoken buf in
	     (apply_command p);
	 with
	     Lexer.Lexing_error s -> 
	       (* Lexical error *)
	       localization (Lexing.lexeme_start_p buf);
	       eprintf "Parse error: %s@." s
		 
	   | Parsing.Parse_error -> 
	       (* Syntactic error *)
	       localization (Lexing.lexeme_start_p buf);
	       eprintf "Parse error: syntactical analysis.@."
	);
    done;
  with Failure "end" -> 
    echo "EOF!"
      
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
    try
      while true do
	(* Parsing: the Parser.prog function transforms the lexical buffer into
	   an abstract syntax tree. *)
	let p = Parser.prog Lexer.nexttoken buf in
	  close_in f;
	  
	  (* Parsing only style *)
	  if not !parse_only 
	  then (apply_command p);
      done
    with
	Lexer.Lexing_error s -> 
	  (* Lexical error *)
	  localization (Lexing.lexeme_start_p buf);
	  eprintf "Parse error: %s@." s
	    
      | Parsing.Parse_error -> 
	  (* Syntactic error *)
	  localization (Lexing.lexeme_start_p buf);
	  eprintf "Parse error: syntactical analysis.@."

(*Go !*)
let _ =   
  (*Parse the command line... *)
  Arg.parse (Arg.align options) (set_file ifile) usage;
  if not !oki then
    Arg.usage (Arg.align options) usage
  else
  (* ...and launch the appropriate program*)
  if !toplvl 
  then toplevel()
  else compiler()

