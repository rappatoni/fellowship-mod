open Format
open Lexing
open Core
open Interpreter
open Interpreter_proof_terms
open Commands
open Print
open Help

(*Run the compiler or the toplevel*)
let toplvl = ref true
let wf_command = ref false (* did the user set the flag on the command-line ? *)
let prompt () = printf "\nfsp < @?" (* '@?' flushes the p-printer *)

let set_top ref bool () = 
  ref := bool ;
  wf_command := true

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
   ("-min", Arg.Set minimal,
    " Disable \"ex falso sequitur quodlibet\" and derived rules");
   ("-full", Arg.Clear minimal,
    " Enable \"ex falso sequitur quodlibet\" and derived rules (default)");
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
  if !toplvl then
    let arrow = sprintf "%s%s" (String.make (5+c) '-') "^" in
      (eprintf "%s@.Line %d, character %d:\n" arrow l c)
  else
      (eprintf "Line %d, character %d:\n" l c)
    
(*References for the proof state, and the proof trace*)
let cairn = ref (create_cairn ()) 
let trace = ref []  (* newest command first *)
let history = ref [!cairn,!trace] (* newest status first *)

let print_current_logic () =
 print_string ("Current logic: " ^ (if !minimal then "minimal " else "") ^
  (if !lj then "intuitionistic " else "classic ") ^ "sequent calculus")

(*Applying a command*)
let apply_command ast = 
  match ast with
    | Vars a -> let nc = (declare_vars a !cairn) in 
	cairn := nc ;
	if not (isExn nc)
	then (trace := (T_vars(a))::!trace ;
	  history := (!cairn,!trace)::!history ;
	  echo (sprintf "%s defined." (pretty_variables a)) )
	else echo (pretty_cairn nc)
    | Gl a ->
        let nc = (goal a !cairn) in 
	cairn := nc ;
	if not (isExn nc) then
         ((trace := (T_goal(get_state nc)):: !trace ;
          history := (!cairn,!trace)::!history) ) ;
	echo (pretty_cairn nc)
    | Tac a -> let nc = (interpret_tac a !cairn) in
	cairn := nc ;
	if not (isExn nc) then 
         (trace := (T_tac(get_state nc))::!trace ; 
          history := (!cairn,!trace)::!history) ;
	echo (pretty_cairn nc)
    | Ack a -> 
	if isCptScs !cairn then
	  (echo (sprintf "Firing up %s for confirmation.@." (pretty_prover a));
	   launch_prover a (List.rev !trace) !ofile_pfx !keep_trace)
	else
	  echo "You need to complete the proof before checking it out."
    | AckProofTerm a -> 
	if isCptScs !cairn then
	  (echo (sprintf "Firing up %s for confirmation.@." (pretty_prover a));
           let s = get_state !cairn in
	    launch_prover_on_proof_term a (List.rev !trace) s.pt !ofile_pfx
             !keep_trace)
	else
	  echo "You need to complete the proof before checking it out."
    | Disc -> let nc = create_cairn () in 
	cairn := nc ;
	trace := [] ;
        history := [] ;
	echo "Pulling out."
    | Next ->
       let nc = next !cairn in
        cairn := nc ;
        if not (isExn nc) then
         (trace := (T_tac(get_state nc))::!trace ; 
          history := (!cairn,!trace)::!history) ;
	echo (pretty_cairn nc)
    | Prev ->
       let nc = prev !cairn in
        cairn := nc ;
        if not (isExn nc) then
         (trace := (T_tac(get_state nc))::!trace ; 
          history := (!cairn,!trace)::!history) ;
	echo (pretty_cairn nc)
    | Undo ->
       (match !history with
           [] -> assert false
         | [_] -> echo "You have not even started! What do you want to undo?"
         | _::(((cairn',trace')::_) as history') ->
             cairn := cairn' ;
             trace := trace' ;
             history := history' ;
	     echo (pretty_cairn !cairn))
    | Lj b -> lj := b ; print_current_logic ()
    | Min b -> minimal := b ; print_current_logic ()
    | Help idopt -> help idopt
    | Quit -> 
	echo ("Out." ^ nl);
	exit 0
	  
(*Toplevel*)
let toplevel () = 
  (*If unspecified, the output file name default to "script"*)
  if !ofile_pfx="" then ofile_pfx := "script" ; 
  (*Iterate dot by dot*)
  Printf.printf "Welcome! This is FSP version 0.0.1.\n";
  help_hint () ;
  print_current_logic () ;
  printf "\n";
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
	       eprintf "Syntax error: %s@." s;
               help_hint ()
		 
	   | Parsing.Parse_error -> 
	       (* Syntactic error *)
	       localization (Lexing.lexeme_start_p buf);
	       eprintf "Parse error@.";
               help_hint ()
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
    print_current_logic () ;
    try
      while true do
	(* Parsing: the Parser.prog function transforms the lexical buffer into
	   an abstract syntax tree. *)
	let p = Parser.prog Lexer.nexttoken buf in
	  
	  (* Parsing only style *)
	  if not !parse_only 
	  then (apply_command p);
          if isExn !cairn then exit (-1)
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
      | Failure "end" -> 
          echo "EOF!"

(*Go !*)
let _ =   
  (*Parse the command line... *)
  Arg.parse (Arg.align options) (set_file ifile) usage;
  if not !wf_command then
    Arg.usage (Arg.align options) usage
  else
  (* ...and launch the appropriate program*)
  if !toplvl 
  then toplevel()
  else compiler()

