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
   ("-ugly", Arg.Set ascii,
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
  
(*Applying a command*)
(*
let apply_command ast = 
  match ast with
    | Declare_vars a -> 
	let nc = declare_vars a !cairn in 
	cairn := nc ;
	begin match nc with
	  | Exception (m,_) -> echo (pretty_string m#to_string)
	  | _ -> (trace := (T_vars a)::!trace ;
	    history := (!cairn,!trace)::!history ;
	    echo (fun frm -> fprintf frm "%s defined." (pretty_variables a)) )
	end
    | Declare_axiom (ident,prop) -> 
	let nc = declare_axiom ident prop !cairn in 
	cairn := nc ;
	begin match nc with 
	  | Exception (m,_) -> echo (pretty_string m#to_string)
	  | _ -> (trace := (T_axiom (ident, prop))::!trace ;
	    history := (!cairn,!trace)::!history ;
	    echo (fun frm -> fprintf frm "%s defined." ident) ) 
	end
    | Gl (name,a) ->
        let nc = (goal name a !cairn) in 
	cairn := nc ;
	if not (isExn nc) then
         ((trace := (T_goal (get_state nc,name)):: !trace ;
          history := (!cairn,!trace)::!history) ) ;
	echo (pretty_cairn nc)
    | Tac a -> 
	let nc = (interpret_tac a !cairn) in
	cairn := nc ;
	if not (isExn nc) then 
         (trace := (T_tac (get_state nc, pretty_tactic (fst a)))::!trace ; 
          history := (!cairn,!trace)::!history) ;
	echo (pretty_cairn nc)
    | Qed -> 
	if isCptScs !cairn then 
	  begin match (get_state !cairn) with
	    | {open_thm = Some (name,prop)} as s -> 
		cairn := Subgoals (0, 
		  {s with index = 0 ;
		    open_thm = None ;
		    goals = [] ;
		    thms = (Coll.add name prop s.thms) ;
		    pt = TermMeta initial_meta });
		trace := (T_qed s.pt)::!trace;
		history := (!cairn,!trace)::!history ;
		echo (fun frm -> fprintf frm "Theorem %s saved." name)
	    | _ -> echo (pretty_string "Quid erat demonstrandum?")
	  end
	else 
	  echo (pretty_string "You need to complete the proof.")
    | Ack a -> 
	if not (isOngoing !cairn) then
	  (echo (fun frm -> fprintf frm "Firing up %s for confirmation." (pretty_prover a));
	   launch_prover a (List.rev !trace) !ofile_pfx !keep_ofile)
	else
	  echo (pretty_string "You need to complete the proof before checking the file out.")
    | AckProofTerm a -> 
	if not (isOngoing !cairn) then
	  (echo (fun frm -> fprintf frm "Firing up %s for confirmation." (pretty_prover a));
           let s = get_state !cairn in
	    launch_prover_on_proof_term a (List.rev !trace) !ofile_pfx
             !keep_ofile)
	else
	  echo (pretty_string "You need to complete the proof before checking the file out.")
    | ExportNaturalLanguage ->
       let s = get_state !cairn in
        (* CSC: this is also quite bad: since sign and thms are collections
           there is no easy way to find back the correct order of the
           declarations from the state. For now I am using the trace for that *)
        let print_trace frm =
         List.iter (function
            T_vars (l,s) ->
             pp_print_string frm
              ("Parameter" ^ (if List.length l = 1 then " " else "s ") ^
              String.concat "," l ^ ": " ^
              pretty_sort s);
             pp_print_newline frm ()
          | T_axiom (n,p) ->
             pp_print_string frm ("Axiom " ^ n ^ ": " ^ pretty_prop p);
             pp_print_newline frm ()
          | T_goal _ | T_tac _ -> ()
	  | T_qed pt -> 
	    (* CSC: very bad imperative code, to be removed. See print.ml *)
	     Print.set_state s; 
	     pretty_natural pt frm;
	     pp_print_newline frm () )
          (List.rev !trace)
        in
	let nl_ofile = !ofile_pfx ^ ".nl" in
	let ohandle = create_ohandle nl_ofile in
         print_trace ohandle.frm;
	 flush_all ();
         echo (fun frm -> fprintf frm "File %s written. Enjoy the reading." nl_ofile)
    | Disc_all -> 
	let nc = new_cairn () in 
	cairn := nc ;
	trace := [] ;
        history := [] ;
	echo (pretty_string "Pulling out. Prover reset.")
    | Disc_goal -> 
	let s = (get_state !cairn) in
	let nc = Subgoals (0, {s with open_thm=None ; goals=[]}) in 
	let rec nt t = (match List.rev t with
	  | [] -> t
	  | T_goal _ :: t' -> List.rev t'
	  | _ :: t' -> nt t'
	) in
	cairn := nc ;
	trace := nt !trace ;
        history := (!cairn,!trace)::!history  ;
	echo (pretty_string "Pulling out of current proof.")
    | Next ->
       let nc = next !cairn in
        cairn := nc ;
        if not (isExn nc) then
         (trace := (T_tac(get_state nc,"next"))::!trace ; 
          history := (!cairn,!trace)::!history) ;
	echo (pretty_cairn nc)
    | Prev ->
       let nc = prev !cairn in
        cairn := nc ;
        if not (isExn nc) then
         (trace := (T_tac(get_state nc,"next"))::!trace ; 
          history := (!cairn,!trace)::!history) ;
	echo (pretty_cairn nc)
    | Undo ->
       (match !history with
           [] -> assert false
         | [_] -> echo (pretty_string "You have not even started! What do you want to undo?")
         | _::(((cairn',trace')::_) as history') ->
             cairn := cairn' ;
             trace := trace' ;
             history := history' ;
	     echo (pretty_cairn !cairn))
    | Lj b -> 
	if isOngoing !cairn 
	then echo (fun frm -> 
	  (fprintf frm "Tut tut tut. Don't even think about it.@\n" ;
	   pretty_current_logic frm) )
	else (lj := b ; 
	  echo (pretty_current_logic))
    | Min b -> 
	if isOngoing !cairn 
	then echo (fun frm -> 
	  (fprintf frm "Tut tut tut. Play by the rules!@\n" ;
	   pretty_current_logic frm) )
	else (minimal := b ; 
	  echo (pretty_current_logic) )
    | Help idopt -> 
	help idopt
    | Quit -> 
	echo (pretty_string "Out.") ;
	exit 0
*)	  

(*Toplevel*)
let toplevel () = 
  (*If unspecified, the output file name default to "script"*)
  if !ofile_pfx="" then ofile_pfx := "script" ; 
  echo (fun frm ->
    fprintf frm "Welcome! This is FSP version 0.0.1.@\n" ;
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

