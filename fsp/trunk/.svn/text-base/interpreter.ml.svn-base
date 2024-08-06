(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
*)

open Core
open Format

exception NotImplemented

(*Keep the output file?*)
let keep_ofile = ref false
		   
(*Name of the target file*)
let ofile_pfx = ref "" (*the output files prefix*)

(*Definition lists*)
let rec gen_var_list prover = function
  | [] -> ""
  | [s] -> s
  | s::sl -> 
      (match prover with 
	 | Coq -> s ^ " " 
	 | Pvs -> s ^ ", "
         | Isabelle -> (*CSC: ???*) "this_should_never_be_used")
      ^ (gen_var_list prover sl)

(* SPECIFICATIONS *)

(*Sorts*)
type prover_sorts = 
    { pSSet: provers -> string;
      pSProp: provers -> string;
      pSSym: provers -> string -> string;
      pSArr: provers -> string -> string -> string }
    
let gen_sort = 
  { pSSet = (function
      | Coq -> "Set"
      | Pvs -> "TYPE+"
      | Isabelle -> raise NotImplemented);
    pSProp = (function
      | Coq -> "Prop"
      | Pvs -> "bool"
      | Isabelle -> "bool");
    pSSym = (function
      | Coq | Pvs | Isabelle -> sprintf "%s");
    pSArr = (function
      | Coq -> sprintf "(%s -> %s)"
      | Pvs -> sprintf "[%s -> %s]"
      | Isabelle -> sprintf "(%s ==> %s)") }

let rec gen_sort_rec prover = function
  | SSet -> gen_sort.pSSet prover
  | SProp -> gen_sort.pSProp prover
  | SSym name -> gen_sort.pSSym prover name
  | SArr (sort,sort') -> 
      gen_sort.pSArr prover (gen_sort_rec prover sort) (gen_sort_rec prover sort')

(*Terms*)
type prover_terms = 
    { pTSym: provers -> string -> string;
      pTApp: provers -> string -> string -> string }
    
let gen_term = 
  { pTSym = (function 
      | Coq | Pvs -> sprintf "%s"
      | Isabelle -> raise NotImplemented);
    pTApp = (function 
      | Coq -> sprintf "(%s %s)"
      | Pvs -> sprintf "%s(%s)"
      | Isabelle -> raise NotImplemented) }
  
let rec gen_term_rec prover = function
  | TSym name -> gen_term.pTSym prover name
  | TApp (term, term') -> 
      gen_term.pTApp prover (gen_term_rec prover term) (gen_term_rec prover term')
      
(*Propositions*)
type prover_props = 
    { pTrue: provers -> string;
      pFalse: provers -> string;
      pPSym: provers -> string -> string;
      pPApp: provers -> string -> string -> string;
      pUn: provers -> unop -> string -> string;
      pBin: provers -> binop -> string -> string -> string;
      pQuant: provers -> quantifier -> string -> string -> string -> string }
    
let gen_prop =
  { pTrue = (function 
      | Coq -> sprintf "True"
      | Pvs -> sprintf "TRUE"
      | Isabelle -> raise NotImplemented);
    pFalse = (function 
      | Coq -> sprintf "False"
      | Pvs -> sprintf "FALSE"
      | Isabelle -> raise NotImplemented);
    pPSym = (function 
      | Coq | Pvs | Isabelle -> sprintf "%s");
    pPApp = (function 
      | Coq -> sprintf "(%s %s)"
      | Pvs -> sprintf "%s(%s)"
      | Isabelle -> raise NotImplemented);
    pUn = (function 
      | Coq -> (function Neg -> sprintf "~%s")
      | Pvs -> (function Neg -> sprintf "NOT %s")
      | Isabelle -> raise NotImplemented);
    pBin = (function 
      | Coq -> (function Imp -> sprintf "(%s -> %s)"
		  | Minus -> (fun str1 str2 -> "("^str1^" /\\ ~"^str2^")")
		  | Conj -> sprintf "(%s /\\  %s)"
		  | Disj -> sprintf "(%s \\/ %s)")
      | Pvs -> (function Imp -> sprintf "(%s => %s)"
		  | Minus -> (fun str1 str2 -> "("^str1^" AND NOT "^str2^")")
		  | Conj -> sprintf "(%s AND %s)"
		  | Disj -> sprintf "(%s OR %s)" )
      | Isabelle -> (function Imp -> sprintf "(%s --> %s)"
                      | _ -> raise NotImplemented));
    pQuant = (function 
      | Coq -> (function Exists -> sprintf "(exists %s:%s, %s)"
		  | Forall -> sprintf "(forall %s:%s, %s)" )
      | Pvs -> (function Exists -> sprintf "(EXISTS (%s:%s): %s)"
		  | Forall -> sprintf "(FORALL (%s:%s): %s)" )
      | Isabelle -> raise NotImplemented) }

let gen_prop_rec prover =
 let rec aux prover = function
  | True -> gen_prop.pTrue prover
  | False -> gen_prop.pFalse prover
  | PSym name -> gen_prop.pPSym prover name
  | PApp (prop,term) -> 
      gen_prop.pPApp prover (aux prover prop) (gen_term_rec prover term)
  | UProp (Neg,prop) -> 
      gen_prop.pUn prover Neg (aux prover prop)
  | BProp (prop,op,prop') -> 
      gen_prop.pBin prover op (aux prover prop) (aux prover prop')
  | Quant (q, (names,sort), prop) -> 
      gen_prop.pQuant prover q (gen_var_list prover names) (gen_sort_rec prover sort) (aux prover prop)
 in
  match prover with
     Coq | Pvs -> aux prover
   | Isabelle -> (function prop -> "\"" ^ aux Isabelle prop ^ "\"")

(**The commands**)

type prover_commands = 
    { startfile: string -> provers -> string;
      endfile: provers -> string;
      startproof: provers -> string -> string -> string -> string;
      endproof: provers -> string -> string;
      declare_vars: provers -> string -> string -> string -> string; 
      declare_axiom: provers -> string -> string -> string -> string;
      tac: provers -> string -> string -> string -> string }

let gen_command =
  { startfile = (fun theoryname prover -> match prover with
      | Coq -> 
	 if !lj then sprintf ""
	 else sprintf "Require Import Classical. @.Implicit Arguments classic [P]. @."
      | Pvs -> sprintf "tmp: THEORY @.BEGIN @."
      | Isabelle -> sprintf "theory %s = Main :@." theoryname);
    endfile = (function 
      | Coq -> sprintf ""
      | Pvs -> sprintf "END tmp @."
      | Isabelle -> sprintf "end@.");
    startproof = (function 
      | Coq -> sprintf "Theorem %s : %s. @. Proof. @.%s" 
      | Pvs -> (fun name prop continuation -> 
	sprintf " %s : THEOREM %s @. %%|- %s : PROOF @. %%|- (THEN %s %%|- (GRIND))@."
	  name prop name continuation)
      | Isabelle -> raise NotImplemented );
    endproof = (function 
      | Coq -> sprintf " firstorder. @.Qed. @.%s"
      | Pvs -> sprintf " %%|- QED @.%s"
      | Isabelle -> raise NotImplemented);
    declare_vars = (function 
      | Coq -> sprintf "Parameter %s : %s. @.%s"
      | Pvs -> sprintf " %s : %s @.%s"
      | Isabelle -> (*CSC: ??? *) (fun _ _ rest -> rest));
    declare_axiom = (function
      | Coq -> sprintf "Axiom %s : %s. @.%s"
      | Pvs -> sprintf " %s : AXIOM %s @.%s"
      | Isabelle -> raise NotImplemented);
    tac = (function 
      | Coq -> 
	 if !lj then sprintf " (*%s*)@. cut %s. @. firstorder. @.%s"
	 else sprintf " (*%s*)@. cut %s. @. apply NNPP. @. firstorder. @.%s"
      | Pvs ->  sprintf " %% %s @. %%|- (BRANCH (CASE \"%s\") @. %%|- ((TRY (TRY (MODEL-CHECK) (FAIL) (SKIP)) (FAIL) (GRIND)) @.%s %%|- )) @."
      | Isabelle -> raise NotImplemented) }

(*File management*)
type output_handle = {chl: out_channel ;
		      frm: formatter}

let create_ohandle ofile = 
  try 
    let channel = open_out ofile in
    let formatter = formatter_of_out_channel channel in
      {chl=channel ; frm=formatter}
  with Sys_error s -> (eprintf "Fatal : %s @." s) ; exit 1

(*The interpreter / launcher*)
let call_prover p = 
  match p with
    | Coq -> "$COQBIN/coqtop -lv "
    | Pvs -> (*"/home/flo/pvs/packages/ProofLite.1d/lib/proveit "*)
	"$PVSBIN/proveit "
	(*"/home/fkirchner/usr/local/pvs/pvs -batch -v 3 -l " *)
    | Isabelle -> "isatool usedir -b HOL "

let gen_file prover ohandle trace = 
  let rec make_str prover trace = 
    match trace with 
      | [] -> ""
      | T_vars (names,sort) :: tl -> 
	let var = gen_var_list prover names
	and typ = gen_sort_rec prover sort in
	gen_command.declare_vars prover var typ (make_str prover tl)
      | T_axiom (name,prop) :: tl ->
	let prp = gen_prop_rec prover prop in
	gen_command.declare_axiom prover name prp (make_str prover tl)
      | T_tac (s,name) :: tl ->
	if s.goals = [] then (make_str prover tl)
	else (gen_command.tac prover name
	  (gen_prop_rec prover (prop_of_state s))
	  (make_str prover tl) )
      | T_goal (s,name) :: tl -> 
	let prp = prop_of_state s in
	(gen_command.startproof prover name
	  (gen_prop_rec prover prp) 
	  (make_str prover tl) )
      | (T_qed _) :: tl ->
	  (gen_command.endproof prover (make_str prover tl))
  in pp_print_string ohandle.frm (make_str prover trace)

let launch_prover0 prover ofile_prefix keep_ofile input file_generator =
  match prover with 
    | Isabelle ->
        let dir_prefix = Filename.dirname ofile_prefix in
        let rootml = Filename.concat dir_prefix "ROOT.ML" in
        let rootfh = open_out rootml in
        output_string rootfh ("use_thy \"" ^ ofile_prefix ^ "\";\n");
        close_out rootfh;
	let isabelle_ofile = ofile_prefix ^ ".thy" in
	let ohandle = (create_ohandle isabelle_ofile) in
	  (pp_print_string ohandle.frm "(* Isabelle script *)\n";
	   pp_print_string ohandle.frm
            (gen_command.startfile (Filename.basename ofile_prefix) Isabelle);
	   file_generator Isabelle ohandle input ;
	   pp_print_string ohandle.frm (gen_command.endfile Isabelle); 
	   flush_all ();
	   let evalue =
	    let exit =
	     Sys.command
               ("xterm -e " ^ "/bin/bash -c '(cd " ^ dir_prefix ^ " && " ^
               call_prover Isabelle ^ Filename.basename ofile_prefix ^
               " || touch .error) && echo \"Press ENTER to exit.\" && read'") in
            let errorfile = Filename.concat dir_prefix ".error" in
	    (try ignore (Unix.stat errorfile) ; Unix.unlink errorfile ; (-1)
	     with _ -> exit) in
	   if evalue <> 0 then
	    (eprintf "External prover failure"; exit evalue) ;
	   close_out ohandle.chl ; 
           Unix.unlink rootml;
	   if not keep_ofile then Sys.remove isabelle_ofile 
	  )
    | Coq -> 
	let coq_ofile = ofile_prefix ^ ".v" in
	let ohandle = (create_ohandle coq_ofile) in
	  (fprintf ohandle.frm "(* Coq script *) @.";
	   pp_print_string ohandle.frm (gen_command.startfile ofile_prefix Coq);
	   file_generator Coq ohandle input ;
	   pp_print_string ohandle.frm (gen_command.endfile Coq); 
	   flush_all ();
	   let evalue =
	    let exit =
	     Sys.command ("xterm -e \"" ^ (call_prover Coq) ^ coq_ofile ^
			  " || touch .error\"")  in
	    (try ignore (Unix.stat ".error") ; Unix.unlink ".error" ; (-1)
	     with _ -> exit) in
	   if evalue <> 0 then
	    (eprintf "External prover failure"; exit evalue) ;
	   close_out ohandle.chl ; 
	   if not keep_ofile then Sys.remove coq_ofile 
	  )
    | Pvs -> 
	let pvs_ofile = ofile_prefix ^ ".pvs" in
	let ohandle = (create_ohandle pvs_ofile) in
	  (fprintf ohandle.frm "%% Pvs script %% @.";
	   pp_print_string ohandle.frm (gen_command.startfile ofile_prefix Pvs);
	   file_generator Pvs ohandle input ;
	   pp_print_string ohandle.frm (gen_command.endfile Pvs);
	   flush_all ();
	   ignore (Sys.command ("xterm -e " ^"\""^ (call_prover Pvs) ^ pvs_ofile 
				^" ; cat "^ofile_prefix^".out"
				^" ; echo Press enter to close this window "
				^" ; read "
				^"\"")) ;
	   close_out ohandle.chl ; 
	   if Sys.file_exists (ofile_prefix ^ ".bin") then Sys.remove (ofile_prefix ^ ".bin") ; 
	   if Sys.file_exists (ofile_prefix ^ ".prf") then Sys.remove (ofile_prefix ^ ".prf") ; 
	   if Sys.file_exists (ofile_prefix ^ ".out") then Sys.remove (ofile_prefix ^ ".out") ;
	   if (not keep_ofile) && (Sys.file_exists pvs_ofile) then Sys.remove pvs_ofile 
	  )
	  
let launch_prover prover trace ofile_prefix keep_ofile = 
  launch_prover0 prover ofile_prefix keep_ofile trace gen_file
