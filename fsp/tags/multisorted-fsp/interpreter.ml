(*Output to prover*)

open Core
open Print
open Format

(*Trace elements*)
type trace_atoms =
  | T_define of (name list*sort)
  | T_goal of state
  | T_tac of state

(*Definition lists*)
let rec gen_def_list prover = function
  | [] -> ""
  | [d] -> sprintf "%s" d
  | d::dl -> (match prover with 
		| Coq -> (sprintf "%s %s" d (gen_def_list prover dl))
		| Pvs -> (sprintf "%s, %s" d (gen_def_list prover dl)) )
      

(**The specification language**)

(*Sorts*)
type prover_sorts = 
    { pSSet: provers -> string;
      pSProp: provers -> string;
      pSSym: provers -> string -> string;
      pSArr: provers -> string -> string -> string }
    
let gen_sort = 
  { pSSet = (function
	       | Coq -> "Set"
	       | Pvs -> "TYPE");
    pSProp = (function
		| Coq -> "Prop"
		| Pvs -> "bool");
    pSSym = (function
	       | Coq | Pvs -> sprintf "%s");
    pSArr = (function
	       | Coq -> sprintf "(%s -> %s)"
	       | Pvs -> sprintf "[%s -> %s]") }

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
	       | Coq | Pvs -> sprintf "%s");
    pTApp = (function 
	       | Coq -> sprintf "(%s %s)"
	       | Pvs -> sprintf "%s(%s)") }
  
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
	       | Pvs -> sprintf "TRUE");
    pFalse = (function 
		| Coq -> sprintf "False"
		| Pvs -> sprintf "FALSE");
    pPSym = (function 
	       | Coq | Pvs -> sprintf "%s");
    pPApp = (function 
	       | Coq -> sprintf "(%s %s)"
	       | Pvs -> sprintf "%s(%s)");
    pUn = (function 
	     | Coq -> (function Neg -> sprintf "~%s")
	     | Pvs -> (function Neg -> sprintf "NOT %s") );
    pBin = (function 
	      | Coq -> (function Imp -> sprintf "(%s -> %s)"
			  | Conj -> sprintf "(%s /\\  %s)"
			  | Disj -> sprintf "(%s \\/ %s)")
	      | Pvs -> (function Imp -> sprintf "(%s => %s)"
			  | Conj -> sprintf "(%s AND %s)"
			  | Disj -> sprintf "(%s OR %s)" ) );
    pQuant = (function 
		| Coq -> (function Exists -> sprintf "(exists %s:%s, %s)"
			    | Forall -> sprintf "(forall %s:%s, %s)" )
		| Pvs -> (function Exists -> sprintf "(EXISTS (%s:%s): %s)"
			    | Forall -> sprintf "(FORALL (%s:%s): %s)" ) ) }

let rec gen_prop_rec prover = function
  | True -> gen_prop.pTrue prover
  | False -> gen_prop.pFalse prover
  | PSym name -> gen_prop.pPSym prover name
  | PApp (prop,term) -> 
      gen_prop.pPApp prover (gen_prop_rec prover prop) (gen_term_rec prover term)
  | UProp (Neg,prop) -> 
      gen_prop.pUn prover Neg (gen_prop_rec prover prop)
  | BProp (prop,op,prop') -> 
      gen_prop.pBin prover op (gen_prop_rec prover prop) (gen_prop_rec prover prop')
  | Quant (q, (names,sort), prop) -> 
      gen_prop.pQuant prover q (gen_def_list prover names) (gen_sort_rec prover sort) (gen_prop_rec prover prop)

(**The commands**)

type prover_commands = 
    { startfile: provers -> string ;
      endfile: provers -> string ;
      startproof: provers -> string -> string -> string ;
      endproof: provers -> string ;
      vars: provers -> string -> string -> string -> string; 
      tac: provers -> string -> string -> string }

(*forge: s/tauto/firstorder/   s/(PROP)/(GROUND)*)
let gen_command =
  { startfile = (function 
		   | Coq -> 
		       if !lj then sprintf ""
		       else sprintf "Require Import Classical. @."
		   | Pvs -> sprintf "tmp: THEORY @.BEGIN @.");
    endfile = (function 
		 | Coq -> sprintf ""
		 | Pvs -> sprintf "END tmp @.");
    startproof = (function 
		    | Coq -> sprintf "Goal %s. @. Proof. @.%s" 
		    | Pvs -> sprintf " Thm : THEOREM %s @. %%|- Thm : PROOF @.%s");
    endproof = (function 
		  | Coq -> sprintf " trivial. @.Qed. @."
		  | Pvs -> sprintf " %%|- QED @.");
    vars = (function 
	      | Coq -> sprintf "Variable %s : %s. @.%s"
	      | Pvs -> sprintf " %s : %s @.%s");
    tac = (function 
	     | Coq -> 
		 if !lj then sprintf " cut %s. @. firstorder. @.%s"
		 else sprintf " cut %s. @. apply NNPP. @. firstorder. @.%s"
	     | Pvs ->  sprintf " %%|- (BRANCH (CASE %s) @. %%|- ((GROUND) @.%s %%|- )) @.") }

(*File management*)
type output_handle = {chl: out_channel ;
		      frm: formatter}

let create_ohandle ofile = 
  try 
    let channel = open_out ofile in
    let formatter = formatter_of_out_channel channel in
      {chl=channel ; frm=formatter}
  with Sys_error s -> (eprintf "Fatal I/O error: %s @." s) ; exit 1

(*The interpreter / launcher*)
let call_prover = function
  | Coq -> "$COQBIN/coqtop -lv "
  | Pvs -> "/home/fkirchner/usr/local/pvs/lib/ProofLite/proveit "
      (*"/home/fkirchner/usr/local/pvs/pvs -batch -v 3 -l " *)

let gen_file prover ohandle trace = 
  let rec make_str prover trace = match trace with 
    | [] -> ""
    | hd::tl -> 
	(match hd with
	   | T_define def_list -> 
	       let var = gen_def_list prover (fst def_list)
	       and typ = gen_sort_rec prover (snd def_list)
	       in gen_command.vars prover var typ (make_str prover tl)
	   | T_tac (s) ->
	       let p = prop_of_state s in
		 if p = True then
		   ((gen_command.tac prover (gen_prop_rec prover p) ""))
		 else
		   (gen_command.tac prover (gen_prop_rec prover p) (make_str prover tl))
	   | T_goal (s) -> 
	       let p = prop_of_state s in
		 (gen_command.startproof prover (gen_prop_rec prover p) (make_str prover tl) )
		 ^
		 (gen_command.endproof prover)
	)
  in pp_print_string ohandle.frm (make_str prover trace)
       (*forge : ne travailler que sur des formes prenexes + skolemizées asap
	 ?*)
       (*forge : appliquer des lemmes au lieu de cut ?*)
       (*forge : plutot : impliquer la signature dans la construction, ou
	 encore stocker les variables en hypothese*)

let launch_prover prover trace ofile_prefix keep_trace = 
  match prover with 
    | Coq -> 
	let coq_ofile = ofile_prefix ^ ".v" in
	let ohandle = (create_ohandle coq_ofile) in
	  (fprintf ohandle.frm "(* Coq script *)@.";
	   pp_print_string ohandle.frm (gen_command.startfile Coq);
	   gen_file Coq ohandle trace;
	   pp_print_string ohandle.frm (gen_command.endfile Coq); 
	   flush_all ();
	   ignore (Sys.command ("xterm -e " ^ (call_prover Coq) ^ coq_ofile)) ;
	   close_out ohandle.chl ; 
	   if not keep_trace then Sys.remove coq_ofile 
	  )
    | Pvs -> 
	let pvs_ofile = ofile_prefix ^ ".pvs" in
	let ohandle = (create_ohandle pvs_ofile) in
	  (fprintf ohandle.frm "%% Pvs script %%@.";
	   pp_print_string ohandle.frm (gen_command.startfile Pvs);
	   gen_file Pvs ohandle trace ;
	   pp_print_string ohandle.frm (gen_command.endfile Pvs);
	   flush_all ();
	   ignore (Sys.command ("xterm -e " ^"\""^ (call_prover Pvs) ^ pvs_ofile 
				^" && echo Press control-c to close this window && sleep 10 \"")) ;
	   close_out ohandle.chl ; 
	   Sys.remove (ofile_prefix ^ ".bin") ; 
	   Sys.remove (ofile_prefix ^ ".prf") ; 
	   Sys.remove (ofile_prefix ^ ".out") ;
	   if not keep_trace then Sys.remove pvs_ofile 
	  )
	  
