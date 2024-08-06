open Ast
open Print
open Format

(*Output to prover*)
(*Miscellaneous*)
let set = "set"
	    
(*The specification language*)
(*Terms*)
type prover_terms = 
    {
      pVar: provers -> string -> string;
      pFun: provers -> string -> string -> string
    }

let gen_term = 
  {
    pVar = (function 
	      | Coq -> sprintf "%s"
	      | Pvs -> sprintf "%s" );
    pFun = (function 
	      | Coq -> sprintf "(%s %s)"
	      | Pvs -> sprintf "%s%s")
  }

let rec gen_term_rec term prover = match term with
  | Var(name) -> (gen_term.pVar prover name)
  | Fun(name, arity, term_list) -> (gen_term.pFun prover name (gen_term_list term_list prover))

and gen_term_list list prover = 
  match list with
    | [] -> ""
    | [t] -> gen_term_rec t prover
    | t::tl -> 
	let arg = match prover with 
	   | Coq -> (sprintf "%s " (gen_term_rec t prover))
	   | Pvs -> (sprintf "(%s)" (gen_term_rec t prover)) 
	in
	  arg ^ (gen_term_list tl prover)

let rec gen_def_list list prover = 
  match list with
    | [] -> ""
    | [d] -> sprintf "%s" (fst d)
    | d::dl -> (match prover with 
		  | Coq -> (sprintf "%s %s" (fst d) (gen_def_list dl prover))
		  | Pvs -> (sprintf "%s, %s" (fst d) (gen_def_list dl prover)) )
	
let typify prover defined_term category =
  let rec arrows arity = 
    match arity with
      | 0 -> 
	  if category = "fun" then sprintf "%s" set
	  else 
	    (match prover with 
	       | Coq -> sprintf "Prop" 
	       | Pvs -> sprintf "bool")
      | n -> (match prover with 
		| Coq -> sprintf "(%s -> %s)" set (arrows (n-1))
		| Pvs -> sprintf "[%s -> %s]" set (arrows (n-1)) )
  in arrows (snd defined_term)
	
(*Propositions*)
type prover_props = 
    {
      pTrue: provers -> string;
      pFalse: provers -> string;
      pPred: provers -> string -> string -> string;
      pUn: provers -> unop -> string -> string;
      pBin: provers -> binop -> string -> string -> string;
      pQuant: provers -> quantifier -> string -> string -> string
    }
    
let gen_prop =
  {
    pTrue = (function 
	       | Coq -> sprintf "True"
	       | Pvs -> sprintf "TRUE");
    pFalse = (function 
		| Coq -> sprintf "False"
		| Pvs -> sprintf "FALSE");
    pPred = (function 
	       | Coq -> sprintf "(%s %s)"
	       | Pvs -> sprintf "%s%s");
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
		| Coq -> (function Exists -> sprintf "(exists %s, %s)"
			    | Forall -> sprintf "(forall %s, %s)" )
		| Pvs -> (function Exists -> sprintf "(EXISTS (%s): %s)"
			    | Forall -> sprintf "(FORALL (%s): %s)" ) )
  }

let rec gen_prop_rec prop prover = match prop with 
  | True -> (gen_prop.pTrue prover)
  | False -> (gen_prop.pFalse prover)
  | Pred(name, arity, term_list) -> (gen_prop.pPred prover name (gen_term_list term_list prover))
  | UProp(Neg,p) -> (gen_prop.pUn prover Neg (gen_prop_rec p prover))
  | BProp(p1,op,p2) -> (gen_prop.pBin prover op (gen_prop_rec p1 prover) (gen_prop_rec p2 prover))
  | Quant(q, name, prop) -> (gen_prop.pQuant prover q name (gen_prop_rec prop prover))

(*The commands*)
type prover_commands = 
    {
      startfile: provers -> string ;
      endfile: provers -> string ;
      startproof: provers -> string -> string -> string ;
      endproof: provers -> string ;
      vars: provers -> string -> string -> string -> string; 
      tac: provers -> string -> string -> string ;
    }

(*forge: quel est l'equivalent de firstorder en PVS ? s/tauto/firstorder/*)
let gen_command =
  {
    startfile = (function 
		   | Coq -> 
		       if !lj then sprintf "Parameter set : Set.@."
		       else sprintf "Require Import Classical. @.Parameter set : Set. @."
		   | Pvs -> sprintf "tmp: THEORY @.BEGIN @. set: TYPE @.");
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
		 if !lj then sprintf " cut %s. @. tauto. @.%s"
		 else sprintf " cut %s. @. apply NNPP. @. tauto. @.%s"
	     | Pvs ->  sprintf " %%|- (BRANCH (CASE %s) @. %%|- ((PROP) @.%s %%|- )) @.") 
  }

(*File management*)
type output_handle = {chl: out_channel ;
		      frm: formatter}

let create_ohandle ofile = 
  try 
    let channel = open_out ofile in
    let formatter = formatter_of_out_channel channel in
      {chl=channel ; frm=formatter}
  with Sys_error s -> (eprintf "Fatal I/O : %s @." s) ; exit 1

(*The interpreter / launcher*)
let call_prover p = 
  match p with
    | Coq -> "$COQBIN/coqtop -lv "
    | Pvs -> "/home/fkirchner/usr/local/pvs/lib/ProofLite/proveit "
	(*"/home/fkirchner/usr/local/pvs/pvs -batch -v 3 -l " *)

let gen_file prover ohandle trace = 
  let rec make_str prover trace = 
    match trace with 
      | [] -> ""
      | hd::tl -> 
	  (match hd with
	     | T_deffun (def_list) -> 
		 (List.fold_right 
		    (fun x -> (gen_command.vars 
				 prover 
				 (fst x) 
				 (typify prover x "fun") ))
		    def_list
		    (make_str prover tl) )
	     | T_defpred (def_list) -> 
		 (List.fold_right 
		    (fun x -> (gen_command.vars 
				 prover 
				 (fst x) 
				 (typify prover x "pred") ))
		    def_list
		    (make_str prover tl) )
	     | T_tac (s) ->
		 let p = prop_of_goallist (get_goallist s) in
		   if p = True then
		     ((gen_command.tac prover
			 (gen_prop_rec p prover)
			 ""))
		   else
		     (gen_command.tac prover
			(gen_prop_rec p prover)
			(make_str prover tl))
	     | T_goal (s) -> 
		 let p = prop_of_goallist (get_goallist s) in
		   (gen_command.startproof prover 
		      (gen_prop_rec p prover) 
		      (make_str prover tl) )
		   ^
		   (gen_command.endproof prover)
	  )
  in pp_print_string ohandle.frm (make_str prover trace)
(*forge : ne travailler que sur des formes prenexes + skolemizées asap ?*)
(*forge : appliquer des lemmes au lieu de cut ?*)

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
	  
