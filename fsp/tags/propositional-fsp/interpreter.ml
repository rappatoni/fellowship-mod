open Ast
open Print
open Format

(*Output to prover*)
let rec gen_var_list list prover = 
  match list with
    | [] -> ""
    | [s] -> s
    | s::sl -> 
	(match prover with 
	   | Coq -> s ^ " " 
	   | Pvs -> s ^ ", " )
	^ (gen_var_list sl prover)

type prover_terms = 
    {
      ptrue: provers -> string ;
      pfalse: provers -> string;
      pUn: provers -> unop -> string -> string;
      pBin: provers -> binop -> string -> string -> string;
    }

let gen_term =
  {
    ptrue = (function Coq -> "True"
	       | Pvs -> "TRUE");
    pfalse = (function Coq -> "False"
		| Pvs -> "FALSE");
    pUn = (function Coq -> (function Neg -> (fun str -> "~" ^ str))
	     | Pvs -> (function Neg -> (fun str -> "NOT " ^ str)));
    pBin = (function Coq -> (function Imp -> (fun str1 str2 -> "("^str1^" -> "^str2^")")
			       | Conj -> (fun str1 str2 -> "("^str1^" /\\ "^str2^")")
			       | Disj -> (fun str1 str2 -> "("^str1^" \\/ "^str2^")") )
	      | Pvs ->(function Imp -> (fun str1 str2 -> "("^str1^" => "^str2^")")
			 | Conj -> (fun str1 str2 -> "("^str1^" AND "^str2^")")
			 | Disj -> (fun str1 str2 -> "("^str1^" OR "^str2^")") )
	   )
  }

let rec gen_prop prop prover = match prop with 
  | Const s -> s
  | True -> (gen_term.ptrue prover)
  | False -> (gen_term.pfalse prover)
  | UProp(Neg,p) -> (gen_term.pUn prover Neg (gen_prop p prover))
  | BProp(p1,op,p2) -> (gen_term.pBin prover op (gen_prop p1 prover) (gen_prop p2 prover))

(*Prover specifics*)
type prover_commands = 
    {
      startfile: provers -> string ;
      endfile: provers -> string ;
      startgoal: provers -> string -> string -> string ;
      endgoal: provers -> string ;
      vars: provers -> string -> string -> string; 
      tac: provers -> string -> string -> string ;
    }

let gen_command =
  {
    startfile = (function 
		   | Coq -> 
		       if !lj then sprintf ""
		       else sprintf "Require Import Classical. @."
		   | Pvs -> sprintf "tmp: THEORY @.BEGIN @.");
    endfile = (function 
		 | Coq -> sprintf ""
		 | Pvs -> sprintf "END tmp @.");
    startgoal = (function 
		   | Coq -> sprintf "Goal %s. @. Proof. @.%s" 
		   | Pvs -> sprintf " Thm : THEOREM %s @. %%|- Thm : PROOF @.%s");
    endgoal = (function 
		 | Coq -> sprintf " trivial. @.Qed. @."
		 | Pvs -> sprintf " %%|- QED @.");
    vars = (function 
	      | Coq -> sprintf "Variable %s : Prop. @.%s"
	      | Pvs -> sprintf " %s : bool @.%s");
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
  with Sys_error s -> (eprintf "Fatal : %s @." s) ; exit 1

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
	     | T_vars (str_list) -> 
		 (gen_command.vars prover) 
		 (gen_var_list str_list prover) 
		 (make_str prover tl)
	     | T_tac (s) ->
		 let p = prop_of_goallist (get_goallist s) in
		   if p = True then
		     ((gen_command.tac prover) 
			(gen_prop p prover)
			"") 
		   else
		     (gen_command.tac prover) 
		     (gen_prop p prover)
		     (make_str prover tl)
	     | T_goal (s) -> 
		 let p = prop_of_goallist (get_goallist s) in
		   (gen_command.startgoal prover) 
		   (gen_prop p prover) 
		   (make_str prover tl)
		   ^
		   (gen_command.endgoal prover)
	  )
  in pp_print_string ohandle.frm (make_str prover trace)

let launch_prover prover trace ofile_prefix keep_trace = 
  match prover with 
    | Coq -> 
	let coq_ofile = ofile_prefix ^ ".v" in
	let ohandle = (create_ohandle coq_ofile) in
	  (fprintf ohandle.frm "(* Coq script *) @.";
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
	  (fprintf ohandle.frm "%% Pvs script %% @.";
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
	  
