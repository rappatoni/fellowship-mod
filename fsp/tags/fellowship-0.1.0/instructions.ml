(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
*)

open Core
open Format
open Tactics
open Interpreter
open Interpreter_proof_terms
open Print

(* * DECLARATIONS * *)

type instruction =
  | Lj of bool
  | Min of bool
  | Declare 
  | Theorem
  | Deny
  | AntiTheorem
  | Next 
  | Prev 
  | Qed 
  | CheckOut 
  | CheckOutProofTerm 
  | ExportNaturalLanguage
  | Undo
  | DiscardAll
  | DiscardTheorem
  | Quit

let pretty_instruction = function
  | Lj b -> if b then "lj" else "lk"
  | Min b -> if b then "minimal" else "full"
  | Declare  -> "declare"
  | Theorem -> "theorem"
  | Deny -> "deny"
  | AntiTheorem -> "antitheorem"
  | Next  -> "next"
  | Prev  -> "prev"
  | Qed  -> "qed"
  | CheckOut  -> "checkout"
  | CheckOutProofTerm  -> "checkout proof term"
  | ExportNaturalLanguage -> "export natural natural"
  | Undo -> "undo"
  | DiscardAll -> "discard all"
  | DiscardTheorem -> "discard theorem"
  | Quit -> "quit"

type instr_plus_args = instruction * (arg list)

let too_late = ref false (*the flag for logic settings timing*)

(* * MESSAGES * *)

(*Command errors and messages*)
type instruction_error = 
  | Logic_switch 
  | Too_late_logic_switch
  | Defined of string
  | What_qed 
  | Firing_prover of provers
  | NL_written of string
  | Discard_all
  | Discard_theorem
  | Nothing_undo

class instruction_msg ?loc error =
object (self)
  inherit message loc
  val msg = error
  method to_string =
    match msg with
      | Logic_switch ->
	  sprintf "Current logic: %s%ssequent calculus."
	    (if !minimal then "minimal " else "")
	    (if !lj then "intuitionistic " else "classic ")
      | Too_late_logic_switch ->
	  sprintf "Only the first instructions can be used to specify a logical setting. Current logic: %s %s sequent calculus."
	    (if !minimal then "minimal " else "")
	    (if !lj then "intuitionistic " else "classic ")
      | Defined s ->
	  sprintf "%s defined." s
      | What_qed ->
	  "Quid erat demonstrandum?"
      | Firing_prover p ->
	  sprintf "Firing up %s for confirmation." (pretty_prover p)
      | NL_written s ->
	  sprintf "File %s written. Enjoy the reading." s
      | Discard_all ->
	  "Pulling out. Prover reset."
      | Discard_theorem ->
	  "Pulling out: trashing the current proof."
      | Nothing_undo ->
	  "You have not even started! What do you want to undo?"
end


(* * INSTRUCTIONS * *)

let logic_switch (min,intuit) args cairn = match !too_late,args, cairn with 
  | false,[], Idle (_,s) -> 
      minimal := min ; lj := intuit ; 
      Idle (Some (new instruction_msg Logic_switch),s) 
  | false, _, Idle (_,s) -> Idle (Some (new tactic_msg No_args),s)
  | true, _, _ | _, _, _-> Idle (Some (new instruction_msg Too_late_logic_switch),(get_state cairn))

let lj_logic_switch intuit args c = 
  logic_switch (!minimal,intuit) args c

let min_logic_switch min args c =
  logic_switch (min,!lj) args c

let rec declare args cairn = match args, cairn with
  | [Labeled_prop ([name],prop)], Idle (_,s) ->
      begin match prop_infer prop s.sign with
	 | Inl SProp -> 
	    let nc = Idle (Some (new instruction_msg (Defined name)), 
	      {s with thms=Coll.add name prop s.thms} ) in
	    trace := (T_axiom (name,prop))::!trace ;
	    history := (nc,!trace)::!history ;
	    nc
	 | Inl x -> Idle (Some (new type_msg (Prop_kind (prop,x))),s)
	 | Inr m -> Idle (Some (new type_msg m),s)
      end
  | [Labeled_sort (var_list,sort)], Idle (_,s) ->
      let sort_sym = get_sort_symbols sort [] in
      begin let rec sort_check = function
	 | [] -> 
	    begin let rec var_check v_list = match v_list with
	      | [] -> 
		  let nc = Idle (Some (new instruction_msg 
		    (Defined (pretty_comma_list var_list))),
		    {s with sign = add_var_list (var_list,sort) s.sign}) in
		  trace := (T_vars (var_list,sort))::!trace ;
		  history := (nc,!trace)::!history ;
		  nc
	      | name::vl -> if Coll.mem name s.sign
		  then Idle (Some (new tactic_msg (Name_clash [name])), s)
		  else var_check vl
	      in var_check var_list
	    end
	 | x::xl -> 
	     (try if (Coll.find x s.sign = SProp)
	      then Idle (Some (new type_msg (Prop_not_sort x)),s)
	      else sort_check xl
	      with Not_found -> Idle (Some (new type_msg (Undefined x)),s) )
       in sort_check sort_sym
      end
  | _, Idle (_,s) -> Idle (Some (new tactic_msg 
	  (Need_args "either a type and some variables ranging over it, or an axiom and its name")),s)
  | _, _ -> Exception (new tactic_msg Instr_misuse,(get_state cairn))

let deny args cairn = match args, cairn with
  | [Labeled_prop ([name],prop)], Idle (_,s) ->
      begin match prop_infer prop s.sign with
        | Inl SProp ->
            let nc = Idle (Some (new instruction_msg (Defined name)),
                           { s with mox = Coll.add name prop s.mox }) in
            history := (nc,!trace)::!history; nc
        | Inl x -> Idle (Some (new type_msg (Prop_kind (prop,x))), s)
        | Inr m -> Idle (Some (new type_msg m), s)
      end
  | _, Idle (_,s) ->
      Idle (Some (new tactic_msg (Need_args "an axiom name and its body")), s)
  | _, _ -> Exception (new tactic_msg Instr_misuse, (get_state cairn))

let theorem args cairn = match args, cairn with
  | [Labeled_prop ([name],prop)], Idle (_,s) -> 
      begin match prop_infer prop s.sign with
	 | Inl SProp -> 
	    let ns = {s with index=1;
	      open_thm=Some (name,prop,false);
	      goals=[initial_meta,(new_goal prop)];
	      pt=Term (Mu (thesis,prop,Play(TermMeta initial_meta,Concl thesis))) }
	    in
	    let nc = Subgoals (1, ns) in
	    trace := (T_goal (ns,name))::!trace ;
	    history := (nc,!trace)::!history ;
	    nc
	 | Inl x -> Idle (Some (new type_msg (Prop_kind (prop,x))),s)
	 | Inr m -> Idle (Some (new type_msg m),s)
      end
  | _, Idle (_,s) -> Idle (Some (new tactic_msg (Need_args "the theorem's name and its body")),s)
  | _, _ -> Exception (new tactic_msg Instr_misuse,(get_state cairn))

let antitheorem args cairn = match args, cairn with
  | [Labeled_prop ([name],prop)], Idle (_,s) ->
      begin match prop_infer prop s.sign with
        | Inl SProp ->
            let g = { (new_goal prop) with active=(LeftHandSide, prop) } in
            let ns = { s with
              index = 1;
              open_thm = Some (name,prop,true);
              goals = [initial_meta, g];
              pt = Context (MuTilde (thesis, prop, Play (Hyp thesis, ContextMeta initial_meta)))
            } in
            let nc = Subgoals (1, ns) in
            trace := (T_goal (ns,name))::!trace;
            history := (nc,!trace)::!history;
            nc
        | Inl x -> Idle (Some (new type_msg (Prop_kind (prop,x))), s)
        | Inr m -> Idle (Some (new type_msg m), s)
      end
  | _, Idle (_,s) ->
      Idle (Some (new tactic_msg (Need_args "the theorem's name and its body")), s)
  | _, _ -> Exception (new tactic_msg Instr_misuse, (get_state cairn))

let next args cairn = match args, cairn with
  | [], Success s | [], Subgoals (_,s) | [], Exception (_,s) -> 
      begin match s.goals with
	| [] -> Exception (new tactic_msg (Goal_exists false),s)
	| [_] -> Exception (new tactic_msg No_switch,s)
	| _ ->
	  let index' = if s.index = List.length s.goals then 1 else s.index + 1 in
	  let nc = Subgoals (1,{s with index = index'}) in
	  history := (nc,!trace)::!history ;
	  nc
      end
  | _, Success s | _, Subgoals (_,s) | _, Exception (_,s) -> 
      Exception (new tactic_msg No_args,(get_state cairn))
  | _, Idle (_,s) -> Idle (Some (new tactic_msg (Goal_exists false)),s)

let prev args cairn = match args, cairn with
  | [], Success s | [], Subgoals (_,s) | [], Exception (_,s) -> 
      begin match s.goals with
	 [] -> Exception (new tactic_msg (Goal_exists false),s)
       | [_] -> Exception (new tactic_msg No_switch,s)
       | _ ->
	  let index' = if s.index = 1 then List.length s.goals else s.index - 1 in
	  let nc = Subgoals (1,{s with index = index'}) in
	  history := (nc,!trace)::!history ;
	  nc
      end
  | _, Success s | _, Subgoals (_,s) | _, Exception (_,s) -> 
      Exception (new tactic_msg No_args,(get_state cairn))
  | _, Idle (_,s) -> Idle (Some (new tactic_msg (Goal_exists false)),s)

let qed args cairn = match args, cairn with 
  | [], Success s | [], Exception (_,s) when s.goals=[] ->
      begin match s.open_thm with
        | Some (name,prop,is_anti) ->
            let s' =
              if is_anti
              then { s with mox = Coll.add name prop s.mox }
              else { s with thms = Coll.add name prop s.thms }
            in
            let nc = Idle (Some (new instruction_msg (Defined name)),
              { s' with index = 0; open_thm=None; goals=[]; pt=Term (TermMeta initial_meta) }) in
            trace := (T_qed s.pt)::!trace;
            history := (nc,!trace)::!history;
            nc
        | None -> assert false
      end
  | _, Success s | [], Exception (_,s) when s.goals=[] -> 
      Exception (new tactic_msg No_args,s)
  | _, Idle(_,s) -> Idle (Some (new instruction_msg What_qed),s)
  | _, _ -> Exception (new tactic_msg Instr_misuse,(get_state cairn))

let ack args cairn = match args, cairn with
  | [Prover p], Idle (_,s) -> 
      launch_prover p (List.rev !trace) !ofile_pfx !keep_ofile ;
      Idle (Some (new instruction_msg (Firing_prover p)),s)
  | _, Idle (_,s) -> Idle (Some (new tactic_msg (Need_args "a prover")),s)
  | _, _ -> Exception (new tactic_msg Instr_misuse,(get_state cairn))

let ackpt args cairn = match args, cairn with
  | [Prover p], Idle (_,s) -> 
      launch_prover_on_proof_term p (List.rev !trace) !ofile_pfx !keep_ofile ;
      Idle (Some (new instruction_msg (Firing_prover p)),s)
  | _, Idle (_,s) -> Idle (Some (new tactic_msg (Need_args "a prover")),s)
  | _, _ -> Exception (new tactic_msg Instr_misuse,(get_state cairn))

let export_nl args cairn = match args,cairn with
  | [], Idle (_,s) ->
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
	   (match pt with
	    | Term t    -> pretty_natural t frm
	    | Context c -> pretty_natural_context 0 frm c);
	   pp_print_newline frm () )
	(List.rev !trace)
      in
      let nl_ofile = !ofile_pfx ^ ".nl" in
      let ohandle = create_ohandle nl_ofile in
       print_trace ohandle.frm;
       flush_all ();
       Idle (Some (new instruction_msg (NL_written nl_ofile)),s)
  | _, Idle (_,s) -> Idle (Some (new tactic_msg No_args),s)
  | _, _ -> Exception (new tactic_msg Instr_misuse,(get_state cairn))

let discard_all args cairn = match args with
  [] ->
  too_late := false;
  trace := [] ;
  history := [] ;
  Idle (Some (new instruction_msg Discard_all),new_state ())
 | _ -> Idle (Some (new tactic_msg No_args),get_state cairn)

let discard_theorem args cairn = match args,cairn with
  | [], Idle (_,s) ->
      Idle (Some (new tactic_msg (Goal_exists false)),s)
  | [], _ ->
      let s = (get_state cairn) in
      let rec prune_trace t = begin match List.rev t with
	| [] -> t
	| T_goal _ :: t' -> List.rev t'
	| _ :: t' -> prune_trace t'
      end in
      trace := prune_trace !trace ;
      Idle (Some (new instruction_msg Discard_theorem),{s with open_thm=None ;
	goals=[]})
 | _, _ -> Idle (Some (new tactic_msg No_args),get_state cairn)

let undo args cairn = match args, cairn with
  | [], Idle (_,s) -> 
     begin match !history with
	 [] -> assert false
       | [_] -> Idle (Some (new instruction_msg Nothing_undo),s)
       | _::(((cairn',trace')::_) as history') ->
	   trace := trace' ;
	   history := history' ;
	   cairn'
      end
  | [], Success s | [], Subgoals (_,s) | [], Exception (_,s) ->
     begin match !history with
	 [] -> assert false
       | [_] -> Exception (new instruction_msg Nothing_undo,s)
       | _::((cairn',trace')::history') ->
	   trace := trace' ;
	   history := history' ;
	   cairn'
      end
  | _, Idle (_,s) -> 
      Idle (Some (new tactic_msg No_args),s)
  | _, Success s | _, Subgoals (_,s) | _, Exception (_,s) ->
      Exception (new tactic_msg No_args,s)

let quit () = echo (pretty_string "Out.") ; exit 0

let jack_instruction (instruction,args) cairn = 
  match instruction with
    | Lj b -> lj_logic_switch b args cairn
    | Min b -> min_logic_switch b args cairn
    | x -> too_late := true ; begin match x with
      | Lj _ | Min _ -> assert false
      | Declare -> declare args cairn
      | Theorem -> theorem args cairn
      | Deny -> deny args cairn
      | AntiTheorem -> antitheorem args cairn
      | Next -> next args cairn
      | Prev -> prev args cairn
      | Qed -> qed args cairn
      | CheckOut -> ack args cairn
      | CheckOutProofTerm -> ackpt args cairn
      | ExportNaturalLanguage -> export_nl args cairn
      | DiscardAll -> discard_all args cairn
      | DiscardTheorem -> discard_theorem args cairn
      | Undo -> undo args cairn
      | Quit -> quit ()
    end

