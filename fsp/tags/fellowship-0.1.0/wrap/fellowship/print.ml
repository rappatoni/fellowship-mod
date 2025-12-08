(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
*)

open Core
open Lexing
open Format

(*Some global flags affecting the pretty-printing*)
let toplvl = ref true
let prompts = "fsp <"

(*There goes the pretty printing*)
let turnstile     = " |-----  "
let env_separator = ""
let ind n frm =
  let n' = if n < 0 then 0 else n in
  fprintf frm "@\n%s" (String.make (n' * 2) ' ')

(*Error localization using line and column info*)
let localization pos frm =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  if !toplvl then
    let offset = String.length prompts in
    (printf "%s%s@." (String.make (offset+c) '-') "^" ;
     fprintf frm "Line %d, character %d:@\n" l c )
  else
    (fprintf frm "Line %d, character %d:@\n." l c)

(*We need a pretty_printing wrapper for strings (or do we?)*)
let pretty_string s frm =  pp_print_string frm s

(*For natural language proofs*)
(* CSC: Very bad imperative code, to avoid one extra constant argument of type
   status for the pretty_natural_* functions. *)
let set_state, is_current_goal =
 let dummy_state = {index=0;open_thm=None;goals=[];sign=Coll.empty;thms=Coll.empty;mox=Coll.empty;pt=Term (TermMeta initial_meta)} in
 let state = ref dummy_state in
 (function state' -> state := state'),
 (function metaid -> fst (List.nth !state.goals (!state.index - 1)) = metaid)

let rec pretty_natural_t3rm n frm =
 function
    TermMeta id -> fprintf frm ".... (%s)%s" 
      id (if is_current_goal id then " <======" else "")
  | True_constructor -> fprintf frm "by definition of %s" (pretty_prop True)
  | Hyp id -> fprintf frm "by %s" id
  | Lambda (id,p,t) -> 
      (fprintf frm "assume %s (%s)" (pretty_prop p) id ;
       ind n frm ;
       pretty_natural_t3rm n frm t )
  | LambdaFO (id,p,t) -> 
      (fprintf frm "consider an arbitrary but fixed %s of type %s"
	id (pretty_sort p) ;
       ind n frm ;
       pretty_natural_t3rm n frm t )
  | Cons' (c,t) -> 
      (pp_print_string frm "<???" ;
       pretty_natural_context (n + 1) frm c ;
       ind (n + 1) frm ;
       pretty_natural_t3rm (n + 1) frm t ;
       ind n frm ;
       pp_print_string frm "???>" )
  | TermsPair(t,t') -> 
      (pretty_natural_t3rm n frm t ;
       ind n frm ;
       pp_print_string frm "and" ;
       ind n frm ;
       pretty_natural_t3rm n frm t' )
  | TermsPairFO(p,t,t') ->
      (* we hide the witness *)
      pretty_natural_t3rm n frm t'
  | Left t -> 
      (pretty_natural_t3rm n frm t ;
       ind n frm ;
       pp_print_string frm "trivial" )
  | Right t -> 
      (pretty_natural_t3rm n frm t ;
       ind n frm ;
       pp_print_string frm "trivial" )
  | Mu (id,p,cmd) -> 
      (fprintf frm "we need to prove %s" (pretty_prop p) ;
       ind (n + 1) frm ;
       pretty_natural_command (n + 1) frm cmd )
and pretty_natural_context n frm =
 function
    ContextMeta id -> 
      (ind (n - 1) frm ;
       fprintf frm "...(%s)%s" 
	id (if is_current_goal id then " <======" else "") )
  | False_eliminator -> 
      (ind (n - 1) frm ;
       pp_print_string frm "absurd" )
  | Concl id -> 
      (ind (n - 1) frm ;
       pp_print_string frm "done" )
  | Cons (t,c) -> 
      (ind n frm ;
       pp_print_string frm "and " ;
       pretty_natural_t3rm n frm t ;
       pretty_natural_context n frm c )
  | ConsFO (t,c) ->
      (* we hide the argument t *)
      pretty_natural_context n frm c
  | Lambda' (id,p,c) -> 
      (ind n frm ;
       fprintf frm "<???%s(%s)" (pretty_prop p) id ;
       pretty_natural_context (n + 1) frm c ;
       ind n frm )
  | DestructTermsPair(id,p,id',p',cmd) -> 
      (ind n frm ;
       fprintf frm "we proved %s (%s) and %s (%s)" 
	(pretty_prop p) id (pretty_prop p') id' ;
       ind n frm ;
       pretty_natural_command n frm cmd )
  | DestructTermsPairFO(id,p,c) -> 
      (ind n frm ;
       fprintf frm "let %s be the element of type %s that satisfies the property"
	id (pretty_sort p) ;
       pretty_natural_context n frm c )
  | ContextsPair(c,c') -> 
      (ind n frm ;
       pp_print_string frm "by cases: " ;
       ind (n + 1) frm ;
       pp_print_string frm "first case:" ;
       ind (n + 2) frm ;
       pp_print_string frm "by case hypothesis" ;
       pretty_natural_context (n + 2) frm c ;
       ind (n + 1) frm ;
       pp_print_string frm "second case:" ;
       ind (n + 2) frm ;
       pp_print_string frm "by case hypothesis" ;
       pretty_natural_context (n + 2) frm c' )
  | MuTilde (id,p,cmd) -> 
      (ind n frm ;
       fprintf frm "we proved %s (%s)" (pretty_prop p) id ;
       ind n frm ;
       pretty_natural_command n frm cmd )
and pretty_natural_command n frm =
 function
    Play (t,c) -> 
      (pretty_natural_t3rm n frm t ;
       pretty_natural_context n frm c )

let pretty_natural pt frm = 
 let pt' = if !lj then pt else Interpreter_proof_terms.lk_to_lj_plus_em pt in
  (*CSC: expensive test, can be avoided *)
  if pt' <> pt then
    (fprintf frm "!!! The proof is classical; converted to the following LJ + EM proof:@\n%s@\n"
      (pretty_t3rm pt') ;
     pretty_natural_t3rm 0 frm pt' )
  else
    pretty_natural_t3rm 0 frm pt'

(*For goals*)
let pretty_hyp frm (s,p,w) = if w then fprintf frm "%s:%s@\n" s (pretty_prop p)
let pretty_ccl frm (s,p,w) = if w then fprintf frm "%s:%s@\n" s (pretty_prop p)

let pretty_hyps hl frm = List.iter (pretty_hyp frm) hl

let pretty_ccls cl frm = List.iter (pretty_ccl frm) cl

let pretty_env env frm =
 fprintf frm "%s@\n%s@\n" 
  (String.concat "\n"
   (List.map (fun (id,sort) -> id ^ ":" ^ pretty_sort sort) env))
  env_separator

let pretty_goal (id,goal) frm =
 pretty_env goal.env frm ;
 match goal.active with
  | (RightHandSide,p) -> 
      pretty_hyps goal.hyp frm ;
      fprintf frm "%s%s@\n" turnstile id ;
      pretty_ccls (("*",p,true)::goal.ccl) frm
  | (LeftHandSide,p) ->
      pretty_hyps (("*",p,true)::goal.hyp) frm ; 
      fprintf frm "%s%s@\n" turnstile id ;
      pretty_ccls goal.ccl frm

let pretty_pt pt frm =
  match pt with
  | Term t ->
      fprintf frm "Proof term: @\n%s@\n" (pretty_t3rm t) ;
      fprintf frm "Natural language:@\n" ;
      pretty_natural t frm ;
      pp_force_newline frm () ;
      pp_force_newline frm ()
  | Context c ->
      fprintf frm "Proof term: @\n%s@\n" (pretty_context c) ;
      fprintf frm "Natural language:@\n" ;
      pretty_natural_context 0 frm c ;
      pp_force_newline frm () ;
      pp_force_newline frm ()

let pretty_goals cur goals frm =
 let n = List.length goals in
 if n <> 0 then 
  try 
  (fprintf frm "%d %s yet to prove!@\n" 
    n (if n = 1 then "goal" else "goals") ;
   pretty_goal (List.nth goals (cur - 1)) frm )
  with _ -> 
  (printf "This is NOT supposed to happen. You get to spank the programmer.@." ;
   exit (-1) )

(*For a cairn*)
let pretty_cairn cairn frm =
 set_state (get_state cairn) ;
 match cairn with
  | Idle (None,s) -> ()
  | Idle (Some m,s) ->
      begin match m#loc with 
	| None ->
	     fprintf frm "%s@\n" m#to_string ;
	| Some pos ->
	    (localization pos frm ;
	     fprintf frm "%s@\n" m#to_string )
      end
  | Success s when s.goals = [] -> 
      (fprintf frm "Closed the last branch: @\nProof completed!@\n@\n" ;
       pretty_pt s.pt frm )
  | Success s -> 
      (fprintf frm "Closed a branch. @\n@\n" ;
       pretty_pt s.pt frm ; 
       pretty_goals s.index s.goals frm )
  | Subgoals (0,s) ->
      fprintf frm "Standing by. @\n@\n"
  | Subgoals (1,s) -> 
      (pretty_pt s.pt frm ;
       pretty_goals s.index s.goals frm )
  | Subgoals (n,s) ->
      (fprintf frm "%d subgoals generated. @\n@\n" n ;
       pretty_pt s.pt frm ;
       pretty_goals s.index s.goals frm)
  | Exception (m,s) ->
      begin match m#loc with 
	| None ->
	    (fprintf frm "%s@\n@\n" m#to_string ;
	     pretty_pt s.pt frm ; 
	     pretty_goals s.index s.goals frm )
	| Some pos ->
	    (localization pos frm ;
	     fprintf frm "%s@\n@\n" m#to_string ;
	     pretty_pt s.pt frm ; 
	     pretty_goals s.index s.goals frm )
      end

(*The main printing function.*)
let echo pp_fsp_fun = 
  let ui_buffer = Buffer.create 16 in
  let ui_frm = formatter_of_buffer ui_buffer in
  pp_print_newline ui_frm () ; 
  (*The first box sets the indentation*)
  pp_open_vbox ui_frm 0 ;
    pp_print_string ui_frm "    > " ;
  pp_close_box ui_frm () ;
  (*The second box prints the real text*)
  pp_open_vbox ui_frm 0 ;
    pp_open_hbox ui_frm () ;
      pp_fsp_fun ui_frm ;
    pp_close_box ui_frm () ;
  pp_close_box ui_frm () ;
  pp_print_newline ui_frm () ;
  Buffer.output_buffer stdout ui_buffer ;
  if !Machine.machine_mode then begin
    let payload = Machine.snapshot !Core.cairn in
    Printf.printf ";;BEGIN_ML_DATA;; %s ;;END_ML_DATA;;\n%!" payload;
  end

