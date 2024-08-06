open Core
open Format

(*There goes the pretty printing*)
let turnstile = " |----- "
let tab = "\t\t\t" 
let nl = sprintf "@\n%s" tab 

(*For sorts*)
(*For terms*)
(*For a proposition*)
(*List of symbols*)
let pretty_symbols def_list = 
  sprintf "%s : %s" (string_of_list (fst def_list)) (string_of_sort (snd def_list))

(*For a goal*)
let rec pretty_hyp h = sprintf "%s:%s%s" (fst h) (string_of_prop (snd h)) nl
let rec pretty_ccl c = sprintf "%s%s" (string_of_prop c) nl

let pretty_hyps hl = List.fold_right (fun x y -> sprintf "%s%s" (pretty_hyp x) y) hl ""

let pretty_ccls cl = 
  if !lj then pretty_ccl (List.hd cl)
  else List.fold_right (fun x y -> sprintf "%s%s" (pretty_ccl x) y) cl ""

let pretty_goal = function
  | Goal(hyp_list,ccl_list,env) -> sprintf "%s%s%s%s%s" (pretty_hyps hyp_list) turnstile nl (pretty_ccls ccl_list) nl

(*For a cairn*)
let pretty_cairn = function
  | Success s when s.goals=[] -> sprintf "Closed the branch: %sProof completed ! %s" nl nl 
  | Success s -> sprintf "Closed a branch %s%s" nl (pretty_goal (List.hd s.goals))
  | Subgoals(n,s) -> (match n with 
			    | 0 -> sprintf "Standing by. %s%s" nl (pretty_goal (List.hd s.goals))
			    | 1 -> pretty_goal (List.hd s.goals)
			    | _ -> sprintf "%s subgoals generated.%s%s" (string_of_int n) nl (pretty_goal (List.hd s.goals)))
  | Exception (m,s) -> 
      sprintf "%s%s%s" m#to_string nl (try pretty_goal (List.hd s.goals) with Failure "hd" -> "")

(*For provers*)
let pretty_prover = function
  | Coq -> "Coq"
  | Pvs -> "PVS"

(*The printing function*)
let echo s = printf "@.%s%s@." tab s 

