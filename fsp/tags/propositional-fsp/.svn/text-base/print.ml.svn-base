open Ast
open Format

(*Deal with env lists*)
let rec string_of_list l = match l with
  | [] -> ""
  | [s] -> s
  | s::sl ->  s ^ ", " ^ (string_of_list sl)

let rec pretty_var_list l = match l with
  | [] -> ""
  | s::sl ->  s ^ " " ^ (string_of_list sl)

(*There goes the pretty printing*)
let turnstile = " |----- "
let tab = "\t\t\t" 
let nl = "\n" ^ tab 

let pretty_prover p = match p with
  | Coq -> "Coq"
  | Pvs -> "Pvs"

(*For a proposition*)
let rec pretty_prop p = match p with 
  | Const s -> s
  | True -> "true"
  | False -> "false"
  | UProp(Neg,p) -> "~" ^ (pretty_prop p)
  | BProp(p1,op,p2) -> 
      let ops = match op with 
	  Imp -> "->" 
	| Conj-> "/\\"
	| Disj -> "\\/" 
      in
	"(" ^ (pretty_prop p1) ^ " " ^ ops ^ " "
	^ (pretty_prop p2) ^ ")"

(*For a goal*)
let rec pretty_hyp h = (fst(h)) ^ " : " ^ pretty_prop (snd(h)) ^ nl
let rec pretty_ccl c = pretty_prop c ^ nl

let pretty_hyps hl = List.fold_right (fun x y -> (pretty_hyp x) ^ y) hl ""

let pretty_ccls cl = 
  if !lj then pretty_ccl (List.hd cl)
  else List.fold_right (fun x y -> (pretty_ccl x) ^ y) cl ""

let pretty_goal g = match g with 
  | Goal(hyp_list,ccl_list) -> 
      pretty_hyps hyp_list
      ^ turnstile ^ nl
      ^ pretty_ccls ccl_list 
      ^ nl

(*For a cairn*)
let pretty_cairn m = match m with
  | Success((l,e))-> if l=[] 
    then "Closed the branch:" ^ nl ^ "Proof completed !" ^ nl 
    else "Closed a branch" ^ nl ^ pretty_goal (List.hd l)
  | Subgoals(n,(l,e)) -> (match n with 
			    | 0 -> "Standing by." ^ nl
				^ pretty_goal (List.hd l)
			    | 1 -> pretty_goal (List.hd l)
			    | _ -> string_of_int(n) ^ " subgoals generated." ^ nl
				^ pretty_goal (List.hd l))
  | Exception (s,(l,e)) -> s ^ nl ^ 
      (try pretty_goal (List.hd l)
       with Failure "hd" -> "")

(*The printing function*)
let echo s = printf "%s%s@?" tab s 

