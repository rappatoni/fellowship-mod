open Ast
open Format

(*Deal with env lists*)
let rec string_of_list l = match l with
  | [] -> ""
  | [s] -> s
  | s::sl ->  sprintf "%s, %s" s (string_of_list sl)

(*List of symbols*)
let pretty_symbol name arity = 
  if arity = max_int then sprintf "%s" name
  else try
    let suffix = String.sub name ((String.length name )-4) 4 in
      if suffix = "_skd" 
      then sprintf "%s:%d" (String.sub name 0 (String.length name-4)) arity
      else sprintf "%s:%d" name arity
  with Invalid_argument x -> sprintf "%s:%d" name arity

let rec pretty_symbols list = match list with
  | [] -> ""
  | [(name,arity)] -> pretty_symbol name arity
  | (name, arity)::rest -> 
      sprintf "%s, %s" (pretty_symbol name arity) (pretty_symbols rest)
	
(*There goes the pretty printing*)
let turnstile = " |----- "
let tab = "\t\t\t" 
let nl = sprintf "@\n%s" tab 

let pretty_prover p = match p with
  | Coq -> "Coq"
  | Pvs -> "Pvs"

(*For terms*)
let rec pretty_term term =  match term with 
  | Var(name) -> name
  | Fun (name, arity, term_list) -> sprintf "%s%s" name (pretty_term_list term_list)
and pretty_term_list l = match l with
  | [] -> ""
  | t::tl -> sprintf "(%s)%s" (pretty_term t) (pretty_term_list tl)

(*For a proposition*)
let rec pretty_prop p = match p with 
  | True -> "true"
  | False -> "false"
  | Pred(name, arity, term_list) -> sprintf "%s%s" name (pretty_term_list term_list)
  | UProp(op,p) -> 
      let ops = match op with
	| Neg -> "\194\172" 
      in  
	ops ^ (pretty_prop p)
  | BProp(p1,op,p2) -> 
      let ops = match op with 
	  Imp -> "\226\135\146" 
	| Conj-> "\226\136\167"
	| Disj -> "\226\136\168" 
      in
	sprintf "(%s %s %s)" (pretty_prop p1) ops (pretty_prop p2)
  | Quant(q,name,p) -> 
      let quants = match q with
      | Exists -> "\226\136\131"
      | Forall -> "\226\136\128"
      in
	sprintf "%s%s.%s" quants name (pretty_prop p)

let rec pretty_prop_list l = match l with
  | [] -> ""
  | p::pl -> sprintf " %s%s" (pretty_prop p) (pretty_prop_list pl)
	 
(*For a goal*)
let rec pretty_hyp h = sprintf "%s:%s%s" (fst h) (pretty_prop (snd h)) nl
let rec pretty_ccl c = sprintf "%s%s" (pretty_prop c) nl

let pretty_hyps hl = List.fold_right (fun x y -> sprintf "%s%s" (pretty_hyp x) y) hl ""

let pretty_ccls cl = 
  if !lj then pretty_ccl (List.hd cl)
  else List.fold_right (fun x y -> sprintf "%s%s" (pretty_ccl x) y) cl ""

let pretty_goal g = match g with 
  | Goal(hyp_list,ccl_list) -> sprintf "%s%s%s%s%s" (pretty_hyps hyp_list) turnstile nl (pretty_ccls ccl_list) nl

(*For a cairn*)
let pretty_cairn m = match m with
  | Success((l,e))-> if l=[] 
    then sprintf "Closed the branch: %sProof completed ! %s" nl nl 
    else sprintf "Closed a branch %s%s" nl (pretty_goal (List.hd l))
  | Subgoals(n,(l,e)) -> (match n with 
			    | 0 -> sprintf "Standing by. %s%s" nl (pretty_goal (List.hd l))
			    | 1 -> pretty_goal (List.hd l)
			    | _ -> sprintf "%s subgoals generated.%s%s" (string_of_int n) nl (pretty_goal (List.hd l)))
  | Exception (s,(l,e)) -> sprintf "%s%s%s" s nl 
      (try pretty_goal (List.hd l) with Failure "hd" -> "")

(*The printing function*)
let echo s = printf "%s%s@?" tab s 

