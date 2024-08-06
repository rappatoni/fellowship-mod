open Core
open Format

(*There goes the pretty printing*)
let turnstile     = " |-----  "
let env_separator = ""
let tab = "\t\t\t" 
let nl = sprintf "@\n%s" tab 
let ind n = sprintf "@\n%s" (String.make (n * 2) ' ')

(*Deal with env lists*)
(*let rec string_of_list l = match l with
  | [] -> ""
  | [s] -> s
  | s::sl -> sprintf "%s, %s" s (string_of_list sl)

let rec pretty_var_list l = match l with
  | [] -> ""
  | s::sl ->  sprintf "%s %s" s (string_of_list sl)
*)

(*For natural language proofs*)
(* CSC: Very bad imperative code, to avoid one extra constant argument of type
   status for the pretty_natural_* functions. *)
let set_state, is_current_goal =
 let dummy_state = {index=0;goals=[];sign=[];pt=TermMeta initial_meta} in
 let state = ref dummy_state in
 (function state' -> state := state'),
 (function metaid -> fst (List.nth !state.goals (!state.index - 1)) = metaid)

let rec pretty_natural_t3rm n =
 function
    TermMeta id -> sprintf ".... (%s)%s" 
      id (if is_current_goal id then " <======" else "")
  | True_constructor -> sprintf "by definition of %s" (pretty_prop True)
  | Hyp id -> sprintf "by %s" id
  | Lambda (id,p,t) -> sprintf "assume %s (%s)%s%s"
     (pretty_prop p) id (ind n) (pretty_natural_t3rm n t)
  | LambdaFO (id,p,t) -> sprintf "consider an arbitrary but fixed %s of type %s%s%s"
     id (pretty_sort p) (ind n) (pretty_natural_t3rm n t)
  | Cons' (c,t) -> sprintf "<???%s%s%s%s???>"
     (pretty_natural_context (n + 1) c) (ind (n + 1))
     (pretty_natural_t3rm (n + 1) t) (ind n)
  | TermsPair(t,t') -> sprintf "%s%sand%s%s"
     (pretty_natural_t3rm n t) (ind n) (ind n) (pretty_natural_t3rm n t')
  | TermsPairFO(p,t,t') ->
     (* we hide the witness *)
     sprintf "%s" (pretty_natural_t3rm n t')
  | Left t -> sprintf "%s%strivial" (pretty_natural_t3rm n t) (ind n)
  | Right t -> sprintf "%s%strivial" (pretty_natural_t3rm n t) (ind n)
  | TermOfContext c -> sprintf "suppose ??? to prove %s%s" 
     (pretty_prop False) (pretty_natural_context (n + 1) c)
  | Mu (id,p,cmd) -> sprintf "we need to prove %s%s%s" 
     (pretty_prop p) (ind (n + 1)) (pretty_natural_command (n + 1) cmd)
and pretty_natural_context n =
 function
    ContextMeta id -> sprintf "%s...(%s)%s"
     (ind (n - 1)) id (if is_current_goal id then " <======" else "")
  | False_eliminator -> sprintf "%sabsurd" (ind (n - 1))
  | Concl id -> sprintf "%sdone" (ind (n - 1))
  | Cons (t,c) -> sprintf "%sand %s%s"
     (ind n) (pretty_natural_t3rm n t) (pretty_natural_context n c)
  | ConsFO (t,c) -> sprintf "%s%s"
     (* we hide the argument t *)
     (pretty_natural_context n c) ""
  | Lambda' (id,p,c) -> sprintf "%s<???%s(%s)%s%s???>"
     (ind n) (pretty_prop p) id (pretty_natural_context (n + 1) c) (ind n)
  | DestructTermsPair(id,p,id',p',cmd) -> 
     sprintf "%swe proved %s (%s) and %s (%s)%s%s"
     (ind n) (pretty_prop p) id (pretty_prop p') id' (ind n)
     (pretty_natural_command n cmd)
  | DestructTermsPairFO(id,p,c) -> 
     sprintf "%slet %s be the element of type %s that satisfies the property%s"
     (ind n) id (pretty_sort p) (pretty_natural_context n c)
  | ContextsPair(c,c') -> sprintf "%sby cases: %sfirst case:%sby case hypothesis%s%ssecond case:%sby case hypothesis%s"
     (ind n) (ind (n + 1)) (ind (n + 2)) (pretty_natural_context (n + 2) c)
             (ind (n + 1)) (ind (n + 2)) (pretty_natural_context (n + 2) c')
  | ContextOfTerm c ->
     sprintf "%swe proved ???. To reach absurdum and complete the proof we prove ???%s%s"
     (ind n) (ind (n + 1)) (pretty_natural_t3rm (n + 1) c) 
  | MuTilde (id,p,cmd) -> sprintf "%swe proved %s (%s)%s%s"
     (ind n) (pretty_prop p) id (ind n) (pretty_natural_command n cmd)
and pretty_natural_command n =
 function
    Play (t,c) -> sprintf "%s%s"
     (pretty_natural_t3rm n t) (pretty_natural_context n c)

let pretty_natural pt = 
 let pt' = if !lj then pt else Interpreter_proof_terms.lk_to_lj_plus_em pt in
  (*CSC: expensive test, can be avoided *)
  if pt' <> pt then
    printf "!!! The proof is classical; converted to the following LJ + EM proof:\n%s@."
     (pretty_t3rm pt') ;
  pretty_natural_t3rm 0 pt'

(*For goals*)
let rec pretty_hyp h = sprintf "%s:%s%s" (fst h) (pretty_prop (snd h)) nl
let rec pretty_ccl c = sprintf "%s:%s%s" (fst c) (pretty_prop (snd c)) nl

let pretty_hyps hl = List.fold_left (fun y x -> (pretty_hyp x) ^ y) "" hl

let pretty_ccls cl = List.fold_right (fun x y -> (pretty_ccl x) ^ y) cl ""

let pretty_env env =
 String.concat nl
  (List.map (fun (id,sort) -> id ^ ":" ^ pretty_sort sort) env) ^ nl ^
 env_separator ^ nl

let pretty_goal (id,goal) =
 pretty_env goal.env ^
 match goal.active with
  | (RightHandSide,p) -> 
      (pretty_hyps goal.hyp) ^ turnstile ^ id ^ nl
      ^ (pretty_ccls (("*",p)::goal.ccl)) ^ nl
  | (LeftHandSide,p) ->
      (pretty_hyps (("*",p)::goal.hyp)) ^ turnstile ^ id ^ nl
      ^ (pretty_ccls goal.ccl) ^ nl

let pretty_pt pt = sprintf "@.Proof term: %s@.Natural language:@.%s" 
  (pretty_t3rm pt) (pretty_natural pt)

let pretty_goals cur l =
 let n = List.length l in
  if n <> 0 then
   sprintf "%d %s yet to prove!@.%s%s" 
    n (if n = 1 then "goal" else "goals") 
    nl (pretty_goal (List.nth l (cur - 1)))
  else ""

(*For a cairn*)
let pretty_cairn cairn =
 set_state (get_state cairn) ;
 match cairn with
  | Success s when s.goals = [] -> 
      sprintf "Closed the branch: %sProof Completed! %s%s" nl nl (pretty_pt s.pt)
  | Success s -> 
      sprintf "Closed a branch %s%s%s%s" 
	nl (pretty_pt s.pt) nl (pretty_goals s.index s.goals)
  | Subgoals (n,s) -> (match n with 
			| 0 -> sprintf "Standing by. %s" nl
			| 1 -> sprintf "%s%s%s" 
			    (pretty_pt s.pt) nl 
			    (pretty_goals s.index s.goals)
			| _ -> sprintf "%d subgoals generated. %s%s%s%s" 
			    n nl (pretty_pt s.pt) nl 
			    (pretty_goals s.index s.goals) )
  | Exception (m,s) -> sprintf "%s%s%s" m#to_string nl (pretty_goals s.index s.goals)

(*The printing function*)
let echo s = printf "%s%s@?" tab s 

