open Ast
open Print 

(*Check the env list for occurences of vars*)
let check_env env vars = 
  let rec comparer env_list var_list ret_list =
    match var_list with 
      | [] -> ret_list
      | hd::tl -> 
	  (if (List.mem hd env_list) 
	   then (comparer env_list tl ret_list)
	   else if (List.mem hd ret_list)
	   then (comparer env_list tl ret_list)
	   else (comparer env_list tl (hd::ret_list))
	  ) in
    (comparer env vars [])
    
(*The proof language*)

let define_vars vars s = match s with
  | Success((s,env)) -> Exception("A goal already exists.",(s,env))
  | Subgoals(0,([],env)) -> Subgoals(0,([],vars@env))
  | Subgoals(_,(s,env)) -> Exception("A goal already exists.",(s,env))
  | Exception(_,([],env)) -> Subgoals(0,([],vars@env))
  | Exception(_,(s,env)) -> Exception("A goal already exists.",(s,env))
      
let rec goal p s = match s with 
  | Subgoals(0,([],env)) -> 
      let needed_vars = (get_const p) in
      let undef_var=(check_env env needed_vars) in
	if undef_var=[] then Subgoals(1,([Goal([],[p])],env))
	else Exception(("Please define " ^(string_of_list undef_var)^ " for me."),([],env))
  | Subgoals(_,(s,env)) -> Exception("A goal already exists.",(s,env))
  | Success((s,env)) -> Exception("A goal already exists.",(s,env))
  | Exception(_,([],env)) -> goal p (Subgoals(0,([],env)))
  | Exception(_,(s,env)) -> Exception("A goal already exists.",(s,env))

(*Tactics*)

(*Check if a variable name is already used in the hypothesis or the environment*)
let check_variable var_list state =
  let rec list_conflicts var_list state results =
    match var_list with 
      | [] -> results
      | hd::tl 
	  when (List.mem_assoc hd (get_current_hyps state)) 
	    || (List.mem hd (get_env state))
	    -> list_conflicts tl state (hd::results)
      | hd::tl -> list_conflicts tl state results
  in
    match (list_conflicts var_list state []) with
      | [] -> ""
      | [name] -> "Choose a new name : hypothesis "^name^" already exists."
      | names -> "Choose new names : hypothesis "^(string_of_list names)^" already exist."
	  
let fresh state = 
  let rec fresh_rec n state = 
    let name = Format.sprintf "_%d" n in
    let conflicts = (check_variable [name] state) in
      if conflicts = "" then name
      else fresh_rec (n+1) state
  in fresh_rec 0 state

(*An easy one to begin with*)
let axiom s = match s with 
  | (Goal(h,c)::r, env) ->
      if (List.exists (fun x -> (snd x = False) || (List.mem (snd x) c)) h) 
      then Success(r,env)
      else Exception("This is not trivial. Work some more.",s)
  | _ -> Exception("Where is the goal ?",s)

(*Implication rules*)
let r_Imp x s = match s with 
  | (Goal(h,BProp(a,Imp,b)::c)::r, env) -> 
      let objection = (check_variable [x] s) in
	if objection = "" then 
	  Subgoals(1,(Goal(h@[x,a],b::c)::r,env))
	else Exception(objection,s)
  | (Goal(h,c)::r, env) -> Exception("I can't get no implication",s)
  | _ -> Exception("Where is the goal ?",s)

let l_Imp x x' s = match s with 
  | (Goal(h,c)::r, env) -> 
      let objection = check_variable [x'] s in
	if objection = "" then
	  (try 
	     let q = List.assoc x h in
	     let h' = List.remove_assoc x h in
	       match q with
		 | BProp(a,Imp,b) -> Subgoals(2, (Goal(h',a::c)::Goal(h'@[x',b],c)::r, env))
		 | _ -> Exception("I can't get no implication",s)
	       with Not_found -> Exception(x^" is not in your list of hypothesis",s)
	  )
	else Exception(objection,s)
  | _ -> Exception("Where is the goal ?",s)
      
(*Conjunction rules*)
let r_Conj s = match s with 
  | (Goal(h,BProp(a,Conj,b)::c)::r, env) ->
      Subgoals(2,(Goal(h,a::c)::Goal(h,b::c)::r,env))
  | _ -> Exception("You seen any conjunctions ?",s)

let l_Conj x x' x'' s = match s with 
  | (Goal(h,c)::r, env) -> 
      let objection = check_variable [x';x''] s in
	if objection = "" then
	  (try
	     let q = List.assoc x h in
	     let h' = List.remove_assoc x h in
	       match q with 
		 | BProp(a,Conj,b) -> Subgoals(1, (Goal(h'@[x',a ; x'',b],c)::r, env))
		 | _ -> Exception("You seen any conjunctions ?",s)
	       with Not_found -> Exception(x^" is not in your list of hypothesis",s)
	  )
	else Exception(objection,s)
  | _ -> Exception("Where is the goal ?",s)
      
(*Disjunction rules*)
let r_Disj_L s = match s with
  | (Goal(h,BProp(a,Disj,b)::c)::r, env) -> Subgoals(2, (Goal(h,a::b::c)::r,env))
  | _ -> Exception("There ain't no disjunction !",s)

let r_Disj_R s = match s with
  | (Goal(h,BProp(a,Disj,b)::c)::r, env) -> Subgoals(1, (Goal(h,b::a::c)::r,env))
  | _ -> Exception("There ain't no disjunction !",s)      

let l_Disj x x' x'' s = match s with 
  | (Goal(h,c)::r, env) -> 
      let objection = check_variable [x';x''] s in
	if objection = "" then
	  (try
	     let q = List.assoc x h in
	     let h' = List.remove_assoc x h in
	       match q with 
		 | BProp(a,Disj,b) -> Subgoals(2, (Goal(h'@[x',a],c)::Goal(h'@[x'',b],c)::r, env))
		 | _ -> Exception("You seen any disjunctions ?",s)
	       with Not_found -> Exception(x^" is not in your list of hypothesis",s)
	  )
	else Exception(objection,s)
  | _ -> Exception("Where is the goal ?",s)
(*forge : replacer les deux sous-termes du \/ a l'endroit ou etait le \/ 
-- de meme pour toutes les regles ! -/\/\-> faire une hashtable ? ou un map ?*)
(*forge : il integrer le système de génération de blazes : comment rendre 
un argument de fonction optionnel ?*)

(*Negation rules*)
let r_Neg x s = match s with 
  | (Goal(h,UProp(Neg,a)::c)::r, env) -> r_Imp x (Goal(h,BProp(a,Imp,False)::c)::r, env)
  | _ -> Exception("No negation around here !",s)

let l_Neg x s = match s with 
  | (Goal(h,c)::r, env) -> 
      (try
	 let q = List.assoc x h in
	 let h' = List.remove_assoc x h in
	   match q with 
	     | UProp(Neg,a) -> Subgoals(1, (Goal(h',a::c)::r, env))
	     | _ -> Exception("You seen any negation ?",s)
	   with Not_found -> Exception(x^" is not in your list of hypothesis",s)
      )
  | _ -> Exception("Where is the goal ?",s)
      
(*Contraction rules*)
let r_Contr s =  match s with 
  | (Goal(h,a::c)::r, env) ->
      Subgoals(1, (Goal(h,a::a::c)::r, env))
  | _ -> Exception("Where is the goal ?",s)

let l_Contr x x' s =  match s with 
  | (Goal(h,c)::r, env) ->
      let objection = check_variable [x'] s in
	if objection = "" then 
	  Subgoals(1, (Goal(h@[x',(List.assoc x h)],c)::r, env))
	else Exception(objection,s)
  | _ -> Exception("Where is the goal ?",s)

(*Switching between multiple conclusions*)
let r_Switch s = match s with
  | (Goal(h,c)::r, env) -> 
      let swap c =  match c with
	| [] -> assert false
	| a::l -> l@[a]
      in 
      let c' = (swap c) in
	Subgoals(0, (Goal(h,c')::r, env))
  | _ -> Exception("Where is the goal ?",s)


(*Applying a tactic*)
let tac t m = match t with 
  | Axiom -> axiom (get_state m)
  | R_Imp x -> r_Imp x (get_state m)
  | L_Imp (x,y) -> l_Imp x y (get_state m)
  | R_Conj -> r_Conj (get_state m)
  | L_Conj (x,y,z) -> l_Conj x y z (get_state m)
  | R_Disj_L -> r_Disj_L (get_state m)
  | R_Disj_R -> r_Disj_R (get_state m)
  | L_Disj (x,y,z) -> l_Disj x y z (get_state m)
  | R_Neg x -> r_Neg x (get_state m)
  | L_Neg x -> l_Neg x (get_state m)
  | R_Contr -> r_Contr (get_state m)
  | L_Contr (x,y) -> l_Contr x y (get_state m)
  | R_Switch -> r_Switch (get_state m)

