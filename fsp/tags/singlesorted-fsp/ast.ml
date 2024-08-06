(*Option : use constructive / classical logic 
Why here and not in main.ml ? 
Because it's very pervasive. *)
let lj = ref true (* constructive by default *)

(* *)
type ('a,'b) sum = 
  | Inl of 'a
  | Inr of 'b

(*Objects : terms*)
type name = string

type term = 
  | Var of name
  | Fun of name * int * (term list) (*name, arity and arguments*)

(*Objects : propositions*)
type prop = 
  | True | False
  | Pred of name * int * (term list) (*name, arity and arguments*)
  | UProp of unop * prop
  | BProp of prop * binop * prop
  | Quant of quantifier * name * prop
and unop = Neg
and binop = Imp | Conj | Disj
and quantifier = Exists | Forall
(*forge: passer aux multi-sortes*)
(*forge : currifier les fonctions => plus de term_list => oki en multi-sorté*)
(*forge : utiliser None et Some a la place de max_int pour les arites / sortes ?*)
(*forge : ameliorer les types d'erreur -?-> mettre dans commands.ml ?
  ou , mieux, declarer des types d'erreur dans ast.ml ? *)
(*forge : utiliser des name list pour Quant*)

let funify l = List.map (fun pair -> Fun((fst pair), (snd pair), [])) l		 
let predify l = List.map (fun pair -> Pred((fst pair), (snd pair), [])) l

(*Objects : 
  - environments are lists of constants
  - goals as a list of named hypothesis and a conclusion
  - a state pairs a goal list with an environment
  - the proof monad (or proof state, or cairn) guides the proof search
*)
type env = (term list) * (prop list)

type goal = Goal of (string * prop) list * prop list

type state = (goal list) * env 

type proof_monad = 
  | Success of state
  | Subgoals of int * state
  | Exception of string * state

type cairn = proof_monad

(*Objects : provers*)
type provers = 
  | Coq 
  | Pvs
	
(*Objects : commands*)
type tactic = 
  | Axiom
  | R_Imp of string | L_Imp of (string *string)
  | R_Neg of string | L_Neg of string 
  | R_Conj | L_Conj of (string*string*string)
  | R_Disj_L | R_Disj_R | L_Disj of (string*string*string)
  | R_Forall | L_Forall of (string*term)
  | R_Exists of term | L_Exists of string
  | R_Contr | L_Contr of (string*string)
  | R_Switch

type instruction =
  | Deffun of (name*int) list
  | Defpred of (name*int) list
  | Disc (*discard environment*)
  | Gl of prop
  | Tac of tactic
  | Ack of provers (*require acknowledgement from outside prover*)
  | Quit

type script = instruction

(*Objects : trace*)
type trace_atoms =
  | T_deffun of (name*int) list
  | T_defpred of (name*int) list
  | T_goal of state
  | T_tac of state

(*Functions*)
(*Monad contructor*)
let create_cairn () = Subgoals(0,([],([],[])))
			   
(*Breaking down the monad*)
let get_state m = match m with
  | Success(s) -> s
  | Subgoals(n,s) -> s
  | Exception(str,s) -> s

let get_msg m = match m with 
  | Success(s) -> ""
  | Subgoals(n,s) -> ""
  | Exception(str,s) -> str    

let isScs m = match m with
  | Success(_) -> true
  | _ -> false

let isCptScs m = match m with
  | Success(([],_)) -> true
  | _ -> false

let isSub m = match m with 
  | Subgoals(_,_) -> true
  |_ -> false

let isExn m = match m with 
  | Exception(_,_) -> true
  | _ -> false

(*Decomposing the proof state*)
let get_goallist s = (fst s)
let get_env s = (snd s)

let get_current_hyps s = match s with 
  | (Goal(h,c)::r, env) -> h
  | _ -> []

let get_current_ccls s = match s with 
  | (Goal(h,c)::r, env) -> c
  | _ -> []

(*Transform goals and goal lists into propositions*)
let rec prop_of_csq p = match p with
  | [] -> False
  | [a] -> a
  | a::p' -> 
      if !lj then a
      else BProp (a, Disj, (prop_of_csq p'))
      
let rec prop_of_goal_rec h p = match h with 
  | [] -> p
  | (_,a)::h' -> (prop_of_goal_rec h' (BProp (a,Imp,p)))

let prop_of_goal (Goal(h,p)) = prop_of_goal_rec h (prop_of_csq p) 

let rec prop_of_goallist gl = match gl with 
  | [] -> True
  | g::s' -> BProp(prop_of_goal g,Conj,prop_of_goallist s')

let prop_of_cairn m = prop_of_goallist (get_goallist (get_state m)) 

(*Fancier functions*)
(*Scanning the environment*)
let rec env_mem str env = match env with
    | ([],[]) -> false
    | ([],p::p_list) -> 
	(match p with
	   | Pred(name, _, _) -> if str = name 
	     then true
	     else env_mem str ([],p_list) 
	   | _ -> assert false)
    | (t::t_list, p_list) -> 
	(match t with
	   | Fun(name, _, _) -> if str = name 
	     then true
	     else env_mem str (t_list,p_list)
	   | _ -> assert false)

(*Output free variables of a term*)
let rec get_term_fv term env ret = 
  match term with 
    | Var(name) -> if List.mem name env then ret else name::ret
    | Fun(name, arity, term_list) -> 
	List.flatten (List.map (fun t -> get_term_fv t env ret) term_list)
	
(*Output free variables of a proposition*)
let get_prop_fv p = 
  let rec get_fv_rec p env ret = match p with 
    | True -> ret
    | False -> ret
    | Pred(name, arity, term_list) -> 
	List.flatten (List.map (fun t -> get_term_fv t env ret) term_list)
    | UProp(Neg,p) -> get_fv_rec p env ret
    | BProp(p1,op,p2) -> 
	(get_fv_rec p1 env []) @ (get_fv_rec p2 env []) @ ret
    | Quant(q,name,p) -> get_fv_rec p (name::env) ret
  in get_fv_rec p [] []

(*Output all the symbol names of a proposition*)
let get_symbols p = 
  let rec get_tsym_rec term ret =  match term with 
    | Var(name) -> name :: ret
    | Fun(name, arity, term_list) -> 
	name :: (List.flatten (List.map (fun t -> get_tsym_rec t ret) term_list))
  in
  let rec get_sym_rec prop ret = match prop with 
    | True -> ret
    | False -> ret
    | Pred(name, arity, term_list) -> 
	name :: (List.flatten (List.map (fun t -> get_tsym_rec t ret) term_list))
    | UProp(Neg,p) -> get_sym_rec p ret
    | BProp(p1,op,p2) -> 
	(get_sym_rec p1 []) @ (get_sym_rec p2 []) @ ret
    | Quant(q,name,p) -> name :: (get_sym_rec p ret)
  in get_sym_rec p []

let rec hyp_mem name hyp = match hyp with
  | [] -> false
  | (id,prop):: hypl -> 
      if (id = name) || (List.mem name (get_symbols prop))
      then true
      else hyp_mem name hypl

let rec ccl_mem name ccl = match ccl with
  | [] -> false
  | prop:: ccll -> 
      if List.mem name (get_symbols prop)
      then true
      else ccl_mem name ccll

(*Typechecking*)
type capsule = {yes: bool ; name: string ; arity: int}

let rec env_pred_mem name arity env = match env with 
  | [] -> {yes=false ; name="" ; arity=0}
  | Pred(name', arity', term_list) :: env_tl -> 
      if (name = name') && ((arity = arity') || (arity = max_int))
      then {yes=true ; name=name' ; arity=arity'}
      else env_pred_mem name arity env_tl
  | _ -> assert false

let rec env_term_mem name arity env = match env with 
  | [] -> {yes=false ; name="" ; arity=0}
  | Fun(name', arity', term_list) :: env_tl -> 
      if (name = name') && ((arity = arity') || (arity = max_int))
      then {yes=true ; name=name' ; arity=arity'}
      else env_term_mem name arity env_tl
  | _ -> assert false

let cons_nodup x list = if List.mem x list then list else x::list

let rec cat_nodup list list' = match list with
  | [] -> list'
  | hd::tl -> cons_nodup hd (cat_nodup tl list')

(* ---> prop correcte + list of undefined symbols*)
let aritycheck p env =
  let rec term_chk term env bound_list undef_list = match term with
    | Var(name) -> 
	if (List.mem name bound_list) 
	then (term , undef_list) 
	else let catch = env_term_mem name 0 (fst env) in
	  if catch.yes
	  then (Fun(name, 0, []) , undef_list)
	  else (term , (cons_nodup (name,0) undef_list))
    | Fun(name, arity, term_list) -> 
	let catch = env_term_mem name arity (fst env) in
	  if not catch.yes
	  then (term, (cons_nodup (name,arity) undef_list))
	  else if (catch.arity = List.length term_list)
	  then let res =
	    List.split (List.map 
			  (fun t -> term_chk t env bound_list []) term_list) in
	    (Fun(name, catch.arity, (fst res)) , 
	     (cat_nodup (List.flatten (snd res)) undef_list))
	  else (term , 
		(cons_nodup (name ^ "_skd",(List.length term_list)) undef_list))
  in
  let rec prop_chk prop env bound_list undef_list = match prop with 
    | True -> (True, undef_list)
    | False -> (False, undef_list)
    | Pred(name, arity, term_list) -> 
	let catch=(env_pred_mem name arity (snd env)) in
	  if not catch.yes 
	  then (prop, (cons_nodup (name,arity) undef_list))
	  else if (catch.arity = List.length term_list)
	  then let res =
	    List.split (List.map 
			  (fun t -> term_chk t env bound_list []) term_list) in
	    (Pred(name, catch.arity, (fst res)) , 
	     (cat_nodup (List.flatten (snd res)) undef_list))
	  else (prop , (cons_nodup (name ^ "_skd",(List.length term_list)) undef_list))
    | UProp(Neg,p) -> 
	let res = prop_chk p env bound_list undef_list in
	  (UProp(Neg,(fst res)) , (snd res))
    | BProp(p1,op,p2) -> 
	let res1 = (prop_chk p1 env bound_list []) in
	let res2 = (prop_chk p2 env bound_list []) in
	  (BProp((fst res1),op,(fst res2)) , 
	   (cat_nodup (cat_nodup (snd res1) (snd res2)) undef_list))
    | Quant(q,name,p) -> 
	let res = prop_chk p env (name::bound_list) undef_list in
	  (Quant(q, name, (fst res)) , (snd res))
  in prop_chk p env [] []

(*Substitution*)
let substitute var term prop = 
  let rec term_subs var term term' = 
    match term' with
      | Var(name) -> if name = var 
	then Inl term 
	else Inl (Var name)
      | Fun(name, arity, term_list) -> Inl term
  in
  let rec prop_subs var term prop = 
    match prop with 
      | True -> Inl True
      | False -> Inl False
      | Pred(name, arity, term_list) -> 
	  let f cont term' = (match cont with
				| Inl t_list -> 
				    (match term_subs var term term' with
				       | Inl t -> Inl (t::t_list)
				       | Inr m -> Inr m )
				| Inr m -> Inr m ) in
	    (match List.fold_left f (Inl []) term_list with
	       | Inl tl -> Inl (Pred (name, arity, tl))
	       | Inr m -> Inr m )
      | UProp(Neg,p0) -> 
	  (match prop_subs var term p0 with
	     | Inl p -> Inl (UProp (Neg,p))
	     | Inr m -> Inr m )
      | BProp(p1,op,p2) -> 
	  (match prop_subs var term p1 with
	     | Inl p -> (match prop_subs var term p2 with
			   | Inl p' -> Inl (BProp (p,op,p'))
			   | Inr m -> Inr m)
	     | Inr m -> Inr m )
      | Quant(q,name,p0) ->
	  (match prop_subs var term p0 with
	     | Inl p -> let term_fv = get_term_fv term [] [] in
		 if List.mem name term_fv then Inr (name^" is bound in this proposition.")
		 else Inl (Quant (q,name,p))
	     | Inr m -> Inr m )
  in prop_subs var term prop
       
