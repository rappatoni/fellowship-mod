(*Option : use constructive / classical logic*)
let lj = ref true (* constructive by default *)
	   
type name = string
    
(*Deal with string lists*)
let rec string_of_list = function
  | [] -> ""
  | [s] -> s
  | s::sl ->  Format.sprintf "%s, %s" s (string_of_list sl)

(*Useful for the quantified variables weird list*)
let rec list_flatten = function
  | ([],s) -> []
  | (x::xn,s) -> (x,s)::(list_flatten (xn,s))

let env_mem x env = List.exists (fun (a,b) -> List.mem x a) env 

(*Allows for exception programming*)
type ('a,'b) sum = 
  | Inl of 'a
  | Inr of 'b
      
      
(**Objects**)

(*Sorts*)
type sort = 
  | SSet (*sort of new sorts*)
  | SProp (*sort of propositions*)
  | SSym of name
  | SArr of sort * sort

let rec string_of_sort = function
  | SSet -> "type"
  | SProp -> "bool"
  | SSym name -> name
  | SArr (sort,sort') -> 
      Format.sprintf "%s \226\134\146 %s" (string_of_sort sort) (string_of_sort sort')

(*Terms*)
type term = 
  | TSym of name
  | TApp of term * term

let rec string_of_term =  function
  | TSym name -> name
  | TApp (term,term') -> 
      Format.sprintf "%s(%s)" (string_of_term term) (string_of_term term')

(*Propositions*)
type prop = 
  | True | False
  | PSym of name
  | PApp of prop * term
  | UProp of unop * prop
  | BProp of prop * binop * prop
  | Quant of quantifier * (name list*sort) * prop
and unop = Neg
and binop = Imp | Conj | Disj
and quantifier = Exists | Forall

let rec string_of_prop = function
  | True -> "true"
  | False -> "false"
  | PSym name -> name
  | PApp (prop,term) -> 
      Format.sprintf "%s(%s)" (string_of_prop prop) (string_of_term term)
  | UProp (op,prop) -> 
      let ops = match op with
	| Neg -> "\194\172" 
      in  
	Format.sprintf "%s%s" ops (string_of_prop prop)
  | BProp (prop,op,prop') -> 
      let ops = match op with 
	  Imp -> "\226\135\146" 
	| Conj-> "\226\136\167"
	| Disj -> "\226\136\168" 
      in
	Format.sprintf "(%s %s %s)" (string_of_prop prop) ops (string_of_prop prop')
  | Quant (q,(names,sort),prop) -> 
      let quants = match q with
	| Exists -> "\226\136\131"
	| Forall -> "\226\136\128"
      in
	Format.sprintf "%s%s:%s.%s" quants (string_of_list names) (string_of_sort sort) (string_of_prop prop)

(*Errors*)
(* The virtual class *)
class virtual message = 
object (self)
  method virtual to_string : string
end

(*type-checker errors*)
type type_error = 
  | Undefined of name
  | Not_a_fun of term * sort
  | Not_a_pred of prop * sort
  | Prop_kind of prop * sort
  | Prop_not_sort of name
  | Fun_apply_err of term * sort * term * sort
  | Pred_apply_err of prop * sort * term * sort
  | Type_mismatch of term * sort * sort

class type_msg error =
object (self)
  inherit message
  val msg = error
  method to_string = 
    match msg with
      | Undefined (name) -> 
	  Format.sprintf "Symbol %s undefined" name
      | Not_a_fun (term,sort) -> 
	  Format.sprintf "Term %s of type %s is not a function, it cannot be applied."
	  (string_of_term term) (string_of_sort sort)
      | Not_a_pred (prop,sort) ->
	  Format.sprintf "Proposition %s of type %s is not a predicate, it cannot be applied."
	  (string_of_prop prop) (string_of_sort sort)
      | Prop_kind (prop,sort) ->
	  Format.sprintf "Proposition %s of type %s is not a boolean."
	  (string_of_prop prop) (string_of_sort sort)
      | Prop_not_sort name -> 
	  Format.sprintf "%s is a proposition, not a sort." name
      | Fun_apply_err (term,sort,term',sort') ->
	  Format.sprintf "Function %s of type %s cannot be applied to %s of type %s."
	  (string_of_term term) (string_of_sort sort)
	  (string_of_term term') (string_of_sort sort')
      | Pred_apply_err (prop,sort,term,sort') ->
	  Format.sprintf "Predicate %s of type %s cannot be applied to %s of type %s."
	  (string_of_prop prop) (string_of_sort sort)
	  (string_of_term term) (string_of_sort sort')
      | Type_mismatch (term, sort, sort') ->
	  Format.sprintf "Term %s of type %s does not match the type %s."
	  (string_of_term term) (string_of_sort sort) (string_of_sort sort')
end
	  
(* 
   - signatures are lists of symbol names associated with their sort
   - idem for environments, which are used to store implicitely quantified variables (cf right forall)
   - goals as a list of named hypothesis and a conclusion
   - a state pairs a goal list with a signature and an environment
   - the proof monad (or proof state, or cairn) guides the proof search
*)
type env = (name list*sort) list

type goal = Goal of (string * prop) list * prop list * env 

type state = 
    { goals: goal list;
      sign: (name*sort) list }

type proof_monad = 
  | Success of state
  | Subgoals of int * state
  | Exception of message * state

type cairn = proof_monad

(*Provers*)
type provers = 
  | Coq 
  | Pvs 


(**Functions**)

(*Monad contructor*)
let create_cairn () = Subgoals (0,{goals=[];sign=[]})
			
(*Monad destructors*)
let get_state = function
  | Success (s) -> s
  | Subgoals (n,s) -> s
  | Exception (m,s) -> s

let get_msg = function
  | Success (s) -> None
  | Subgoals (n,s) -> None
  | Exception (m,s) -> Some m

let isScs = function
  | Success (_) -> true
  | _ -> false

let isCptScs = function
  | Success s when s.goals = [] -> true
  | _ -> false

let isSub = function
  | Subgoals (_,_) -> true
  |_ -> false

let isExn = function
  | Exception (_,_) -> true
  | _ -> false

(*Decomposing the proof state*)
let get_current_hyps = function
  | Goal (h,c,e)::r -> h
  | _ -> []

let get_current_ccls = function
  | Goal (h,c,e)::r -> c
  | _ -> []

let get_current_env = function
  | Goal (h,c,e)::r -> e
  | _ -> []

(*Transform goals, goal lists and states into propositions*)
let rec prop_of_csq = function
  | [] -> False
  | [a] -> a
  | a::p' -> 
      if !lj then a
      else BProp (a,Disj,(prop_of_csq p'))
	
let rec prop_of_goal_rec h e p = match (h,e) with 
  | ([],[]) -> p
  | ([],x::xl) -> prop_of_goal_rec [] xl (Quant (Forall,x,p))
  | ((x,a)::h',e) -> prop_of_goal_rec h' [] (BProp (a,Imp,p))

let prop_of_goal (Goal (h,p,e)) = prop_of_goal_rec h e (prop_of_csq p) 

let rec prop_of_goals = function
  | [] -> True
  | g::s' -> BProp (prop_of_goal g,Conj,prop_of_goals s')

let prop_of_state s = prop_of_goals s.goals

(*Fancier functions*)
(*Sort equality*)
let rec eq_sort sort sort' = match sort with
  | SSet -> sort' = SSet
  | SProp -> sort' = SProp
  | SSym name -> 
      (match sort' with 
	 | SSet -> false
	 | SProp -> false
	 | SSym(name') -> name = name'
	 | SArr(_,_) -> false)
  | SArr (s,s') -> 
      (match sort' with
	 | SSet -> false
	 | SProp -> false
	 | SSym(_) -> false
	 | SArr(s'',s''') -> 
	     (match eq_sort s s'' with
		| true -> eq_sort s' s'''
		| false -> false))

(*Output free variables of a term*)
let rec get_term_fv t sign env free = 
  match t with 
    | TSym name -> 
	if (List.mem name sign) 
	  || (List.mem name env) 
	  || (List.mem name free) 
	  (*if the name is bound by a quantifier or in the signature
	    or already listed as free*)
	then free else name::free
    | TApp (term,term') -> 
	let free' = (get_term_fv term sign env free) 
	in get_term_fv term' sign env free'
	     
(*Output free variables of a proposition*)
let get_prop_fv p sign = 
  let rec get_fv_rec p sign env free = match p with 
    | True -> free
    | False -> free
    | PSym name -> free
    | PApp (prop,term) -> get_term_fv term sign env free
    | UProp (Neg,prop) -> get_fv_rec prop sign env free
    | BProp (prop,op,prop') -> 
	let free' = (get_fv_rec prop sign env free) in get_fv_rec prop' sign env free'
    | Quant (q,(names,sort),prop) -> get_fv_rec prop sign (names@env) free
  in get_fv_rec p sign [] []

(*Output symbol names of a sort*)
let rec get_sort_symbols s ret = match s with
  | SSet | SProp -> ret
  | SSym name -> if List.mem name ret then ret else name::ret
  | SArr (sort, sort') -> let ret' = get_sort_symbols sort ret in 
      get_sort_symbols sort' ret'

(*Output symbol names of a term*)
let rec get_term_symbols t sign ret =  match t with 
  | TSym name -> 
      if (not(List.mem name sign)) || (List.mem name ret) 
      then ret else name::ret
  | TApp (term,term') -> 
      let ret'= get_term_symbols term sign ret in 
	get_term_symbols term' sign ret'
	  
(*Output symbol names of a proposition*)
let rec get_prop_symbols p sign ret = match p with 
  | True -> ret
  | False -> ret
  | PSym name -> if (List.mem name ret) then ret else name::ret
  | PApp (prop,term) -> 
      let ret' = (get_prop_symbols prop sign ret) in 
	get_term_symbols term sign ret'
  | UProp (Neg,prop) -> get_prop_symbols prop sign ret
  | BProp (prop,op,prop') -> 
      let ret' = (get_prop_symbols prop sign ret) in 
	get_prop_symbols prop' sign ret'
  | Quant (q,(names,sort),prop) -> get_prop_symbols prop sign ret
      
(*Substitution*)
let substitute name term proposition =
  let rec term_subst name t t' = match t' with
    | TSym name' -> if name = name' then Inl t else Inl (TSym name')
    | TApp (term,term') -> 
	(match term_subst name t term with
	   | Inl trm -> 
	       (match term_subst name t term' with
		  | Inl trm' -> Inl (TApp (trm,trm'))
		  | Inr m -> Inr m )
	   | Inr m -> Inr m )
  in 
  let rec prop_subst name t p = match p with
    | True -> Inl True
    | False -> Inl False
    | PSym name -> Inl (PSym name)
    | PApp (prop,term) -> 
	(match prop_subst name t prop with
	   | Inl prp -> 
	       (match term_subst name t term with
		  | Inl trm -> Inl (PApp (prp,trm))
		  | Inr m -> Inr m )
	   | Inr m -> Inr m )
    | UProp (Neg,prop) -> 
	(match prop_subst name t prop with
	   | Inl prp -> Inl (UProp (Neg,prp))
	   | Inr m -> Inr m )
    | BProp (prop,op,prop') -> 
	(match prop_subst name t prop with
	   | Inl prp -> 
	       (match prop_subst name t prop' with
		  | Inl prp' -> Inl (BProp (prp,op,prp'))
		  | Inr m -> Inr m )
	   | Inr m -> Inr m )
    | Quant(q,(names,sort'),prop) ->
	(match prop_subst name t prop with 
	   | Inl prp -> Inl (Quant (q,(names,sort'),p))
	   | Inr m -> Inr m )
  in prop_subst name term proposition

(*Typechecking*)
let rec term_infer t sign = match t with 
  | TSym name -> 
      (try Inl (List.assoc name sign)
       with Not_found -> Inr (Undefined name))
  | TApp (term,term') -> 
      (match term_infer term sign with
	 | Inl (SArr (sort,sort')) -> 
	     (match term_infer term' sign with
		| Inl sort'' -> if eq_sort sort sort'' 
		  then Inl sort'
		  else Inr (Fun_apply_err (term,SArr (sort,sort'),term',sort''))
		| Inr m -> Inr m )
	 | Inl sort -> Inr (Not_a_fun (term,sort))
	 | Inr m -> Inr m )
      
let infer proposition signature = 
  let rec prop_infer p sign = match p with
    | True -> Inl SProp
    | False -> Inl SProp
    | PSym name -> 
	(try Inl (List.assoc name sign)
	 with Not_found -> Inr (Undefined name))
    | PApp (prop,term) -> 
	(match prop_infer prop sign with
	   | Inl (SArr (sort,sort')) -> 
	       (match term_infer term sign with
		  | Inl sort'' -> if eq_sort sort sort'' 
		    then Inl sort'
		    else Inr (Pred_apply_err (prop,SArr (sort,sort'),term,sort''))
		  | Inr m -> Inr m )
	   | Inl sort -> Inr (Not_a_pred (prop,sort))
	   | Inr m -> Inr m )
    | UProp (Neg,prop) -> 
	(match prop_infer prop sign with
	   | Inl SProp -> Inl SProp
	   | Inl sort -> Inr (Prop_kind (prop,sort))
	   | Inr m -> Inr m)
    | BProp (prop,op,prop') -> 
	(match prop_infer prop sign with
	   | Inl SProp -> 
	       (match prop_infer prop' sign with
	          | Inl SProp -> Inl SProp
		  | Inl sort' -> Inr (Prop_kind (prop',sort'))
	          | Inr m -> Inr m ) 
	   | Inl sort -> Inr (Prop_kind (prop,sort))
	   | Inr m -> Inr m )
    | Quant (q,(names,sort),prop) -> 
	(match prop_infer prop ((list_flatten (names,sort))@sign) with
	   | Inl SProp -> Inl SProp
	   | Inl sort  -> Inr (Prop_kind (prop,sort))
	   | Inr m -> Inr m )
  in prop_infer proposition signature

