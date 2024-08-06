(*The proof language*)

open Core

(*Command errors and messages*)
type command_error = 
  | Name_clash of name list
  | Hyp_match of name
  | Not_trivial
  | Missing_con of string
  | Goal_exists of bool

class command_msg error =
object (self)
  inherit message
  val msg = error
  method to_string =
    match msg with
      | Name_clash [name] -> 
	  Format.sprintf "A symbol named %s is already defined. Be more imaginative !"
	  name
      | Name_clash name_list -> 
	  Format.sprintf "Symbols named %s are already defined. Be more imaginative !"
	  (string_of_list name_list)
      | Hyp_match name ->
	  Format.sprintf "%s is not in your list of hypothesis." name
      | Not_trivial -> 
	  "This is not trivial. Work some more."
      | Missing_con name -> 
	  Format.sprintf "I can't get no %s." name
      | Goal_exists true -> 
	  Format.sprintf "You cannot issue this command while a goal is being proved."
      | Goal_exists false -> 
	  Format.sprintf "Where is the goal ?"
end

(*A couple of definitions*)
type instruction =
  | Define of (name list*sort)
  | Disc (*discard environment*)
  | Gl of prop
  | Tac of (state -> cairn)
  | Ack of provers (*require acknowledgement from outside prover*)
  | Quit

type script = instruction


(**Commands**)

(*Signature definition*)
let define_sigel name sort cairn = 
  match cairn with
    | Subgoals (0,s) | Exception (_,s) when s.goals = [] -> 
	let sort_sym = get_sort_symbols sort [] in
	  (let rec check = function
	     | [] -> if List.mem_assoc name s.sign
	       then Exception (new command_msg (Name_clash [name]), s)
	       else Subgoals (0,{s with sign = (name,sort)::s.sign})
	     | x::xl -> 
		 (try if (List.assoc x s.sign = SProp)
		  then Exception (new type_msg (Prop_not_sort x),s)
		  else if List.mem_assoc name s.sign
		  then Exception (new command_msg (Name_clash [name]), s)
		  else check xl
		  with Not_found -> Exception (new type_msg (Undefined x),s) )
	   in check sort_sym
	  )
    | _ -> Exception (new command_msg (Goal_exists true),(get_state cairn))
	
let define_sigels def_list cairn = 
  List.fold_right (fun n c -> define_sigel n (snd def_list) c) (fst def_list) cairn
    
(*Goal declaration*)
let rec goal prop cairn = match cairn with
  | Subgoals (0,s) | Exception (_,s) when s.goals = [] -> 
      (match infer prop s.sign with
	 | Inl SProp -> Subgoals (1,{s with goals=[Goal([],[prop],[])]})
	 | Inl x -> Exception (new type_msg (Prop_kind (prop,x)),s)
	 | Inr m -> Exception (new type_msg m,s))
  | _ -> Exception (new command_msg (Goal_exists true),(get_state cairn))


(**Tactics**)

(*Check if a variable name is already used in the hypothesis or the
  signature*)
let check_variable var_list state =
  let conflicts = 
    List.filter (fun x -> (List.mem_assoc x (get_current_hyps state.goals)) 
		   || (env_mem x (get_current_env state.goals))
		   || (List.mem_assoc x state.sign)) var_list 
  in match conflicts with
    | [] -> None
    | names -> Some (Name_clash names)
	
let fresh state = 
  let rec fresh_rec n state = 
    let name = Format.sprintf "_%d" n in
    let conflicts = (check_variable [name] state) in
      if conflicts = None then name
      else fresh_rec (n+1) state
  in fresh_rec 0 state

(*An easy one to begin with: axiom.*)
let axiom s = match s.goals with
  | Goal (h,c,e)::r ->
      if (List.exists (fun x -> (snd x = False) || (List.mem (snd x) c)) h) 
      then Success {s with goals = r}
      else Exception (new command_msg (Not_trivial),s)
  | _ -> Exception (new command_msg (Goal_exists false),s)

(*Implication rules*)
let r_Imp x s = match s.goals with 
  | Goal (h,BProp(a,Imp,b)::c,e)::r -> 
      (match check_variable [x] s with
	 | None -> Subgoals (1,{s with goals=Goal (h@[x,a],b::c,e)::r})
	 | Some m -> Exception (new command_msg m,s))
  | Goal (h,c,e)::r -> Exception (new command_msg (Missing_con "\226\135\146"),s)
  | _ -> Exception (new command_msg (Goal_exists false),s)

let l_Imp x x' s = match s.goals with 
  | Goal (h,c,e)::r -> 
      (match check_variable [x'] s with
	 | None -> 
	     (try 
		let q = List.assoc x h in
		let h' = List.remove_assoc x h in
		  match q with
		    | BProp (a,Imp,b) -> Subgoals (2, {s with goals=Goal (h',a::c,e)::Goal (h'@[x',b],c,e)::r})
		    | _ -> Exception (new command_msg (Missing_con "\226\135\146"),s)
		  with Not_found -> Exception (new command_msg (Hyp_match x),s)
	     )
	 | Some m -> Exception (new command_msg m,s))
  | _ -> Exception (new command_msg (Goal_exists false),s)
      
(*Conjunction rules*)
let r_Conj s = match s.goals with 
  | Goal (h,BProp(a,Conj,b)::c,e)::r ->
      Subgoals (2,{s with goals=Goal (h,a::c,e)::Goal (h,b::c,e)::r})
  | _ -> Exception (new command_msg (Missing_con "\226\136\167"),s)

let l_Conj x x' x'' s = match s.goals with 
  | Goal (h,c,e)::r -> 
      (match check_variable [x';x''] s with 
	 | None -> 
	     (try
		let q = List.assoc x h in
		let h' = List.remove_assoc x h in
		  match q with 
		    | BProp (a,Conj,b) -> Subgoals (1, {s with goals=Goal (h'@[x',a ; x'',b],c,e)::r})
		    | _ -> Exception (new command_msg (Missing_con "\226\136\167"),s)
		  with Not_found -> Exception (new command_msg (Hyp_match x),s)
	     )
	 | Some m -> Exception (new command_msg m,s))
  | _ -> Exception (new command_msg (Goal_exists false),s)
      
(*Disjunction rules*)
let r_Disj_L s = match s.goals with
  | Goal (h,BProp (a,Disj,b)::c,e)::r -> Subgoals (1, {s with goals=Goal (h,a::b::c,e)::r})
  | _ -> Exception (new command_msg (Missing_con "\226\136\168"),s)

let r_Disj_R s = match s.goals with
  | Goal (h,BProp (a,Disj,b)::c,e)::r -> Subgoals (1, {s with goals=Goal (h,b::a::c,e)::r})
  | _ -> Exception (new command_msg (Missing_con "\226\136\168"),s)

let l_Disj x x' x'' s = match s.goals with 
  | Goal (h,c,e)::r -> 
      (match check_variable [x';x''] s with
	 | None ->
	     (try
		let q = List.assoc x h in
		let h' = List.remove_assoc x h in
		  match q with 
		    | BProp (a,Disj,b) -> Subgoals (2, {s with goals=Goal (h'@[x',a],c,e)::Goal (h'@[x'',b],c,e)::r})
		    | _ -> Exception (new command_msg (Missing_con "\226\136\168"),s)
		  with Not_found -> Exception (new command_msg (Hyp_match x),s)
	     )
	 | Some m -> Exception (new command_msg m,s))
  | _ -> Exception (new command_msg (Goal_exists false),s)
(*forge : replacer les deux sous-termes du \/ a l'endroit ou etait le \/ 
-- de meme pour toutes les regles ! => fonction replace 1 elt par 1 list*)
(*forge : integrer le système de génération de blazes : comment rendre 
un argument de fonction optionnel ?*)

(*Negation rules*)
let r_Neg x s = match s.goals with 
  | Goal (h,UProp (Neg,a)::c,e)::r -> r_Imp x s
  | _ -> Exception (new command_msg (Missing_con "\194\172"),s)

let l_Neg x s = match s.goals with 
  | Goal (h,c,e)::r -> 
      (try
	 let q = List.assoc x h in
	 let h' = List.remove_assoc x h in
	   match q with 
	     | UProp (Neg,a) -> Subgoals (1, {s with goals=Goal (h',a::c,e)::r})
	     | _ -> Exception (new command_msg (Missing_con "\194\172"),s)
	   with Not_found -> Exception (new command_msg (Hyp_match x),s)
      )
  | _ -> Exception (new command_msg (Goal_exists false),s)
      
(*Quantifier rules*)
let r_Forall s = match s.goals with 
  | Goal (h,Quant (Forall,(names,sort),prop)::c,e)::r -> 
      (match check_variable names s with
	 | None -> Subgoals (1, {s with goals=Goal (h,prop::c,(names,sort)::e)::r})
	 | Some m -> Exception (new command_msg m,s) )
  | _ -> Exception (new command_msg (Missing_con "\226\136\128"),s)
      
let l_Forall x t s = match s.goals with 
  | Goal (h,c,e)::r -> 
      (try
	 let q = List.assoc x h in
	 let h' = List.remove_assoc x h in
	   match q with 
	     | Quant (Forall,(names,sort),prop) -> 
		 (match term_infer t s.sign with
		    | Inl typ -> if not (eq_sort sort typ) 
		      then Exception (new type_msg (Type_mismatch (t,typ,sort)),s)
		      else (match substitute (List.hd names) t prop with
			      | Inl prop' -> Subgoals (1, {s with goals=Goal ((x,Quant (Forall,(List.tl names,sort),prop'))::h',c,e)::r})
			      | Inr m -> assert false )
		    | Inr m -> Exception (new type_msg m,s) )
	     | _ -> Exception (new command_msg (Missing_con "\226\136\128"),s)
	   with Not_found -> Exception (new command_msg (Hyp_match x),s)
      )
  | _ -> Exception (new command_msg (Goal_exists false),s)
      
let r_Exists t s = match s.goals with
  | Goal (h,Quant (Exists,(names,sort),prop)::c,e)::r -> 
      (match term_infer t s.sign with
	 | Inl typ -> if not (eq_sort sort typ) 
	   then Exception (new type_msg (Type_mismatch (t,typ,sort)),s)
	   else (match substitute (List.hd names) t prop with
		   | Inl prop' -> Subgoals (1, {s with goals=Goal (h,Quant (Exists,(List.tl names,sort),prop')::c,e)::r})
		   | Inr m -> assert false )
	 | Inr m -> Exception (new type_msg m,s) )
  | _ -> Exception (new command_msg (Missing_con "\226\136\131"),s)
      
let l_Exists x s = match s.goals with
  | Goal (h,c,e)::r ->
      (try
	 let q = List.assoc x h in
	 let h' = List.remove_assoc x h in
	   match q with 
	     | Quant (Exists,(names,sort),p) ->
		 (match check_variable names s with
		   | None -> Subgoals (1, {s with goals=Goal ((x,p)::h',c,(names,sort)::e)::r})
		   | Some m -> Exception (new command_msg m,s) )
	     | _ -> Exception (new command_msg (Missing_con "\226\136\131"),s)
	   with Not_found -> Exception (new command_msg (Hyp_match x),s)
      )
  | _ -> Exception (new command_msg (Goal_exists false),s)
	
(*Contraction rules*)
let r_Contr s =  match s.goals with 
  | Goal (h,a::c,e)::r ->
      Subgoals (1, {s with goals=Goal (h,a::a::c,e)::r})
  | _ -> Exception (new command_msg (Goal_exists false),s)

let l_Contr x x' s =  match s.goals with 
  | Goal (h,c,e)::r ->
      (match check_variable [x'] s with
	 | None -> Subgoals (1, {s with goals=Goal (h@[x',(List.assoc x h)],c,e)::r})
	 | Some m -> Exception (new command_msg m,s) )
  | _ -> Exception (new command_msg (Goal_exists false),s)

(*Cycling between multiple conclusions*)
let r_Switch s = match s.goals with
  | Goal (h,c,e)::r -> 
      let swap c =  match c with
	| [] -> assert false
	| a::l -> l@[a]
      in 
      let c' = (swap c) in
	Subgoals (0, {s with goals=Goal (h,c',e)::r})
  | _ -> Exception (new command_msg (Goal_exists false),s)

