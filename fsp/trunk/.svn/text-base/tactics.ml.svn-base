(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
*)

open Core
open Format
open Interpreter

(* * DECLARATIONS * *)

type arg =
  | OnTheLeft
  | OnTheRight
  | Ident of string
  | Formula of prop
  | Expression of term
  | Labeled_prop of string list * prop
  | Labeled_sort of string list * sort
  | Prover of provers

let pretty_arg = function
  | OnTheLeft -> "left"
  | OnTheRight -> "right"
  | Ident s -> s
  | Formula p -> pretty_prop p
  | Expression t -> pretty_term t
  | Labeled_prop ([s],p) -> sprintf "%s : %s" s (pretty_prop p)
  | Labeled_prop (_,p) -> assert false
  | Labeled_sort (sl,p) -> sprintf "%s : %s" (pretty_comma_list sl) (pretty_sort p)
  | Prover p -> pretty_prover p

let pretty_arg_list al = (pretty_comma_list (List.map pretty_arg al))

type tactic = 
  (* primitive tactics *)
  | Axiom 
  | Cut 
  | Elim 
  (* tacticals *)
  | Idtac
  (* derived tactics *)
  | Focus 
  | Elim_In 
  | Contraction 
  | Weaken
  
type tactical = 
  | TPlug of tactic * (arg list) * Lexing.position
  | Then of tactical * tactical * Lexing.position
  | Thens of tactical * (tactical list) * Lexing.position

let pretty_tactic = function
  | Axiom -> "axiom"
  | Cut -> "cut"
  | Elim -> "elim"
  | Idtac -> "idtac"
  | Focus -> "focus"
  | Elim_In -> "elim in"
  | Contraction -> "contraction"
  | Weaken -> "weaken"

let rec pretty_tactical = function
  | TPlug (t,args,pos) -> 
      sprintf "%s %s" (pretty_tactic t) (pretty_arg_list args)
  | Then (t1,t2,pos) ->
      sprintf "%s ; %s" (pretty_tactical t1) (pretty_tactical t2)
  | Thens (t,t_list,pos) ->
      sprintf "%s ; [%s]" (pretty_tactical t)
       (pretty_pipe_list (List.map pretty_tactical t_list))


(* * MESSAGES * *)

type tactic_error = 
  | Name_clash of string list
  | Hyp_match of string
  | Hyp_ccl_match of string
  | Not_trivial
  | Goal_exists of bool
  | No_switch
  | Need_args of string
  | No_args
  | Minimal_undef
  | Elim_coward
  | No_elim
  | No_thm of string
  | ContractionNotAllowed
  | Arg_count
  | Instr_misuse

class tactic_msg ?loc error =
object (self)
  inherit message loc
  val msg = error
  method to_string =
    match msg with
      | Name_clash [name] -> 
	  sprintf "A symbol named %s is already defined. Be more imaginative !"
	  name
      | Name_clash name_list -> 
	  sprintf "Symbols named %s are already defined. Be more imaginative !" 
	  (pretty_comma_list name_list)
      | Hyp_match name ->
	  sprintf "%s is not in your list of hypothesis." name
      | Hyp_ccl_match name ->
	  sprintf "%s is neither in your hypothesis nor in your conclusion." 
	  name
      | Not_trivial -> 
	  "This is not trivial. Work some more."
      | Goal_exists true -> 
	  "You cannot issue this command while a goal is being proved."
      | Goal_exists false -> 
	  "Where is the goal ?"
      | No_switch -> 
	  "There is only one goal, impossible to switch."
      | Need_args args ->
	  sprintf "You need to specify %s. " args
      | No_args ->
	  "You don't need to specify any argument."
      | Minimal_undef ->
	  "Nothing to do in minimal logic."
      | Elim_coward ->
	  "Cowardly refusing to do something complex to obtain back the same goal."
      | No_elim -> 
	  "Nothing to elim."
      | No_thm name -> 
	  sprintf "%s is neither a proved theorem nor an axiom." name
      | ContractionNotAllowed ->
         "Contraction of a conclusion not allowed in intuitionistic logic."
      | Arg_count -> 
	  "Not the right number of tactics."
      | Instr_misuse ->
	  "Tut tut tut. Finish the proof before issuing instructions!"
end


(* * TACTICS * *)

(*Check if a variable name is already used in the hypothesis or the environment
  or the conclusion*)
let check_variable var_list (h,c,context) =
  let conflicts = 
    (*CSC: we ignore conflicts with weakened variables. Moreover, we cannot   *)
    (*CSC: override a name in the context. Is this what we always want to do? *)
    List.filter (fun x -> (List.exists (function x',_,w -> x'=x && w) h) 
		    || (List.exists (function x',_,w -> x'=x && w) c) 
		    || (Coll.mem x context)
                    || List.length (List.filter (fun x' -> x'=x) var_list) <> 1
                ) var_list
  in match conflicts with
    | [] -> None
    | names -> Some (Name_clash names)

let fresh_name var_list (h,c,context) =
 assert (check_variable var_list (h,c,context) = None);
 let rec aux n =
  let id = "H" ^ string_of_int n in
   if check_variable (id::var_list) (h,c,context) = None then id
   else aux (n + 1)
 in
  aux 1
	
(* RSuccess (l,pt) means that the new goals l have been generated and
   that the new proof term is pt *)
type result_monad =
 | RSuccess of ((metaid * goal) list) * proof_t3rm
 | RException of message

(* split_list n l = (l_before, i, l_after)
   iff  l = l_before @ [i] @ l_after
   and i is the n-th element of l where n is 1-based *)
let split_list n l =
 let rec aux =
  function
   | (0,_)
   | (_,[]) -> assert false
   | (1,he::tl) -> [],he,tl
   | (n,he::tl) ->
       let before,i,after = aux ((n - 1),tl) in
        he::before,i,after
 in
  let before_rev, i, after = aux (n,l) in
   List.rev before_rev, i, after

(* Applying an internal tactic and localizing its exceptions *)
let apply_tac (tac,loc) state =
 match tac state with
    RException msg -> RException (msg#localize loc)
  | RSuccess _ as res -> res

(* Applying a localized tactic *)
let apply__tac localized_tac m =
 match m, (get_state m) with 
  | Idle (m,s), _ -> Idle (Some (new tactic_msg (Goal_exists false)),s)
  | _, s when s.goals <> [] ->
      let g_before, (id,g), g_after = split_list s.index s.goals in
      let context = coll_add_list s.sign g.env in
       (match apply_tac localized_tac (id,g,context,s.thms,s.pt) with
           RException msg -> Exception (msg,s)
         | RSuccess (l,pt') ->
            (match List.length l with
                0 ->
                 let index' =
                  match g_before, g_after with
                     _, _::_ -> s.index
                   | _::_,[] -> 1
                   | [],[] -> 0
                 in
                  Success {s with index=index';goals=g_before@g_after;pt=pt'}
              | n -> Subgoals (n,{s with goals=g_before@l@g_after;pt=pt'})))
  | _, s -> Exception(new tactic_msg (Goal_exists false),s)


(* * PRIMITIVE TACTICS (axiom, cut, elim) * *)

(*An easy one to begin with*)
let axiom args (id,g,context,thms,pt) =
  (match args with
    | [Ident x] -> 
      begin
	try if (snd g.active) = 
	 match fst g.active with 
	    RightHandSide -> (search x g.hyp thms)
          | LeftHandSide ->
             let _,p,_ = List.find (function (x',p,w) -> x=x' && w) g.ccl in p
	then 
	 let pt' =
	  match (fst g.active) with
	     RightHandSide -> instantiate_t3rm id (Hyp x) pt
	   | LeftHandSide -> instantiate_context id (Concl x) pt
	 in
	  RSuccess ([],pt')
	else RException (new tactic_msg Not_trivial)
	with Not_found -> RException (new tactic_msg (Hyp_ccl_match x)) 
      end
    | _ -> RException (new tactic_msg (Need_args "one identifier")) )

(* Cut rule *)
let cut args (id,g,context,thms,pt) =
  let id1 = new_meta id 1 in
  let id2 = new_meta id 2 in
  match args with
     [(Formula p) ; Ident x] ->
      begin
       match check_variable [x] (g.hyp,g.ccl,context), fst g.active with 
       | Some m, _ -> RException (new tactic_msg m)
       | None, RightHandSide -> 
           (match prop_infer p context with
            | Inl SProp ->
	       let c' = if !lj then [] else (x,(snd g.active),true)::g.ccl in
	         RSuccess(
	          [(id1, {g with ccl=c';active=(RightHandSide,p)});
	           (id2, {g with ccl=(x,(snd g.active),true)::g.ccl;active=(LeftHandSide,p)})] ,
	          instantiate_t3rm id
	           (Mu (x, (snd g.active), Play (TermMeta id1, ContextMeta id2))) pt)
            | Inl x -> RException (new type_msg (Prop_kind (p,x)))
            | Inr m -> RException (new type_msg m) )
       | None, LeftHandSide -> 
           (match prop_infer p context with 
            | Inl SProp ->
	       let c' = if !lj then [] else g.ccl in
	         RSuccess(
	          [(id1,{g with hyp=(x,(snd g.active),true)::g.hyp;ccl=c'; 
	           active=(RightHandSide,p)}) ;
	           (id2,{g with hyp=(x,(snd g.active),true)::g.hyp; 
	           active=(LeftHandSide,p)})] ,
	          instantiate_context id
	           (MuTilde (x, (snd g.active), Play (TermMeta id1, ContextMeta id2))) pt)
            | Inl x -> RException (new type_msg (Prop_kind (p,x)))
            | Inr m -> RException (new type_msg m) )
      end
   | _ ->  RException (new tactic_msg (Need_args "a proposition"))

(* Elim rule (left or right) *)
let rec elim args (id,g,context,thms,pt) = 
 match g.active with
  (* Right Impl *)
  | RightHandSide, BProp(a,Imp,b) ->
      (match args with
        | [Ident x] ->
	    (match check_variable [x] (g.hyp,g.ccl,context) with 
	      | Some m -> RException (new tactic_msg m)
	      | None -> let id1 = new_meta id 1 in
		  RSuccess(
		  [id1,{g with hyp=(x,a,true)::g.hyp;active=(RightHandSide,b)}],
		  instantiate_t3rm id (Lambda (x,a,TermMeta id1)) pt) )
        | _ -> RException (new tactic_msg (Need_args "an identifier")) )
  (* Left Impl *)
  | LeftHandSide, BProp(a,Imp,b) ->
     let id1 = new_meta id 1 in
     let id2 = new_meta id 2 in
     let c' = if !lj then [] else g.ccl in
      RSuccess(
       [(id1,{g with ccl=c'; active=(RightHandSide,a)}) ;
        (id2,{g with active=(LeftHandSide,b)})] ,
       instantiate_context id (Cons (TermMeta id1, ContextMeta id2)) pt)
  (* Right Minus *)
  | RightHandSide, BProp(a,Minus,b) ->
     let id1 = new_meta id 1 in
     let id2 = new_meta id 2 in
     (*CSC: interesting open question *)
     if !minimal then print_string "Warning: in minimal logic (A - B) is no longer equivalent to (A /\\ ~B). You will probably get stuck." ;
      RSuccess(
       [(id1,{g with active=(LeftHandSide,b)}) ;
        (id2,{g with active=(RightHandSide,a)})] ,
       instantiate_t3rm id (Cons' (ContextMeta id1, TermMeta id2)) pt)
  (* Left Minus *)
  | LeftHandSide, BProp(a,Minus,b) ->
      (match args with
          [Ident x] ->
	    (match check_variable [x] (g.hyp,g.ccl,context) with
	      | Some m -> 
		  RException (new tactic_msg m)
	      | None ->
		  let id1 = new_meta id 1 in
		  let c' = if !lj then [] else g.ccl in
		    RSuccess(
		    [id1,{g with ccl=(x,b,true)::c';active=(LeftHandSide,a)}] ,
		    instantiate_context id (Lambda' (x,b,ContextMeta id1)) pt) )
        | _ -> RException (new tactic_msg (Need_args "an identifier")) )
  (* Right Conj *)
  | RightHandSide, BProp(a,Conj,b) ->
     let id1 = new_meta id 1 in
     let id2 = new_meta id 2 in
      RSuccess(
       [(id1,{g with active=(RightHandSide,a)}) ;
        (id2,{g with active=(RightHandSide,b)})] ,
       instantiate_t3rm id (TermsPair (TermMeta id1, TermMeta id2)) pt)
  (* Left Conj *)
  | LeftHandSide, BProp(a,Conj,b) ->
      (match args with
          [Ident x; Ident y; Formula p] ->
	   (match check_variable [x;y] (g.hyp,g.ccl,context), prop_infer p context with
	      | Some m, _ -> 
		  RException (new tactic_msg m)
	      | None, Inl SProp ->
		  let id1 = new_meta id 1 in
		  let id2 = new_meta id 2 in
		  let h' = (x,a,true)::(y,b,true)::g.hyp in
		  let c' = if !lj then [] else g.ccl in
		   RSuccess (
		    [(id1,{g with hyp=h'; ccl=c'; active=(RightHandSide,p)}) ;
		     (id2,{g with hyp=h'; active=(LeftHandSide,p)})] ,
		    instantiate_context id
		     (DestructTermsPair (x,a,y,b,
		       Play (TermMeta id1,ContextMeta id2))) pt) 
	      | _, Inl x -> RException (new type_msg (Prop_kind (p,x)))
	      | _, Inr m -> RException (new type_msg m) )
        | _ -> RException (new tactic_msg (Need_args "two identifers and a proposition")) )
  (* Right Disj *)
  | RightHandSide, BProp(a,Disj,b) ->
     let id1 = new_meta id 1 in
     (match args with
         [OnTheLeft] ->
           RSuccess (
	    [id1,{g with active=(RightHandSide,a)}] ,
            instantiate_t3rm id (Left (TermMeta id1)) pt)
       | [OnTheRight] ->
           RSuccess (
	    [id1,{g with active=(RightHandSide,b)}] ,
            instantiate_t3rm id (Right (TermMeta id1)) pt)
      | _ -> 
	RException (new tactic_msg (Need_args "either \"left\" or \"right\"")) )
  (* Left Disj *)
  | LeftHandSide, BProp(a,Disj,b) ->
     let id1 = new_meta id 1 in
     let id2 = new_meta id 2 in
      RSuccess (
       [(id1,{g with active=(LeftHandSide,a)}) ;
        (id2,{g with active=(LeftHandSide,b)})] ,
       instantiate_context id
         (ContextsPair (ContextMeta id1, ContextMeta id2)) pt)
  (* Right Negation *)
  | RightHandSide, UProp(Neg,a) ->
     let id1 = new_meta id 1 in
      if !minimal then
       elim args (id,{g with active=(RightHandSide,BProp(a,Imp,False))},context,thms,pt)
      else
       (*CSC: this version corresponds to an implicit delta-expansion step from
         ~A to A -> false
       let y = fresh_name () in
       let z = fresh_name () in
       RSuccess(
        [id1,{g with active=(LeftHandSide,a)}],
        instantiate_t3rm id
         (Lambda (y,a,Mu (z,False,Play(Hyp y,ContextMeta id1)))) pt)
*)
       (*CSC: this is slightly better: ~A is still expanded to A -> false, but
         a top-down conversion is made explicit by using the Mu *)
       let x = fresh_name [] (g.hyp,g.ccl,context) in
       let y = fresh_name [x] (g.hyp,g.ccl,context) in
       let z = fresh_name [x; y] (g.hyp,g.ccl,context) in
       let ccl' =
        if !lj then g.ccl else (x,UProp(Neg,a),false)::(z,False,false)::g.ccl in
       RSuccess(
        [id1,{g with ccl = ccl' ;
                     hyp = (y,a,false)::g.hyp ;
                     active=(LeftHandSide,a)}],
        instantiate_t3rm id
         (Mu (x,UProp(Neg,a),Play (Lambda (y,a,Mu (z,False,Play(Hyp y,ContextMeta id1))),Concl x))) pt)
  (* Left Negation *)
  | LeftHandSide, UProp(Neg,a) ->
     if !minimal then
      elim args (id,{g with active=(LeftHandSide,BProp(a,Imp,False))},context,thms,pt)
     else
      let id1 = new_meta id 1 in
      let c' = if !lj then [] else g.ccl in
       (*CSC: this version corresponds to an implicit delta-expansion step from
         ~A to A -> false:
       RSuccess(
        [id1,Goal(h,c',(RightHandSide,a))],
        instantiate_context id (Cons (TermMeta id1, False_eliminator)) pt)
       *)
       (*CSC: this is slightly better: ~A is still expanded to A -> false, but
         a bottom-up conversion is made explicit by using the MuTilde *)
       let x = fresh_name [] (g.hyp,g.ccl,context) in
       RSuccess(
        [id1,{g with ccl=c';
                     hyp = (x,UProp(Neg,a),false)::g.hyp;
                     active=(RightHandSide,a)}],
        instantiate_context id
         (MuTilde (x,UProp(Neg,a),Play(Hyp x,(Cons (TermMeta id1, False_eliminator))))) pt)
  (* Right Forall *) 
  | RightHandSide, Quant(Forall,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with 
	| [] -> 
	  (match check_variable names (g.hyp,g.ccl,context) with
	    | None ->
	       let pt' =
		instantiate_t3rm id
		 (List.fold_right
		   (fun name target -> LambdaFO (name,sort,target))
		   names (TermMeta id1)) pt in
	       RSuccess (
		[id1,{g with active=(RightHandSide,a); 
		  env=(list_flatten (names,sort))@g.env}] ,
		pt')
	    | Some m -> 
		RException (new tactic_msg m) )
	| [Ident x] -> 
	  (match check_variable [x] (g.hyp,g.ccl,context) with
	    | None -> 
		let a' = prop_subst (List.hd names) (TSym x) a in
		let names' = List.tl names in
		let a'' = if names' = [] then a' 
		  else Quant (Forall, (names',sort),a') in
		RSuccess (
		  [id1, {g with active=(RightHandSide,a''); 
		   env=(x,sort)::g.env}] ,
		  instantiate_t3rm id (LambdaFO (x,sort,TermMeta id1)) pt )
	    | Some m -> RException (new tactic_msg m) )
	| _ -> RException (new tactic_msg (Need_args "at most one identifier")) )
  (* Left Forall *)
  | LeftHandSide, Quant(Forall,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with
	| [Expression t] ->
            (match term_infer t context with
	      | Inl typ -> 
		  if not (eq_sort sort typ)
		  then RException (new type_msg (Type_mismatch (t,typ,sort)))
		  else 
		    let a' = prop_subst (List.hd names) t a in
                    let names' = List.tl names in
                    let f =
		      if names' = [] then a' 
		      else Quant(Forall,(names',sort),a') in
                    RSuccess (
		       [id1,{g with active=(LeftHandSide,f)}] ,
                       instantiate_context id (ConsFO (t, ContextMeta id1)) pt)
	      | Inr m -> RException (new type_msg m) )
	| _ -> RException (new tactic_msg (Need_args "a term")) )
  (* Right Exists *)
  | RightHandSide, Quant(Exists,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with
	| [Expression t] ->
            (match term_infer t context with
	      | Inl typ -> 
		  if not (eq_sort sort typ)
		  then RException (new type_msg (Type_mismatch (t,typ,sort)))
		  else 
		    let a' = prop_subst (List.hd names) t a in
		    let names' = List.tl names in
                    let f =
		      if names' = [] then a' 
		      else Quant(Exists,(names',sort),a') in
                    RSuccess (
		      [id1,{g with active=(RightHandSide,f)}] ,
		      instantiate_t3rm id
			(TermsPairFO ((List.hd names,f), t, TermMeta id1)) pt)
	      | Inr m -> RException (new type_msg m) )
	| _ -> RException (new tactic_msg (Need_args "a term")) )
  (* Left Exists *)
  | LeftHandSide, Quant(Exists,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with 
	| [] -> 
	  (match check_variable names (g.hyp,g.ccl,context) with
	    | None ->
	       let pt' =
	        instantiate_context id
	         (List.fold_right
	           (fun name target -> DestructTermsPairFO (name,sort,target))
	           names (ContextMeta id1))
	         pt
	       in
	       RSuccess (
		[id1,{g with active=(LeftHandSide,a); 
		  env=(list_flatten (names,sort))@g.env}],
		pt')
	    | Some m -> 
		RException (new tactic_msg m) )
	| [Ident x] -> 
	  (match check_variable [x] (g.hyp,g.ccl,context) with
	    | None -> 
	       let a' = prop_subst (List.hd names) (TSym x) a in
	       let pt' =
	        instantiate_context id
	         (DestructTermsPairFO (x,sort,ContextMeta id1)) pt
               in
                let names' = List.tl names in
                let a'' = if names' = [] then a' 
                  else Quant (Exists, (names',sort),a') in
               RSuccess (
                [id1,{g with active=(LeftHandSide,a''); env=(x,sort)::g.env}],
                pt')
	    | Some m -> RException (new tactic_msg m) )
	| _ -> RException (new tactic_msg (Need_args "at most one identifier"))
      )
  (* Right True *)
  | RightHandSide, True ->
     RSuccess ([],instantiate_t3rm id True_constructor pt)
  (* Left False *)
  | LeftHandSide, False ->
     if !minimal then
      RException (new tactic_msg Minimal_undef)
     else
      RSuccess ([],instantiate_context id False_eliminator pt)
  (* Nothing to do *)
  | LeftHandSide, True
  | RightHandSide, False ->
     RException (new tactic_msg Elim_coward)
  | _, PSym _ | _, PApp _ -> RException (new tactic_msg No_elim)

(* * TACTICALS and AUXILIARY TACTICS * *)

exception Toss of message

let id_tac args (id,g,context,thms,pt) =
  match args with
   | [] -> RSuccess ([id,g],pt)
   | _ -> RException (new tactic_msg No_args)

let then_ t1 t2 (id,g,context,thms,pt) = 
 match apply_tac t1 (id,g,context,thms,pt) with
    RException msg -> RException msg
  | RSuccess (lr,pt') ->
     try
      let goals, pt =
       List.fold_left (fun (goals,pt) (id,g) ->
         match apply_tac t2 (id,g,context,thms,pt) with
            RException msg -> raise (Toss msg)
          | RSuccess (goals',pt') -> goals @ goals', pt'
       ) ([],pt') lr
      in RSuccess (goals,pt)
     with
      Toss msg -> RException msg

let thens t1 lt (id,g,context,thms,pt) =
 match apply_tac t1 (id,g,context,thms,pt) with
    RException msg -> RException msg
  | RSuccess (lr,pt') ->
     try
      let goals, pt =
       List.fold_left2 (fun (goals,pt) tac (id,g) ->
         match apply_tac tac (id,g,context,thms,pt) with
            RException msg -> raise (Toss msg)
          | RSuccess (goals',pt') -> goals @ goals', pt'
       ) ([],pt') lt lr
      in RSuccess (goals,pt)
     with
        Invalid_argument ("List.fold_left2") ->
         RException (new tactic_msg Arg_count) 
      | Toss msg -> RException msg


(* * DERIVED TACTICS * *)

let smart_axiom args (id,g,context,thms,pt) =
  (match args with
    | [] -> 
      (let l = match (fst g.active) with 
      RightHandSide -> g.hyp | LeftHandSide -> g.ccl in
      try
       (* smart_axiom does not detect weakened hypotheses/conclusions *)
       (*CSC: is this what we always want? do you want to give a warning *)
       (*CSC: to the user? how?                                          *)
       let decl,_,_ =
        List.find (function (_,p,w) -> (snd g.active) = p && w) l in
       axiom [Ident decl] (id,g,context,thms,pt) 
      with
       Not_found -> RException (new tactic_msg Not_trivial) )
    | [Ident _] -> axiom args (id,g,context,thms,pt)
    | _ -> RException (new tactic_msg (Need_args "at most one identifier")))

let focus args (id,g,context,thms,pt) =
  match args with 
    | [Ident ident ; Ident ident'] -> 
       (try
	let q = search ident g.hyp thms in
	  match check_variable [ident'] (g.hyp,g.ccl,context) with
	    | Some m -> RException (new tactic_msg m)
	    | None -> thens (cut [Formula q; Ident ident'], Lexing.dummy_pos) [axiom [Ident ident],Lexing.dummy_pos ; id_tac [],Lexing.dummy_pos] (id,g,context,thms,pt)
	with Not_found ->
	 try
	  let _,q,_ = List.find (function (ident',_,w) -> ident=ident' && w) g.ccl in
	    match check_variable [ident'] (g.hyp,g.ccl,context) with 
	      | Some m -> RException (new tactic_msg m)
	      | None -> thens (cut [Formula q; Ident ident'],Lexing.dummy_pos) [id_tac [],Lexing.dummy_pos ; axiom [Ident ident],Lexing.dummy_pos] (id,g,context,thms,pt)
	 with Not_found -> RException (new tactic_msg (Hyp_ccl_match ident)) )
    | _ -> RException (new tactic_msg (Need_args "two identifiers: the name of the target formula and a new name for the current formula"))

let elim_in args details = 
  match args with 
    | Ident ident :: Ident ident' :: argl ->
	thens (focus [Ident ident ; Ident ident'],Lexing.dummy_pos) 
	      [elim argl,Lexing.dummy_pos] details
    | _ -> RException (new tactic_msg (Need_args "two identifiers: the name of the target formula and a new name for the current formula"))

let contraction args ((id,g,context,thms,pt) as details) =
  match args with
    | [Ident ident] ->
       if fst g.active = RightHandSide then
	if !lj then RException (new tactic_msg (ContractionNotAllowed))
	else
	 thens (cut [Formula (snd g.active); Ident ident],Lexing.dummy_pos)
	  [id_tac [],Lexing.dummy_pos ; axiom [Ident ident],Lexing.dummy_pos] details
       else
	thens (cut [Formula (snd g.active); Ident ident],Lexing.dummy_pos)
	 [axiom [Ident ident],Lexing.dummy_pos; id_tac [],Lexing.dummy_pos] details
    | _ -> RException (new tactic_msg (Need_args "an identifier"))

let weaken args ((id,g,context,thms,pt) as details) =
  match args with
    | [Ident ident] ->
        let rec aux =
         function
            [] -> raise Not_found
          | (x,p,w)::tl when x=ident && w -> (x,p,false)::tl
          | decl::tl -> decl::(aux tl)
        in
         let id1 = new_meta id 1 in
	 let pt' =
	  match (fst g.active) with
	     RightHandSide -> instantiate_t3rm id (TermMeta id1) pt
	   | LeftHandSide -> instantiate_context id (ContextMeta id1) pt in
         begin
          try
           let hyp' = aux g.hyp in RSuccess ([id1,{g with hyp = hyp'}], pt')
          with Not_found -> try
           let ccl' = aux g.ccl in RSuccess ([id1,{g with ccl = ccl'}], pt')
          with Not_found ->
           RException (new tactic_msg (Hyp_ccl_match ident))
         end
    | _ -> RException (new tactic_msg (Need_args "an identifier"))

(* * STRAP * *)

let jack_tactical tactical cairn = 
  let rec multiplex = function
    | TPlug (Axiom,args,pos) -> smart_axiom args , pos
    | TPlug (Cut,args,pos) -> cut args , pos
    | TPlug (Elim,args,pos) -> elim args , pos
    | TPlug (Idtac,args,pos) -> id_tac args , pos
    | TPlug (Focus,args,pos) -> focus args , pos
    | TPlug (Elim_In,args,pos) -> elim_in args , pos
    | TPlug (Contraction,args,pos) -> contraction args , pos
    | TPlug (Weaken,args,pos) -> weaken args , pos
    | Then (t1,t2,pos) -> 
	then_ (multiplex t1) (multiplex t2) , pos
    | Thens (t,t_list,pos ) -> 
	thens (multiplex t) (List.map multiplex t_list) ,pos
  in 
  let nc = apply__tac (multiplex tactical) cairn in
    if not (isExn nc) then
     (trace := (T_tac (get_state nc, pretty_tactical tactical))::!trace ; 
      history := (nc,!trace)::!history) ;
    nc

