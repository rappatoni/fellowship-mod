open Core
open Print 

(*A couple of definitions*)
type arg =
  | OnTheLeft
  | OnTheRight
  | Ident of string
  | Formula of prop
  | Expression of term

type tactic = 
  (* primitive tactics *)
  | Axiom of arg list
  | Cut of (string * prop)
  | Elim of arg list
  (* tacticals *)
  | Idtac
  | Then of (tactic * tactic)
  | Thens of (tactic * tactic list)
  (* derived tactics *)
  | Focus of (string * string)
  | Elim_In of (string * string * arg list)

type instruction =
  | Vars of (string list * sort)
  | Disc (*discard environment*)
  | Next (* next goal *)
  | Prev (* prev goal *)
  | Gl of prop
  | Tac of tactic
  | Ack of provers (*require acknowledgement from outside prover*)
  | AckProofTerm of provers
  | Quit
  | Help of string option
  | Lj of bool
  | Min of bool
  | Undo

type script = instruction

(*Command errors and messages*)
type command_error = 
  | Name_clash of string list
  | Hyp_match of string
  | Hyp_ccl_match of string
  | Not_trivial
  | Goal_exists of bool
  | No_switch
  | Need_args of string
  | Minimal_undef
  | Elim_coward
  | No_elim

class command_msg error =
object (self)
  inherit message
  val msg = error
  method to_string =
    match msg with
      | Name_clash [name] -> 
	  Format.sprintf "A symbol named %s is already defined. Be more
	  imaginative !"
	  name
      | Name_clash name_list -> 
	  Format.sprintf "Symbols named %s are already defined. Be more
	  imaginative !" (pretty_comma_list name_list)
      | Hyp_match name ->
	  Format.sprintf "%s is not in your list of hypothesis." name
      | Hyp_ccl_match name ->
	  Format.sprintf "%s is neither in your hypothesis nor in your
	  conclusion." name
      | Not_trivial -> 
	  "This is not trivial. Work some more."
      | Goal_exists true -> 
	  "You cannot issue this command while a goal is being proved."
      | Goal_exists false -> 
	  "Where is the goal ?"
      | No_switch -> 
	  "There is only one goal, impossible to switch."
      | Need_args args ->
	  Format.sprintf "You need to specify %s. " args
      | Minimal_undef ->
	  "Nothing to do in minimal logic."
      | Elim_coward ->
	  "Cowardly refusing to do something complex to obtain back the same
	  goal"
      | No_elim -> 
	  "Nothing to elim."
end

type tactical_error = 
  | Arg_count

class tactical_msg error =
object (self)
  inherit message
  val msg = error
  method to_string =
    match msg with
      | Arg_count -> 
	  "Not the right number of tactics"
end


(* THE PROOF LANGUAGE *)

let declare_var name sort cairn = match cairn with
  | Subgoals (0,s) | Exception (_,s) when s.goals = [] -> 
	let sort_sym = get_sort_symbols sort [] in
	  (let rec check = function
	     | [] -> if List.mem_assoc name s.sign
	       then Exception (new command_msg (Name_clash [name]), s)
	       else Subgoals (0,{s with sign = (name,sort)::s.sign})
	     | x::xl -> 
		 (try if (List.assoc x s.sign = SProp)
		  then Exception (new type_msg (Prop_not_sort x),s)
		  else check xl
		  with Not_found -> Exception (new type_msg (Undefined x),s) )
	   in check sort_sym
	  )
  | _ -> Exception (new command_msg (Goal_exists true),(get_state cairn))

let declare_vars var_list cairn = 
  List.fold_right (fun name cairn -> declare_var name (snd var_list) cairn) 
    (List.rev (fst var_list)) cairn

let rec goal prop cairn = match cairn with 
  | Subgoals (0,s) | Exception (_,s) when s.goals = [] -> 
      (match prop_infer prop s.sign with
	 | Inl SProp -> Subgoals (1,
	    {s with index=1;
	      goals=[initial_meta,(new_goal prop)];
	      pt=Mu (thesis,prop,Play(TermMeta initial_meta,Concl thesis))
	    } )
	 | Inl x -> Exception (new type_msg (Prop_kind (prop,x)),s)
	 | Inr m -> Exception (new type_msg m,s) )
  | _ -> Exception (new command_msg (Goal_exists true),(get_state cairn))

let next m =
 let s = get_state m in
  match s.goals with
     [] -> Exception (new command_msg (Goal_exists false),s)
   | [_] -> Exception (new command_msg No_switch,s)
   | _ ->
      let index' = if s.index = List.length s.goals then 1 else s.index + 1 in
       Subgoals (1,{s with index = index'})

let prev m =
 let s = get_state m in
  match s.goals with
     [] -> Exception (new command_msg (Goal_exists false),s)
   | [_] -> Exception (new command_msg No_switch,s)
   | _ ->
      let index' = if s.index = 1 then List.length s.goals else s.index - 1 in
       Subgoals (1,{s with index = index'})


(* INSTRUCTIONS *)

(*Check if a variable name is already used in the hypothesis or the environment
  or the conclusion*)
let check_variable var_list (h,c,sign) =
  let conflicts = 
    List.filter (fun x -> (List.mem_assoc x h) 
		    || (List.mem_assoc x c)
		    || (List.mem_assoc x sign)) var_list 
  in match conflicts with
    | [] -> None
    | names -> Some (Name_clash names)
	
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

(*Applying a tactic*)
let apply_tac tac m =
 match get_state m with
  | s when s.goals <> [] ->
      let g_before, (id,g), g_after = 
      split_list s.index s.goals in
       (match tac (id,g,s.sign,s.pt) with
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
  | s -> Exception(new command_msg (Goal_exists false),s)

(* PRIMITIVE TACTICS (axiom, cut, elim) *)

(*An easy one to begin with*)
let axiom args (id,g,sign,pt) =
  (match args with
    | [Ident x] -> 
      (let l = match (fst g.active) with 
        RightHandSide -> g.hyp | LeftHandSide -> g.ccl in
      try if (snd g.active) = (List.assoc x l) 
      then 
       let pt' =
        match (fst g.active) with
           RightHandSide -> instantiate_t3rm id (Hyp x) pt
         | LeftHandSide -> instantiate_context id (Concl x) pt
       in
        RSuccess ([],pt')
      else RException (new command_msg Not_trivial)
      with Not_found -> RException (new command_msg (Hyp_ccl_match x)) )
    | _ -> RException (new command_msg (Need_args "one identifier")) )

(* Cut rule *)
let cut x p (id,g,sign,pt) =
  let id1 = new_meta id 1 in
  let id2 = new_meta id 2 in
  match check_variable [x] (g.hyp,g.ccl,sign @ g.env), prop_infer p (sign @ g.env) with 
    | Some m , _ -> RException (new command_msg m)
    | None, Inl SProp -> 
	(match (fst g.active) with
	  | RightHandSide ->
	      let c' = if !lj then [] else (x,(snd g.active))::g.ccl in
		RSuccess(
		 [(id1, {g with ccl=c';active=(RightHandSide,p)});
		  (id2, {g with ccl=(x,(snd g.active))::g.ccl;active=(LeftHandSide,p)})] ,
		 instantiate_t3rm id
		  (Mu (x, (snd g.active), Play (TermMeta id1, ContextMeta id2))) pt)
	  | LeftHandSide ->
	      let c' = if !lj then [] else g.ccl in
		RSuccess(
		 [(id1,{g with hyp=(x,(snd g.active))::g.hyp;ccl=c'; 
		  active=(RightHandSide,p)}) ;
		  (id2,{g with hyp=(x,(snd g.active))::g.hyp; 
		  active=(LeftHandSide,p)})] ,
		 instantiate_context id
		  (MuTilde (x, (snd g.active), Play (TermMeta id1, ContextMeta id2))) pt) )
     | _, Inl x -> RException (new type_msg (Prop_kind (p,x)))
     | _, Inr m -> RException (new type_msg m) 

(*CSC: to be got rid of... *)
let fresh_name =
 let i = ref 0 in
  function () -> incr i; "toimprove" ^ string_of_int !i

(* Elim rule (left or right) *)
let rec elim args (id,g,sign,pt) = 
 match g.active with
  (* Right Impl *)
  | RightHandSide, BProp(a,Imp,b) ->
      (match args with
        | [Ident x] ->
	    (match check_variable [x] (g.hyp,g.ccl,sign @ g.env) with 
	      | Some m -> RException (new command_msg m)
	      | None -> let id1 = new_meta id 1 in
		  RSuccess(
		  [id1,{g with hyp=(x,a)::g.hyp;active=(RightHandSide,b)}],
		  instantiate_t3rm id (Lambda (x,a,TermMeta id1)) pt) )
        | _ -> RException (new command_msg (Need_args "an identifier")) )
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
	    (match check_variable [x] (g.hyp,g.ccl,sign @ g.env) with
	      | Some m -> 
		  RException (new command_msg m)
	      | None ->
		  let id1 = new_meta id 1 in
		  let c' = if !lj then [] else g.ccl in
		    RSuccess(
		    [id1,{g with ccl=(x,b)::c';active=(LeftHandSide,a)}] ,
		    instantiate_context id (Lambda' (x,b,ContextMeta id1)) pt) )
        | _ -> RException (new command_msg (Need_args "an identifier")) )
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
	   (match check_variable [x;y] (g.hyp,g.ccl,sign @ g.env), prop_infer p (sign @ g.env) with
	      | Some m, _ -> 
		  RException (new command_msg m)
	      | None, Inl SProp ->
		  let id1 = new_meta id 1 in
		  let id2 = new_meta id 2 in
		  let h' = (x,a)::(y,b)::g.hyp in
		  let c' = if !lj then [] else g.ccl in
		   RSuccess (
		    [(id1,{g with hyp=h'; ccl=c'; active=(RightHandSide,p)}) ;
		     (id2,{g with hyp=h'; active=(LeftHandSide,p)})] ,
		    instantiate_context id
		     (DestructTermsPair (x,a,y,b,
		       Play (TermMeta id1,ContextMeta id2))) pt) 
	      | _, Inl x -> RException (new type_msg (Prop_kind (p,x)))
	      | _, Inr m -> RException (new type_msg m) )
        | _ -> RException (new command_msg (Need_args "two identifers and a proposition")) )
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
	RException (new command_msg (Need_args "either \"left\" or \"right\"")) )
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
       elim args (id,{g with active=(RightHandSide,BProp(a,Imp,False))},sign,pt)
      else
(*CSC: primitive negation, horrible proof rendering because of lack of
       inner types
       RSuccess (
	[id1,{g with active=(LeftHandSide,a)}] ,
        instantiate_t3rm id (TermOfContext (ContextMeta id1)) pt)
*)
(*CSC: ~A definito come A -> false + bottom up conversion; ma e' troppo complicato (perche' c'e' di mezzo anche il False e il focus) *)
       let x = fresh_name () in
       let y = fresh_name () in
       let z = fresh_name () in
       RSuccess(
        [id1,{g with active=(LeftHandSide,a)}],
        instantiate_t3rm id
         (Mu (x,UProp(Neg,a),Play (Lambda (y,a,Mu (z,False,Play(Hyp y,ContextMeta id1))),Concl x))) pt)
  (* Left Negation *)
  | LeftHandSide, UProp(Neg,a) ->
     if !minimal then
      elim args (id,{g with active=(LeftHandSide,BProp(a,Imp,False))},sign,pt)
     else
      let id1 = new_meta id 1 in
      let c' = if !lj then [] else g.ccl in
(*CSC: primitive negation, horrible proof rendering because of lack of
       inner types
       RSuccess (
        [id1,{g with ccl=c'; active=(RightHandSide,a)}] ,
        instantiate_context id (ContextOfTerm (TermMeta id1)) pt)
*)
(*CSC: questa versione corrisponde a un passo implicito di espansione ~A = A -> false
       RSuccess(
        [id1,Goal(h,c',(RightHandSide,a))],
        instantiate_context id (Cons (TermMeta id1, False_eliminator)) pt)
*)
(*CSC: versione migliore: ~A e' definito come A -> false, ma la bottom-up
       conversion viene esplicitata grazie al MuTilde *)
       let x = fresh_name () in
       RSuccess(
        [id1,{g with ccl=c'; active=(RightHandSide,a)}],
        instantiate_context id
         (MuTilde (x,UProp(Neg,a),Play(Hyp x,(Cons (TermMeta id1, False_eliminator))))) pt)
  (* Right Forall *) 
  | RightHandSide, Quant(Forall,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with 
	| [] -> 
	  (match check_variable names (g.hyp,g.ccl,sign @ g.env) with
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
		RException (new command_msg m) )
	| [Ident x] -> 
	  (match check_variable [x] (g.hyp,g.ccl,sign @ g.env) with
	    | None -> 
		let a' = prop_subst (List.hd names) (TSym x) a in
		let names' = List.tl names in
		let a'' = if names' = [] then a' 
		  else Quant (Forall, (names',sort),a') in
		RSuccess (
		  [id1, {g with active=(RightHandSide,a''); 
		   env=(x,sort)::g.env}] ,
		  instantiate_t3rm id (LambdaFO (x,sort,TermMeta id1)) pt )
	    | Some m -> RException (new command_msg m) )
	| _ -> RException (new command_msg (Need_args "at most one identifier")) )
  (* Left Forall *)
  | LeftHandSide, Quant(Forall,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with
	| [Expression t] ->
            (match term_infer t (sign @ g.env) with
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
	| _ -> RException (new command_msg (Need_args "a term")) )
  (* Right Exists *)
  | RightHandSide, Quant(Exists,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with
	| [Expression t] ->
            (match term_infer t (sign @ g.env) with
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
	| _ -> RException (new command_msg (Need_args "a term")) )
  (* Left Exists *)
  | LeftHandSide, Quant(Exists,(names,sort),a) -> 
      let id1 = new_meta id 1 in
      (match args with 
	| [] -> 
	  (match check_variable names (g.hyp,g.ccl,sign @ g.env) with
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
		RException (new command_msg m) )
	| [Ident x] -> 
	  (match check_variable [x] (g.hyp,g.ccl,sign @ g.env) with
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
	    | Some m -> RException (new command_msg m) )
	| _ -> RException (new command_msg (Need_args "at most one identifier"))
      )
  (* Right True *)
  | RightHandSide, True ->
     RSuccess ([],instantiate_t3rm id True_constructor pt)
  (* Left False *)
  | LeftHandSide, False ->
     if !minimal then
      RException (new command_msg Minimal_undef)
     else
      RSuccess ([],instantiate_context id False_eliminator pt)
  (* Nothing to do *)
  | LeftHandSide, True
  | RightHandSide, False ->
     RException (new command_msg Elim_coward)
  | _, PSym _ | _, PApp _ -> RException (new command_msg No_elim)

(* TACTICALS and AUXILIARY TACTICS *)
(*Exception*)
exception Toss of message

let id_tac (id,g,sign,pt) =
 RSuccess ([id,g],pt)

let then_ t1 t2 (id,g,sign,pt) = 
 match t1 (id,g,sign,pt) with
    RException msg -> RException msg
  | RSuccess (lr,pt') ->
     try
      let goals, pt =
       List.fold_left (fun (goals,pt) (id,g) ->
         match t2 (id,g,sign,pt) with
            RException msg -> raise (Toss msg)
          | RSuccess (goals',pt') -> goals @ goals', pt'
       ) ([],pt') lr
      in RSuccess (goals,pt)
     with
      Toss msg -> RException msg

let thens t1 lt (id,g,sign,pt) =
 match t1 (id,g,sign,pt) with
    RException msg -> RException msg
  | RSuccess (lr,pt') ->
     try
      let goals, pt =
       List.fold_left2 (fun (goals,pt) tac (id,g) ->
         match tac (id,g,sign,pt) with
            RException msg -> raise (Toss msg)
          | RSuccess (goals',pt') -> goals @ goals', pt'
       ) ([],pt') lt lr
      in RSuccess (goals,pt)
     with
        Invalid_argument ("List.fold_left2") ->
         RException (new tactical_msg Arg_count) 
      | Toss msg -> RException msg

(* DERIVED TACTICS *)

let smart_axiom args (id,g,sign,pt) =
  (match args with
    | [] -> 
      (let l = match (fst g.active) with 
      RightHandSide -> g.hyp | LeftHandSide -> g.ccl in
      try
       let decl = fst (List.find (fun x -> (snd g.active) = snd x) l) in
       axiom [Ident decl] (id,g,sign,pt) 
      with
       Not_found -> RException (new command_msg Not_trivial) )
    | [Ident _] -> axiom args (id,g,sign,pt)
    | _ -> RException (new command_msg (Need_args "at most one identifier")))

let focus ident ident' (id,g,sign,pt) =
 try
  let q = List.assoc ident g.hyp in
    match check_variable [ident'] (g.hyp,g.ccl,sign @ g.env) with
      | Some m -> RException (new command_msg m)
      | None -> thens (cut ident' q) [axiom [Ident ident] ; id_tac] (id,g,sign,pt)
  with Not_found ->
   try
    let q = List.assoc ident g.ccl in
      match check_variable [ident'] (g.hyp,g.ccl,sign @ g.env) with 
	| Some m -> RException (new command_msg m)
	| None -> thens (cut ident' q) [id_tac ; axiom [Ident ident]] (id,g,sign,pt)
   with Not_found -> RException (new command_msg (Hyp_ccl_match ident))

let elim_in ident ident' args details = 
 thens (focus ident ident') [elim args] details

(* Tactic multiplexer *)
let interpret_tac t m =
 let rec aux =
   function
    (* primitive tactics *)
    | Axiom args -> smart_axiom args
    | Cut (x,p) -> cut x p
    (* tacticals *)
    | Idtac -> id_tac
    | Then(tac,tac') -> then_ (aux tac) (aux tac')
    | Thens(tac,taclist) ->
       thens (aux tac) (List.map aux taclist)
    (* derived tactics *)
    | Elim args -> elim args
    | Focus (ident,ident') -> focus ident ident'
    | Elim_In (ident,ident',args) -> elim_in ident ident' args
 in
  apply_tac (aux t) m
