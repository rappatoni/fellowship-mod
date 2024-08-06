open Core
open Format
open Interpreter

(* lambda-bar-mu-mu tilde to lambda *)

let fresh_name bound =
 (* very inefficient implementation *)
 let rec aux n =
  let id = sprintf "x%d" n in
   if List.mem id bound then aux (n + 1) else id
 in
  aux 1

let rec coq_of_term bound =
 function
  | TermMeta _ -> assert false
  | True_constructor -> "I"
  | Hyp id -> id
  | Lambda (id,p,t') ->
     sprintf "(fun (%s:%s) => %s)" 
     id (gen_prop_rec Coq p)  (coq_of_term (id::bound) t')
  | LambdaFO (id,p,t') ->
     sprintf "(fun (%s:%s) => %s)" 
     id (gen_sort_rec Coq p)  (coq_of_term (id::bound) t')
  | Cons'(c,t') ->
     let fresh = fresh_name bound in
      sprintf "(conj %s (fun %s => %s))"
      (coq_of_term bound t') fresh (coq_of_context (fresh::bound) fresh c)
  | TermsPair (t',t'') ->
     sprintf "(conj %s %s)"
     (coq_of_term bound t') (coq_of_term bound t'')
  | TermsPairFO ((id,p),t',t'') ->
     sprintf "(ex_intro (fun %s => %s) %s %s)"
      id (gen_prop_rec Coq p) (gen_term_rec Coq t') (coq_of_term bound t'')
  | Left t' -> sprintf "(or_introl _ %s)" (coq_of_term bound t')
  | Right t' -> sprintf "(or_intror _ %s)" (coq_of_term bound t')
  | TermOfContext c ->
     let fresh = fresh_name bound in
      sprintf "(fun %s => %s)" fresh (coq_of_context (fresh::bound) fresh c)
  | Mu (id,p,cmd) ->
     (* we assume to be intuitionistic *)
     coq_of_command bound cmd
and coq_of_context bound t =
 function
    ContextMeta _ -> assert false
  | False_eliminator -> sprintf "(match %s with end)" t
  | Concl id -> t (* we assume to be intuitionistic *)
  | Cons (t',c) ->
     coq_of_context bound (sprintf "(%s %s)" t (coq_of_term bound t')) c
  | ConsFO (t',c) ->
     coq_of_context bound (sprintf "(%s %s)" t (gen_term_rec Coq t')) c
  | Lambda' (id,p,c) ->
     let fresh = fresh_name bound in
      sprintf "(match %s with (conj %s %s) => %s end)"
      t fresh id (coq_of_context (fresh::bound) fresh c)
  | DestructTermsPair (id',p',id'',p'',cmd) ->
     sprintf "(match %s with (conj %s %s) => %s end)"
     t id' id'' (coq_of_command (id'::id''::bound) cmd)
  | DestructTermsPairFO (id',p',c) ->
     let fresh = fresh_name bound in
      sprintf "(match %s with (ex_intro %s %s) => %s end)"
      t id' fresh (coq_of_context (id'::fresh::bound) fresh c)
  | ContextsPair (c,c') ->
     let fresh = fresh_name bound in
      sprintf "(match %s with or_introl %s => %s | or_intror %s => %s end)"
      t fresh (coq_of_context (fresh::bound) fresh c)
      fresh (coq_of_context (fresh::bound) fresh c')
  | ContextOfTerm t' ->
     sprintf "(match (%s %s) with end)" t (coq_of_term bound t')
  | MuTilde (id,p,cmd) ->
     sprintf "let %s:%s := %s in %s" 
     id (gen_prop_rec Coq p) t (coq_of_command (id::bound) cmd)
and coq_of_command bound =
 function
    Play (t',c) -> coq_of_context bound (coq_of_term bound t') c

let coq_of_pt = coq_of_term []

let gen_file prover ohandle (trace,pt) = 
  let rec make_str prover trace = 
    match trace with 
      | [] -> ""
      | hd::tl -> 
	  (match hd with
	     | T_vars (def_list) -> 
		let var = gen_var_list prover (fst def_list)
		and typ = gen_sort_rec prover (snd def_list)
		in gen_command.vars prover var typ (make_str prover tl)
	     | T_tac (s) -> assert false
	     | T_goal (s) ->
		 let p = prop_of_goals s.goals in
		  sprintf "Goal %s. @.Proof %s."
                  (gen_prop_rec Coq p) (coq_of_pt pt)
	  )
  in pp_print_string ohandle.frm (make_str prover trace)

(* LK to LJ + EM *)

let p_or_not_p = "classic"

(*CSC: should the linear_* set of functions be given a different name? *)
(*CSC: the function set linear_* can be merged with the function set
       lk_to_lj_plus_em_*, halving the computational complexity *)
let rec linear_term id canoccur =
 function
  | TermMeta _
  | True_constructor
  | Hyp _ -> true
  | Lambda (id',p,t) -> assert (id <> id'); linear_term id canoccur t
  | LambdaFO (id',p,t) -> assert (id <> id'); linear_term id canoccur t
  | Cons'(c,t) -> linear_context id canoccur c && linear_term id canoccur t
  | TermsPair (t',t'') ->
     linear_term id canoccur t' && linear_term id canoccur t''
  | TermsPairFO (p,t',t'') -> linear_term id canoccur t''
  | Left t
  | Right t -> linear_term id canoccur t
  | TermOfContext c -> linear_context id false c
  | Mu (id',p,cmd) -> assert (id <> id' || !lj); linear_command id false cmd
and linear_context id canoccur =
 function
    ContextMeta _
  | False_eliminator -> true
  | Concl id' when id = id' -> canoccur
  | Concl id' -> true
  | Cons (t,c) -> linear_term id canoccur t && linear_context id canoccur c
  | ConsFO (t,c) -> linear_context id canoccur c
  | Lambda' (id',p,c) ->
     assert (id <> id'); linear_context id false (*canoccur*) c
  | DestructTermsPair (id',p',id'',p'',cmd) ->
     assert (id <> id'); assert (id <> id'');
     linear_command id canoccur cmd
  | DestructTermsPairFO (id',p',c) ->
     assert (id <> id');
     linear_context id canoccur c
  | ContextsPair (c,c') ->
     linear_context id canoccur c && linear_context id canoccur c'
  | ContextOfTerm t -> linear_term id false t
  | MuTilde (id',p,cmd) -> assert (id <> id'); linear_command id canoccur cmd
and linear_command id canoccur =
 function
    Play (t,c) ->
     linear_term id canoccur t && linear_context id canoccur c

let rec lk_to_lj_plus_em_term bound non_linear_conts =
 function
  | TermMeta id -> TermMeta id
  | True_constructor -> True_constructor
  | Hyp id -> Hyp id
  | Lambda (id,p,t) ->
     Lambda (id,p,lk_to_lj_plus_em_term (id::bound) non_linear_conts t)
  | LambdaFO (id,p,t) ->
     LambdaFO (id,p,lk_to_lj_plus_em_term (id::bound) non_linear_conts t)
  | Cons'(c,t) ->
     Cons'
      (lk_to_lj_plus_em_context bound non_linear_conts c,
       lk_to_lj_plus_em_term bound non_linear_conts t)
  | TermsPair (t',t'') ->
     TermsPair
      (lk_to_lj_plus_em_term bound non_linear_conts t',
       lk_to_lj_plus_em_term bound non_linear_conts t'')
  | TermsPairFO (p,t',t'') ->
     TermsPairFO (p, t', lk_to_lj_plus_em_term bound non_linear_conts t'')
  | Left t -> Left (lk_to_lj_plus_em_term bound non_linear_conts t)
  | Right t -> Right (lk_to_lj_plus_em_term bound non_linear_conts t)
  | TermOfContext c ->
     TermOfContext (lk_to_lj_plus_em_context bound non_linear_conts c)
  | Mu (id,p,cmd) ->
     if linear_command id true cmd then
      Mu (id,p,lk_to_lj_plus_em_command (id::bound) non_linear_conts cmd)
     else
      let fresh = fresh_name bound in
      let fresh' = fresh_name (fresh::bound) in
      Mu (fresh,p,
       Play
        (Hyp p_or_not_p,
         ContextsPair
          (* This MuTilde delta expansion make p happear for natural language *)
          (MuTilde(fresh',p,Play(Hyp fresh',Concl fresh)),
           MuTilde
            (id,UProp(Neg,p),
              lk_to_lj_plus_em_command (fresh::bound)
               ((id,p)::non_linear_conts) cmd))))
and lk_to_lj_plus_em_context bound non_linear_conts =
 function
    ContextMeta id -> ContextMeta id
  | False_eliminator -> False_eliminator
  | Concl id ->
     (try let p = List.assoc id non_linear_conts in
       let fresh = fresh_name bound in
       MuTilde (fresh,p,Play (Hyp id, Cons (Hyp fresh, False_eliminator)))
      with Not_found ->
       Concl id)
  | Cons (t,c) ->
     Cons
      (lk_to_lj_plus_em_term bound non_linear_conts t,
       lk_to_lj_plus_em_context bound non_linear_conts c)
  | ConsFO (t,c) ->
     ConsFO (t, lk_to_lj_plus_em_context bound non_linear_conts c)
  | Lambda' (id,p,c) ->
     (*CSC: adding id to non_linear_conts is something deep that need
            further understanding. Cfr. test2 *)
     (*CSC: moreover, there is a bug since adding (id,p) to non_linear_conts
            make the term ill-typed (because of concl); thus I should do
            something slightly different somehow. In particular, either
            I get rid of subtractive logic in this set of functions, or I
            postpone the non_linear_conts translation for subtractive logic
            to the translation from lambda-bar-mu-mu tilde to CIC *)
     Lambda'
      (id,p,lk_to_lj_plus_em_context (id::bound) ((id,p)::non_linear_conts) c)
  | DestructTermsPair (id',p',id'',p'',cmd) ->
     DestructTermsPair
      (id',p',id'',p'',
        lk_to_lj_plus_em_command (id'::id''::bound) non_linear_conts cmd)
  | DestructTermsPairFO (id',p',c) ->
     DestructTermsPairFO
      (id',p', lk_to_lj_plus_em_context (id'::bound) non_linear_conts c)
  | ContextsPair (c,c') ->
     ContextsPair
      (lk_to_lj_plus_em_context bound non_linear_conts c,
       lk_to_lj_plus_em_context bound non_linear_conts c')
  | ContextOfTerm t ->
     ContextOfTerm (lk_to_lj_plus_em_term bound non_linear_conts t)
  | MuTilde (id,p,cmd) ->
     MuTilde (id,p,lk_to_lj_plus_em_command (id::bound) non_linear_conts cmd)
and lk_to_lj_plus_em_command bound non_linear_conts =
 function
    Play (t,c) ->
     Play
      (lk_to_lj_plus_em_term bound non_linear_conts t,
       lk_to_lj_plus_em_context bound non_linear_conts c)

let lk_to_lj_plus_em = lk_to_lj_plus_em_term [] []

(* Putting everything together *)

exception NotImplemented

let launch_prover_on_proof_term prover trace pt ofile_prefix keep_trace =
 if prover <> Coq then raise NotImplemented
 else
  let pt' = if !lj then pt else lk_to_lj_plus_em pt in
   launch_prover0 prover ofile_prefix keep_trace (trace,pt') gen_file
