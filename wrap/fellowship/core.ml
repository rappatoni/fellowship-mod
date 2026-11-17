(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
  // Modifications Copyright (C) 2025-2026 Max Rapp
  // The modifications are licensed under GPL-3.0-only. This combined file is
  // distributed under GPL-3.0-only as permitted by CeCILL v2.0 Article 5.3.4;
  // the original CeCILL notice and terms remain applicable to the original work.
*)

open Format

(* MISCELLANEOUS *)
(*Collections are Maps indexed by strings*)
module Coll = Map.Make (String)
let coll_add_list collection l = 
  List.fold_right (fun x y -> Coll.add (fst x) (snd x) y) l collection

let search x l c =
 try
  let _,p,_ = List.find (fun (x',_,w) -> x'=x && w) l in p
 with 
  Not_found -> Coll.find x c 
	
let add_var_list (vl,sort) coll = 
  List.fold_right (fun v c -> Coll.add v sort c) vl coll

(*Options *)
let lj = ref true (* constructive by default *)
let minimal = ref false (* not minimal by default *)
let ascii = ref false (* use unicode by default *)

(*Some unicode love*)
type symbol_chart =
  {
    lambda: string;
    mu: string;
    mutilde: string;
    top: string;
    bottom: string;
    neg: string;
    imply: string;
    minus: string;
    disj: string;
    conj: string;
    arrow: string;
    fall: string;
    exts: string;
  }	

let utf_symbol = 
  {
    lambda = "\206\187";
    mu = "\206\188";
    mutilde = "\206\188'";
    top = "\226\138\164";
    bottom = "\226\138\165";
    neg = "\194\172";
    imply = "\226\135\146";
    minus = "-";
    disj = "\226\136\168";
    conj = "\226\136\167";
    arrow = "\226\134\146";
    fall = "\226\136\128";
    exts = "\226\136\131";
  }

let ascii_symbol =
  {
    lambda = "\\";
    mu = ";:";
    mutilde = ";:'";
    top = "true";
    bottom = "false";
    neg = "~";
    imply = "->";
    minus = "-";
    disj = "\\/";
    conj = "/\\";
    arrow = "->";
    fall = "forall ";
    exts = "exists ";
  }

let symbol () = if !ascii then ascii_symbol else utf_symbol

(*Label for the active formula*)
let thesis = "thesis"

(*Allows for exception programming*)
type ('a,'b) sum = 
  | Inl of 'a
  | Inr of 'b
      

(* ERRORS *)
(*The virtual class*)
class virtual message (loc : Lexing.position option) =
object (self)
  val loc = loc
  method loc = loc
  method localize new_loc =
   if new_loc = Lexing.dummy_pos then {< >} else
    {< loc = match loc with None -> Some new_loc | Some _ -> loc >}
  method virtual to_string : string
end


(* SORTS *)
type sort = 
  | SSet (*sort of new sorts*)
  | SProp (*sort of propositions*)
  | SSym of string
  | SArr of sort * sort

let rec pretty_sort = function
  | SSet -> "type"
  | SProp -> "bool"
  | SSym name -> name
  | SArr (s1,s2) ->
      let lhs =
        match s1 with
        | SArr _ -> "(" ^ pretty_sort s1 ^ ")"
        | _      -> pretty_sort s1
      in
      sprintf "%s%s%s" lhs (symbol ()).arrow (pretty_sort s2)

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

(*Output symbol names of a sort*)
let rec get_sort_symbols s ret = match s with
  | SSet | SProp -> ret
  | SSym name -> if List.mem name ret then ret else name::ret
  | SArr (sort, sort') -> let ret' = get_sort_symbols sort ret in 
      get_sort_symbols sort' ret'


(* TERMS *)
type term = 
  | TSym of string
  | TApp of term * term

let rec pretty_term =  function
  | TSym name -> name
  | TApp (term,term') -> 
      match term' with
         TSym name -> sprintf "%s %s" (pretty_term term) name
       | TApp _ -> sprintf "%s (%s)" (pretty_term term) (pretty_term term')

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
	     
(*Output symbol names of a term*)
let rec get_term_symbols t sign ret =  match t with 
  | TSym name -> 
      if (not(List.mem name sign)) || (List.mem name ret) 
      then ret else name::ret
  | TApp (term,term') -> 
      let ret'= get_term_symbols term sign ret in 
	get_term_symbols term' sign ret'
	  
(*Substitution*)
let rec term_subst name t t' = match t' with
  | TSym name' -> if name = name' then t else TSym name'
  | TApp (term,term') -> 
      TApp (term_subst name t term, term_subst name t term')

(* PROPOSITIONS *)
type prop = 
  | True | False
  | PSym of string 
  | PApp of prop * term
  | UProp of unop * prop
  | BProp of prop * binop * prop
  | Quant of quantifier * (string list * sort) * prop
and unop = Neg
and binop = Imp | Minus | Conj | Disj
and quantifier = Exists | Forall

(*Proposition pretty_printer*)
let rec pretty_sep_list sep = function
  | [] -> ""
  | [s] -> s
  | s::sl ->  Format.sprintf "%s%s%s" s sep (pretty_sep_list sep sl)

let pretty_comma_list = pretty_sep_list ","
let pretty_pipe_list = pretty_sep_list " | "

let pretty_variables def_list = 
  sprintf "%s : %s" (pretty_comma_list (fst def_list)) (pretty_sort (snd def_list))

let rec pretty_prop ?(parentprio = 0) ?(assoc = false) p = match p with 
  | True -> (symbol ()).top
  | False -> (symbol ()).bottom
  | PSym name  -> name
  | PApp (prop,term) ->
     (match term with
         TSym name -> sprintf "%s %s" (pretty_prop prop) name
       | TApp _ -> sprintf "%s (%s)" (pretty_prop prop) (pretty_term term))
  | UProp(Neg,p) -> sprintf "%s%s" (symbol ()).neg (pretty_prop ~parentprio:12 p)
  | BProp(p1,op,p2) ->
      (* Priorities match parser.mly's precedence declarations: subtraction
         is the loosest binary connective, then implication, then disjunction,
         then conjunction.  The associativity flags are (left_tolerates_same,
         right_tolerates_same).  Conj and Disj used to be (true, true), which
         parenthesises neither operand and so printed A/\(B/\C) and
         (A/\B)/\C alike -- the grouping was unrecoverable.  They are
         left-associative, so only the left operand may share the level. *)
      let ops,prio,assoc1,assoc2 = match op with
	  Conj->   (symbol ()).conj, 10, true, false
	| Disj ->  (symbol ()).disj,  8, true, false
	| Imp ->   (symbol ()).imply, 6, false, true
        | Minus -> (symbol ()).minus, 4, true, false
      in
       if prio < parentprio || (parentprio = prio && not assoc) then
         sprintf "(%s%s%s)"
          (pretty_prop ~parentprio:prio ~assoc:assoc1 p1) ops
          (pretty_prop ~parentprio:prio ~assoc:assoc2 p2)
       else
         sprintf "%s%s%s"
          (pretty_prop ~parentprio:prio ~assoc:assoc1 p1) ops
          (pretty_prop ~parentprio:prio ~assoc:assoc2 p2)
  | Quant (q,(names,sort),prop) ->
      let quants = match q with
	| Exists -> (symbol ()).exts
	| Forall -> (symbol ()).fall
      in
      (* A quantifier body extends as far to the right as it can, so a
         quantifier standing as a strict subformula needs parentheses to keep
         its scope.  Without them (forall x:A,Q x)->C and forall x:A,(Q x->C)
         print identically and the reading is lost: the first is not
         recoverable from the string by any amount of context.  Every other
         connective already guards on parentprio this way; this branch used to
         ignore its argument. *)
      let body =
	sprintf "%s%s:%s,%s" quants (pretty_comma_list names) (pretty_sort sort) (pretty_prop prop)
      in
       if parentprio > 0 then sprintf "(%s)" body else body

(*Useful for the weird quantified variables list*)
let rec list_flatten = function
  | ([],s) -> []
  | (x::xn,s) -> (x,s)::(list_flatten (xn,s))

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
(* The term symbols of a term, and those of a proposition not bound in
   it; predicate symbols are not counted. *)
let rec term_names t acc = match t with
  | TSym name -> name :: acc
  | TApp (term,term') -> term_names term (term_names term' acc)

let rec prop_free p = match p with
  | True | False | PSym _ -> []
  | PApp (prop,term) -> prop_free prop @ term_names term []
  | UProp (_,prop) -> prop_free prop
  | BProp (prop,_,prop') -> prop_free prop @ prop_free prop'
  | Quant (_,(names,_),prop) ->
      List.filter (fun n -> not (List.mem n names)) (prop_free prop)

(* [base] itself when free, else [base2], [base3], ... *)
let fresh_variant base taken =
  if not (List.mem base taken) then base else
  let rec aux n =
    let name = base ^ string_of_int n in
    if List.mem name taken then aux (n + 1) else name
  in aux 2

(* Capture-avoiding: the substitution stops at a quantifier that binds
   [name] or under which [name] is not free, and otherwise a quantifier
   binding a name of [t] is renamed first.  The
   original went under every quantifier unchecked, so that [elim [y]] on
   [exists x, forall y, R x y] gave [forall y, R y y].  The fresh names
   are the ones AIDA chooses (core/ac/prop.py, [PQuant.subst]), so that
   AIDA's replayed statements match Fellowship's. *)
let rec prop_subst name t p = match p with
  | True -> True
  | False -> False
  | PSym name' -> assert(name <> name'); PSym name'
  | PApp (prop,term) ->
      PApp (prop_subst name t prop, term_subst name t term)
  | UProp (Neg,prop) ->
      UProp (Neg, prop_subst name t prop)
  | BProp (prop,op,prop') ->
      BProp (prop_subst name t prop, op, prop_subst name t prop')
  | Quant(q,(names,sort),prop)
    when List.mem name names || not (List.mem name (prop_free prop)) -> p
  | Quant(q,(names,sort),prop) ->
      let captured = term_names t [] in
      let taken = ref (name :: captured @ prop_free prop) in
      let rename (ns, prop) n =
	if List.mem n captured then
	  let n' = fresh_variant n !taken in
	  taken := n' :: !taken;
	  n' :: ns, prop_subst n (TSym n') prop
	else n :: ns, prop in
      let names', prop' = List.fold_left rename ([], prop) names in
      Quant (q,(List.rev names',sort), prop_subst name t prop')

(*Typechecking*)
type type_error = 
  | Undefined of string
  | Not_a_fun of term * sort
  | Not_a_pred of prop * sort
  | Prop_kind of prop * sort
  | Prop_not_sort of string
  | Fun_apply_err of term * sort * term * sort
  | Pred_apply_err of prop * sort * term * sort
  | Type_mismatch of term * sort * sort
  | Referent_sort of string
  | Referent_conflict of string * sort * sort
  | Referent_unlisted of string
  | Referent_kind of string * sort

class type_msg error =
object (self)
  inherit message None
  val msg = error
  method to_string = 
    match msg with
      | Undefined (name) -> 
	  Format.sprintf "Symbol %s undefined" name
      | Not_a_fun (term,sort) -> 
	  Format.sprintf "Term %s of type %s is not a function, it cannot be applied."
	  (pretty_term term) (pretty_sort sort)
      | Not_a_pred (prop,sort) ->
	  Format.sprintf "Proposition %s of type %s is not a predicate, it cannot be applied."
	  (pretty_prop prop) (pretty_sort sort)
      | Prop_kind (prop,sort) ->
	  Format.sprintf "Proposition %s of type %s is not a boolean."
	  (pretty_prop prop) (pretty_sort sort)
      | Prop_not_sort name -> 
	  Format.sprintf "%s is a proposition, not a sort." name
      | Fun_apply_err (term,sort,term',sort') ->
	  Format.sprintf "Function %s of type %s cannot be applied to %s of type %s."
	  (pretty_term term) (pretty_sort sort)
	  (pretty_term term') (pretty_sort sort')
      | Pred_apply_err (prop,sort,term,sort') ->
	  Format.sprintf "Predicate %s of type %s cannot be applied to %s of type %s."
	  (pretty_prop prop) (pretty_sort sort)
	  (pretty_term term) (pretty_sort sort')
      | Type_mismatch (term, sort, sort') ->
	  Format.sprintf "Term %s of type %s does not match the type %s."
	  (pretty_term term) (pretty_sort sort) (pretty_sort sort')
      | Referent_sort name ->
	  Format.sprintf "The sort of the referent %s cannot be inferred: give it, as in [%s:SORT]." name name
      | Referent_conflict (name, sort, sort') ->
	  Format.sprintf "The referent %s is used with the sort %s and with the sort %s." name
	  (pretty_sort sort) (pretty_sort sort')
      | Referent_unlisted name ->
	  Format.sprintf "The referent %s is missing from the theorem's referent environment." name
      | Referent_kind (name, sort) ->
	  Format.sprintf "The referent %s has the sort %s: a referent stands for an individual or a function, not a proposition or a sort." name (pretty_sort sort)
end
	  
let rec term_infer t sign = match t with 
  | TSym name -> 
      (try Inl (Coll.find name sign)
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
      
let rec prop_infer p sign = match p with
  | True -> Inl SProp
  | False -> Inl SProp
  | PSym name -> 
      (try Inl (Coll.find name sign)
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
      (match prop_infer prop 
	(List.fold_right (fun x y -> Coll.add x sort y) names sign)
      with
	 | Inl SProp -> Inl SProp
	 | Inl sort  -> Inr (Prop_kind (prop,sort))
	 | Inr m -> Inr m )

(* DISCOURSE REFERENTS *)
(* A referent ?x is a pseudo-constant local to a theorem: it lives in the
   theorem's goal environment, never in the signature, and a theorem with
   one cannot be registered (qed refuses it).  Its sort is given in the
   theorem's referent environment or inferred from its uses. *)
let is_referent name = String.length name > 1 && (name.[0] = '?' || name.[0] = '!')

let rec referents_of_term t acc = match t with
  | TSym n -> if is_referent n && not (List.mem n acc) then acc @ [n] else acc
  | TApp (t, t') -> referents_of_term t' (referents_of_term t acc)

let rec referents_of_prop p acc = match p with
  | True | False | PSym _ -> acc
  | PApp (p, t) -> referents_of_term t (referents_of_prop p acc)
  | UProp (_, p) -> referents_of_prop p acc
  | BProp (p, _, p') -> referents_of_prop p' (referents_of_prop p acc)
  | Quant (_, _, p) -> referents_of_prop p acc

exception Referent_error of type_error

(* The sorts of the referents of [p], from the sort expected where each
   occurs: an argument of a predicate or a function, or the head of an
   application to a term of known sort. *)
let infer_referents p sign =
  let found = ref [] in
  let ctx sign = coll_add_list sign !found in
  let assign n s = match List.assoc_opt n !found with
    | Some s' -> if not (eq_sort s s') then raise (Referent_error (Referent_conflict (n, s', s)))
    | None -> found := !found @ [(n, s)] in
  let unassigned t =
    List.filter (fun n -> not (List.mem_assoc n !found)) (referents_of_term t []) in
  let rec expect t s sign = match t with
    | TSym n -> if is_referent n then assign n s
    | TApp (f, a) ->
        (match term_infer f (ctx sign) with
           | Inl (SArr (s1, _)) -> expect a s1 sign
           | Inl _ -> ()
           | Inr _ ->
               (match term_infer a (ctx sign) with
                  | Inl sa -> expect f (SArr (sa, s)) sign
                  | Inr _ ->
                      (match unassigned t with
                         | n :: _ -> raise (Referent_error (Referent_sort n))
                         | [] -> ())))
  in
  let rec walk p sign = match p with
    | True | False | PSym _ -> ()
    | PApp (q, t) ->
        walk q sign;
        (match prop_infer q (ctx sign) with
           | Inl (SArr (s, _)) -> expect t s sign
           | _ -> ())
    | UProp (_, q) -> walk q sign
    | BProp (q, _, q') -> walk q sign; walk q' sign
    | Quant (_, (names, s), q) ->
        walk q (List.fold_right (fun x y -> Coll.add x s y) names sign)
  in
  walk p sign;
  !found

let rec sort_result s = match s with SArr (_, s') -> sort_result s' | _ -> s

(* The referent environment of a theorem stating [p]: the one given
   (complete: every referent of [p] listed; a sort left out inferred), or,
   with none given, the referents of [p] with their inferred sorts. *)
let referent_env p given sign =
  try
    let inferred = infer_referents p sign in
    let mentioned = referents_of_prop p [] in
    let env = match given with
      | None ->
          List.map (fun n -> match List.assoc_opt n inferred with
            | Some s -> (n, s)
            | None -> raise (Referent_error (Referent_sort n))) mentioned
      | Some listed ->
          List.iter (fun n -> if not (List.mem_assoc n listed)
                      then raise (Referent_error (Referent_unlisted n))) mentioned;
          List.map (fun (n, so) -> match so, List.assoc_opt n inferred with
            | Some s, Some s' ->
                if eq_sort s s' then (n, s)
                else raise (Referent_error (Referent_conflict (n, s, s')))
            | Some s, None -> (n, s)
            | None, Some s' -> (n, s')
            | None, None -> raise (Referent_error (Referent_sort n))) listed
    in
    List.iter (fun (n, s) ->
      List.iter (fun sym -> if not (Coll.mem sym sign)
                  then raise (Referent_error (Undefined sym))) (get_sort_symbols s []);
      match sort_result s with
        | SProp | SSet -> raise (Referent_error (Referent_kind (n, s)))
        | _ -> ()) env;
    Inl env
  with Referent_error e -> Inr e

(* PROOF TERMS *)
(*Metavariables*)
type metaid = string
let initial_meta = "1"
let new_meta id n = id ^ "." ^ string_of_int n

type t3rm =
   TermMeta of metaid
 | DelegationTermMeta of metaid
 | True_constructor
 | Hyp of string
 | Lambda of string * prop * t3rm
 | LambdaFO of string * sort * t3rm
 | Cons' of context * t3rm
 | TermsPair of t3rm * t3rm
   (* the string * prop (e.g. "x","P") is there since the proposition
      \lambda x.P cannot be inferred automatically [by Coq] *)
 | TermsPairFO of (string * prop) * term * t3rm
 | Left of t3rm
 | Right of t3rm
 | Mu of string * prop * command
and context =
   ContextMeta of metaid
 | DelegationContextMeta of metaid
 | False_eliminator
 | Concl of string
 | Cons of t3rm * context
 | ConsFO of term * context
 | Lambda' of string * prop * context
 | DestructTermsPair of string * prop * string * prop * command
 | DestructTermsPairFO of string * sort * context
 | ContextsPair of context * context
 | MuTilde of string * prop * command
and command =
   Play of t3rm * context

type context_or_t3rm =
   Context of context
 | Term of t3rm

type proof_t3rm = context_or_t3rm

(*Term pretty-printer*)
let rec pretty_t3rm =
 function
    TermMeta id -> sprintf "?%s" id
  | DelegationTermMeta id -> sprintf "!%s" id
  | True_constructor -> "_T_"
  | Hyp id -> id
  | Lambda (id,p,t) ->
      (* A lambda binds tighter than `*`, so a body that is itself an
         application must be parenthesised to stay inside the lambda.  Without
         this, Lambda over a Cons' and the other reading print alike:
         λh:R.b*c would mean (λh:R.b)*c, which is the commoner shape and so
         the default, leaving this one inexpressible. *)
      let body = pretty_t3rm t in
      let body = (match t with Cons' _ -> "(" ^ body ^ ")" | _ -> body) in
      sprintf "%s%s:%s.%s" (symbol ()).lambda id (pretty_prop p) body
  | LambdaFO (id,p,t) ->
      let body = pretty_t3rm t in
      let body = (match t with Cons' _ -> "(" ^ body ^ ")" | _ -> body) in
      sprintf "%s%s:%s.%s" (symbol ()).lambda id (pretty_sort p) body
  | Cons' (c,t) ->
      (* `*` nests to the right, so a left operand that is itself built with
         `*` must be parenthesised: a stack (th*1!)*r would print as th*1!*r,
         which reads as th*(1!*r) with th a context variable.  A first-order
         destructor's body extends maximally and would swallow the `*`. *)
      let left = pretty_context c in
      let left =
       (match c with
          Cons _ | ConsFO _ | DestructTermsPairFO _ -> "(" ^ left ^ ")"
        | _ -> left) in
      sprintf "%s*%s" left (pretty_t3rm t)
  | TermsPair (t,t') -> sprintf "(%s,%s)" (pretty_t3rm t) (pretty_t3rm t')
  | TermsPairFO (_,t,t') -> sprintf "(%s,%s)" (pretty_term t) (pretty_t3rm t')
  | Left t -> sprintf "L(%s)" (pretty_t3rm t)
  | Right t -> sprintf "R(%s)" (pretty_t3rm t)
  | Mu (id,p,cmd) -> 
      sprintf "%s%s:%s.%s" (symbol ()).mu id (pretty_prop p) (pretty_command cmd)
and pretty_context =
 function
    (* Marker position encodes the category: leading for terms (see
       pretty_t3rm), trailing for contexts.  Without this the spellings
       overlap and "!n"/"?n" can be read as either a term or a context,
       which makes application chains structurally ambiguous. *)
    ContextMeta id -> sprintf "%s?" id
  | DelegationContextMeta id -> sprintf "%s!" id
  | False_eliminator -> "_F_"
  | Concl id -> id
  | Cons (t,c) ->
      (* The mirror case of Cons': a subtraction introduction (1!*!2)*r as
         the head of a stack would print as 1!*!2*r. *)
      let left = pretty_t3rm t in
      let left = (match t with Cons' _ -> "(" ^ left ^ ")" | _ -> left) in
      sprintf "%s*%s" left (pretty_context c)
  | ConsFO (t,c) -> sprintf "%s*%s" (pretty_term t) (pretty_context c)
  | Lambda' (id,p,t) ->
      let body = pretty_context t in
      let body =
       (match t with Cons _ | ConsFO _ -> "(" ^ body ^ ")" | _ -> body) in
      sprintf "%s%s:%s.%s" (symbol ()).lambda id (pretty_prop p) body
  | DestructTermsPair (id,p,id',p',cmd) ->
      sprintf "(%s:%s,%s:%s).%s" id (pretty_prop p) id' (pretty_prop p') (pretty_command cmd)
  | DestructTermsPairFO (id,p,c) ->
      sprintf "(%s:%s).%s" id (pretty_sort p) (pretty_context c)
  | ContextsPair (c,c') ->
     sprintf "(%s,%s)" (pretty_context c) (pretty_context c')
  | MuTilde (id,p,cmd) ->
     sprintf "%s%s:%s.%s" (symbol ()).mutilde id (pretty_prop p) (pretty_command cmd)
and pretty_command =
 function
    Play (t,c) -> sprintf "<%s||%s>" (pretty_t3rm t) (pretty_context c)

(*Metavariable instantiation*)
let rec instantiate_in_t3rm id t =
 function
    TermMeta id' when id = id' ->
     (match t with Term t -> t | Context _ -> assert false)
  | DelegationTermMeta id' when id = id' ->
     (match t with Term t -> t | Context _ -> assert false)
  | TermMeta _
  | DelegationTermMeta _
  | True_constructor
  | Hyp _ as t' -> t' 
  | Lambda (id',p,t') -> Lambda (id',p,instantiate_in_t3rm id t t')
  | LambdaFO (id',p,t') -> LambdaFO (id',p,instantiate_in_t3rm id t t')
  | Cons'(c,t') ->
     Cons'
      (instantiate_in_context id t c,
       instantiate_in_t3rm id t t')
  | TermsPair (t',t'') ->
     TermsPair
      (instantiate_in_t3rm id t t',
       instantiate_in_t3rm id t t'')
  | TermsPairFO (p,t',t'') ->
     TermsPairFO (p,t', instantiate_in_t3rm id t t'')
  | Left t' -> Left (instantiate_in_t3rm id t t')
  | Right t' -> Right (instantiate_in_t3rm id t t')
  | Mu (id',p,cmd) -> Mu (id',p,instantiate_in_command id t cmd)
and instantiate_in_context id t =
 function
    ContextMeta id' when id = id' ->
     (match t with Term _ -> assert false | Context c -> c)
  | DelegationContextMeta id' when id = id' ->
     (match t with Term _ -> assert false | Context c -> c)
  | ContextMeta _
  | DelegationContextMeta _
  | False_eliminator
  | Concl _ as t' -> t' 
  | Cons (t',c) ->
     Cons
      (instantiate_in_t3rm id t t',
       instantiate_in_context id t c)
  | ConsFO (t',c) ->
     ConsFO (t',instantiate_in_context id t c)
  | Lambda' (id',p,c) -> Lambda' (id',p,instantiate_in_context id t c)
  | DestructTermsPair (id',p',id'',p'',cmd) ->
     DestructTermsPair (id',p',id'',p'',instantiate_in_command id t cmd)
  | DestructTermsPairFO (id',p',c) ->
     DestructTermsPairFO (id',p',instantiate_in_context id t c)
  | ContextsPair (c,c') ->
     ContextsPair
      (instantiate_in_context id t c,
       instantiate_in_context id t c')
  | MuTilde (id',p,cmd) -> MuTilde (id',p,instantiate_in_command id t cmd)
and instantiate_in_command id t =
 function
    Play (t',c) ->
     Play
      (instantiate_in_t3rm id t t',
       instantiate_in_context id t c)

let instantiate_t3rm    id t = instantiate_in_t3rm id (Term t)
let instantiate_context id c = instantiate_in_t3rm id (Context c)

let instantiate_pt_t3rm id t =
 function
  | Term t0    -> Term (instantiate_in_t3rm id (Term t) t0)
  | Context c0 -> Context (instantiate_in_context id (Term t) c0)

let instantiate_pt_context id c =
 function
  | Term t0    -> Term (instantiate_in_t3rm id (Context c) t0)
  | Context c0 -> Context (instantiate_in_context id (Context c) c0)


(* GOALS *)
(*
  - goals as a list of named hypothesis, a list of named conclusions,
    an active formula (in the left hand side or in the right hand side) and a
    local environment
  - a state pairs a goal list with a signature
  - the proof monad (or proof state, or cairn) guides the proof search
*)
type active_formula_position =
   LeftHandSide
 | RightHandSide

type goal_kind =
   GoalObligation
 | DelegationObligation

(* A goal is made of
    1. a list of hypothesis (most recent hypothesis first)
    2. a list of conclusions (most recent conclusion first)
    3. an active formula (either on the l.h.s. or on the r.h.s.) 
    4. a list of implicit variables and their types *)
type goal =
  { kind: goal_kind;
    hyp: (string * prop * bool) list;  (* true = visible; false = weakened *)
    ccl: (string * prop * bool) list;  (* true = visible; false = weakened *)
    active: (active_formula_position * prop); (*make two fields out of
    this one? *)
    env: (string * sort) list }

let new_goal prop = {kind=GoalObligation; hyp=[];ccl=[];active=(RightHandSide,prop);env=[]}
let new_delegation prop = {kind=DelegationObligation; hyp=[];ccl=[];active=(RightHandSide,prop);env=[]}

(*Transform goals and goal lists into propositions*)
let rec prop_of_csq = function
  | [] -> False
  | [(_,a,true)] -> a
  | (_,a,true)::p' -> 
      if !lj then (assert (p' = []) ; a)
      else BProp (a, Disj, (prop_of_csq p'))
  | (_,_,false)::p' ->
      assert (not !lj);
      prop_of_csq p'

let rec close_wrt_env p =
 function
    [] -> p
  | (b,s)::tl -> close_wrt_env (Quant (Forall,([b],s),p)) tl

let rec prop_of_goal_rec h p = match h with 
  | [] -> p
  | (_,a,true)::h' -> (prop_of_goal_rec h' (BProp (a,Imp,p)))
  | (_,_,false)::h' -> (prop_of_goal_rec h' p)

let prop_of_goal {hyp=h;ccl=c;active=a;env=e} = 
 let p =
  match a with
    | (LeftHandSide,p) -> prop_of_goal_rec (("",p,true)::h) (prop_of_csq c)
    | (RightHandSide,p) -> prop_of_goal_rec h (prop_of_csq (("",p,true)::c))
 in
  close_wrt_env p e

let rec prop_of_goals gl = match gl with 
  | [] -> assert false
  | [_,g] -> prop_of_goal g
  | (_,g)::s' -> BProp(prop_of_goal g,Conj,prop_of_goals s')


(* PROOF STATE *)
(* The integer is the 1-based index of the current goal in the list *)
type state = 
  { index: int;
    open_thm: (string * prop * bool) option;  (* false = theorem, true = anti-theorem *)
    open_refs: (string * sort) list;  (* the open theorem's discourse referents *)
    goals: (metaid * goal) list;
    sign: (sort Coll.t);
    thms: (prop Coll.t);
    mox: (prop Coll.t);
    pt: proof_t3rm }

let new_state () =
  { index=0; open_thm=None; open_refs=[]; goals=[]; sign=Coll.empty; thms=Coll.empty; mox=Coll.empty; pt=Term (TermMeta initial_meta) }

(*Transform states into propositions*)
let prop_of_state s = 
  Coll.fold (fun x y z -> BProp (y,Imp,z) ) s.thms (prop_of_goals s.goals)


(* CAIRN *)
(* The monad is composed of the following constructors :
   - Idle is the non-prooving mode. Axioms, variables are being declared, etc
   - Success, Subgoals and Exception belong to the proving mode *)
type proof_monad = 
  | Idle of message option * state 
  | Success of state
  | Subgoals of int * state  (* number of new goals generated, 0 = initial *)
  | Exception of message * state

type cairn = proof_monad

(*Cairn contructor*)
let new_cairn () = 
  Idle (None,new_state ())

(*Cairn destructors*)
let get_state = function
  | Idle (_,s) -> s
  | Success s -> s
  | Subgoals (_,s) -> s
  | Exception (_,s) -> s

let get_msg = function
  | Idle (Some msg,_) | Exception (msg,_) -> Some msg
  | _ -> None
  
let isScs = function
  | Success _ -> true
  | _ -> false

let isCptScs = function
  | Success s when s.goals=[] -> true
(*  | Exception (_,s) when s.goals=[] -> true *)
  | _ -> false

let isSub = function
  | Subgoals (_,_) -> true
  |_ -> false

let isExn = function
  | Exception (_,_) -> true
  | _ -> false

let isOngoing = function
  | Idle _ -> false
  | _ -> true

(*References for the proof state, and the proof trace*)
let cairn = ref (new_cairn ()) 

(* PROVERS *)
type provers = 
  | Coq 
  | Pvs
  | Isabelle

(*Provers pretty printer*)
let pretty_prover p = match p with
  | Coq -> "Coq"
  | Pvs -> "Pvs"
  | Isabelle -> "Isabelle"

(*Trace elements*)
type trace_atoms =
  | T_vars of string list * sort
  | T_axiom of string * prop
  | T_goal of state * string
  | T_tac of state * string
  | T_qed of proof_t3rm

(*The trace*)
let trace = ref ([] : trace_atoms list) (* newest command first *)
let history = ref [!cairn,!trace] (* newest status first *)

