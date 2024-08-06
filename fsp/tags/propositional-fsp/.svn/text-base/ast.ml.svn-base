(*Objects : propositions*)
type prop = 
  | True | False
  | Const of string 
  | UProp of unop * prop
  | BProp of prop * binop * prop
and unop = Neg
and binop = Imp | Conj | Disj

(*Objects : 
  - environments are lists of constants
  - goals as a list of named hypothesis and a conclusion
  - a state pairs a goal list with an environment
  - the proof monad (or proof state, or cairn) guides the proof search
*)
type env = string list 

type goal = Goal of (string * prop) list * prop list

type state = (goal list) * env 

type proof_monad = 
  | Success of state
  | Subgoals of int * state
  | Exception of string * state

type cairn = proof_monad

(*Provers*)
type provers = 
  | Coq 
  | Pvs
	
(*Commands*)
type tactic = 
  | Axiom
  | R_Imp of string | L_Imp of (string *string)
  | R_Neg of string | L_Neg of string 
  | R_Conj | L_Conj of (string*string*string)
  | R_Disj_L | R_Disj_R | L_Disj of (string*string*string)
  | R_Contr | L_Contr of (string*string)
  | R_Switch

type instruction =
  | Vars of string list (* ident list plus tard*)
  | Disc (*discard environment*)
  | Gl of prop
  | Tac of tactic
  | Ack of provers (*require acknowledgement from outside prover*)
  | Quit

type script = instruction

(*Trace*)
type trace_atoms =
  | T_vars of string list
  | T_goal of state
  | T_tac of state

(*Option : use constructive / classical logic 
Why here and not in main.ml ? 
Because it is very pervasive. *)
let lj = ref true (* constructive by default *)

(*Methods and functions*)
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

let get_const p = 
  let rec get_const_rec p l = match p with 
    | True -> l
    | False -> l
    | UProp(Neg,p) -> get_const_rec p l
    | Const(s) -> s::l
    | BProp(p1,op,p2) -> 
	(get_const_rec p1 [])@(get_const_rec p2 [])@l
  in get_const_rec p []

(*let rec decomp h l n cons = match h with 
  | BProp(a,cons,b) -> decomp b (a::l) (n+1) cons
  | _ -> (h,l,n)*)
       
(*Decomposing the proof state*)
let get_goallist s = (fst s)
let get_env s = (snd s)

let get_current_hyps s = match s with 
  | (Goal(h,c)::r, env) -> h
  | _ -> []

let get_current_ccls s = match s with 
  | (Goal(h,c)::r, env) -> c
  | _ -> []

(*Monad contructor*)
let create_cairn () = Subgoals(0,([],[]))
			   
(*Breaking down the monad*)
let get_state m = match m with
  | Success(s) -> s
  | Subgoals(n,s) -> s
  | Exception(str,s) -> s

let get_msg m = match m with 
  | Success(s) -> ""
  | Subgoals(n,s) -> ""
  | Exception(str,s) -> str    

let prop_of_cairn m = prop_of_goallist (get_goallist (get_state m)) 

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

