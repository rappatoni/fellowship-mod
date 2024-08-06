open Instructions
open Tactics

let register_instr, help__instr, instr_list =
 let table = ref [] in
  (fun s info -> table := (s,info)::!table; s),
  (fun s -> List.assoc s !table),
  (fun () -> String.concat ", " 
      (List.map (fun (s,info) -> pretty_instruction s) !table) 
    ^ ".")

let register_tac, help__tac, tac_list =
 let table = ref [] in
  (fun s info -> table := (s,info)::!table; s),
  (fun s -> List.assoc s !table),
  (fun () -> String.concat ", " 
      (List.map (fun (s,info) -> pretty_tactic s) !table) 
    ^ ".")

let register_elim, help__elim =
 let table = ref [] in
  (fun s info -> table := (s,info)::!table; s),
  (fun s -> List.assoc s !table)

let (^^) s1 s2 = s1 ^ "\n" ^ s2
let (^^^) s1 s2 = s1 ^ "\n\n" ^ s2

let help_discard =
 register_instr Discard
  ("discard [all|theorem]",
   "Reset the system. \"discard all\" resets it to its initial state, while
   \"discard theorem\" only forgets the ongoing theorem.")

let help_next =
 register_instr Next
  ("next",
   "Consider the next open goal. If the current goal is the last one, consider" ^^
   "the first goal.")

let help_prev =
 register_instr Prev
  ("prev",
   "Consider the previous open goal. If the current goal is the first one," ^^
   "consider the last goal.")

let help_checkout =
 register_instr Ack
  ("checkout [coq|pvs]",
   "Generate a proof script for Coq or PVS and invoke the external proof" ^^
   "assistant to check it." ^^
   "See also \"checkout proof term\".")

let help_checkout_proof_term =
 register_instr AckProofTerm
  ("checkout proof term [coq|pvs]",
   "Generate a proof term for Coq or PVS and invoke the external proof" ^^
   "assistant to check it." ^^
   "See also \"checkout\".")

let help_export_natural_language =
 register_instr ExportNaturalLanguage
  ("checkout natural language",
   "Saves in a file the pseudo-natural language representation of the proof.")

let help_quit =
 register_instr Quit
  ("quit",
   "Terminate the current session")

let help_undo =
 register_instr Undo
  ("undo",
   "Undo the last successfull command")

let help_lj =
 register_instr (Lj true)
  ("lj",
   "Switch to intuitionistic sequent calculus")

let help_lk =
 register_instr (Lj false)
  ("lk",
   "Switch to classical sequent calculus")

let help_minimal =
 register_instr (Min true)
  ("minimal",
   "Switch to minimal logic (i.e. disable \"ex falso sequitur quodlibet\")")

let help_full =
 register_instr (Min false)
  ("full",
   "Switch to non minimal logic (i.e. enable \"ex falso sequitur quodlibet\")")

let help_goal =
 register_instr Goal
  ("theorem x : P",
   "Starts a proof of P. All the free variables in P must have been previously" ^^
   "declared using \"declare\"" )

let help_declare =
 register_instr Declare
  ("declare [A1,...,An : t | A : p]",
   "Declare A1,...,An : t declares A1,...,An as variables of type t." ^^
   "Two basic types are available: \"bool\", i.e. the type of propositions; " ^^
   "and \"type\" which is the type of user-defined types."^^^
   "Declare A : p declares the proposition p as being an axiom," ^^
   "named A. Note that the axiom is implicitely added to the proof environment" ^^
   "as an hypothesis: as such, it is focusable (type \"help focus\" for more" ^^
   "information).")

(*
let help_declare_axiom = 
 register_instr "declare axiom"
  ("declare axiom A : P",
   "Declare the proposition P as being an axiom, named A. Note that the axiom is implicitely added
   to the proof environment as an hypothesis: as such, it is focusable (type
   \"help focus\" for more information).")
*)

(*
let help_help =
 register_instr "help"
  ("help [command]",
   "Show the help for a given command. Without any argument shows the list" ^^
   "of available commands.")
*)

let help_axiom =
 register_tac Axiom
  ("axiom [x]",
   "This is one of the three primitive tactics. It implements the rule" ^^^
   "    ================================" ^^
   "    Gamma; *:P |- Delta, x:P, Delta'" ^^^
   "or the rule " ^^^
   "    ================================" ^^
   "    Gamma, x:P, Gamma' |- *:P; Delta" ^^^
   "If the optional argument x is not provided, the tactics looks for the " ^^
   "first conclusion/hypothesis (in an unspecified order) whose type matches" ^^
   " the current formula. " ^^^
   "Notice that in the second rule, x:P may be a previously defined
   theorem."^^
   "Notice also that these rules are a combination of the weakening and axiom " ^^
   "rules. Moreover it works both for atoms and for general formulae." ^^^
   "The proof term generated for * is: x")

let help_cut =
 register_tac Cut
  ("cut Q x",
   "This is one of the three primitive tactics. It implements the rule" ^^^
   "    Gamma, x:P |- *1:Q; Delta     Gamma, x:P; *2:Q |- Delta" ^^
   "    =======================================================" ^^
   "                   Gamma; *:P |- Delta" ^^^
   "or the rule " ^^^
   "    Gamma |- *1:Q; x:P, Delta     Gamma; *2:Q |- x:P, Delta" ^^
   "    =======================================================" ^^
   "                   Gamma |- *:P; Delta" ^^^
   "Notice that these rules are a combination of the contraction and cut rules." ^^
   "If the current logic is intuitionistic, than the right hand side of all" ^^
   "the sequents in the above rules are forced to have just one conclusion" ^^
   "(i.e. Delta is dropped in the sequent *1 of the first rule; both Delta " ^^
   "and x are dropped in the sequent *1, and x is dropped in the sequent *2" ^^
   "of the second rule)" ^^^
   "The proof term generated for * on the left  hand side is \\mu' x:P.<*1|*2>"^^
   "The proof term generated for * on the right hand side is \\mu  x:P.<*1|*2>"
)

let help_elim =
 register_tac Elim
  ("elim args",
   "This is one of the three primitive tactics. It applies the appropriate" ^^
   "elimination rule for the connective that is the type of the formula marked"^^
   "by * (the formula in the stoup)." ^^^
   "Type \"help elim [left|right] connective\" to get a description of the applied"^^
   "elimination rule for connective on the chosen side. Connective must be" ^^
   "one of the following: /\\, \\/, ->, -, ~, forall, exists, false, true." ^^^
   "See also the \"elim in\" derived tactic.")

let help_elim' b s = if b then "r" ^ s else "l" ^ s

let help_elim_left_and =
 register_elim "land"
  ("elim x y P",
   "The tactic implements the logical rule" ^^^
   "    Gamma, x:Q, y:Q' |- *1:P; Delta     Gamma, x:Q, y:Q'; *2:P |- Delta" ^^
   "    ===================================================================" ^^
   "                        Gamma; *:Q/\\Q' |- Delta" ^^^
   "If the current logic is intuitionistic, than the right hand side of all" ^^
   "the sequents in the above rules are forced to have just one conclusion" ^^
   "(i.e. Delta is dropped in the sequent *1)" ^^^
   "The proof term generated for * is (x:A,y:B).<*1|*2>")

let help_elim_right_and =
 register_elim "rand"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma |- *1:P; Delta     Gamma |- *2:Q; Delta" ^^
   "    ============================================" ^^
   "             Gamma |- *:P/\\Q; Delta" ^^^
   "Note that the rule also performs contraction over Gamma and Delta." ^^^
   "The proof term generated for * is (*1,*2)")

let help_elim_left_or =
 register_elim "lor"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma; *1:P |- Delta     Gamma; *2:Q |- Delta" ^^
   "    =============================================" ^^
   "             Gamma; *:P\\/Q |- Delta" ^^^
   "The proof term generated for * is (*1,*2)")

let help_elim_right_or =
 register_elim "ror"
  ("elim [left|right]",
   "The tactic implements the logical rule" ^^^
   "     Gamma |- *1:P; Delta" ^^
   "    =======================" ^^
   "    Gamma |- *:P\\/Q; Delta" ^^^
   " if the argument is left; otherwise it implements the rule" ^^^
   "     Gamma |- *1:Q; Delta" ^^
   "    =======================" ^^
   "    Gamma |- *:P\\/Q; Delta" ^^^
   "Note that these are the usual right elimination rules for additive " ^^
   "disjunction. Multiplicative disjunction can be derived as " ^^
   "cut (P \\/ Q) H ; [ elim left ; focus H; elim right | axiom ]" ^^^
   "The proof term generated for * is either _L_(*1) or _R_(*1)")

let help_elim_left_neg =
 register_elim "lneg"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma |- *1:P; Delta" ^^
   "    ====================" ^^
   "    Gamma; *:~P |- Delta" ^^^
   "in non-minimal logic. In minimal logic it implements the rule" ^^^
   "    Gamma |- *1:P; Delta    Gamma; *2:false |- Delta" ^^
   "    ================================================" ^^
   "                 Gamma; *:~P |- Delta" ^^^
   "Note that in minimal logic the tactic is equivalent to elim applied" ^^
   "to the goal obtained by replacing *:~P with *:(P -> false)" ^^
   "If the current logic is intuitionistic, than the right hand side of all" ^^
   "the sequents in the above rules are forced to have just one conclusion" ^^
   "(i.e. Delta is dropped in the sequent *1)" ^^^
   "The proof term generated for * in non-minimal logic is \\mu' x:~P.<x|*1 * _F_>" ^^
   "The proof term generated for * in minimal logic is     *1 * *2" ^^^
   "Note that the proof term generated for * in non-minimal logic is not" ^^
   "the smallest possible one, that is *1 * _F_. The one currently generated" ^^
   "has one redundant explicit bottom-up conversion (the \\mu'-eta-redex) for"^^
   "natural language explanation purposes." )

let help_elim_right_neg =
 register_elim "rneg"
  ("elim [x]",
   "The tactic implements the logical rule" ^^^
   "    Gamma; *1:P |- Delta" ^^
   "    ====================" ^^
   "    Gamma |- *:~P; Delta" ^^^
   "in non-minimal logic. In minimal logic it implements the rule" ^^^
   "    Gamma, x:P |- *1:false; Delta" ^^
   "    =============================" ^^
   "       Gamma |- *:~P; Delta" ^^^
   "Note that in minimal logic the tactic is equivalent to elim applied" ^^
   "to the goal obtained by replacing *:~P with *:(P -> false)" ^^^
   "The proof term generated for * in non-minimal logic is" ^^
   "   \\mu x:~P.<\\y:P.\\mu z:false.<y|*1>|x>" ^^
   "The proof term generated for * in minimal logic is \\x:P.*1" ^^^
   "Note that the proof term generated for * in non-minimal logic is not the"^^
   "smallest possible one, that is \\x:P.\\mu z:false.<x|*1>. The one currently"^^
   "generated has one redundant explicit top-down conversion (the \\mu-eta-redex)"^^
   "for natural language explanation purposes.")

let help_elim_left_imply =
 register_elim "limply"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma |- *1:P, Delta    Gamma; *2:Q |- Delta" ^^
   "    ============================================" ^^
   "               Gamma; *:P->Q |- Delta" ^^^
   "Note that the rules also performs contraction of Gamma and Delta." ^^
   "If the current logic is intuitionistic, than the right hand side of all" ^^
   "the sequents in the above rules are forced to have just one conclusion" ^^
   "(i.e. Delta is dropped in the sequent *1)" ^^^
   "The proof term generated for * is *1 * *2")

let help_elim_right_imply =
 register_elim "rimply"
  ("elim x",
   "The tactic implements the logical rule" ^^^
   "    Gamma, x:P |- *1:Q; Delta" ^^
   "    =========================" ^^
   "     Gamma |- *:P->Q; Delta" ^^^
   "The proof term generated for * is \\x:P.*1")

let help_elim_left_minus =
 register_elim "lminus"
  ("elim x",
   "The tactic implements the logical rule" ^^^
   "    Gamma; *1:P |- x:Q, Delta" ^^
   "    =========================" ^^
   "     Gamma; *:P-Q |- Delta" ^^^
   "If the current logic is intuitionistic, than the right hand side of all" ^^
   "the sequents in the above rules are forced to have just one conclusion" ^^
   "(i.e. Delta is dropped in the sequent *1)"  ^^^
   "The proof term generated for * is \\x:P.*1")

let help_elim_right_minus =
 register_elim "rminus"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma; *1:Q |- Delta    Gamma |- *2:P, Delta" ^^
   "    ============================================" ^^
   "                 Gamma |- *:P-Q; Delta" ^^^
   "Note that the rules also performs contraction of Gamma and Delta." ^^
   "The proof term generated for * is *1 * *2")

let help_elim_left_forall =
 register_elim "lforall"
  ("elim t",
   "The tactic implements the logical rule" ^^^
   "    Gamma; *1: Q[x<-t] |- Delta" ^^
   "  ================================== " ^^
   "  Gamma; *:forall (x:P) Q |- Delta")

let help_elim_right_forall =
 register_elim "rforall"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma |- *1: Q; Delta" ^^
   "  ================================== " ^^
   "  Gamma |- *:forall (x:P) Q; Delta")

let help_elim_left_exists =
 register_elim "lexists"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    Gamma; *1: Q |- Delta" ^^
   "  ================================== " ^^
   "  Gamma; *:forall (x:P) Q |- Delta")

let help_elim_right_exists =
 register_elim "rexists"
  ("elim t",
   "The tactic implements the logical rule" ^^^
   "    Gamma |- *1: Q[x<-t]; Delta" ^^
   "  ================================== " ^^
   "  Gamma |- *:forall (x:P) Q; Delta")

let help_elim_left_true =
 register_elim "ltrue"
  ("elim",
   "True has no elimination rule on the left hand side. The tactic fails.")

let help_elim_right_true =
 register_elim "rtrue"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    =======================" ^^
   "    Gamma |- *:True; Delta" ^^^
   "The proof term generated for * is _T_.")

let help_elim_left_false =
 register_elim "lfalse"
  ("elim",
   "The tactic implements the logical rule" ^^^
   "    =======================" ^^
   "    Gamma; *:False |- Delta" ^^^
   "This elimination rule is called \"ex falso sequitur quodlibet\" and it" ^^
   "does not hold in minimal logic. When the current logic is minimal, the" ^^
   "tactic fails." ^^^
   "The proof term generated for * is _F_.")

let help_elim_right_false =
 register_elim "rfalse"
  ("elim",
   "False has no elimination rule on the right hand side. The tactic fails.")

let help_idtac =
 register_tac Idtac
  ("idtac",
   "Do nothing and return the current goal as it is. This tactic never fails."^^
   "Formally this is neither a primitive not a derived tactic since it implements" ^^
   "no logical rule. It is one of the two 0-ary tacticals.")

let help_focus =
 register_tac Focus
  ("focus x y",
   "This derived tactic is equivalent to     cut P y ; [ axiom | idtac ]" ^^
   "if x is an hypothesis of type P, and to  cut P y ; [ idtac | axiom ]" ^^
   "if x a conclusion of type P." ^^
   "It implements the logical rule" ^^^
   "    Gamma, y:Q |- *1:P; Delta, x:P, Delta'" ^^
   "    ======================================" ^^
   "       Gamma; *:Q |- Delta, x:P, Delta'" ^^^
   " or the rule" ^^^
   "    Gamma, x:P, Gamma', y:Q; *1:P |- Delta" ^^
   "    ======================================" ^^
   "       Gamma, x:P, Gamma'; *:Q |- Delta" ^^^
   " or the rule" ^^^
   "    Gamma, x:P, Gamma'; *1:P |- y:Q, Delta" ^^
   "    ======================================" ^^
   "       Gamma, x:P, Gamma' |- *:Q; Delta" ^^^
   " or the rule" ^^^
   "    Gamma |- *1:P; y:Q, Delta, x:P, Delta'" ^^
   "    ======================================" ^^
   "       Gamma |- *:Q; Delta, x:P, Delta'" ^^^
   "Notice that these rules correspond to the contraction rule." ^^
   "Also, in the second and third rules, x:P may be a previously defined
   theorem / axiom."^^^
   "The proof term generated for * on the left  hand side is \\mu' y:Q.<x|*1>"^^
   "if x is an hypothesis; if x is a conclusion the term is  \\mu' y:Q.<*1|x>"^^
   "The proof term generated for * on the right hand side is \\mu  y:Q.<x|*1>"^^
   "if x is an hypothesis; if x is a conclusion the term is  \\mu  y:Q.<*1|x>")

let help_elim_in =
 register_tac Elim_In
  ("elim in x y args",
   "This derived tactic is equivalent to: focus x y ; elim args")

let help_contraction =
 register_tac Contraction
  ("contraction x",
   "This derived tactic is equivalent to either" ^^
   "   cut T x; [idtac | axiom x]" ^^
   "if the active formula T is on the right hand side or to" ^^
   "   cut T x; [axiom x | idtac]" ^^
   "if the active formula T is on the left hand side." ^^^
   "Note: contraction on the right hand side is not allowed in inuitionistic logic.")

let help_tacticals =
 "Syntax: tac1 ; tac2\n" ^
 "Apply tac1 to the current goal and tac2 on each goal generated by tac1.\n" ^
 "It fails if tac1 fails or tac2 fails on any goal.\n\n" ^
 "Syntax: tac ; [ tac1 | ... | tacn ]\n" ^
 "Apply tac to the current goal, that is supposed to generated n new goals.\n" ^
 "Then apply taci to the i-th goal for each i. It fails if tac produces an\n" ^
 "unexpected number of goals or if any tactic taci fails."

let help_sorts =
 "BNF Syntax: s ::= x | type | bool | s -> s | (s) \n" ^
 "Two predefined sorts exist: type and bool. New sorts can be declared" ^
 "(try \"help declare\")and built using the \"->\" operator."


(*********************)

let help_hint frm =
  Format.fprintf frm
   "To get help type \"help\", \"help instruction\" or \"help tacticals\" followed by a dot.@\n"

type help_args = 
  | Nix
  | HInstr of instruction
  | HTac of tactic
  | HElim of bool * string
  | HTacticals

let jack_help args =
 match args with
    Nix ->
     Print.echo (fun frm -> Format.fprintf frm
       "Available instructions: %s\nAvailable tactics: %s\nEach instruction must be ended by a dot.\nType \"help instruction\" to receive more help on a tactic or instruction.\nType \"help tacticals\" for a description of the available tacticals." 
       (instr_list ()) (tac_list ()) )
  | HInstr i -> 
      let syntax, semantics = help__instr i in
       Print.echo (fun frm -> Format.fprintf frm "Syntax: %s\n%s\n" syntax semantics)
  | HTac t -> 
      let syntax, semantics = help__tac t in
       Print.echo (fun frm -> Format.fprintf frm "Syntax: %s\n%s\n" syntax semantics)
  | HElim (b,s) -> 
      let syntax, semantics = help__elim (help_elim' b s) in
       Print.echo (fun frm -> Format.fprintf frm "Syntax: %s\n%s\n" syntax semantics)
  | HTacticals -> 
      Print.echo (fun frm -> Format.fprintf frm "%s" help_tacticals)

type script = 
  | Instruction of instr_plus_args
  | Tactical of tactical
  | Help of help_args

