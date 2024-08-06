/* Parser for fsp */

%{
  open Core
  open Commands
  open Help
%}

%token <string> IDENT
%token COQ PVS
%token DECLARE DISCARD NEXT PREV GOAL ACK PROOF TERM QUIT UNDO LJ LK MIN FULL HELP
%token AXIOM CUT ELIM IDTAC IN FOCUS
%token PROP SET
%token NEG ARROW MINUS AND OR FORALL EXISTS TRUE FALSE LEFT RIGHT
%token LPAR RPAR LBRA RBRA VIR PVIR PIPE COLON DOT
%token EOF 

/* Token priorities / associativity */

%nonassoc FORALL EXISTS
%right ARROW PVIR
%left MINUS AND OR 
%nonassoc NEG

/* The grammar entry point */
%start prog

/* The type of values returned by the syntactic analyzer */
%type <Commands.script> prog

%%


prog:
| instr DOT                                { $1 }
| EOF                                      { raise (Failure "end") } 
;

instr:
| DISCARD                                  { Disc }
| NEXT                                     { Next }
| PREV                                     { Prev }
| ACK prover                               { Ack $2 }
| ACK PROOF TERM prover                    { AckProofTerm $4 }
| QUIT                                     { Quit }
| UNDO                                     { Undo }
| LJ                                       { Lj true }
| LK                                       { Lj false }
| MIN                                      { Min true }
| FULL                                     { Min false }
| HELP                                     { Help None }
| HELP hinstr                              { Help (Some $2) }
| HELP IDENT                               { Help (Some $2) }
| GOAL delimited_p_expr                    { Gl $2 }
| DECLARE varlist COLON s_expr             { Vars ($2,$4) }
| tac                                      { Tac $1 }
;

hinstr:
| DISCARD                                  { help_discard }
| NEXT                                     { help_next }
| PREV                                     { help_prev }
| ACK                                      { help_checkout }
| ACK PROOF TERM                           { help_checkout_proof_term }
| QUIT                                     { help_quit }
| UNDO                                     { help_undo }
| LJ                                       { help_lj }
| LK                                       { help_lk }
| MIN                                      { help_minimal }
| FULL                                     { help_full }
| HELP                                     { help_help }
| GOAL                                     { help_goal }
| DECLARE                                  { help_declare }
| htac                                     { $1 }
;


prover:
| COQ                                      { Coq }
| PVS                                      { Pvs }

tac: 
/* primitive tactics */
| AXIOM args                               { Axiom $2 }
| CUT delimited_p_expr IDENT               { Cut ($3,$2) }
| ELIM args                                { Elim ($2) }
/* tacticals */
| IDTAC                                    { Idtac }
| tac PVIR tac                             { Then ($1,$3) }
| tac PVIR LBRA taclist RBRA               { Thens ($1,$4) }
/* derived tactics */
| FOCUS IDENT IDENT                        { Focus ($2,$3) }
| ELIM IN IDENT IDENT args                 { Elim_In ($3,$4,$5) }
;

htac: 
/* primitive tactics */
| AXIOM                                    { help_axiom }
| CUT                                      { help_cut }
| ELIM                                     { help_elim }
| helim                                    { $1 }
/* tacticals */
| IDTAC                                    { help_idtac }
/* derived tactics */
| FOCUS                                    { help_focus }
| ELIM IN                                  { help_elim_in }
;

helim:
| ELIM dir AND                             { help_elim' $2 "and" }
| ELIM dir OR                              { help_elim' $2 "or" }
| ELIM dir NEG                             { help_elim' $2 "neg" }
| ELIM dir ARROW                           { help_elim' $2 "imply" }
| ELIM dir MINUS                           { help_elim' $2 "minus" }
| ELIM dir FORALL                          { help_elim' $2 "forall" }
| ELIM dir EXISTS                          { help_elim' $2 "exists" }
| ELIM dir TRUE                            { help_elim' $2 "true" }
| ELIM dir FALSE                           { help_elim' $2 "false" }
;

dir:
| RIGHT                                    { true }
| LEFT                                     { false }

taclist:
| tac PIPE taclist                         { $1::$3 }
| tac                                      { [$1] }

arg:
| LEFT                                    { OnTheLeft }
| RIGHT                                   { OnTheRight }
| IDENT                                   { Ident $1 }
| delimited_p_expr                        { Formula $1 }
| delimited_t_expr                        { Expression $1 }

args:
| arg args                                 { $1::$2 }
|                                          { [] }

varlist:
| IDENT                                    { [$1] }
| IDENT VIR varlist                        { $1::$3 }
;

s_expr:
| SET                                      { SSet }
| PROP                                     { SProp }
| IDENT                                    { SSym $1 }
| s_expr ARROW s_expr                      { SArr ($1,$3) }
;

t_expr:
| t_exprat                                 { $1 }
| t_expr t_exprat                          { TApp ($1,$2) }
;

t_exprat:
| IDENT                                    { TSym $1 }
| LPAR t_expr RPAR                         { $2 }

p_expr:
| TRUE                                     { True }
| FALSE                                    { False }
| IDENT                                    { PSym $1 }
| NEG p_expr                               { UProp(Neg,$2) }
| p_expr t_exprat                          { PApp($1,$2) }
| p_expr ARROW p_expr                      { BProp($1,Imp,$3) }
| p_expr MINUS p_expr                      { BProp($1,Minus,$3) }
| p_expr OR p_expr                         { BProp($1,Disj,$3) }
| p_expr AND p_expr                        { BProp($1,Conj,$3) }
| FORALL varlist COLON s_expr VIR p_expr   { Quant(Forall,($2,$4),$6) } 
| EXISTS varlist COLON s_expr VIR p_expr   { Quant(Exists,($2,$4),$6) }
| LPAR p_expr RPAR                         { $2 }
;

delimited_p_expr:
| LPAR p_expr RPAR            { $2 }

delimited_t_expr:
| LBRA t_expr RBRA            { $2 }
