/* Parser for fsp */

%{
  open Ast
%}

%token <string> IDENT
%token <int> ARITY
%token COQ PVS
%token DEFFUN DEFPRED DISCARD GOAL ACK QUIT 
%token AXIOM LEFT RIGHT IMPLY OR AND NOT COPY SWITCH
%token NOT IMPLY AND OR FORALL EXISTS TRUE FALSE
%token LPAR RPAR DOT VIR PVIR COLON UDS
%token EOF 

/* Token priorities / associativity */

%nonassoc FORALL EXISTS
%right ARROW
%left AND OR 
%nonassoc NEG

/* The grammar entry point */
%start prog

/* The type of values returned by the syntactic analyzer */
%type <Ast.script> prog

%%


prog:
| instr DOT                     { $1 }
| EOF                           { raise (Failure "end") } 
;

instr:
| DISCARD                       { Disc }
| ACK prover                    { Ack($2) }
| QUIT                          { Quit }
| GOAL p_expr                   { Gl($2) }
| DEFFUN varlist                { Deffun($2) }
| DEFPRED varlist               { Defpred($2) }
| tac                           { Tac($1) }
;

prover:
| COQ                           { Coq }
| PVS                           { Pvs }

tac: 
| AXIOM                         { Axiom }
| RIGHT IMPLY IDENT             { R_Imp($3) }
| LEFT IMPLY IDENT IDENT        { L_Imp($3,$4) }
| RIGHT AND                     { R_Conj }
| LEFT AND IDENT IDENT IDENT    { L_Conj($3,$4,$5) }
| RIGHT OR LEFT                 { R_Disj_L }
| RIGHT OR RIGHT                { R_Disj_R }
| LEFT OR IDENT IDENT IDENT     { L_Disj($3,$4,$5) }
| RIGHT NOT IDENT               { R_Neg($3) }
| LEFT NOT IDENT                { L_Neg($3) }
| RIGHT FORALL                  { R_Forall}
| LEFT FORALL IDENT t_expr      { L_Forall($3,$4) }
| RIGHT EXISTS t_expr           { R_Exists($3) }
| LEFT EXISTS IDENT             { L_Exists($3) }
| RIGHT COPY                    { R_Contr }
| LEFT COPY IDENT IDENT         { L_Contr($3,$4) }
| RIGHT SWITCH                  { R_Switch }
;

varlist:
| IDENT COLON ARITY             { [($1,$3)] }
| IDENT COLON ARITY VIR varlist { ($1,$3)::$5 }
;

t_expr:
| IDENT                         { Var($1) }
| IDENT t_expr_list             { Fun($1,max_int,$2) }
| LPAR t_expr RPAR              { $2 }
;

t_expr_list:
| /*liste vide*/                { [] }
| LPAR t_expr RPAR t_expr_list  { $2::$4 }

p_expr:
| TRUE                          { True }
| FALSE                         { False }
| IDENT t_expr_list             { Pred($1,max_int,$2) } 
| NOT p_expr                    { UProp(Neg,$2) }
| p_expr IMPLY p_expr           { BProp($1,Imp,$3) }
| p_expr OR p_expr              { BProp($1,Disj,$3) }
| p_expr AND p_expr             { BProp($1,Conj,$3) }
| FORALL IDENT VIR p_expr       { Quant(Forall,$2,$4) } 
| EXISTS IDENT VIR p_expr       { Quant(Exists,$2,$4) }
| LPAR p_expr RPAR              { $2 }
;

