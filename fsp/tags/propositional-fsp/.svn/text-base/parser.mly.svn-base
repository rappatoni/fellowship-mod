/* Parser for fsp */

%{
  open Ast
%}

%token <string> IDENT
%token COQ PVS
%token DEFINE DISCARD GOAL ACK QUIT 
%token AXIOM LEFT RIGHT IMPLY OR AND NOT COPY SWITCH
%token DOT
%token NEG ARROW AND OR TRUE FALSE
%token LPAR RPAR VIR PVIR 
%token EOF 

/* Token priorities and associativity */

%right ARROW
%left AND OR 
%nonassoc NEG

/* The grammar entry point */
%start prog

/* The type of values returned by the syntactic analyzer */
%type <Ast.script> prog

%%


prog:
| instr DOT                 { $1 }
| EOF                       { raise (Failure "end") } 
;

instr:
| DISCARD                   { Disc }
| ACK prover                { Ack($2) }
| QUIT                      { Quit }
| GOAL expr                 { Gl($2) }
| DEFINE varlist            { Vars($2) }
| tac                       { Tac($1) }
;

prover:
| COQ                       { Coq }
| PVS                       { Pvs }

tac: 
| AXIOM                     { Axiom }
| RIGHT IMPLY IDENT         { R_Imp($3) }
| LEFT IMPLY IDENT IDENT    { L_Imp($3,$4) }
| RIGHT AND                 { R_Conj }
| LEFT AND IDENT IDENT IDENT { L_Conj($3,$4,$5) }
| RIGHT OR LEFT             { R_Disj_L }
| RIGHT OR RIGHT            { R_Disj_R }
| LEFT OR IDENT IDENT IDENT { L_Disj($3,$4,$5) }
| RIGHT NOT IDENT           { R_Neg($3) }
| LEFT NOT IDENT            { L_Neg($3) }
| RIGHT COPY                { R_Contr }
| LEFT COPY IDENT IDENT     { L_Contr($3,$4) }
| RIGHT SWITCH              { R_Switch }
;

varlist:
| IDENT                     { [$1] }
| IDENT VIR varlist         { $1::$3 }
;

expr:
| IDENT                     { Const($1) }
| TRUE                      { True }
| FALSE                     { False }
| NEG expr                  { UProp(Neg,$2) }
| expr ARROW expr           { BProp($1,Imp,$3) }
| expr OR expr              { BProp($1,Disj,$3) }
| expr AND expr             { BProp($1,Conj,$3) }
| LPAR expr RPAR            { $2 }
;

