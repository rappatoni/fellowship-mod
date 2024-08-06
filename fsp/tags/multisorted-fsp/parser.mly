/* Parser for fsp */

%{
  open Core
  open Commands
%}

%token <string> IDENT
%token COQ PVS
%token DEFINE DISCARD GOAL ACK QUIT 
%token PROP SET
%token AXIOM LEFT RIGHT IMPLY OR AND NOT COPY SWITCH
%token NOT IMPLY AND OR FORALL EXISTS TRUE FALSE
%token LPAR RPAR DOT VIR PVIR COLON
%token EOF 

/* Token priorities / associativity */

%nonassoc FORALL EXISTS
%right ARROW
%left AND OR 
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
| ACK prover                               { Ack $2 }
| QUIT                                     { Quit }
| GOAL p_expr                              { Gl $2 }
| DEFINE varlist COLON s_expr              { Define ($2,$4) }
| tac                                      { Tac $1 }
;

prover:
| COQ                                      { Coq }
| PVS                                      { Pvs }

tac: 
| AXIOM                                    { axiom }
| RIGHT IMPLY IDENT                        { r_Imp $3 }
| LEFT IMPLY IDENT IDENT                   { l_Imp $3 $4 }
| RIGHT AND                                { r_Conj }
| LEFT AND IDENT IDENT IDENT               { l_Conj $3 $4 $5 }
| RIGHT OR LEFT                            { r_Disj_L }
| RIGHT OR RIGHT                           { r_Disj_R }
| LEFT OR IDENT IDENT IDENT                { l_Disj $3 $4 $5 }
| RIGHT NOT IDENT                          { r_Neg $3 }
| LEFT NOT IDENT                           { l_Neg $3 }
| RIGHT FORALL                             { r_Forall}
| LEFT FORALL IDENT t_expr                 { l_Forall $3 $4 }
| RIGHT EXISTS t_expr                      { r_Exists $3 }
| LEFT EXISTS IDENT                        { l_Exists $3 }
| RIGHT COPY                               { r_Contr }
| LEFT COPY IDENT IDENT                    { l_Contr $3 $4 }
| RIGHT SWITCH                             { r_Switch }
;

varlist:
| IDENT                                    { [$1] }
| IDENT VIR varlist                        { $1::$3 }
;

s_expr:
| SET                                      { SSet }
| PROP                                     { SProp }
| IDENT                                    { SSym $1 }
| s_expr IMPLY s_expr                      { SArr ($1,$3) }
;

t_expr:
| IDENT                                    { TSym $1  }
| t_expr t_expr                            { TApp ($1,$2) }
| LPAR t_expr RPAR                         { $2 }
;

p_expr:
| TRUE                                     { True }
| FALSE                                    { False }
| IDENT                                    { PSym $1 }
| p_expr t_expr                            { PApp($1,$2) } 
| NOT p_expr                               { UProp(Neg,$2) }
| p_expr IMPLY p_expr                      { BProp($1,Imp,$3) }
| p_expr OR p_expr                         { BProp($1,Disj,$3) }
| p_expr AND p_expr                        { BProp($1,Conj,$3) }
| FORALL varlist COLON s_expr VIR p_expr   { Quant(Forall,($2,$4),$6) } 
| EXISTS varlist COLON s_expr VIR p_expr   { Quant(Exists,($2,$4),$6) }
| LPAR p_expr RPAR                         { $2 }
;

