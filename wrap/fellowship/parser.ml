type token =
  | IDENT of (
# 16 "parser.mly"
        string
# 6 "parser.ml"
)
  | COQ
  | PVS
  | ISABELLE
  | LJ
  | LK
  | MIN
  | FULL
  | DECLARE
  | THEOREM
  | NEXT
  | PREV
  | QED
  | CHECKOUT
  | EXPORT
  | PROOF
  | TERM
  | NATURAL
  | LANGUAGE
  | UNDO
  | DISCARD
  | QUIT
  | HELP
  | AXIOM
  | CUT
  | ELIM
  | IDTAC
  | IN
  | FOCUS
  | CONTRACTION
  | WEAKEN
  | TACTICALS
  | TYPES
  | TERMS
  | FORMULAE
  | PROP
  | SET
  | NEG
  | ARROW
  | MINUS
  | AND
  | OR
  | FORALL
  | EXISTS
  | TRUE
  | FALSE
  | LEFT
  | RIGHT
  | ALL
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | VIR
  | PVIR
  | PIPE
  | COLON
  | DOT
  | EOF
  | MOXIA
  | ANTITHEOREM
  | DENY

open Parsing
let _ = parse_error;;
# 8 "parser.mly"
  open Core
  open Tactics
  open Instructions
  open Help
  open Print

# 79 "parser.ml"
let yytransl_const = [|
  258 (* COQ *);
  259 (* PVS *);
  260 (* ISABELLE *);
  261 (* LJ *);
  262 (* LK *);
  263 (* MIN *);
  264 (* FULL *);
  265 (* DECLARE *);
  266 (* THEOREM *);
  267 (* NEXT *);
  268 (* PREV *);
  269 (* QED *);
  270 (* CHECKOUT *);
  271 (* EXPORT *);
  272 (* PROOF *);
  273 (* TERM *);
  274 (* NATURAL *);
  275 (* LANGUAGE *);
  276 (* UNDO *);
  277 (* DISCARD *);
  278 (* QUIT *);
  279 (* HELP *);
  280 (* AXIOM *);
  281 (* CUT *);
  282 (* ELIM *);
  283 (* IDTAC *);
  284 (* IN *);
  285 (* FOCUS *);
  286 (* CONTRACTION *);
  287 (* WEAKEN *);
  288 (* TACTICALS *);
  289 (* TYPES *);
  290 (* TERMS *);
  291 (* FORMULAE *);
  292 (* PROP *);
  293 (* SET *);
  294 (* NEG *);
  295 (* ARROW *);
  296 (* MINUS *);
  297 (* AND *);
  298 (* OR *);
  299 (* FORALL *);
  300 (* EXISTS *);
  301 (* TRUE *);
  302 (* FALSE *);
  303 (* LEFT *);
  304 (* RIGHT *);
  305 (* ALL *);
  306 (* LPAR *);
  307 (* RPAR *);
  308 (* LBRA *);
  309 (* RBRA *);
  310 (* VIR *);
  311 (* PVIR *);
  312 (* PIPE *);
  313 (* COLON *);
  314 (* DOT *);
    0 (* EOF *);
  315 (* MOXIA *);
  316 (* ANTITHEOREM *);
  317 (* DENY *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\005\000\
\005\000\005\000\009\000\009\000\007\000\007\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\004\000\004\000\014\000\014\000\014\000\014\000\015\000\
\015\000\016\000\016\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\013\000\
\013\000\011\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\002\000\002\000\004\000\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\002\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\001\000\001\000\001\000\002\000\
\003\000\005\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\001\000\
\001\000\002\000\000\000\001\000\001\000\001\000\003\000\001\000\
\002\000\001\000\003\000\001\000\001\000\001\000\002\000\002\000\
\003\000\003\000\003\000\003\000\006\000\006\000\003\000\001\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\016\000\017\000\018\000\
\021\000\022\000\023\000\000\000\000\000\027\000\000\000\030\000\
\000\000\031\000\032\000\000\000\034\000\035\000\037\000\038\000\
\012\000\039\000\020\000\019\000\092\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\028\000\000\000\008\000\009\000\
\010\000\011\000\005\000\006\000\036\000\001\000\000\000\063\000\
\064\000\065\000\056\000\057\000\000\000\000\000\002\000\000\000\
\059\000\060\000\000\000\000\000\040\000\025\000\026\000\046\000\
\045\000\000\000\000\000\078\000\000\000\000\000\000\000\076\000\
\077\000\000\000\000\000\074\000\000\000\000\000\072\000\066\000\
\000\000\000\000\000\000\049\000\050\000\051\000\047\000\048\000\
\052\000\053\000\054\000\055\000\007\000\000\000\089\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\090\000\
\080\000\000\000\091\000\073\000\070\000\069\000\068\000\062\000\
\000\000\000\000\000\000\000\000\000\000\087\000\000\000\000\000\
\000\000\000\000\075\000\000\000\000\000\042\000\000\000\000\000\
\000\000\043\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\029\000\030\000\031\000\055\000\114\000\033\000\066\000\093\000\
\115\000\056\000\057\000\105\000\059\000\113\000\078\000\079\000\
\075\000"

let yysindex = "\016\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\255\007\255\000\000\250\254\000\000\
\105\255\000\000\000\000\014\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\255\254\120\255\004\255\
\120\255\051\255\054\255\000\000\000\000\254\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\255\000\000\
\000\000\000\000\000\000\000\000\140\255\001\255\000\000\120\255\
\000\000\000\000\021\255\150\255\000\000\000\000\000\000\000\000\
\000\000\214\255\082\255\000\000\140\255\082\255\082\255\000\000\
\000\000\140\255\164\255\000\000\001\255\002\255\000\000\000\000\
\108\255\167\255\004\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\255\000\000\033\255\
\038\255\040\255\171\255\140\255\140\255\140\255\140\255\000\000\
\000\000\010\255\000\000\000\000\000\000\000\000\000\000\000\000\
\052\255\091\255\047\255\043\255\043\255\000\000\178\255\033\255\
\033\255\033\255\000\000\043\255\167\255\000\000\230\254\232\254\
\052\255\000\000\140\255\140\255\178\255\178\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\255\000\000\000\000\000\000\000\000\
\045\255\000\000\000\000\046\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\070\255\075\255\
\236\255\000\000\000\000\000\000\000\000\090\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\236\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\227\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\092\255\000\000\192\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\104\255\100\255\000\000\000\000\000\000\000\000\110\255\196\255\
\200\255\234\255\000\000\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\000\000\000\000\112\255\118\255"

let yygindex = "\000\000\
\000\000\000\000\138\000\241\255\005\000\156\000\000\000\000\000\
\046\000\000\000\101\000\239\255\154\000\026\000\110\000\190\255\
\187\255"

let yytablesize = 318
let yytable = "\096\000\
\025\000\076\000\076\000\036\000\099\000\032\000\024\000\024\000\
\024\000\024\000\076\000\108\000\124\000\058\000\124\000\058\000\
\001\000\061\000\034\000\058\000\058\000\058\000\058\000\041\000\
\035\000\045\000\041\000\131\000\041\000\132\000\119\000\120\000\
\121\000\122\000\071\000\071\000\071\000\071\000\058\000\108\000\
\080\000\045\000\037\000\109\000\064\000\065\000\033\000\033\000\
\033\000\033\000\077\000\077\000\024\000\024\000\107\000\024\000\
\046\000\024\000\060\000\077\000\123\000\133\000\134\000\024\000\
\083\000\058\000\058\000\062\000\058\000\067\000\058\000\058\000\
\063\000\058\000\058\000\088\000\058\000\081\000\110\000\111\000\
\071\000\071\000\094\000\071\000\054\000\071\000\071\000\071\000\
\071\000\071\000\124\000\071\000\033\000\033\000\116\000\033\000\
\117\000\033\000\033\000\126\000\033\000\033\000\004\000\033\000\
\061\000\061\000\061\000\061\000\109\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\047\000\048\000\049\000\050\000\014\000\015\000\016\000\067\000\
\018\000\019\000\038\000\021\000\003\000\022\000\023\000\024\000\
\039\000\040\000\041\000\042\000\068\000\127\000\128\000\110\000\
\111\000\060\000\125\000\033\000\088\000\129\000\061\000\061\000\
\044\000\061\000\043\000\061\000\061\000\053\000\061\000\061\000\
\081\000\061\000\085\000\026\000\027\000\028\000\051\000\052\000\
\086\000\053\000\130\000\054\000\044\000\018\000\019\000\020\000\
\021\000\069\000\022\000\023\000\024\000\112\000\070\000\071\000\
\072\000\073\000\106\000\000\000\000\000\074\000\018\000\019\000\
\020\000\021\000\000\000\022\000\023\000\024\000\000\000\000\000\
\000\000\082\000\100\000\101\000\102\000\103\000\000\000\000\000\
\026\000\100\000\101\000\102\000\103\000\000\000\104\000\054\000\
\100\000\101\000\102\000\103\000\095\000\118\000\054\000\097\000\
\098\000\026\000\000\000\000\000\000\000\054\000\079\000\079\000\
\079\000\079\000\082\000\082\000\082\000\082\000\084\000\084\000\
\084\000\084\000\079\000\000\000\000\000\000\000\082\000\000\000\
\000\000\000\000\084\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\092\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\083\000\083\000\083\000\083\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\083\000\022\000\023\000\024\000\
\067\000\000\000\067\000\067\000\000\000\067\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\027\000\028\000"

let yycheck = "\069\000\
\000\000\001\001\001\001\010\001\074\000\001\000\001\001\002\001\
\003\001\004\001\001\001\078\000\039\001\031\000\039\001\033\000\
\001\000\033\000\016\001\001\001\002\001\003\001\004\001\053\001\
\018\001\028\001\056\001\054\001\058\001\054\001\100\000\101\000\
\102\000\103\000\001\001\002\001\003\001\004\001\056\000\106\000\
\056\000\028\001\049\001\001\001\047\001\048\001\001\001\002\001\
\003\001\004\001\050\001\050\001\047\001\048\001\053\001\050\001\
\058\001\052\001\055\001\050\001\051\001\131\000\132\000\058\001\
\060\000\047\001\048\001\017\001\050\001\054\001\052\001\053\001\
\019\001\055\001\056\001\057\001\058\001\057\001\036\001\037\001\
\047\001\048\001\001\001\050\001\052\001\052\001\053\001\054\001\
\055\001\056\001\039\001\058\001\047\001\048\001\057\001\050\001\
\057\001\052\001\053\001\053\001\055\001\056\001\058\001\058\001\
\001\001\002\001\003\001\004\001\001\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\001\001\002\001\003\001\004\001\020\001\021\001\022\001\058\001\
\024\001\025\001\026\001\027\001\058\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\001\001\116\000\117\000\036\001\
\037\001\055\001\056\001\058\001\057\001\124\000\047\001\048\001\
\053\001\050\001\017\000\052\001\053\001\050\001\055\001\056\001\
\051\001\058\001\051\001\059\001\060\001\061\001\047\001\048\001\
\051\001\050\001\125\000\052\001\017\000\024\001\025\001\026\001\
\027\001\038\001\029\001\030\001\031\001\081\000\043\001\044\001\
\045\001\046\001\077\000\255\255\255\255\050\001\024\001\025\001\
\026\001\027\001\255\255\029\001\030\001\031\001\255\255\255\255\
\255\255\052\001\039\001\040\001\041\001\042\001\255\255\255\255\
\059\001\039\001\040\001\041\001\042\001\255\255\051\001\052\001\
\039\001\040\001\041\001\042\001\067\000\051\001\052\001\070\000\
\071\000\059\001\255\255\255\255\255\255\052\001\039\001\040\001\
\041\001\042\001\039\001\040\001\041\001\042\001\039\001\040\001\
\041\001\042\001\051\001\255\255\255\255\255\255\051\001\255\255\
\255\255\255\255\051\001\038\001\039\001\040\001\041\001\042\001\
\043\001\044\001\045\001\046\001\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\039\001\040\001\041\001\042\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\051\001\029\001\030\001\031\001\
\053\001\255\255\055\001\056\001\255\255\058\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\059\001\060\001\061\001"

let yynames_const = "\
  COQ\000\
  PVS\000\
  ISABELLE\000\
  LJ\000\
  LK\000\
  MIN\000\
  FULL\000\
  DECLARE\000\
  THEOREM\000\
  NEXT\000\
  PREV\000\
  QED\000\
  CHECKOUT\000\
  EXPORT\000\
  PROOF\000\
  TERM\000\
  NATURAL\000\
  LANGUAGE\000\
  UNDO\000\
  DISCARD\000\
  QUIT\000\
  HELP\000\
  AXIOM\000\
  CUT\000\
  ELIM\000\
  IDTAC\000\
  IN\000\
  FOCUS\000\
  CONTRACTION\000\
  WEAKEN\000\
  TACTICALS\000\
  TYPES\000\
  TERMS\000\
  FORMULAE\000\
  PROP\000\
  SET\000\
  NEG\000\
  ARROW\000\
  MINUS\000\
  AND\000\
  OR\000\
  FORALL\000\
  EXISTS\000\
  TRUE\000\
  FALSE\000\
  LEFT\000\
  RIGHT\000\
  ALL\000\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  VIR\000\
  PVIR\000\
  PIPE\000\
  COLON\000\
  DOT\000\
  EOF\000\
  MOXIA\000\
  ANTITHEOREM\000\
  DENY\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'command) in
    Obj.repr(
# 45 "parser.mly"
                                           ( _1 )
# 403 "parser.ml"
               : Help.script))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 48 "parser.mly"
                                           ( Instruction (_1,_2) )
# 411 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 49 "parser.mly"
                                           ( Tactical _1 )
# 418 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                                           ( Help Nix )
# 424 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 52 "parser.mly"
                                           ( Help (HInstr _2) )
# 431 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tac) in
    Obj.repr(
# 53 "parser.mly"
                                           ( Help (HTac _2) )
# 438 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dir) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'connector) in
    Obj.repr(
# 54 "parser.mly"
                                           ( Help (HElim (_3,_4)) )
# 446 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                                           ( Help HTacticals )
# 452 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
                                           ( Help HTypes )
# 458 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                           ( Help HTerms )
# 464 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                           ( Help HFormulae )
# 470 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                           ( raise (Failure "end") )
# 476 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                                           ( Lj true )
# 482 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                           ( Lj false )
# 488 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                           ( Min true )
# 494 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                           ( Min false )
# 500 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                           ( Declare )
# 506 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                           ( Theorem )
# 512 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                           ( Deny )
# 518 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                           ( AntiTheorem )
# 524 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                           ( Next )
# 530 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                           ( Prev )
# 536 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( Qed )
# 542 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( CheckOut )
# 548 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                           ( CheckOutProofTerm )
# 554 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                                           ( ExportNaturalLanguage )
# 560 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                           ( if !toplvl then Undo 
					     else raise Parsing.Parse_error )
# 567 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                                           ( if !toplvl then DiscardAll
					     else raise Parsing.Parse_error )
# 574 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( if !toplvl then DiscardTheorem
					     else raise Parsing.Parse_error )
# 581 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                           ( Quit )
# 587 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                           ( Axiom )
# 593 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                           ( Cut )
# 599 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                                           ( Elim )
# 605 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                                           ( Idtac )
# 611 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                                           ( Focus )
# 617 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                                           ( Elim_In )
# 623 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                           ( Contraction )
# 629 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                           ( Weaken )
# 635 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                           ( Moxia )
# 641 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tac) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 100 "parser.mly"
                                           ( TPlug (_1,_2,symbol_start_pos ()) )
# 649 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 101 "parser.mly"
                                           ( Then (_1,_3,symbol_start_pos ()) )
# 657 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'tactical) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'taclist) in
    Obj.repr(
# 102 "parser.mly"
                                           ( Thens (_1,_4,symbol_start_pos ()) )
# 665 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taclist) in
    Obj.repr(
# 105 "parser.mly"
                                           ( _1::_3 )
# 673 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 106 "parser.mly"
                                           ( [_1] )
# 680 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                                           ( true )
# 686 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                                           ( false )
# 692 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                                           ( "and" )
# 698 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                                           ( "or" )
# 704 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                                           ( "neg" )
# 710 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                                           ( "imply" )
# 716 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                                           ( "minus" )
# 722 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                           ( "forall" )
# 728 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                           ( "exists" )
# 734 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                                           ( "true" )
# 740 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
                                           ( "false" )
# 746 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                                          ( OnTheLeft )
# 752 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                                          ( OnTheRight )
# 758 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
                                          ( Ident _1 )
# 765 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 127 "parser.mly"
                                          ( Formula _1 )
# 772 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 128 "parser.mly"
                                          ( Expression _1 )
# 779 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 129 "parser.mly"
                                          ( Labeled_sort (_1,_3) )
# 787 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 130 "parser.mly"
                                          ( Labeled_prop (_1,_3) )
# 795 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                                          ( Prover Coq )
# 801 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                          ( Prover Pvs )
# 807 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                                          ( Prover Isabelle )
# 813 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 136 "parser.mly"
                                           ( _1::_2 )
# 821 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                                           ( [] )
# 827 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                           ( SSet )
# 833 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
                                           ( SProp )
# 839 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "parser.mly"
                                           ( SSym _1 )
# 846 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 143 "parser.mly"
                                           ( SArr (_1,_3) )
# 854 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 147 "parser.mly"
                                           ( _1 )
# 861 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 148 "parser.mly"
                                           ( TApp (_1,_2) )
# 869 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "parser.mly"
                                           ( TSym _1 )
# 876 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 153 "parser.mly"
                                           ( _2 )
# 883 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
                                           ( True )
# 889 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
                                           ( False )
# 895 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                                           ( PSym _1 )
# 902 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 159 "parser.mly"
                                           ( UProp(Neg,_2) )
# 909 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 160 "parser.mly"
                                           ( PApp(_1,_2) )
# 917 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 161 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 925 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 162 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 933 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 163 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 941 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 164 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 949 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 165 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 958 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 166 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 967 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 167 "parser.mly"
                                           ( _2 )
# 974 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 171 "parser.mly"
                                           ( [_1] )
# 981 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 172 "parser.mly"
                                           ( _1::_3 )
# 989 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 176 "parser.mly"
                                           ( _2 )
# 996 "parser.ml"
               : 'delimited_p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 179 "parser.mly"
                                           ( _2 )
# 1003 "parser.ml"
               : 'delimited_t_expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Help.script)
