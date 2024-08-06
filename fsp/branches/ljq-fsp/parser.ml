type token =
  | IDENT of (string)
  | COQ
  | PVS
  | DECLARE
  | DISCARD
  | NEXT
  | PREV
  | GOAL
  | ACK
  | PROOF
  | TERM
  | QUIT
  | UNDO
  | LJ
  | LK
  | MIN
  | FULL
  | HELP
  | AXIOM
  | CUT
  | ELIM
  | IDTAC
  | IN
  | FOCUS
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

open Parsing;;
# 4 "parser.mly"
  open Core
  open Commands
  open Help
# 56 "parser.ml"
let yytransl_const = [|
  258 (* COQ *);
  259 (* PVS *);
  260 (* DECLARE *);
  261 (* DISCARD *);
  262 (* NEXT *);
  263 (* PREV *);
  264 (* GOAL *);
  265 (* ACK *);
  266 (* PROOF *);
  267 (* TERM *);
  268 (* QUIT *);
  269 (* UNDO *);
  270 (* LJ *);
  271 (* LK *);
  272 (* MIN *);
  273 (* FULL *);
  274 (* HELP *);
  275 (* AXIOM *);
  276 (* CUT *);
  277 (* ELIM *);
  278 (* IDTAC *);
  279 (* IN *);
  280 (* FOCUS *);
  281 (* PROP *);
  282 (* SET *);
  283 (* NEG *);
  284 (* ARROW *);
  285 (* MINUS *);
  286 (* AND *);
  287 (* OR *);
  288 (* FORALL *);
  289 (* EXISTS *);
  290 (* TRUE *);
  291 (* FALSE *);
  292 (* LEFT *);
  293 (* RIGHT *);
  294 (* LPAR *);
  295 (* RPAR *);
  296 (* LBRA *);
  297 (* RBRA *);
  298 (* VIR *);
  299 (* PVIR *);
  300 (* PIPE *);
  301 (* COLON *);
  302 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\003\000\003\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\013\000\013\000\011\000\011\000\
\014\000\014\000\014\000\014\000\014\000\010\000\010\000\006\000\
\006\000\007\000\007\000\007\000\007\000\016\000\016\000\017\000\
\017\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\005\000\015\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\002\000\004\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\002\000\004\000\001\000\001\000\001\000\001\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\003\000\002\000\001\000\
\003\000\005\000\003\000\005\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\000\000\001\000\
\003\000\001\000\001\000\001\000\003\000\001\000\002\000\001\000\
\003\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\006\000\006\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\004\000\005\000\000\000\000\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\000\000\
\000\000\000\000\040\000\000\000\002\000\096\000\000\000\000\000\
\000\000\000\000\000\000\017\000\035\000\036\000\000\000\006\000\
\016\000\033\000\020\000\021\000\022\000\032\000\000\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\045\000\046\000\
\000\000\049\000\050\000\015\000\034\000\048\000\067\000\065\000\
\066\000\000\000\068\000\037\000\000\000\069\000\000\000\000\000\
\039\000\000\000\001\000\000\000\000\000\000\000\084\000\000\000\
\000\000\000\000\082\000\083\000\000\000\000\000\000\000\000\000\
\051\000\062\000\061\000\000\000\080\000\000\000\000\000\078\000\
\070\000\038\000\000\000\043\000\000\000\000\000\073\000\076\000\
\075\000\074\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\094\000\086\000\007\000\024\000\054\000\
\055\000\056\000\052\000\053\000\057\000\058\000\059\000\060\000\
\000\000\095\000\079\000\000\000\000\000\000\000\000\000\000\000\
\000\000\093\000\000\000\000\000\000\000\000\000\081\000\044\000\
\000\000\042\000\000\000\000\000\000\000\063\000\000\000\000\000\
\000\000\000\000"

let yydgoto = "\002\000\
\022\000\023\000\032\000\052\000\059\000\026\000\099\000\125\000\
\053\000\060\000\126\000\054\000\084\000\061\000\062\000\087\000\
\109\000\078\000"

let yysindex = "\005\000\
\001\000\000\000\008\255\000\000\000\000\000\000\026\255\160\255\
\000\000\000\000\000\000\000\000\000\000\000\000\096\255\010\255\
\026\255\015\255\000\000\028\255\000\000\000\000\020\255\037\255\
\050\255\045\255\061\255\000\000\000\000\000\000\087\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\119\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\247\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\255\000\000\000\000\010\255\000\000\106\255\118\255\
\000\000\133\255\000\000\001\255\008\255\014\255\000\000\061\255\
\008\255\008\255\000\000\000\000\061\255\030\255\069\255\120\255\
\000\000\000\000\000\000\126\255\000\000\025\255\004\255\000\000\
\000\000\000\000\148\255\000\000\145\255\037\255\000\000\000\000\
\000\000\000\000\122\255\025\255\123\255\127\255\048\255\061\255\
\061\255\061\255\061\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\255\000\000\000\000\010\255\230\254\110\255\014\255\014\255\
\014\255\000\000\053\255\025\255\025\255\025\255\000\000\000\000\
\145\255\000\000\122\255\238\254\002\255\000\000\061\255\061\255\
\053\255\053\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\129\255\130\255\
\000\000\130\255\000\000\000\000\000\000\000\000\000\000\131\255\
\134\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\132\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\135\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\130\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\089\255\000\000\000\000\
\000\000\000\000\136\255\093\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\130\255\139\255\000\000\000\000\000\000\
\000\000\000\000\144\255\097\255\109\255\113\255\000\000\000\000\
\000\000\000\000\043\255\000\000\000\000\000\000\000\000\000\000\
\146\255\147\255"

let yygindex = "\000\000\
\000\000\000\000\105\000\000\000\058\000\195\255\018\000\002\000\
\000\000\238\255\050\000\000\000\000\000\000\000\000\000\102\000\
\202\255\186\255"

let yytablesize = 281
let yytable = "\065\000\
\021\000\100\000\024\000\088\000\085\000\001\000\103\000\095\000\
\025\000\127\000\055\000\101\000\102\000\081\000\096\000\055\000\
\068\000\137\000\085\000\016\000\017\000\018\000\019\000\143\000\
\020\000\085\000\082\000\083\000\066\000\127\000\085\000\088\000\
\123\000\131\000\132\000\133\000\134\000\064\000\097\000\098\000\
\093\000\086\000\089\000\144\000\122\000\056\000\057\000\027\000\
\085\000\058\000\056\000\057\000\027\000\085\000\058\000\086\000\
\135\000\104\000\105\000\106\000\107\000\071\000\086\000\027\000\
\028\000\067\000\123\000\086\000\108\000\094\000\029\000\030\000\
\145\000\146\000\063\000\104\000\105\000\106\000\107\000\068\000\
\104\000\105\000\106\000\107\000\077\000\086\000\130\000\072\000\
\077\000\070\000\086\000\069\000\073\000\074\000\075\000\076\000\
\033\000\079\000\077\000\034\000\035\000\036\000\037\000\038\000\
\039\000\136\000\090\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\049\000\050\000\091\000\051\000\
\085\000\085\000\085\000\085\000\088\000\088\000\088\000\088\000\
\080\000\041\000\111\000\085\000\041\000\092\000\041\000\088\000\
\090\000\090\000\090\000\090\000\089\000\089\000\089\000\089\000\
\139\000\140\000\141\000\090\000\124\000\127\000\138\000\089\000\
\112\000\113\000\114\000\115\000\116\000\117\000\118\000\119\000\
\120\000\029\000\030\000\016\000\017\000\018\000\019\000\128\000\
\020\000\031\000\071\000\129\000\071\000\071\000\014\000\071\000\
\019\000\023\000\072\000\064\000\047\000\018\000\087\000\110\000\
\091\000\092\000\142\000\121\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\004\000\005\000\006\000\
\007\000\008\000\000\000\000\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\000\000\
\020\000"

let yycheck = "\018\000\
\000\000\072\000\001\000\058\000\001\001\001\000\077\000\069\000\
\001\001\028\001\001\001\073\000\074\000\023\001\001\001\001\001\
\043\001\044\001\001\001\019\001\020\001\021\001\022\001\042\001\
\024\001\001\001\036\001\037\001\001\001\028\001\001\001\086\000\
\087\000\104\000\105\000\106\000\107\000\023\001\025\001\026\001\
\040\001\038\001\061\000\042\001\041\001\036\001\037\001\038\001\
\001\001\040\001\036\001\037\001\038\001\001\001\040\001\038\001\
\039\001\028\001\029\001\030\001\031\001\001\001\038\001\038\001\
\007\000\046\001\121\000\038\001\039\001\068\000\002\001\003\001\
\143\000\144\000\017\000\028\001\029\001\030\001\031\001\043\001\
\028\001\029\001\030\001\031\001\042\001\038\001\039\001\027\001\
\046\001\045\001\038\001\042\001\032\001\033\001\034\001\035\001\
\001\001\011\001\038\001\004\001\005\001\006\001\007\001\008\001\
\009\001\124\000\001\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\024\001\
\028\001\029\001\030\001\031\001\028\001\029\001\030\001\031\001\
\010\001\041\001\011\001\039\001\044\001\001\001\046\001\039\001\
\028\001\029\001\030\001\031\001\028\001\029\001\030\001\031\001\
\127\000\128\000\129\000\039\001\001\001\028\001\041\001\039\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\002\001\003\001\019\001\020\001\021\001\022\001\045\001\
\024\001\010\001\041\001\045\001\043\001\044\001\046\001\046\001\
\046\001\046\001\045\001\041\001\046\001\046\001\039\001\079\000\
\039\001\039\001\137\000\086\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001"

let yynames_const = "\
  COQ\000\
  PVS\000\
  DECLARE\000\
  DISCARD\000\
  NEXT\000\
  PREV\000\
  GOAL\000\
  ACK\000\
  PROOF\000\
  TERM\000\
  QUIT\000\
  UNDO\000\
  LJ\000\
  LK\000\
  MIN\000\
  FULL\000\
  HELP\000\
  AXIOM\000\
  CUT\000\
  ELIM\000\
  IDTAC\000\
  IN\000\
  FOCUS\000\
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
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'instr) in
    Obj.repr(
# 35 "parser.mly"
                                           ( _1 )
# 348 "parser.ml"
               : Commands.script))
; (fun parser_env ->
    Obj.repr(
# 36 "parser.mly"
                                           ( raise (Failure "end") )
# 354 "parser.ml"
               : Commands.script))
; (fun parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                           ( Disc )
# 360 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 41 "parser.mly"
                                           ( Next )
# 366 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 42 "parser.mly"
                                           ( Prev )
# 372 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'prover) in
    Obj.repr(
# 43 "parser.mly"
                                           ( Ack _2 )
# 379 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _4 = (peek_val parser_env 0 : 'prover) in
    Obj.repr(
# 44 "parser.mly"
                                           ( AckProofTerm _4 )
# 386 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 45 "parser.mly"
                                           ( Quit )
# 392 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                           ( Undo )
# 398 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                           ( Lj true )
# 404 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 48 "parser.mly"
                                           ( Lj false )
# 410 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 49 "parser.mly"
                                           ( Min true )
# 416 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 50 "parser.mly"
                                           ( Min false )
# 422 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 51 "parser.mly"
                                           ( Help None )
# 428 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'hinstr) in
    Obj.repr(
# 52 "parser.mly"
                                           ( Help (Some _2) )
# 435 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                           ( Help (Some _2) )
# 442 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 54 "parser.mly"
                                           ( Gl _2 )
# 449 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'varlist) in
    let _4 = (peek_val parser_env 0 : 's_expr) in
    Obj.repr(
# 55 "parser.mly"
                                           ( Vars (_2,_4) )
# 457 "parser.ml"
               : 'instr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'tac) in
    Obj.repr(
# 56 "parser.mly"
                                           ( Tac _1 )
# 464 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 60 "parser.mly"
                                           ( help_discard )
# 470 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 61 "parser.mly"
                                           ( help_next )
# 476 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 62 "parser.mly"
                                           ( help_prev )
# 482 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                           ( help_checkout )
# 488 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                           ( help_checkout_proof_term )
# 494 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                           ( help_quit )
# 500 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                           ( help_undo )
# 506 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                           ( help_lj )
# 512 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                           ( help_lk )
# 518 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                           ( help_minimal )
# 524 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                           ( help_full )
# 530 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                           ( help_help )
# 536 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( help_goal )
# 542 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( help_declare )
# 548 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'htac) in
    Obj.repr(
# 74 "parser.mly"
                                           ( _1 )
# 555 "parser.ml"
               : 'hinstr))
; (fun parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                           ( Coq )
# 561 "parser.ml"
               : 'prover))
; (fun parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( Pvs )
# 567 "parser.ml"
               : 'prover))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'args) in
    Obj.repr(
# 84 "parser.mly"
                                           ( Axiom _2 )
# 574 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'delimited_p_expr) in
    let _3 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                                           ( Cut (_3,_2) )
# 582 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'args) in
    Obj.repr(
# 86 "parser.mly"
                                           ( Elim (_2) )
# 589 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 88 "parser.mly"
                                           ( Idtac )
# 595 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'tac) in
    let _3 = (peek_val parser_env 0 : 'tac) in
    Obj.repr(
# 89 "parser.mly"
                                           ( Then (_1,_3) )
# 603 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'tac) in
    let _4 = (peek_val parser_env 1 : 'taclist) in
    Obj.repr(
# 90 "parser.mly"
                                           ( Thens (_1,_4) )
# 611 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : string) in
    let _3 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                                           ( Focus (_2,_3) )
# 619 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _3 = (peek_val parser_env 2 : string) in
    let _4 = (peek_val parser_env 1 : string) in
    let _5 = (peek_val parser_env 0 : 'args) in
    Obj.repr(
# 93 "parser.mly"
                                           ( Elim_In (_3,_4,_5) )
# 628 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 98 "parser.mly"
                                           ( help_axiom )
# 634 "parser.ml"
               : 'htac))
; (fun parser_env ->
    Obj.repr(
# 99 "parser.mly"
                                           ( help_cut )
# 640 "parser.ml"
               : 'htac))
; (fun parser_env ->
    Obj.repr(
# 100 "parser.mly"
                                           ( help_elim )
# 646 "parser.ml"
               : 'htac))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'helim) in
    Obj.repr(
# 101 "parser.mly"
                                           ( _1 )
# 653 "parser.ml"
               : 'htac))
; (fun parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                           ( help_idtac )
# 659 "parser.ml"
               : 'htac))
; (fun parser_env ->
    Obj.repr(
# 105 "parser.mly"
                                           ( help_focus )
# 665 "parser.ml"
               : 'htac))
; (fun parser_env ->
    Obj.repr(
# 106 "parser.mly"
                                           ( help_elim_in )
# 671 "parser.ml"
               : 'htac))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 110 "parser.mly"
                                           ( help_elim' _2 "and" )
# 678 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 111 "parser.mly"
                                           ( help_elim' _2 "or" )
# 685 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 112 "parser.mly"
                                           ( help_elim' _2 "neg" )
# 692 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 113 "parser.mly"
                                           ( help_elim' _2 "imply" )
# 699 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 114 "parser.mly"
                                           ( help_elim' _2 "minus" )
# 706 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 115 "parser.mly"
                                           ( help_elim' _2 "forall" )
# 713 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 116 "parser.mly"
                                           ( help_elim' _2 "exists" )
# 720 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 117 "parser.mly"
                                           ( help_elim' _2 "true" )
# 727 "parser.ml"
               : 'helim))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'dir) in
    Obj.repr(
# 118 "parser.mly"
                                           ( help_elim' _2 "false" )
# 734 "parser.ml"
               : 'helim))
; (fun parser_env ->
    Obj.repr(
# 122 "parser.mly"
                                           ( true )
# 740 "parser.ml"
               : 'dir))
; (fun parser_env ->
    Obj.repr(
# 123 "parser.mly"
                                           ( false )
# 746 "parser.ml"
               : 'dir))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'tac) in
    let _3 = (peek_val parser_env 0 : 'taclist) in
    Obj.repr(
# 126 "parser.mly"
                                           ( _1::_3 )
# 754 "parser.ml"
               : 'taclist))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'tac) in
    Obj.repr(
# 127 "parser.mly"
                                           ( [_1] )
# 761 "parser.ml"
               : 'taclist))
; (fun parser_env ->
    Obj.repr(
# 130 "parser.mly"
                                          ( OnTheLeft )
# 767 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 131 "parser.mly"
                                          ( OnTheRight )
# 773 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
                                          ( Ident _1 )
# 780 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 133 "parser.mly"
                                          ( Formula _1 )
# 787 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 134 "parser.mly"
                                          ( Expression _1 )
# 794 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'arg) in
    let _2 = (peek_val parser_env 0 : 'args) in
    Obj.repr(
# 137 "parser.mly"
                                           ( _1::_2 )
# 802 "parser.ml"
               : 'args))
; (fun parser_env ->
    Obj.repr(
# 138 "parser.mly"
                                           ( [] )
# 808 "parser.ml"
               : 'args))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 141 "parser.mly"
                                           ( [_1] )
# 815 "parser.ml"
               : 'varlist))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _3 = (peek_val parser_env 0 : 'varlist) in
    Obj.repr(
# 142 "parser.mly"
                                           ( _1::_3 )
# 823 "parser.ml"
               : 'varlist))
; (fun parser_env ->
    Obj.repr(
# 146 "parser.mly"
                                           ( SSet )
# 829 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    Obj.repr(
# 147 "parser.mly"
                                           ( SProp )
# 835 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 148 "parser.mly"
                                           ( SSym _1 )
# 842 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 's_expr) in
    let _3 = (peek_val parser_env 0 : 's_expr) in
    Obj.repr(
# 149 "parser.mly"
                                           ( SArr (_1,_3) )
# 850 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 't_exprat) in
    Obj.repr(
# 153 "parser.mly"
                                           ( _1 )
# 857 "parser.ml"
               : 't_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 't_expr) in
    let _2 = (peek_val parser_env 0 : 't_exprat) in
    Obj.repr(
# 154 "parser.mly"
                                           ( TApp (_1,_2) )
# 865 "parser.ml"
               : 't_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                                           ( TSym _1 )
# 872 "parser.ml"
               : 't_exprat))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 't_expr) in
    Obj.repr(
# 159 "parser.mly"
                                           ( _2 )
# 879 "parser.ml"
               : 't_exprat))
; (fun parser_env ->
    Obj.repr(
# 162 "parser.mly"
                                           ( True )
# 885 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    Obj.repr(
# 163 "parser.mly"
                                           ( False )
# 891 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 164 "parser.mly"
                                           ( PSym _1 )
# 898 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 165 "parser.mly"
                                           ( UProp(Neg,_2) )
# 905 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'p_expr) in
    let _2 = (peek_val parser_env 0 : 't_exprat) in
    Obj.repr(
# 166 "parser.mly"
                                           ( PApp(_1,_2) )
# 913 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 167 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 921 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 168 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 929 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 169 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 937 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 170 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 945 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'varlist) in
    let _4 = (peek_val parser_env 2 : 's_expr) in
    let _6 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 171 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 954 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'varlist) in
    let _4 = (peek_val parser_env 2 : 's_expr) in
    let _6 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 172 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 963 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'p_expr) in
    Obj.repr(
# 173 "parser.mly"
                                           ( _2 )
# 970 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'p_expr) in
    Obj.repr(
# 177 "parser.mly"
                              ( _2 )
# 977 "parser.ml"
               : 'delimited_p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 't_expr) in
    Obj.repr(
# 180 "parser.mly"
                              ( _2 )
# 984 "parser.ml"
               : 'delimited_t_expr))
(* Entry prog *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Commands.script)
