type token =
  | IDENT of (string)
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
  | ACK
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
  | TACTICALS
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

open Parsing;;
# 4 "parser.mly"
  open Core
  open Tactics
  open Instructions
  open Help
  open Print

# 66 "parser.ml"
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
  270 (* ACK *);
  271 (* PROOF *);
  272 (* TERM *);
  273 (* NATURAL *);
  274 (* LANGUAGE *);
  275 (* UNDO *);
  276 (* DISCARD *);
  277 (* QUIT *);
  278 (* HELP *);
  279 (* AXIOM *);
  280 (* CUT *);
  281 (* ELIM *);
  282 (* IDTAC *);
  283 (* IN *);
  284 (* FOCUS *);
  285 (* CONTRACTION *);
  286 (* TACTICALS *);
  287 (* PROP *);
  288 (* SET *);
  289 (* NEG *);
  290 (* ARROW *);
  291 (* MINUS *);
  292 (* AND *);
  293 (* OR *);
  294 (* FORALL *);
  295 (* EXISTS *);
  296 (* TRUE *);
  297 (* FALSE *);
  298 (* LEFT *);
  299 (* RIGHT *);
  300 (* ALL *);
  301 (* LPAR *);
  302 (* RPAR *);
  303 (* LBRA *);
  304 (* RBRA *);
  305 (* VIR *);
  306 (* PVIR *);
  307 (* PIPE *);
  308 (* COLON *);
  309 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\004\000\004\000\
\004\000\008\000\008\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\003\000\003\000\013\000\013\000\013\000\013\000\
\014\000\014\000\015\000\015\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\012\000\012\000\010\000\011\000\000\000"

let yylen = "\002\000\
\003\000\002\000\002\000\003\000\003\000\005\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\001\000\002\000\003\000\
\005\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\001\000\001\000\001\000\
\001\000\001\000\002\000\000\000\001\000\001\000\001\000\003\000\
\001\000\002\000\001\000\003\000\001\000\001\000\001\000\002\000\
\002\000\003\000\003\000\003\000\003\000\006\000\006\000\003\000\
\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\000\000\021\000\022\000\023\000\000\000\
\024\000\025\000\000\000\027\000\028\000\030\000\008\000\085\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\029\000\000\000\054\000\055\000\056\000\058\000\
\047\000\048\000\057\000\000\000\000\000\000\000\000\000\050\000\
\051\000\000\000\000\000\002\000\031\000\019\000\020\000\037\000\
\036\000\000\000\007\000\004\000\005\000\000\000\071\000\000\000\
\000\000\000\000\069\000\070\000\000\000\000\000\067\000\000\000\
\000\000\065\000\001\000\059\000\000\000\000\000\000\000\040\000\
\041\000\042\000\038\000\039\000\043\000\044\000\045\000\046\000\
\000\000\000\000\082\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\083\000\073\000\000\000\084\000\066\000\
\063\000\062\000\061\000\053\000\000\000\000\000\000\000\006\000\
\000\000\000\000\080\000\000\000\000\000\000\000\000\000\068\000\
\000\000\000\000\033\000\000\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\024\000\025\000\046\000\110\000\027\000\058\000\089\000\111\000\
\047\000\048\000\101\000\050\000\109\000\073\000\074\000\070\000"

let yysindex = "\037\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\253\254\000\000\000\000\000\000\155\255\
\000\000\000\000\242\254\000\000\000\000\000\000\000\000\000\000\
\185\255\228\254\185\255\043\255\049\255\063\255\019\255\000\000\
\034\255\040\255\000\000\052\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\079\255\001\255\050\255\185\255\000\000\
\000\000\059\255\168\255\000\000\000\000\000\000\000\000\000\000\
\000\000\218\255\000\000\000\000\000\000\112\255\000\000\079\255\
\112\255\112\255\000\000\000\000\079\255\164\255\000\000\001\255\
\015\255\000\000\000\000\000\000\098\255\219\255\071\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\070\255\052\255\000\000\080\255\076\255\082\255\170\255\079\255\
\079\255\079\255\079\255\000\000\000\000\016\255\000\000\000\000\
\000\000\000\000\000\000\000\000\097\255\064\255\084\255\000\000\
\009\255\009\255\000\000\240\254\080\255\080\255\080\255\000\000\
\009\255\219\255\000\000\233\254\246\254\097\255\000\000\079\255\
\079\255\240\254\240\254"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\106\255\000\000\000\000\000\000\000\000\
\000\000\000\000\041\255\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\104\255\000\000\000\000\087\255\000\000\000\000\
\000\000\000\000\000\000\005\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\104\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\161\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\255\000\000\136\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\094\255\108\255\000\000\000\000\
\000\000\000\000\000\000\100\255\187\255\200\255\204\255\000\000\
\000\000\000\000\000\000\000\000\000\000\026\255\000\000\000\000\
\000\000\131\255\144\255"

let yygindex = "\000\000\
\000\000\142\000\232\255\003\000\186\000\000\000\000\000\081\000\
\000\000\136\000\075\000\016\000\012\000\146\000\220\255\192\255"

let yytablesize = 286
let yytable = "\092\000\
\023\000\071\000\053\000\026\000\095\000\049\000\049\000\049\000\
\049\000\105\000\121\000\028\000\035\000\029\000\049\000\071\000\
\071\000\096\000\097\000\098\000\099\000\051\000\076\000\121\000\
\052\000\128\000\064\000\064\000\064\000\064\000\045\000\116\000\
\117\000\118\000\119\000\064\000\104\000\001\000\129\000\106\000\
\107\000\026\000\026\000\026\000\026\000\072\000\049\000\049\000\
\049\000\049\000\026\000\049\000\049\000\079\000\049\000\049\000\
\081\000\049\000\054\000\072\000\072\000\120\000\103\000\130\000\
\131\000\104\000\055\000\064\000\064\000\064\000\064\000\059\000\
\064\000\064\000\064\000\064\000\064\000\091\000\064\000\063\000\
\093\000\094\000\026\000\026\000\026\000\026\000\060\000\026\000\
\026\000\035\000\026\000\026\000\061\000\026\000\052\000\052\000\
\052\000\052\000\105\000\049\000\062\000\049\000\075\000\052\000\
\056\000\057\000\018\000\018\000\018\000\018\000\077\000\064\000\
\090\000\051\000\122\000\018\000\065\000\066\000\067\000\068\000\
\051\000\049\000\112\000\069\000\124\000\125\000\045\000\113\000\
\106\000\107\000\121\000\123\000\126\000\114\000\081\000\052\000\
\052\000\052\000\052\000\026\000\052\000\052\000\044\000\052\000\
\052\000\074\000\052\000\018\000\018\000\018\000\018\000\060\000\
\018\000\060\000\060\000\035\000\060\000\033\000\018\000\003\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\072\000\072\000\072\000\072\000\013\000\014\000\015\000\
\078\000\017\000\018\000\030\000\020\000\072\000\021\000\022\000\
\031\000\036\000\037\000\038\000\039\000\079\000\017\000\018\000\
\019\000\020\000\040\000\021\000\022\000\096\000\097\000\098\000\
\099\000\034\000\127\000\096\000\097\000\098\000\099\000\032\000\
\032\000\100\000\045\000\032\000\108\000\032\000\078\000\115\000\
\045\000\102\000\000\000\000\000\075\000\075\000\075\000\075\000\
\000\000\000\000\041\000\042\000\043\000\044\000\000\000\045\000\
\075\000\077\000\077\000\077\000\077\000\076\000\076\000\076\000\
\076\000\017\000\018\000\019\000\020\000\077\000\021\000\022\000\
\000\000\076\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\000\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\000\000\
\000\000\000\000\000\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\000\000\021\000\022\000"

let yycheck = "\064\000\
\000\000\001\001\027\000\001\000\069\000\001\001\002\001\003\001\
\004\001\001\001\034\001\015\001\027\001\017\001\010\001\001\001\
\001\001\034\001\035\001\036\001\037\001\050\001\047\000\034\001\
\053\001\049\001\001\001\002\001\003\001\004\001\047\001\096\000\
\097\000\098\000\099\000\010\001\073\000\001\000\049\001\031\001\
\032\001\001\001\002\001\003\001\004\001\045\001\042\001\043\001\
\044\001\045\001\010\001\047\001\048\001\051\000\050\001\051\001\
\052\001\053\001\016\001\045\001\045\001\046\001\048\001\128\000\
\129\000\102\000\018\001\042\001\043\001\044\001\045\001\053\001\
\047\001\048\001\049\001\050\001\051\001\062\000\053\001\001\001\
\065\000\066\000\042\001\043\001\044\001\045\001\053\001\047\001\
\048\001\027\001\050\001\051\001\053\001\053\001\001\001\002\001\
\003\001\004\001\001\001\025\000\049\001\027\000\053\001\010\001\
\042\001\043\001\001\001\002\001\003\001\004\001\052\001\033\001\
\001\001\050\001\051\001\010\001\038\001\039\001\040\001\041\001\
\050\001\047\000\053\001\045\001\113\000\114\000\047\001\052\001\
\031\001\032\001\034\001\048\001\121\000\052\001\052\001\042\001\
\043\001\044\001\045\001\053\001\047\001\048\001\045\001\050\001\
\051\001\046\001\053\001\042\001\043\001\044\001\045\001\048\001\
\047\001\050\001\051\001\048\001\053\001\016\000\053\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\034\001\035\001\036\001\037\001\019\001\020\001\021\001\
\046\001\023\001\024\001\025\001\026\001\046\001\028\001\029\001\
\030\001\001\001\002\001\003\001\004\001\046\001\023\001\024\001\
\025\001\026\001\010\001\028\001\029\001\034\001\035\001\036\001\
\037\001\016\000\122\000\034\001\035\001\036\001\037\001\053\001\
\048\001\046\001\047\001\051\001\077\000\053\001\047\001\046\001\
\047\001\072\000\255\255\255\255\034\001\035\001\036\001\037\001\
\255\255\255\255\042\001\043\001\044\001\045\001\255\255\047\001\
\046\001\034\001\035\001\036\001\037\001\034\001\035\001\036\001\
\037\001\023\001\024\001\025\001\026\001\046\001\028\001\029\001\
\255\255\046\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\255\255\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\029\001"

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
  ACK\000\
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
  TACTICALS\000\
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
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'instr) in
    let _2 = (peek_val parser_env 1 : 'args) in
    Obj.repr(
# 41 "parser.mly"
                                           ( Instruction (_1,_2) )
# 363 "parser.ml"
               : Help.script))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'tactical) in
    Obj.repr(
# 42 "parser.mly"
                                           ( Tactical _1 )
# 370 "parser.ml"
               : Help.script))
; (fun parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                           ( Help Nix )
# 376 "parser.ml"
               : Help.script))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'instr) in
    Obj.repr(
# 45 "parser.mly"
                                           ( Help (HInstr _2) )
# 383 "parser.ml"
               : Help.script))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'tac) in
    Obj.repr(
# 46 "parser.mly"
                                           ( Help (HTac _2) )
# 390 "parser.ml"
               : Help.script))
; (fun parser_env ->
    let _3 = (peek_val parser_env 2 : 'dir) in
    let _4 = (peek_val parser_env 1 : 'connector) in
    Obj.repr(
# 47 "parser.mly"
                                           ( Help (HElim (_3,_4)) )
# 398 "parser.ml"
               : Help.script))
; (fun parser_env ->
    Obj.repr(
# 48 "parser.mly"
                                           ( Help HTacticals )
# 404 "parser.ml"
               : Help.script))
; (fun parser_env ->
    Obj.repr(
# 49 "parser.mly"
                                           ( raise (Failure "end") )
# 410 "parser.ml"
               : Help.script))
; (fun parser_env ->
    Obj.repr(
# 52 "parser.mly"
                                           ( Lj true )
# 416 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 53 "parser.mly"
                                           ( Lj false )
# 422 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 54 "parser.mly"
                                           ( Min true )
# 428 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 55 "parser.mly"
                                           ( Min false )
# 434 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 56 "parser.mly"
                                           ( Declare )
# 440 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                           ( Goal )
# 446 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                           ( Next )
# 452 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                           ( Prev )
# 458 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 60 "parser.mly"
                                           ( Qed )
# 464 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 61 "parser.mly"
                                           ( Ack )
# 470 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 62 "parser.mly"
                                           ( AckProofTerm )
# 476 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                           ( ExportNaturalLanguage )
# 482 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                           ( if !toplvl then Undo 
					     else raise Parsing.Parse_error )
# 489 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                           ( if !toplvl then Discard
					     else raise Parsing.Parse_error )
# 496 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                           ( Quit )
# 502 "parser.ml"
               : 'instr))
; (fun parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( Axiom )
# 508 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( Cut )
# 514 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                           ( Elim )
# 520 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                           ( Idtac )
# 526 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 78 "parser.mly"
                                           ( Focus )
# 532 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                           ( Elim_In )
# 538 "parser.ml"
               : 'tac))
; (fun parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( Contraction )
# 544 "parser.ml"
               : 'tac))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'tac) in
    let _2 = (peek_val parser_env 0 : 'args) in
    Obj.repr(
# 84 "parser.mly"
                                           ( TPlug (_1,_2,symbol_start_pos ()) )
# 552 "parser.ml"
               : 'tactical))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'tactical) in
    let _3 = (peek_val parser_env 0 : 'tactical) in
    Obj.repr(
# 85 "parser.mly"
                                           ( Then (_1,_3,symbol_start_pos ()) )
# 560 "parser.ml"
               : 'tactical))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'tactical) in
    let _4 = (peek_val parser_env 1 : 'taclist) in
    Obj.repr(
# 86 "parser.mly"
                                           ( Thens (_1,_4,symbol_start_pos ()) )
# 568 "parser.ml"
               : 'tactical))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'tactical) in
    let _3 = (peek_val parser_env 0 : 'taclist) in
    Obj.repr(
# 89 "parser.mly"
                                           ( _1::_3 )
# 576 "parser.ml"
               : 'taclist))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'tactical) in
    Obj.repr(
# 90 "parser.mly"
                                           ( [_1] )
# 583 "parser.ml"
               : 'taclist))
; (fun parser_env ->
    Obj.repr(
# 93 "parser.mly"
                                           ( true )
# 589 "parser.ml"
               : 'dir))
; (fun parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                           ( false )
# 595 "parser.ml"
               : 'dir))
; (fun parser_env ->
    Obj.repr(
# 97 "parser.mly"
                                           ( "and" )
# 601 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 98 "parser.mly"
                                           ( "or" )
# 607 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 99 "parser.mly"
                                           ( "neg" )
# 613 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 100 "parser.mly"
                                           ( "imply" )
# 619 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 101 "parser.mly"
                                           ( "minus" )
# 625 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 102 "parser.mly"
                                           ( "forall" )
# 631 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                           ( "exists" )
# 637 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 104 "parser.mly"
                                           ( "true" )
# 643 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 105 "parser.mly"
                                           ( "false" )
# 649 "parser.ml"
               : 'connector))
; (fun parser_env ->
    Obj.repr(
# 108 "parser.mly"
                                          ( OnTheLeft )
# 655 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 109 "parser.mly"
                                          ( OnTheRight )
# 661 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                                          ( Ident _1 )
# 668 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 111 "parser.mly"
                                          ( Formula _1 )
# 675 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 112 "parser.mly"
                                          ( Expression _1 )
# 682 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'varlist) in
    let _3 = (peek_val parser_env 0 : 's_expr) in
    Obj.repr(
# 113 "parser.mly"
                                          ( Labeled_sort (_1,_3) )
# 690 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'varlist) in
    let _3 = (peek_val parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 114 "parser.mly"
                                          ( Labeled_prop (_1,_3) )
# 698 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 115 "parser.mly"
                                          ( Prover Coq )
# 704 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 116 "parser.mly"
                                          ( Prover Pvs )
# 710 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 117 "parser.mly"
                                          ( Prover Isabelle )
# 716 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                          ( All )
# 722 "parser.ml"
               : 'arg))
; (fun parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                          ( Theorem )
# 728 "parser.ml"
               : 'arg))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'arg) in
    let _2 = (peek_val parser_env 0 : 'args) in
    Obj.repr(
# 122 "parser.mly"
                                           ( _1::_2 )
# 736 "parser.ml"
               : 'args))
; (fun parser_env ->
    Obj.repr(
# 123 "parser.mly"
                                           ( [] )
# 742 "parser.ml"
               : 'args))
; (fun parser_env ->
    Obj.repr(
# 126 "parser.mly"
                                           ( SSet )
# 748 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    Obj.repr(
# 127 "parser.mly"
                                           ( SProp )
# 754 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
                                           ( SSym _1 )
# 761 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 's_expr) in
    let _3 = (peek_val parser_env 0 : 's_expr) in
    Obj.repr(
# 129 "parser.mly"
                                           ( SArr (_1,_3) )
# 769 "parser.ml"
               : 's_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 't_exprat) in
    Obj.repr(
# 133 "parser.mly"
                                           ( _1 )
# 776 "parser.ml"
               : 't_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 't_expr) in
    let _2 = (peek_val parser_env 0 : 't_exprat) in
    Obj.repr(
# 134 "parser.mly"
                                           ( TApp (_1,_2) )
# 784 "parser.ml"
               : 't_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
                                           ( TSym _1 )
# 791 "parser.ml"
               : 't_exprat))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 't_expr) in
    Obj.repr(
# 139 "parser.mly"
                                           ( _2 )
# 798 "parser.ml"
               : 't_exprat))
; (fun parser_env ->
    Obj.repr(
# 142 "parser.mly"
                                           ( True )
# 804 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    Obj.repr(
# 143 "parser.mly"
                                           ( False )
# 810 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 144 "parser.mly"
                                           ( PSym _1 )
# 817 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 145 "parser.mly"
                                           ( UProp(Neg,_2) )
# 824 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'p_expr) in
    let _2 = (peek_val parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 146 "parser.mly"
                                           ( PApp(_1,_2) )
# 832 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 147 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 840 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 148 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 848 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 149 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 856 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'p_expr) in
    let _3 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 150 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 864 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'varlist) in
    let _4 = (peek_val parser_env 2 : 's_expr) in
    let _6 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 151 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 873 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'varlist) in
    let _4 = (peek_val parser_env 2 : 's_expr) in
    let _6 = (peek_val parser_env 0 : 'p_expr) in
    Obj.repr(
# 152 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 882 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'p_expr) in
    Obj.repr(
# 153 "parser.mly"
                                           ( _2 )
# 889 "parser.ml"
               : 'p_expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                                           ( [_1] )
# 896 "parser.ml"
               : 'varlist))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _3 = (peek_val parser_env 0 : 'varlist) in
    Obj.repr(
# 158 "parser.mly"
                                           ( _1::_3 )
# 904 "parser.ml"
               : 'varlist))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'p_expr) in
    Obj.repr(
# 162 "parser.mly"
                                           ( _2 )
# 911 "parser.ml"
               : 'delimited_p_expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 't_expr) in
    Obj.repr(
# 165 "parser.mly"
                                           ( _2 )
# 918 "parser.ml"
               : 'delimited_t_expr))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Help.script)
