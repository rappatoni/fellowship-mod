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
\010\000\004\000\004\000\014\000\014\000\014\000\014\000\014\000\
\015\000\015\000\016\000\016\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\013\000\013\000\011\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\002\000\002\000\004\000\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\002\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\001\000\001\000\001\000\002\000\
\003\000\005\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\001\000\
\001\000\002\000\000\000\001\000\001\000\001\000\003\000\003\000\
\001\000\002\000\001\000\003\000\001\000\001\000\001\000\002\000\
\002\000\003\000\003\000\003\000\003\000\006\000\006\000\003\000\
\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\016\000\017\000\018\000\
\021\000\022\000\023\000\000\000\000\000\027\000\000\000\030\000\
\000\000\031\000\032\000\000\000\034\000\035\000\037\000\038\000\
\012\000\039\000\020\000\019\000\093\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\028\000\000\000\008\000\009\000\
\010\000\011\000\005\000\006\000\036\000\001\000\000\000\063\000\
\064\000\065\000\056\000\057\000\000\000\000\000\002\000\000\000\
\059\000\060\000\000\000\000\000\040\000\025\000\026\000\046\000\
\045\000\000\000\000\000\079\000\000\000\000\000\000\000\077\000\
\078\000\000\000\000\000\075\000\000\000\000\000\073\000\066\000\
\000\000\000\000\000\000\049\000\050\000\051\000\047\000\048\000\
\052\000\053\000\054\000\055\000\007\000\000\000\090\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\091\000\
\081\000\000\000\092\000\074\000\070\000\069\000\068\000\000\000\
\062\000\000\000\000\000\000\000\000\000\000\000\088\000\000\000\
\000\000\000\000\000\000\076\000\000\000\000\000\000\000\042\000\
\000\000\000\000\072\000\000\000\043\000\000\000\000\000\000\000\
\000\000"

let yydgoto = "\002\000\
\029\000\030\000\031\000\055\000\115\000\033\000\066\000\093\000\
\116\000\056\000\057\000\105\000\059\000\114\000\078\000\079\000\
\075\000"

let yysindex = "\019\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\255\027\255\000\000\250\254\000\000\
\105\255\000\000\000\000\041\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\255\120\255\019\255\
\120\255\066\255\072\255\000\000\000\000\238\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\255\000\000\
\000\000\000\000\000\000\000\000\140\255\014\255\000\000\120\255\
\000\000\000\000\071\255\243\254\000\000\000\000\000\000\000\000\
\000\000\207\255\099\255\000\000\140\255\099\255\099\255\000\000\
\000\000\140\255\152\255\000\000\014\255\001\255\000\000\000\000\
\108\255\150\255\019\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\255\000\000\081\255\
\092\255\098\255\159\255\140\255\140\255\140\255\140\255\000\000\
\000\000\096\255\000\000\000\000\000\000\000\000\000\000\043\255\
\000\000\114\255\087\255\106\255\043\255\043\255\000\000\166\255\
\081\255\081\255\081\255\000\000\244\254\043\255\150\255\000\000\
\242\254\049\255\000\000\114\255\000\000\140\255\140\255\166\255\
\166\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\104\255\000\000\000\000\000\000\000\000\
\103\255\000\000\000\000\034\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\111\255\113\255\
\183\255\000\000\000\000\000\000\000\000\131\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\183\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\201\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\100\255\000\000\173\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\255\110\255\000\000\000\000\000\000\000\000\144\255\
\180\255\186\255\193\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\255\000\000\000\000\000\000\146\255\
\151\255"

let yygindex = "\000\000\
\000\000\000\000\199\000\242\255\002\000\200\000\000\000\000\000\
\096\000\000\000\148\000\117\000\222\000\070\000\153\000\206\255\
\187\255"

let yytablesize = 318
let yytable = "\096\000\
\025\000\076\000\032\000\036\000\099\000\071\000\071\000\071\000\
\071\000\045\000\018\000\019\000\020\000\021\000\076\000\022\000\
\023\000\024\000\061\000\001\000\058\000\058\000\058\000\058\000\
\126\000\034\000\126\000\108\000\064\000\065\000\120\000\121\000\
\122\000\123\000\033\000\033\000\033\000\033\000\082\000\134\000\
\131\000\080\000\037\000\109\000\035\000\026\000\061\000\061\000\
\061\000\061\000\077\000\071\000\071\000\107\000\071\000\108\000\
\071\000\071\000\071\000\071\000\071\000\083\000\071\000\077\000\
\136\000\137\000\058\000\058\000\045\000\058\000\046\000\058\000\
\058\000\060\000\058\000\058\000\089\000\058\000\110\000\111\000\
\033\000\033\000\062\000\033\000\067\000\033\000\033\000\126\000\
\033\000\033\000\063\000\033\000\061\000\061\000\112\000\061\000\
\076\000\061\000\061\000\094\000\061\000\061\000\135\000\061\000\
\024\000\024\000\024\000\024\000\109\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\047\000\048\000\049\000\050\000\014\000\015\000\016\000\081\000\
\018\000\019\000\038\000\021\000\054\000\022\000\023\000\024\000\
\039\000\040\000\041\000\042\000\068\000\060\000\127\000\110\000\
\111\000\077\000\124\000\058\000\117\000\058\000\024\000\024\000\
\126\000\024\000\118\000\024\000\089\000\053\000\128\000\112\000\
\004\000\024\000\044\000\026\000\027\000\028\000\051\000\052\000\
\067\000\053\000\003\000\054\000\058\000\018\000\019\000\020\000\
\021\000\069\000\022\000\023\000\024\000\125\000\070\000\071\000\
\072\000\073\000\129\000\130\000\033\000\074\000\100\000\101\000\
\102\000\103\000\082\000\132\000\086\000\100\000\101\000\102\000\
\103\000\087\000\104\000\054\000\100\000\101\000\102\000\103\000\
\026\000\119\000\054\000\080\000\080\000\080\000\080\000\043\000\
\044\000\054\000\083\000\083\000\083\000\083\000\133\000\080\000\
\085\000\085\000\085\000\085\000\113\000\106\000\083\000\084\000\
\084\000\084\000\084\000\067\000\085\000\067\000\067\000\000\000\
\067\000\000\000\000\000\084\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\041\000\000\000\000\000\
\041\000\000\000\041\000\000\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\000\000\000\000\000\000\000\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\000\000\022\000\023\000\024\000\
\095\000\000\000\000\000\097\000\098\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\027\000\028\000"

let yycheck = "\069\000\
\000\000\001\001\001\000\010\001\074\000\001\001\002\001\003\001\
\004\001\028\001\024\001\025\001\026\001\027\001\001\001\029\001\
\030\001\031\001\033\000\001\000\001\001\002\001\003\001\004\001\
\039\001\016\001\039\001\078\000\047\001\048\001\100\000\101\000\
\102\000\103\000\001\001\002\001\003\001\004\001\052\001\054\001\
\053\001\056\000\049\001\001\001\018\001\059\001\001\001\002\001\
\003\001\004\001\050\001\047\001\048\001\053\001\050\001\106\000\
\052\001\053\001\054\001\055\001\056\001\060\000\058\001\050\001\
\134\000\135\000\047\001\048\001\028\001\050\001\058\001\052\001\
\053\001\055\001\055\001\056\001\057\001\058\001\036\001\037\001\
\047\001\048\001\017\001\050\001\054\001\052\001\053\001\039\001\
\055\001\056\001\019\001\058\001\047\001\048\001\052\001\050\001\
\001\001\052\001\053\001\001\001\055\001\056\001\054\001\058\001\
\001\001\002\001\003\001\004\001\001\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\001\001\002\001\003\001\004\001\020\001\021\001\022\001\057\001\
\024\001\025\001\026\001\027\001\052\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\001\001\055\001\056\001\036\001\
\037\001\050\001\051\001\031\000\057\001\033\000\047\001\048\001\
\039\001\050\001\057\001\052\001\057\001\050\001\053\001\052\001\
\058\001\058\001\053\001\059\001\060\001\061\001\047\001\048\001\
\058\001\050\001\058\001\052\001\056\000\024\001\025\001\026\001\
\027\001\038\001\029\001\030\001\031\001\112\000\043\001\044\001\
\045\001\046\001\117\000\118\000\058\001\050\001\039\001\040\001\
\041\001\042\001\051\001\126\000\051\001\039\001\040\001\041\001\
\042\001\051\001\051\001\052\001\039\001\040\001\041\001\042\001\
\059\001\051\001\052\001\039\001\040\001\041\001\042\001\017\000\
\017\000\052\001\039\001\040\001\041\001\042\001\127\000\051\001\
\039\001\040\001\041\001\042\001\081\000\077\000\051\001\039\001\
\040\001\041\001\042\001\053\001\051\001\055\001\056\001\255\255\
\058\001\255\255\255\255\051\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\053\001\255\255\255\255\
\056\001\255\255\058\001\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\031\001\
\067\000\255\255\255\255\070\000\071\000\255\255\255\255\255\255\
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
# 406 "parser.ml"
               : Help.script))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 48 "parser.mly"
                                           ( Instruction (_1,_2) )
# 414 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 49 "parser.mly"
                                           ( Tactical _1 )
# 421 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                                           ( Help Nix )
# 427 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 52 "parser.mly"
                                           ( Help (HInstr _2) )
# 434 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tac) in
    Obj.repr(
# 53 "parser.mly"
                                           ( Help (HTac _2) )
# 441 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dir) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'connector) in
    Obj.repr(
# 54 "parser.mly"
                                           ( Help (HElim (_3,_4)) )
# 449 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                                           ( Help HTacticals )
# 455 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
                                           ( Help HTypes )
# 461 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                           ( Help HTerms )
# 467 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                           ( Help HFormulae )
# 473 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                           ( raise (Failure "end") )
# 479 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                                           ( Lj true )
# 485 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                           ( Lj false )
# 491 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                           ( Min true )
# 497 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                           ( Min false )
# 503 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                           ( Declare )
# 509 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                           ( Theorem )
# 515 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                           ( Deny )
# 521 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                           ( AntiTheorem )
# 527 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                           ( Next )
# 533 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                           ( Prev )
# 539 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( Qed )
# 545 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( CheckOut )
# 551 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                           ( CheckOutProofTerm )
# 557 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                                           ( ExportNaturalLanguage )
# 563 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                           ( if !toplvl then Undo 
					     else raise Parsing.Parse_error )
# 570 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                                           ( if !toplvl then DiscardAll
					     else raise Parsing.Parse_error )
# 577 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( if !toplvl then DiscardTheorem
					     else raise Parsing.Parse_error )
# 584 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                           ( Quit )
# 590 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                           ( Axiom )
# 596 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                           ( Cut )
# 602 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                                           ( Elim )
# 608 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                                           ( Idtac )
# 614 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                                           ( Focus )
# 620 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                                           ( Elim_In )
# 626 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                           ( Contraction )
# 632 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                           ( Weaken )
# 638 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                           ( Moxia )
# 644 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tac) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 100 "parser.mly"
                                           ( TPlug (_1,_2,symbol_start_pos ()) )
# 652 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 101 "parser.mly"
                                           ( Then (_1,_3,symbol_start_pos ()) )
# 660 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'tactical) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'taclist) in
    Obj.repr(
# 102 "parser.mly"
                                           ( Thens (_1,_4,symbol_start_pos ()) )
# 668 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taclist) in
    Obj.repr(
# 105 "parser.mly"
                                           ( _1::_3 )
# 676 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 106 "parser.mly"
                                           ( [_1] )
# 683 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                                           ( true )
# 689 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                                           ( false )
# 695 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                                           ( "and" )
# 701 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                                           ( "or" )
# 707 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                                           ( "neg" )
# 713 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                                           ( "imply" )
# 719 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                                           ( "minus" )
# 725 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                           ( "forall" )
# 731 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                           ( "exists" )
# 737 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                                           ( "true" )
# 743 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
                                           ( "false" )
# 749 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                                          ( OnTheLeft )
# 755 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                                          ( OnTheRight )
# 761 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
                                          ( Ident _1 )
# 768 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 127 "parser.mly"
                                          ( Formula _1 )
# 775 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 128 "parser.mly"
                                          ( Expression _1 )
# 782 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 129 "parser.mly"
                                          ( Labeled_sort (_1,_3) )
# 790 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 130 "parser.mly"
                                          ( Labeled_prop (_1,_3) )
# 798 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                                          ( Prover Coq )
# 804 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                          ( Prover Pvs )
# 810 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                                          ( Prover Isabelle )
# 816 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 136 "parser.mly"
                                           ( _1::_2 )
# 824 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                                           ( [] )
# 830 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                           ( SSet )
# 836 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
                                           ( SProp )
# 842 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "parser.mly"
                                           ( SSym _1 )
# 849 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 143 "parser.mly"
                                           ( SArr (_1,_3) )
# 857 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's_expr) in
    Obj.repr(
# 144 "parser.mly"
                                           ( _2 )
# 864 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 148 "parser.mly"
                                           ( _1 )
# 871 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 149 "parser.mly"
                                           ( TApp (_1,_2) )
# 879 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
                                           ( TSym _1 )
# 886 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 154 "parser.mly"
                                           ( _2 )
# 893 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
                                           ( True )
# 899 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
                                           ( False )
# 905 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 159 "parser.mly"
                                           ( PSym _1 )
# 912 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 160 "parser.mly"
                                           ( UProp(Neg,_2) )
# 919 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 161 "parser.mly"
                                           ( PApp(_1,_2) )
# 927 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 162 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 935 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 163 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 943 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 164 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 951 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 165 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 959 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 166 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 968 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 167 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 977 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 168 "parser.mly"
                                           ( _2 )
# 984 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
                                           ( [_1] )
# 991 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 173 "parser.mly"
                                           ( _1::_3 )
# 999 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 177 "parser.mly"
                                           ( _2 )
# 1006 "parser.ml"
               : 'delimited_p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 180 "parser.mly"
                                           ( _2 )
# 1013 "parser.ml"
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
