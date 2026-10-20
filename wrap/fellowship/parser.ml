type token =
  | IDENT of (
# 22 "parser.mly"
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
  | MACHINE
  | AXIOM
  | CUT
  | ELIM
  | IDTAC
  | IN
  | FOCUS
  | CONTRACTION
  | WEAKEN
  | BY
  | DEFAULT
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
  | REFIDENT of (
# 32 "parser.mly"
        string
# 75 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 12 "parser.mly"
  open Core
  open Tactics
  open Instructions
  open Help
  open Print

# 87 "parser.ml"
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
  280 (* MACHINE *);
  281 (* AXIOM *);
  282 (* CUT *);
  283 (* ELIM *);
  284 (* IDTAC *);
  285 (* IN *);
  286 (* FOCUS *);
  287 (* CONTRACTION *);
  288 (* WEAKEN *);
  289 (* BY *);
  290 (* DEFAULT *);
  291 (* TACTICALS *);
  292 (* TYPES *);
  293 (* TERMS *);
  294 (* FORMULAE *);
  295 (* PROP *);
  296 (* SET *);
  297 (* NEG *);
  298 (* ARROW *);
  299 (* MINUS *);
  300 (* AND *);
  301 (* OR *);
  302 (* FORALL *);
  303 (* EXISTS *);
  304 (* TRUE *);
  305 (* FALSE *);
  306 (* LEFT *);
  307 (* RIGHT *);
  308 (* ALL *);
  309 (* LPAR *);
  310 (* RPAR *);
  311 (* LBRA *);
  312 (* RBRA *);
  313 (* VIR *);
  314 (* PVIR *);
  315 (* PIPE *);
  316 (* COLON *);
  317 (* DOT *);
    0 (* EOF *);
  318 (* MOXIA *);
  319 (* ANTITHEOREM *);
  320 (* DENY *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  321 (* REFIDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\005\000\005\000\005\000\009\000\009\000\007\000\
\007\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\004\000\004\000\014\000\
\014\000\014\000\014\000\014\000\016\000\016\000\017\000\017\000\
\017\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\015\000\015\000\019\000\
\019\000\013\000\013\000\011\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\002\000\002\000\004\000\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\002\000\002\000\001\000\003\000\002\000\
\001\000\001\000\001\000\002\000\001\000\001\000\002\000\001\000\
\001\000\001\000\002\000\003\000\005\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\006\000\001\000\001\000\001\000\002\000\000\000\001\000\
\001\000\001\000\003\000\003\000\001\000\002\000\001\000\001\000\
\003\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\006\000\006\000\003\000\001\000\003\000\001\000\
\003\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\016\000\017\000\018\000\
\021\000\022\000\023\000\000\000\000\000\027\000\000\000\030\000\
\000\000\000\000\033\000\034\000\000\000\037\000\038\000\040\000\
\041\000\000\000\012\000\042\000\020\000\019\000\102\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\028\000\000\000\
\008\000\009\000\010\000\011\000\005\000\006\000\000\000\039\000\
\036\000\001\000\000\000\067\000\068\000\069\000\059\000\060\000\
\000\000\000\000\002\000\000\000\062\000\063\000\000\000\000\000\
\043\000\025\000\026\000\049\000\048\000\000\000\031\000\000\000\
\084\000\000\000\000\000\000\000\082\000\083\000\000\000\000\000\
\079\000\000\000\080\000\000\000\077\000\070\000\000\000\000\000\
\000\000\052\000\053\000\054\000\050\000\051\000\055\000\056\000\
\057\000\058\000\007\000\000\000\099\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\100\000\086\000\000\000\
\101\000\078\000\074\000\073\000\072\000\000\000\065\000\000\000\
\000\000\000\000\000\000\000\000\093\000\000\000\000\000\000\000\
\000\000\081\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\000\000\000\000\000\000\076\000\000\000\000\000\
\000\000\046\000\000\000\000\000\000\000\066\000\095\000\000\000\
\000\000"

let yydgoto = "\002\000\
\031\000\032\000\033\000\059\000\121\000\035\000\070\000\099\000\
\122\000\060\000\061\000\111\000\063\000\133\000\134\000\084\000\
\085\000\080\000\135\000"

let yysindex = "\035\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\249\254\021\255\000\000\250\254\000\000\
\183\255\047\255\000\000\000\000\022\255\000\000\000\000\000\000\
\000\000\023\255\000\000\000\000\000\000\000\000\000\000\017\255\
\181\255\004\255\181\255\063\255\082\255\000\000\000\000\083\255\
\000\000\000\000\000\000\000\000\000\000\000\000\110\255\000\000\
\000\000\000\000\062\255\000\000\000\000\000\000\000\000\000\000\
\250\255\010\255\000\000\181\255\000\000\000\000\066\255\248\254\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\134\255\
\000\000\250\255\134\255\134\255\000\000\000\000\250\255\195\255\
\000\000\010\255\000\000\002\255\000\000\000\000\069\255\197\255\
\004\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\255\000\000\028\255\078\255\080\255\
\199\255\250\255\250\255\250\255\250\255\000\000\000\000\006\255\
\000\000\000\000\000\000\000\000\000\000\001\255\000\000\100\255\
\055\255\092\255\013\255\013\255\000\000\101\255\101\255\028\255\
\227\254\000\000\013\255\099\255\051\255\098\255\105\255\013\255\
\197\255\000\000\230\254\008\255\013\255\000\000\113\255\103\255\
\100\255\000\000\250\255\250\255\100\255\000\000\000\000\157\255\
\157\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\126\255\000\000\000\000\000\000\000\000\
\117\255\000\000\000\000\000\000\102\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\119\255\125\255\024\000\000\000\000\000\000\000\000\000\145\255\
\000\000\000\000\000\000\000\000\000\000\000\000\086\255\000\000\
\000\000\000\000\026\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\064\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\111\255\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\114\255\
\118\255\000\000\000\000\000\000\000\000\018\255\067\255\012\000\
\016\000\000\000\000\000\094\255\000\000\000\000\161\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\255\000\000\000\000\000\000\201\255\000\000\000\000\172\255\
\179\255"

let yygindex = "\000\000\
\000\000\000\000\218\000\234\255\005\000\231\000\000\000\000\000\
\123\000\000\000\177\255\233\255\180\000\008\000\117\000\191\000\
\193\255\182\255\000\000"

let yytablesize = 341
let yytable = "\102\000\
\027\000\115\000\081\000\038\000\105\000\034\000\081\000\119\000\
\036\000\062\000\081\000\062\000\065\000\115\000\108\000\136\000\
\019\000\020\000\021\000\022\000\114\000\023\000\024\000\025\000\
\026\000\058\000\061\000\061\000\061\000\061\000\147\000\126\000\
\127\000\128\000\129\000\001\000\062\000\086\000\037\000\116\000\
\117\000\075\000\075\000\075\000\075\000\039\000\088\000\047\000\
\114\000\136\000\048\000\116\000\117\000\028\000\082\000\131\000\
\049\000\113\000\082\000\130\000\087\000\064\000\082\000\150\000\
\148\000\132\000\083\000\131\000\089\000\115\000\083\000\087\000\
\152\000\153\000\083\000\061\000\061\000\050\000\061\000\066\000\
\061\000\061\000\058\000\061\000\061\000\098\000\061\000\032\000\
\032\000\032\000\075\000\075\000\136\000\075\000\120\000\075\000\
\075\000\075\000\075\000\075\000\067\000\075\000\035\000\035\000\
\035\000\035\000\142\000\116\000\117\000\088\000\071\000\048\000\
\064\000\137\000\064\000\064\000\064\000\064\000\072\000\044\000\
\088\000\057\000\044\000\118\000\044\000\087\000\024\000\024\000\
\024\000\024\000\139\000\140\000\068\000\069\000\100\000\032\000\
\032\000\123\000\032\000\124\000\032\000\136\000\106\000\145\000\
\108\000\109\000\032\000\138\000\149\000\096\000\096\000\035\000\
\035\000\143\000\035\000\058\000\035\000\035\000\141\000\035\000\
\035\000\144\000\035\000\064\000\064\000\057\000\064\000\132\000\
\064\000\064\000\098\000\064\000\064\000\047\000\064\000\024\000\
\024\000\004\000\024\000\071\000\024\000\051\000\052\000\053\000\
\054\000\003\000\024\000\003\000\004\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\106\000\107\000\
\108\000\109\000\014\000\015\000\016\000\035\000\018\000\019\000\
\020\000\040\000\022\000\058\000\023\000\024\000\025\000\026\000\
\094\000\041\000\042\000\043\000\044\000\019\000\020\000\021\000\
\022\000\091\000\023\000\024\000\025\000\026\000\055\000\056\000\
\092\000\057\000\045\000\058\000\106\000\107\000\108\000\109\000\
\106\000\107\000\108\000\109\000\028\000\029\000\030\000\046\000\
\110\000\058\000\073\000\101\000\125\000\058\000\103\000\104\000\
\097\000\097\000\028\000\146\000\151\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\112\000\000\000\000\000\000\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\022\000\000\000\023\000\024\000\
\025\000\026\000\074\000\000\000\000\000\000\000\000\000\075\000\
\076\000\077\000\078\000\000\000\000\000\000\000\079\000\085\000\
\085\000\085\000\085\000\000\000\000\000\090\000\090\000\090\000\
\090\000\089\000\089\000\085\000\089\000\000\000\028\000\029\000\
\030\000\090\000\000\000\000\000\000\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\071\000\
\000\000\071\000\071\000\000\000\071\000"

let yycheck = "\074\000\
\000\000\001\001\001\001\010\001\079\000\001\000\001\001\087\000\
\016\001\033\000\001\001\035\000\035\000\001\001\044\001\042\001\
\025\001\026\001\027\001\028\001\084\000\030\001\031\001\032\001\
\033\001\055\001\001\001\002\001\003\001\004\001\057\001\106\000\
\107\000\108\000\109\000\001\000\060\000\060\000\018\001\039\001\
\040\001\001\001\002\001\003\001\004\001\052\001\055\001\001\001\
\112\000\042\001\029\001\039\001\040\001\062\001\053\001\055\001\
\034\001\056\001\053\001\054\001\043\001\058\001\053\001\143\000\
\057\001\065\001\065\001\055\001\064\000\001\001\065\001\054\001\
\147\000\148\000\065\001\050\001\051\001\061\001\053\001\017\001\
\055\001\056\001\055\001\058\001\059\001\060\001\061\001\002\001\
\003\001\004\001\050\001\051\001\042\001\053\001\087\000\055\001\
\056\001\057\001\058\001\059\001\019\001\061\001\001\001\002\001\
\003\001\004\001\056\001\039\001\040\001\043\001\001\001\029\001\
\058\001\059\001\001\001\002\001\003\001\004\001\057\001\056\001\
\054\001\053\001\059\001\055\001\061\001\060\001\001\001\002\001\
\003\001\004\001\123\000\124\000\050\001\051\001\001\001\050\001\
\051\001\060\001\053\001\060\001\055\001\042\001\042\001\136\000\
\044\001\045\001\061\001\056\001\141\000\056\001\057\001\050\001\
\051\001\056\001\053\001\055\001\055\001\056\001\060\001\058\001\
\059\001\057\001\061\001\050\001\051\001\053\001\053\001\065\001\
\055\001\056\001\060\001\058\001\059\001\056\001\061\001\050\001\
\051\001\061\001\053\001\061\001\055\001\001\001\002\001\003\001\
\004\001\061\001\061\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\042\001\043\001\
\044\001\045\001\020\001\021\001\022\001\061\001\024\001\025\001\
\026\001\027\001\028\001\055\001\030\001\031\001\032\001\033\001\
\056\001\035\001\036\001\037\001\038\001\025\001\026\001\027\001\
\028\001\054\001\030\001\031\001\032\001\033\001\050\001\051\001\
\054\001\053\001\017\000\055\001\042\001\043\001\044\001\045\001\
\042\001\043\001\044\001\045\001\062\001\063\001\064\001\017\000\
\054\001\055\001\001\001\072\000\054\001\055\001\075\000\076\000\
\056\001\057\001\062\001\137\000\144\000\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\082\000\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\030\001\031\001\
\032\001\033\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001\255\255\255\255\255\255\053\001\042\001\
\043\001\044\001\045\001\255\255\255\255\042\001\043\001\044\001\
\045\001\042\001\043\001\054\001\045\001\255\255\062\001\063\001\
\064\001\054\001\255\255\255\255\255\255\054\001\041\001\042\001\
\043\001\044\001\045\001\046\001\047\001\048\001\049\001\056\001\
\255\255\058\001\059\001\255\255\061\001"

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
  MACHINE\000\
  AXIOM\000\
  CUT\000\
  ELIM\000\
  IDTAC\000\
  IN\000\
  FOCUS\000\
  CONTRACTION\000\
  WEAKEN\000\
  BY\000\
  DEFAULT\000\
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
  REFIDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'command) in
    Obj.repr(
# 61 "parser.mly"
                                           ( _1 )
# 436 "parser.ml"
               : Help.script))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 64 "parser.mly"
                                           ( Instruction (_1,_2) )
# 444 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 65 "parser.mly"
                                           ( Tactical _1 )
# 451 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                           ( Help Nix )
# 457 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 68 "parser.mly"
                                           ( Help (HInstr _2) )
# 464 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tac) in
    Obj.repr(
# 69 "parser.mly"
                                           ( Help (HTac _2) )
# 471 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dir) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'connector) in
    Obj.repr(
# 70 "parser.mly"
                                           ( Help (HElim (_3,_4)) )
# 479 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                           ( Help HTacticals )
# 485 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( Help HTypes )
# 491 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( Help HTerms )
# 497 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                           ( Help HFormulae )
# 503 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                                           ( raise (Failure "end") )
# 509 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                                           ( Lj true )
# 515 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                           ( Lj false )
# 521 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( Min true )
# 527 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                                           ( Min false )
# 533 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                           ( Declare )
# 539 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                           ( Theorem )
# 545 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                           ( Deny )
# 551 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                                           ( AntiTheorem )
# 557 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                           ( Next )
# 563 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                           ( Prev )
# 569 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                                           ( Qed )
# 575 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                                           ( CheckOut )
# 581 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                                           ( CheckOutProofTerm )
# 587 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                                           ( ExportNaturalLanguage )
# 593 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                                           ( if !toplvl then Undo 
					     else raise Parsing.Parse_error )
# 600 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                           ( if !toplvl then DiscardAll
					     else raise Parsing.Parse_error )
# 607 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                           ( if !toplvl then DiscardTheorem
					     else raise Parsing.Parse_error )
# 614 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                                           ( Quit )
# 620 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
                                           ( if _2 = "quiet" && _3 = "on" then MachineQuiet true else if _2 = "quiet" && _3 = "off" then MachineQuiet false else raise Parsing.Parse_error )
# 628 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                                           ( if _2 = "snapshot" then MachineSnapshot else raise Parsing.Parse_error )
# 635 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                                           ( Axiom )
# 641 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                                           ( Cut )
# 647 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                                           ( Elim )
# 653 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                                           ( ByDefault )
# 659 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                                           ( Idtac )
# 665 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                                           ( Focus )
# 671 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                                           ( Elim_In )
# 677 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                                           ( Contraction )
# 683 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                                           ( Weaken )
# 689 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                                           ( Moxia )
# 695 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tac) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 119 "parser.mly"
                                           ( TPlug (_1,_2,symbol_start_pos ()) )
# 703 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 120 "parser.mly"
                                           ( Then (_1,_3,symbol_start_pos ()) )
# 711 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'tactical) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'taclist) in
    Obj.repr(
# 121 "parser.mly"
                                           ( Thens (_1,_4,symbol_start_pos ()) )
# 719 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taclist) in
    Obj.repr(
# 124 "parser.mly"
                                           ( _1::_3 )
# 727 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 125 "parser.mly"
                                           ( [_1] )
# 734 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                                           ( true )
# 740 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
                                           ( false )
# 746 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                           ( "and" )
# 752 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                                           ( "or" )
# 758 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
                                           ( "neg" )
# 764 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
                                           ( "imply" )
# 770 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
                                           ( "minus" )
# 776 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                                           ( "forall" )
# 782 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
                                           ( "exists" )
# 788 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
                                           ( "true" )
# 794 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                           ( "false" )
# 800 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                                          ( OnTheLeft )
# 806 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                                          ( OnTheRight )
# 812 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                                          ( Ident _1 )
# 819 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 146 "parser.mly"
                                          ( Formula _1 )
# 826 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 147 "parser.mly"
                                          ( Expression _1 )
# 833 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 148 "parser.mly"
                                          ( Labeled_sort (_1,_3) )
# 841 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 149 "parser.mly"
                                          ( Labeled_prop (_1,_3) )
# 849 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reflist) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 151 "parser.mly"
                                          ( Labeled_ref_prop (_1,_4,_6) )
# 858 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
                                          ( Prover Coq )
# 864 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "parser.mly"
                                          ( Prover Pvs )
# 870 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser.mly"
                                          ( Prover Isabelle )
# 876 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 157 "parser.mly"
                                           ( _1::_2 )
# 884 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
                                           ( [] )
# 890 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
                                           ( SSet )
# 896 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "parser.mly"
                                           ( SProp )
# 902 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 163 "parser.mly"
                                           ( SSym _1 )
# 909 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 164 "parser.mly"
                                           ( SArr (_1,_3) )
# 917 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's_expr) in
    Obj.repr(
# 165 "parser.mly"
                                           ( _2 )
# 924 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 169 "parser.mly"
                                           ( _1 )
# 931 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 170 "parser.mly"
                                           ( TApp (_1,_2) )
# 939 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 174 "parser.mly"
                                           ( TSym _1 )
# 946 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 175 "parser.mly"
                                           ( TSym _1 )
# 953 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 176 "parser.mly"
                                           ( _2 )
# 960 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "parser.mly"
                                           ( True )
# 966 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "parser.mly"
                                           ( False )
# 972 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 181 "parser.mly"
                                           ( PSym _1 )
# 979 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 182 "parser.mly"
                                           ( UProp(Neg,_2) )
# 986 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 183 "parser.mly"
                                           ( PApp(_1,_2) )
# 994 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 184 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 1002 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 185 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 1010 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 186 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 1018 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 187 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 1026 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 188 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 1035 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 189 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 1044 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 190 "parser.mly"
                                           ( _2 )
# 1051 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'refdecl) in
    Obj.repr(
# 196 "parser.mly"
                                           ( [_1] )
# 1058 "parser.ml"
               : 'reflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'refdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'reflist) in
    Obj.repr(
# 197 "parser.mly"
                                           ( _1::_3 )
# 1066 "parser.ml"
               : 'reflist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 201 "parser.mly"
                                           ( (_1, None) )
# 1073 "parser.ml"
               : 'refdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 202 "parser.mly"
                                           ( (_1, Some _3) )
# 1081 "parser.ml"
               : 'refdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 206 "parser.mly"
                                           ( [_1] )
# 1088 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 207 "parser.mly"
                                           ( _1::_3 )
# 1096 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 211 "parser.mly"
                                           ( _2 )
# 1103 "parser.ml"
               : 'delimited_p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 214 "parser.mly"
                                           ( _2 )
# 1110 "parser.ml"
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
