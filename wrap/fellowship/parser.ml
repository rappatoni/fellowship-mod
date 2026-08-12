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

open Parsing
let _ = parse_error;;
# 12 "parser.mly"
  open Core
  open Tactics
  open Instructions
  open Help
  open Print

# 82 "parser.ml"
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
\010\000\010\000\010\000\010\000\004\000\004\000\014\000\014\000\
\014\000\014\000\014\000\015\000\015\000\016\000\016\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\013\000\013\000\011\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\002\000\002\000\004\000\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\002\000\002\000\001\000\003\000\002\000\
\001\000\001\000\001\000\002\000\001\000\001\000\002\000\001\000\
\001\000\001\000\002\000\003\000\005\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\001\000\001\000\001\000\002\000\000\000\001\000\001\000\
\001\000\003\000\003\000\001\000\002\000\001\000\003\000\001\000\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\006\000\006\000\003\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\016\000\017\000\018\000\
\021\000\022\000\023\000\000\000\000\000\027\000\000\000\030\000\
\000\000\000\000\033\000\034\000\000\000\037\000\038\000\040\000\
\041\000\000\000\012\000\042\000\020\000\019\000\096\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\028\000\000\000\
\008\000\009\000\010\000\011\000\005\000\006\000\000\000\039\000\
\036\000\001\000\000\000\066\000\067\000\068\000\059\000\060\000\
\000\000\000\000\002\000\000\000\062\000\063\000\000\000\000\000\
\043\000\025\000\026\000\049\000\048\000\000\000\031\000\000\000\
\082\000\000\000\000\000\000\000\080\000\081\000\000\000\000\000\
\078\000\000\000\000\000\076\000\069\000\000\000\000\000\000\000\
\052\000\053\000\054\000\050\000\051\000\055\000\056\000\057\000\
\058\000\007\000\000\000\093\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\094\000\084\000\000\000\095\000\
\077\000\073\000\072\000\071\000\000\000\065\000\000\000\000\000\
\000\000\000\000\000\000\091\000\000\000\000\000\000\000\000\000\
\079\000\000\000\000\000\000\000\045\000\000\000\000\000\075\000\
\000\000\046\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\031\000\032\000\033\000\059\000\120\000\035\000\070\000\098\000\
\121\000\060\000\061\000\110\000\063\000\119\000\083\000\084\000\
\080\000"

let yysindex = "\023\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\255\012\255\000\000\255\254\000\000\
\136\255\043\255\000\000\000\000\016\255\000\000\000\000\000\000\
\000\000\019\255\000\000\000\000\000\000\000\000\000\000\241\254\
\151\255\250\254\151\255\047\255\044\255\000\000\000\000\234\254\
\000\000\000\000\000\000\000\000\000\000\000\000\069\255\000\000\
\000\000\000\000\015\255\000\000\000\000\000\000\000\000\000\000\
\074\255\005\255\000\000\151\255\000\000\000\000\026\255\185\255\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\087\255\
\000\000\074\255\087\255\087\255\000\000\000\000\074\255\187\255\
\000\000\005\255\001\255\000\000\000\000\137\255\195\255\250\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\255\000\000\039\255\038\255\040\255\191\255\
\074\255\074\255\074\255\074\255\000\000\000\000\002\255\000\000\
\000\000\000\000\000\000\000\000\169\255\000\000\061\255\240\254\
\050\255\169\255\169\255\000\000\206\255\039\255\039\255\039\255\
\000\000\226\254\169\255\195\255\000\000\222\254\224\254\000\000\
\061\255\000\000\074\255\074\255\206\255\206\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\079\255\000\000\000\000\000\000\000\000\
\049\255\000\000\000\000\000\000\046\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\255\057\255\197\255\000\000\000\000\000\000\000\000\063\255\
\000\000\000\000\000\000\000\000\000\000\000\000\133\255\000\000\
\000\000\000\000\018\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\197\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\072\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\065\255\000\000\249\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\255\070\255\
\000\000\000\000\000\000\000\000\085\255\253\255\012\000\016\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\000\000\000\000\105\255\111\255"

let yygindex = "\000\000\
\000\000\000\000\161\000\237\255\003\000\162\000\000\000\000\000\
\050\000\000\000\099\000\236\255\198\255\058\000\105\000\184\255\
\182\255"

let yytablesize = 335
let yytable = "\101\000\
\027\000\081\000\081\000\034\000\104\000\081\000\048\000\131\000\
\038\000\131\000\113\000\131\000\062\000\100\000\062\000\065\000\
\102\000\103\000\061\000\061\000\061\000\061\000\139\000\001\000\
\140\000\136\000\036\000\068\000\069\000\037\000\125\000\126\000\
\127\000\128\000\074\000\074\000\074\000\074\000\113\000\062\000\
\085\000\064\000\132\000\047\000\048\000\050\000\035\000\035\000\
\035\000\035\000\039\000\064\000\049\000\082\000\082\000\129\000\
\112\000\082\000\064\000\064\000\064\000\064\000\067\000\066\000\
\141\000\142\000\088\000\061\000\061\000\071\000\061\000\072\000\
\061\000\061\000\073\000\061\000\061\000\092\000\061\000\024\000\
\024\000\024\000\024\000\074\000\074\000\086\000\074\000\099\000\
\074\000\074\000\074\000\074\000\074\000\058\000\074\000\035\000\
\035\000\122\000\035\000\123\000\035\000\035\000\131\000\035\000\
\035\000\133\000\035\000\064\000\064\000\004\000\064\000\070\000\
\064\000\064\000\074\000\064\000\064\000\003\000\064\000\075\000\
\076\000\077\000\078\000\035\000\092\000\047\000\079\000\044\000\
\024\000\024\000\044\000\024\000\044\000\024\000\032\000\032\000\
\032\000\114\000\085\000\024\000\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\051\000\
\052\000\053\000\054\000\014\000\015\000\016\000\089\000\018\000\
\019\000\020\000\040\000\022\000\090\000\023\000\024\000\025\000\
\026\000\114\000\041\000\042\000\043\000\044\000\130\000\115\000\
\116\000\045\000\046\000\134\000\135\000\138\000\032\000\032\000\
\118\000\032\000\111\000\032\000\137\000\057\000\000\000\117\000\
\000\000\032\000\000\000\000\000\000\000\028\000\029\000\030\000\
\055\000\056\000\000\000\057\000\000\000\058\000\000\000\115\000\
\116\000\019\000\020\000\021\000\022\000\000\000\023\000\024\000\
\025\000\026\000\000\000\019\000\020\000\021\000\022\000\117\000\
\023\000\024\000\025\000\026\000\105\000\106\000\107\000\108\000\
\105\000\106\000\107\000\108\000\000\000\000\000\000\000\087\000\
\109\000\058\000\000\000\000\000\124\000\058\000\028\000\105\000\
\106\000\107\000\108\000\000\000\070\000\000\000\070\000\070\000\
\028\000\070\000\000\000\000\000\058\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\000\000\000\000\000\000\000\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\022\000\000\000\023\000\024\000\
\025\000\026\000\083\000\083\000\083\000\083\000\086\000\086\000\
\086\000\086\000\000\000\000\000\000\000\000\000\083\000\000\000\
\000\000\000\000\086\000\000\000\000\000\088\000\088\000\088\000\
\088\000\087\000\087\000\087\000\087\000\000\000\028\000\029\000\
\030\000\088\000\000\000\000\000\000\000\087\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\096\000\097\000"

let yycheck = "\074\000\
\000\000\001\001\001\001\001\000\079\000\001\001\029\001\042\001\
\010\001\042\001\083\000\042\001\033\000\072\000\035\000\035\000\
\075\000\076\000\001\001\002\001\003\001\004\001\057\001\001\000\
\057\001\056\001\016\001\050\001\051\001\018\001\105\000\106\000\
\107\000\108\000\001\001\002\001\003\001\004\001\111\000\060\000\
\060\000\058\001\059\001\001\001\029\001\061\001\001\001\002\001\
\003\001\004\001\052\001\058\001\034\001\053\001\053\001\054\001\
\056\001\053\001\001\001\002\001\003\001\004\001\019\001\017\001\
\139\000\140\000\064\000\050\001\051\001\001\001\053\001\057\001\
\055\001\056\001\001\001\058\001\059\001\060\001\061\001\001\001\
\002\001\003\001\004\001\050\001\051\001\060\001\053\001\001\001\
\055\001\056\001\057\001\058\001\059\001\055\001\061\001\050\001\
\051\001\060\001\053\001\060\001\055\001\056\001\042\001\058\001\
\059\001\056\001\061\001\050\001\051\001\061\001\053\001\061\001\
\055\001\056\001\041\001\058\001\059\001\061\001\061\001\046\001\
\047\001\048\001\049\001\061\001\060\001\056\001\053\001\056\001\
\050\001\051\001\059\001\053\001\061\001\055\001\002\001\003\001\
\004\001\001\001\054\001\061\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\001\001\
\002\001\003\001\004\001\020\001\021\001\022\001\054\001\024\001\
\025\001\026\001\027\001\028\001\054\001\030\001\031\001\032\001\
\033\001\001\001\035\001\036\001\037\001\038\001\117\000\039\001\
\040\001\017\000\017\000\122\000\123\000\132\000\050\001\051\001\
\086\000\053\001\082\000\055\001\131\000\053\001\255\255\055\001\
\255\255\061\001\255\255\255\255\255\255\062\001\063\001\064\001\
\050\001\051\001\255\255\053\001\255\255\055\001\255\255\039\001\
\040\001\025\001\026\001\027\001\028\001\255\255\030\001\031\001\
\032\001\033\001\255\255\025\001\026\001\027\001\028\001\055\001\
\030\001\031\001\032\001\033\001\042\001\043\001\044\001\045\001\
\042\001\043\001\044\001\045\001\255\255\255\255\255\255\055\001\
\054\001\055\001\255\255\255\255\054\001\055\001\062\001\042\001\
\043\001\044\001\045\001\255\255\056\001\255\255\058\001\059\001\
\062\001\061\001\255\255\255\255\055\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\030\001\031\001\
\032\001\033\001\042\001\043\001\044\001\045\001\042\001\043\001\
\044\001\045\001\255\255\255\255\255\255\255\255\054\001\255\255\
\255\255\255\255\054\001\255\255\255\255\042\001\043\001\044\001\
\045\001\042\001\043\001\044\001\045\001\255\255\062\001\063\001\
\064\001\054\001\255\255\255\255\255\255\054\001\041\001\042\001\
\043\001\044\001\045\001\046\001\047\001\048\001\049\001"

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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'command) in
    Obj.repr(
# 51 "parser.mly"
                                           ( _1 )
# 419 "parser.ml"
               : Help.script))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 54 "parser.mly"
                                           ( Instruction (_1,_2) )
# 427 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 55 "parser.mly"
                                           ( Tactical _1 )
# 434 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                           ( Help Nix )
# 440 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 58 "parser.mly"
                                           ( Help (HInstr _2) )
# 447 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tac) in
    Obj.repr(
# 59 "parser.mly"
                                           ( Help (HTac _2) )
# 454 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dir) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'connector) in
    Obj.repr(
# 60 "parser.mly"
                                           ( Help (HElim (_3,_4)) )
# 462 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                                           ( Help HTacticals )
# 468 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                                           ( Help HTypes )
# 474 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                           ( Help HTerms )
# 480 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                           ( Help HFormulae )
# 486 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                           ( raise (Failure "end") )
# 492 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                           ( Lj true )
# 498 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                           ( Lj false )
# 504 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                           ( Min true )
# 510 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                           ( Min false )
# 516 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( Declare )
# 522 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( Theorem )
# 528 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                           ( Deny )
# 534 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                                           ( AntiTheorem )
# 540 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                           ( Next )
# 546 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
                                           ( Prev )
# 552 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                                           ( Qed )
# 558 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                           ( CheckOut )
# 564 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( CheckOutProofTerm )
# 570 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                                           ( ExportNaturalLanguage )
# 576 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                           ( if !toplvl then Undo 
					     else raise Parsing.Parse_error )
# 583 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                           ( if !toplvl then DiscardAll
					     else raise Parsing.Parse_error )
# 590 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                           ( if !toplvl then DiscardTheorem
					     else raise Parsing.Parse_error )
# 597 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                                           ( Quit )
# 603 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
                                           ( if _2 = "quiet" && _3 = "on" then MachineQuiet true else if _2 = "quiet" && _3 = "off" then MachineQuiet false else raise Parsing.Parse_error )
# 611 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                                           ( if _2 = "snapshot" then MachineSnapshot else raise Parsing.Parse_error )
# 618 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                           ( Axiom )
# 624 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                           ( Cut )
# 630 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                           ( Elim )
# 636 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                                           ( ByDefault )
# 642 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                                           ( Idtac )
# 648 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                                           ( Focus )
# 654 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                                           ( Elim_In )
# 660 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                           ( Contraction )
# 666 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                                           ( Weaken )
# 672 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                                           ( Moxia )
# 678 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tac) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 109 "parser.mly"
                                           ( TPlug (_1,_2,symbol_start_pos ()) )
# 686 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 110 "parser.mly"
                                           ( Then (_1,_3,symbol_start_pos ()) )
# 694 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'tactical) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'taclist) in
    Obj.repr(
# 111 "parser.mly"
                                           ( Thens (_1,_4,symbol_start_pos ()) )
# 702 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taclist) in
    Obj.repr(
# 114 "parser.mly"
                                           ( _1::_3 )
# 710 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 115 "parser.mly"
                                           ( [_1] )
# 717 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                           ( true )
# 723 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                           ( false )
# 729 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
                                           ( "and" )
# 735 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
                                           ( "or" )
# 741 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                                           ( "neg" )
# 747 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                                           ( "imply" )
# 753 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                                           ( "minus" )
# 759 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                                           ( "forall" )
# 765 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                                           ( "exists" )
# 771 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
                                           ( "true" )
# 777 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
                                           ( "false" )
# 783 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                                          ( OnTheLeft )
# 789 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
                                          ( OnTheRight )
# 795 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
                                          ( Ident _1 )
# 802 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 136 "parser.mly"
                                          ( Formula _1 )
# 809 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 137 "parser.mly"
                                          ( Expression _1 )
# 816 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 138 "parser.mly"
                                          ( Labeled_sort (_1,_3) )
# 824 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 139 "parser.mly"
                                          ( Labeled_prop (_1,_3) )
# 832 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                          ( Prover Coq )
# 838 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
                                          ( Prover Pvs )
# 844 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
                                          ( Prover Isabelle )
# 850 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 145 "parser.mly"
                                           ( _1::_2 )
# 858 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
                                           ( [] )
# 864 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                                           ( SSet )
# 870 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "parser.mly"
                                           ( SProp )
# 876 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 151 "parser.mly"
                                           ( SSym _1 )
# 883 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 152 "parser.mly"
                                           ( SArr (_1,_3) )
# 891 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's_expr) in
    Obj.repr(
# 153 "parser.mly"
                                           ( _2 )
# 898 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 157 "parser.mly"
                                           ( _1 )
# 905 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 158 "parser.mly"
                                           ( TApp (_1,_2) )
# 913 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 162 "parser.mly"
                                           ( TSym _1 )
# 920 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 163 "parser.mly"
                                           ( _2 )
# 927 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "parser.mly"
                                           ( True )
# 933 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "parser.mly"
                                           ( False )
# 939 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 168 "parser.mly"
                                           ( PSym _1 )
# 946 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 169 "parser.mly"
                                           ( UProp(Neg,_2) )
# 953 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 170 "parser.mly"
                                           ( PApp(_1,_2) )
# 961 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 171 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 969 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 172 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 977 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 173 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 985 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 174 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 993 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 175 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 1002 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 176 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 1011 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 177 "parser.mly"
                                           ( _2 )
# 1018 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 181 "parser.mly"
                                           ( [_1] )
# 1025 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 182 "parser.mly"
                                           ( _1::_3 )
# 1033 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 186 "parser.mly"
                                           ( _2 )
# 1040 "parser.ml"
               : 'delimited_p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 189 "parser.mly"
                                           ( _2 )
# 1047 "parser.ml"
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
