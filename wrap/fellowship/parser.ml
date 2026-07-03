type token =
  | IDENT of (
# 18 "parser.mly"
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
# 8 "parser.mly"
  open Core
  open Tactics
  open Instructions
  open Help
  open Print

# 81 "parser.ml"
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
  288 (* BY *);
  289 (* DEFAULT *);
  290 (* TACTICALS *);
  291 (* TYPES *);
  292 (* TERMS *);
  293 (* FORMULAE *);
  294 (* PROP *);
  295 (* SET *);
  296 (* NEG *);
  297 (* ARROW *);
  298 (* MINUS *);
  299 (* AND *);
  300 (* OR *);
  301 (* FORALL *);
  302 (* EXISTS *);
  303 (* TRUE *);
  304 (* FALSE *);
  305 (* LEFT *);
  306 (* RIGHT *);
  307 (* ALL *);
  308 (* LPAR *);
  309 (* RPAR *);
  310 (* LBRA *);
  311 (* RBRA *);
  312 (* VIR *);
  313 (* PVIR *);
  314 (* PIPE *);
  315 (* COLON *);
  316 (* DOT *);
    0 (* EOF *);
  317 (* MOXIA *);
  318 (* ANTITHEOREM *);
  319 (* DENY *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\005\000\005\000\005\000\009\000\009\000\007\000\007\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\004\000\004\000\014\000\014\000\014\000\014\000\
\014\000\015\000\015\000\016\000\016\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\013\000\013\000\011\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\002\000\002\000\004\000\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\002\000\002\000\001\000\001\000\001\000\
\001\000\002\000\001\000\001\000\002\000\001\000\001\000\001\000\
\002\000\003\000\005\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\001\000\
\001\000\001\000\002\000\000\000\001\000\001\000\001\000\003\000\
\003\000\001\000\002\000\001\000\003\000\001\000\001\000\001\000\
\002\000\002\000\003\000\003\000\003\000\003\000\006\000\006\000\
\003\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\016\000\017\000\018\000\
\021\000\022\000\023\000\000\000\000\000\027\000\000\000\030\000\
\000\000\031\000\032\000\000\000\035\000\036\000\038\000\039\000\
\000\000\012\000\040\000\020\000\019\000\094\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\028\000\000\000\008\000\
\009\000\010\000\011\000\005\000\006\000\037\000\034\000\001\000\
\000\000\064\000\065\000\066\000\057\000\058\000\000\000\000\000\
\002\000\000\000\060\000\061\000\000\000\000\000\041\000\025\000\
\026\000\047\000\046\000\000\000\000\000\080\000\000\000\000\000\
\000\000\078\000\079\000\000\000\000\000\076\000\000\000\000\000\
\074\000\067\000\000\000\000\000\000\000\050\000\051\000\052\000\
\048\000\049\000\053\000\054\000\055\000\056\000\007\000\000\000\
\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\092\000\082\000\000\000\093\000\075\000\071\000\070\000\
\069\000\000\000\063\000\000\000\000\000\000\000\000\000\000\000\
\089\000\000\000\000\000\000\000\000\000\077\000\000\000\000\000\
\000\000\043\000\000\000\000\000\073\000\000\000\044\000\000\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\030\000\031\000\032\000\057\000\117\000\034\000\068\000\095\000\
\118\000\058\000\059\000\107\000\061\000\116\000\080\000\081\000\
\077\000"

let yysindex = "\012\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\248\254\007\255\000\000\000\255\000\000\
\134\255\000\000\000\000\254\254\000\000\000\000\000\000\000\000\
\010\255\000\000\000\000\000\000\000\000\000\000\242\254\132\255\
\253\254\132\255\011\255\033\255\000\000\000\000\251\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\255\000\000\000\000\000\000\000\000\000\000\161\255\006\255\
\000\000\132\255\000\000\000\000\004\255\190\255\000\000\000\000\
\000\000\000\000\000\000\007\000\073\255\000\000\161\255\073\255\
\073\255\000\000\000\000\161\255\135\255\000\000\006\255\001\255\
\000\000\000\000\003\255\199\255\253\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\255\
\000\000\031\255\028\255\038\255\149\255\161\255\161\255\161\255\
\161\255\000\000\000\000\068\255\000\000\000\000\000\000\000\000\
\000\000\113\255\000\000\052\255\216\254\044\255\113\255\113\255\
\000\000\131\255\031\255\031\255\031\255\000\000\230\254\113\255\
\199\255\000\000\224\254\227\254\000\000\052\255\000\000\161\255\
\161\255\131\255\131\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\078\255\000\000\000\000\000\000\000\000\
\042\255\000\000\000\000\046\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\045\255\
\049\255\195\255\000\000\000\000\000\000\000\000\051\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\195\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\188\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\060\255\
\000\000\192\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\058\255\069\255\000\000\000\000\000\000\
\000\000\072\255\196\255\232\255\249\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\255\000\000\000\000\
\000\000\076\255\084\255"

let yygindex = "\000\000\
\000\000\000\000\109\000\228\255\002\000\133\000\000\000\000\000\
\024\000\000\000\074\000\238\255\185\000\003\000\101\000\187\255\
\185\255"

let yytablesize = 320
let yytable = "\098\000\
\026\000\078\000\033\000\111\000\101\000\063\000\078\000\035\000\
\128\000\037\000\110\000\128\000\001\000\060\000\128\000\060\000\
\062\000\129\000\059\000\059\000\059\000\059\000\046\000\136\000\
\036\000\046\000\137\000\064\000\133\000\082\000\122\000\123\000\
\124\000\125\000\072\000\072\000\072\000\072\000\110\000\060\000\
\112\000\113\000\047\000\066\000\067\000\048\000\033\000\033\000\
\033\000\033\000\038\000\065\000\079\000\062\000\055\000\109\000\
\114\000\079\000\062\000\062\000\062\000\062\000\083\000\085\000\
\138\000\139\000\059\000\059\000\078\000\059\000\069\000\059\000\
\059\000\096\000\059\000\059\000\090\000\059\000\024\000\024\000\
\024\000\024\000\072\000\072\000\056\000\072\000\119\000\072\000\
\072\000\072\000\072\000\072\000\128\000\072\000\033\000\033\000\
\120\000\033\000\130\000\033\000\033\000\004\000\033\000\033\000\
\068\000\033\000\062\000\062\000\003\000\062\000\033\000\062\000\
\062\000\111\000\062\000\062\000\127\000\062\000\090\000\079\000\
\126\000\131\000\132\000\045\000\083\000\044\000\024\000\024\000\
\087\000\024\000\134\000\024\000\049\000\050\000\051\000\052\000\
\088\000\024\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\045\000\112\000\113\000\
\135\000\014\000\015\000\016\000\115\000\018\000\019\000\039\000\
\021\000\070\000\022\000\023\000\024\000\025\000\114\000\040\000\
\041\000\042\000\043\000\102\000\103\000\104\000\105\000\102\000\
\103\000\104\000\105\000\108\000\053\000\054\000\000\000\055\000\
\056\000\056\000\000\000\106\000\056\000\102\000\103\000\104\000\
\105\000\000\000\027\000\028\000\029\000\000\000\000\000\000\000\
\071\000\121\000\056\000\000\000\000\000\072\000\073\000\074\000\
\075\000\000\000\000\000\000\000\076\000\018\000\019\000\020\000\
\021\000\000\000\022\000\023\000\024\000\025\000\018\000\019\000\
\020\000\021\000\000\000\022\000\023\000\024\000\025\000\000\000\
\081\000\081\000\081\000\081\000\084\000\084\000\084\000\084\000\
\000\000\000\000\042\000\084\000\081\000\042\000\000\000\042\000\
\084\000\068\000\027\000\068\000\068\000\097\000\068\000\000\000\
\099\000\100\000\000\000\027\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\086\000\086\000\086\000\086\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\086\000\022\000\023\000\024\000\
\025\000\085\000\085\000\085\000\085\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\029\000"

let yycheck = "\071\000\
\000\000\001\001\001\000\001\001\076\000\034\000\001\001\016\001\
\041\001\010\001\080\000\041\001\001\000\032\000\041\001\034\000\
\057\001\058\001\001\001\002\001\003\001\004\001\028\001\056\001\
\018\001\028\001\056\001\017\001\055\001\058\000\102\000\103\000\
\104\000\105\000\001\001\002\001\003\001\004\001\108\000\058\000\
\038\001\039\001\033\001\049\001\050\001\060\001\001\001\002\001\
\003\001\004\001\051\001\019\001\052\001\057\001\052\001\055\001\
\054\001\052\001\001\001\002\001\003\001\004\001\059\001\062\000\
\136\000\137\000\049\001\050\001\001\001\052\001\056\001\054\001\
\055\001\001\001\057\001\058\001\059\001\060\001\001\001\002\001\
\003\001\004\001\049\001\050\001\054\001\052\001\059\001\054\001\
\055\001\056\001\057\001\058\001\041\001\060\001\049\001\050\001\
\059\001\052\001\055\001\054\001\055\001\060\001\057\001\058\001\
\060\001\060\001\049\001\050\001\060\001\052\001\060\001\054\001\
\055\001\001\001\057\001\058\001\114\000\060\001\059\001\052\001\
\053\001\119\000\120\000\055\001\053\001\017\000\049\001\050\001\
\053\001\052\001\128\000\054\001\001\001\002\001\003\001\004\001\
\053\001\060\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\017\000\038\001\039\001\
\129\000\020\001\021\001\022\001\083\000\024\001\025\001\026\001\
\027\001\001\001\029\001\030\001\031\001\032\001\054\001\034\001\
\035\001\036\001\037\001\041\001\042\001\043\001\044\001\041\001\
\042\001\043\001\044\001\079\000\049\001\050\001\255\255\052\001\
\054\001\054\001\255\255\053\001\054\001\041\001\042\001\043\001\
\044\001\255\255\061\001\062\001\063\001\255\255\255\255\255\255\
\040\001\053\001\054\001\255\255\255\255\045\001\046\001\047\001\
\048\001\255\255\255\255\255\255\052\001\024\001\025\001\026\001\
\027\001\255\255\029\001\030\001\031\001\032\001\024\001\025\001\
\026\001\027\001\255\255\029\001\030\001\031\001\032\001\255\255\
\041\001\042\001\043\001\044\001\041\001\042\001\043\001\044\001\
\255\255\255\255\055\001\054\001\053\001\058\001\255\255\060\001\
\053\001\055\001\061\001\057\001\058\001\069\000\060\001\255\255\
\072\000\073\000\255\255\061\001\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\041\001\042\001\043\001\044\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\053\001\029\001\030\001\031\001\
\032\001\041\001\042\001\043\001\044\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\053\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\047\001\048\001\255\255\
\255\255\255\255\255\255\255\255\255\255\061\001\062\001\063\001"

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
# 47 "parser.mly"
                                           ( _1 )
# 412 "parser.ml"
               : Help.script))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 50 "parser.mly"
                                           ( Instruction (_1,_2) )
# 420 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 51 "parser.mly"
                                           ( Tactical _1 )
# 427 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                                           ( Help Nix )
# 433 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 54 "parser.mly"
                                           ( Help (HInstr _2) )
# 440 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tac) in
    Obj.repr(
# 55 "parser.mly"
                                           ( Help (HTac _2) )
# 447 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dir) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'connector) in
    Obj.repr(
# 56 "parser.mly"
                                           ( Help (HElim (_3,_4)) )
# 455 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                           ( Help HTacticals )
# 461 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                           ( Help HTypes )
# 467 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                           ( Help HTerms )
# 473 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                                           ( Help HFormulae )
# 479 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                                           ( raise (Failure "end") )
# 485 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                           ( Lj true )
# 491 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                           ( Lj false )
# 497 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                           ( Min true )
# 503 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                           ( Min false )
# 509 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                           ( Declare )
# 515 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                           ( Theorem )
# 521 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                           ( Deny )
# 527 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                           ( AntiTheorem )
# 533 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                           ( Next )
# 539 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                           ( Prev )
# 545 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                           ( Qed )
# 551 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                                           ( CheckOut )
# 557 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                           ( CheckOutProofTerm )
# 563 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
                                           ( ExportNaturalLanguage )
# 569 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                                           ( if !toplvl then Undo 
					     else raise Parsing.Parse_error )
# 576 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( if !toplvl then DiscardAll
					     else raise Parsing.Parse_error )
# 583 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                           ( if !toplvl then DiscardTheorem
					     else raise Parsing.Parse_error )
# 590 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                           ( Quit )
# 596 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                                           ( Axiom )
# 602 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                                           ( Cut )
# 608 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                                           ( Elim )
# 614 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                                           ( ByDefault )
# 620 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                                           ( Idtac )
# 626 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                           ( Focus )
# 632 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                           ( Elim_In )
# 638 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                                           ( Contraction )
# 644 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                                           ( Weaken )
# 650 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                                           ( Moxia )
# 656 "parser.ml"
               : 'tac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tac) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 103 "parser.mly"
                                           ( TPlug (_1,_2,symbol_start_pos ()) )
# 664 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 104 "parser.mly"
                                           ( Then (_1,_3,symbol_start_pos ()) )
# 672 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'tactical) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'taclist) in
    Obj.repr(
# 105 "parser.mly"
                                           ( Thens (_1,_4,symbol_start_pos ()) )
# 680 "parser.ml"
               : 'tactical))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tactical) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taclist) in
    Obj.repr(
# 108 "parser.mly"
                                           ( _1::_3 )
# 688 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactical) in
    Obj.repr(
# 109 "parser.mly"
                                           ( [_1] )
# 695 "parser.ml"
               : 'taclist))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                                           ( true )
# 701 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                                           ( false )
# 707 "parser.ml"
               : 'dir))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                                           ( "and" )
# 713 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                                           ( "or" )
# 719 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                           ( "neg" )
# 725 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                           ( "imply" )
# 731 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                                           ( "minus" )
# 737 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
                                           ( "forall" )
# 743 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
                                           ( "exists" )
# 749 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
                                           ( "true" )
# 755 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                                           ( "false" )
# 761 "parser.ml"
               : 'connector))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                                          ( OnTheLeft )
# 767 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                                          ( OnTheRight )
# 773 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
                                          ( Ident _1 )
# 780 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 130 "parser.mly"
                                          ( Formula _1 )
# 787 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 131 "parser.mly"
                                          ( Expression _1 )
# 794 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 132 "parser.mly"
                                          ( Labeled_sort (_1,_3) )
# 802 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_p_expr) in
    Obj.repr(
# 133 "parser.mly"
                                          ( Labeled_prop (_1,_3) )
# 810 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
                                          ( Prover Coq )
# 816 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
                                          ( Prover Pvs )
# 822 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
                                          ( Prover Isabelle )
# 828 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 139 "parser.mly"
                                           ( _1::_2 )
# 836 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                           ( [] )
# 842 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                                           ( SSet )
# 848 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                                           ( SProp )
# 854 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                                           ( SSym _1 )
# 861 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's_expr) in
    Obj.repr(
# 146 "parser.mly"
                                           ( SArr (_1,_3) )
# 869 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's_expr) in
    Obj.repr(
# 147 "parser.mly"
                                           ( _2 )
# 876 "parser.ml"
               : 's_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 151 "parser.mly"
                                           ( _1 )
# 883 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't_exprat) in
    Obj.repr(
# 152 "parser.mly"
                                           ( TApp (_1,_2) )
# 891 "parser.ml"
               : 't_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                                           ( TSym _1 )
# 898 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 157 "parser.mly"
                                           ( _2 )
# 905 "parser.ml"
               : 't_exprat))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
                                           ( True )
# 911 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
                                           ( False )
# 917 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 162 "parser.mly"
                                           ( PSym _1 )
# 924 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 163 "parser.mly"
                                           ( UProp(Neg,_2) )
# 931 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'delimited_t_expr) in
    Obj.repr(
# 164 "parser.mly"
                                           ( PApp(_1,_2) )
# 939 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 165 "parser.mly"
                                           ( BProp(_1,Imp,_3) )
# 947 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 166 "parser.mly"
                                           ( BProp(_1,Minus,_3) )
# 955 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 167 "parser.mly"
                                           ( BProp(_1,Disj,_3) )
# 963 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 168 "parser.mly"
                                           ( BProp(_1,Conj,_3) )
# 971 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 169 "parser.mly"
                                           ( Quant(Forall,(_2,_4),_6) )
# 980 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 's_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'p_expr) in
    Obj.repr(
# 170 "parser.mly"
                                           ( Quant(Exists,(_2,_4),_6) )
# 989 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 171 "parser.mly"
                                           ( _2 )
# 996 "parser.ml"
               : 'p_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 175 "parser.mly"
                                           ( [_1] )
# 1003 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 176 "parser.mly"
                                           ( _1::_3 )
# 1011 "parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_expr) in
    Obj.repr(
# 180 "parser.mly"
                                           ( _2 )
# 1018 "parser.ml"
               : 'delimited_p_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't_expr) in
    Obj.repr(
# 183 "parser.mly"
                                           ( _2 )
# 1025 "parser.ml"
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
