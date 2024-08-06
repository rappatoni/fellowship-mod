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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Commands.script
