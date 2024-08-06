(* 
  // The Fellowship Proof Assistant
  // Copyright Florent Kirchner, Claudio Sacerdoti Coen
  // This file is distributed under the terms of the CeCILL license version 2.
*)

{
  open Lexing
  open Parser
   
  exception Lexing_error of string
    
  let kwd_tbl = [
    "lj", LJ;
    "lk", LK;
    "minimal", MIN;
    "full", FULL;
    "declare", DECLARE;
    "discard", DISCARD;
    "all", ALL;
    "next", NEXT;
    "prev", PREV;
    "theorem", THEOREM;
    "qed", QED;
    "checkout", CHECKOUT;
    "export", EXPORT;
    "proof", PROOF;
    "term", TERM;
    "natural", NATURAL;
    "language", LANGUAGE;
    "coq", COQ;
    "pvs", PVS;
    "isabelle", ISABELLE;
    "help", HELP;
    "undo", UNDO;
    "quit", QUIT;
    "axiom", AXIOM;
    "cut", CUT;
    "elim", ELIM;
    "idtac", IDTAC;
    "focus", FOCUS;
    "in", IN;
    "contraction", CONTRACTION;
    "weaken", WEAKEN;
    "tacticals", TACTICALS;
    "types", TYPES;
    "terms", TERMS;
    "formulae", FORMULAE;
    "bool",PROP;
    "type",SET;
    "forall", FORALL;
    "exists", EXISTS;
    "true", TRUE;
    "false", FALSE;
    "left", LEFT;
    "right", RIGHT]
  let id_or_kwd s =  try List.assoc s kwd_tbl with _ -> IDENT(s) 

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let sep = ['_']
let ident = letter (letter | digit | sep)*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule nexttoken = parse
  | '\n'    { Format.printf "fsp < @?" ; newline lexbuf; nexttoken lexbuf }
  | "(*"[^'\n']*"*)" { nexttoken lexbuf }
  | space+  { nexttoken lexbuf }
  | ident   { id_or_kwd (lexeme lexbuf) }
  | '.'     { DOT }
  | ','     { VIR } 
  | ';'     { PVIR }
  | ':'     { COLON }
  | '~'     { NEG }
  | "->"    { ARROW }
  | "-"     { MINUS }
  | "/\\"   { AND }
  | "\\/"   { OR }
  | '('     { LPAR }
  | ')'     { RPAR }
  | '['     { LBRA }
  | ']'     { RBRA }
  | '|'     { PIPE }
  | eof     { EOF }
  | _       { raise (Lexing_error (lexeme lexbuf)) }
 

