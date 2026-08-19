"""One grammar for everything AIDA parses.

Propositions, sorts, first-order terms and proof terms are all described here,
in one Lark grammar with several start symbols.  Keeping them together is the
point: a proof term's binder annotations *are* propositions, and a second
description of the proposition syntax would be one more thing to keep in step
with ``core.ml`` and ``parser.mly``.  Precedence drifting between two
implementations of the same grammar is a bug this codebase has already had
several times.

Why a grammar rather than a hand-written parser: ambiguity becomes visible.
Running with ``ambiguity="explicit"`` and failing on any ``_ambig`` node turns
"the parser silently picked a reading" -- the failure mode behind
``λx:A.y*z``, ``?n``/``n?`` and the precedence bugs -- into a test.
``tests/test_grammar_unambiguous.py`` does exactly that.

Earley rather than LALR: the combined grammar is not LALR.  A bare identifier
is category-neutral, and only the slot it sits in decides whether it is a term
or a context, which shows up as a reduce/reduce conflict on ``*``.  Earley
handles it, and the ambiguity test supplies the guarantee LALR would have.
"""

from __future__ import annotations

from core.ac.ast import (
    Admal, Cons, ConsFO, DI, Deleg, DestructTermsPairFO, Geled, Goal, Hyp, ID,
    Laog, Lamda, Mu, Mutilde, Pyh, Sonc, TermsPairFO,
)
from core.ac.prop import (
    BinOp,
    PropError, PApp, PBin, PFalse, PNeg, PQuant, PSym, PTrue, Quantifier,
    SArr, SProp, SSet, SSym, TApp, TSym,
)

GRAMMAR = r"""
// ===================== propositions =====================================
// Precedence mirrors core.ml's pretty_prop and parser.mly, which agree:
// subtraction loosest, then implication, then disjunction, then conjunction.
// Conjunction and disjunction are outside AIDA's fragment and absent here.

// A quantifier body extends maximally, so a quantifier only appears at the
// loosest level or inside parentheses.  Allowing one as an operand makes
// `forall n:N,P n->Q n` mean either `forall n:N,(P n->Q n)` or
// `(forall n:N,P n)->Q n` -- and core.ml parenthesises a quantifier in
// subformula position precisely so the two stay apart.
?prop:      quant
          | minus
?quant:     _FORALL varlist ":" sort "," prop   -> p_forall
          | _EXISTS varlist ":" sort "," prop   -> p_exists
?minus:     imp
          | minus _MINUS imp                    -> p_minus
?imp:       neg
          | neg _ARROW imp                      -> p_imp
?neg:       _NEG neg                            -> p_neg
          | papp
?papp:      patom
          | papp fatom                          -> p_app
?patom:     _TRUE                               -> p_true
          | _FALSE                              -> p_false
          | NAME                                -> p_sym
          | "(" prop ")"
varlist:    NAME ("," NAME)*

// A predicate argument is a first-order term.  Fellowship prints them
// juxtaposed and parses them bracketed; both spellings are accepted here.
?foterm:    fatom
          | foterm fatom                        -> t_app
?fatom:     NAME                                -> t_sym
          | "(" foterm ")"                      -> t_group
          | "[" foterm "]"                      -> t_group

// ===================== sorts ============================================
?sort:      sortatom
          | sortatom _ARROW sort                -> s_arr
?sortatom:  _SET                                -> s_set
          | _BOOL                               -> s_prop
          | NAME                                -> s_sym
          | "(" sort ")"
          | "[" sort "]"

// ===================== proof terms ======================================
// Two mutually recursive categories.  X*Y is Cons(term,context) in a context
// slot and Sonc(context,term) in a term slot, so the slot decides how each
// operand is read.  A lambda binds tighter than `*`; the other reading is
// written with parentheses, which core.ml's printer emits for that case.

?term:      tight_context _STAR term            -> sonc
          | tight_term
?context:   tight_term _STAR context            -> cons
          | fo_app _STAR context                -> consfo
          | tight_context

// At least two atoms: `S (S O)*c` is first-order beyond doubt, whereas a lone
// `O*c` could equally be a proof application and is left to resolution.
fo_app:     foterm fatom

?tight_term:    lamda | mu | goal | deleg | pairfo
          | NAME annot?                         -> di
          | "(" term ")"
?tight_context: admal | mutilde | destruct | laog | geled
          | NAME annot?                         -> id
          | "(" context ")"

lamda:      _LAMBDA NAME ":" prop "." tight_term
admal:      _LAMBDA NAME ":" prop "." tight_context
mu:         _MU NAME ":" prop "." command
mutilde:    _MUTILDE NAME ":" prop "." command
command:    "<" term _PARALLEL context ">"

pairfo:     "(" foterm "," term ")"
destruct:   "(" NAME ":" sort ")" "." tight_context

goal:       "?" number annot?
deleg:      "!" number annot?
laog:       number annot? "?"
geled:      number annot? "!"
?annot:     ":" prop
number:     NUMBER ("." NUMBER)*

// ===================== lexis ============================================
// The ASCII spellings are what machine mode emits; the unicode ones are what
// `Argument._normalize_pt_to_unicode` rewrites them to.  Both are accepted so
// that neither a raw payload nor a normalised one is a special case.
_MUTILDE.5: "μ'" | ";:'"
_MU.4:      "μ"  | ";:"
_LAMBDA.4:  "λ"  | "\\"
_PARALLEL.6: "||"
_STAR:      "*"
_ARROW.3:   "->"
_MINUS:     "-"
_NEG:       "~" | "¬"
// Keywords need an explicit right boundary.  The basic lexer ranks terminals
// by priority before length, so a bare "true" would otherwise win against
// NAME on an identifier like `truef` and split it into `true` + `f`.
_TRUE.3:    /true(?![A-Za-z0-9_])/   | "⊤"
_FALSE.3:   /false(?![A-Za-z0-9_])/  | "⊥"
_FORALL.3:  /forall(?![A-Za-z0-9_])/ | "∀"
_EXISTS.3:  /exists(?![A-Za-z0-9_])/ | "∃"
_SET.3:     /type(?![A-Za-z0-9_])/
_BOOL.3:    /bool(?![A-Za-z0-9_])/
NAME:       /(?!(?:true|false|forall|exists|type|bool)\b)[A-Za-z_][A-Za-z0-9_]*/
NUMBER:     /[0-9]+/
%import common.WS
%ignore WS
"""


from functools import lru_cache

from lark import Lark, Transformer, v_args
from lark.exceptions import LarkError


class SyntaxErrorInSource(PropError):
    """Raised when a proposition, sort, term or proof term cannot be parsed.

    Subclasses :class:`~core.ac.prop.PropError` so that callers which already
    catch a malformed proposition keep working.
    """


#: The name this error had while proof terms had their own parser.
ProofTermSyntaxError = SyntaxErrorInSource


@lru_cache(maxsize=None)
def _parser(ambiguity: str = "resolve") -> Lark:
    """The shared parser.

    Earley, because the combined grammar is not LALR: a bare identifier is
    category-neutral and only its slot decides whether it is a term or a
    context, which is a reduce/reduce conflict on ``*``.

    ``lexer="basic"`` matters.  Earley's dynamic lexer finds several ways to
    skip whitespace and reports the resulting identical derivations as
    ambiguity, which would drown the real signal in the ambiguity test.
    """
    return Lark(
        GRAMMAR,
        start=["prop", "sort", "foterm", "term", "context"],
        parser="earley",
        lexer="basic",
        ambiguity=ambiguity,
    )


def parse(text: str, start: str):
    """Parse ``text`` at the given start symbol into the AC AST."""
    try:
        tree = _parser().parse(text, start=start)
    except LarkError as error:
        raise SyntaxErrorInSource(
            f"cannot parse {text!r} as a {start}: {error}"
        ) from error
    return _Build().transform(tree)


@v_args(inline=True)
class _Build(Transformer):
    """Turns the parse tree into the AST classes the rest of AIDA uses."""

    # -- propositions ------------------------------------------------------

    def p_minus(self, left, right):
        return PBin(left, BinOp.MINUS, right)

    def p_imp(self, left, right):
        return PBin(left, BinOp.IMP, right)

    def p_neg(self, body):
        return PNeg(body)

    def p_forall(self, names, sort, body):
        return PQuant(Quantifier.FORALL, names, sort, body)

    def p_exists(self, names, sort, body):
        return PQuant(Quantifier.EXISTS, names, sort, body)

    def p_app(self, pred, arg):
        return PApp(pred, arg)

    def p_true(self):
        return PTrue()

    def p_false(self):
        return PFalse()

    def p_sym(self, name):
        return PSym(str(name))

    def varlist(self, *names):
        return tuple(str(name) for name in names)

    # -- first-order terms -------------------------------------------------

    def t_sym(self, name):
        return TSym(str(name))

    def t_app(self, fun, arg):
        return TApp(fun, arg)

    def t_group(self, inner):
        return inner

    def fo_app(self, fun, arg):
        return TApp(fun, arg)

    # -- sorts -------------------------------------------------------------

    def s_arr(self, left, right):
        return SArr(left, right)

    def s_set(self):
        return SSet()

    def s_prop(self):
        return SProp()

    def s_sym(self, name):
        return SSym(str(name))

    # -- proof terms -------------------------------------------------------
    #
    # Binder annotations are stored as strings: node `.prop` is a string
    # everywhere downstream, and threading structured propositions through the
    # proof-term AST is a separate change.  Parsing them still earns
    # something, since a malformed annotation fails here rather than later.

    def sonc(self, context, term):
        return Sonc(context, term)

    def cons(self, term, context):
        return Cons(term, context)

    def consfo(self, fo_term, context):
        return ConsFO(fo_term, context)

    def lamda(self, name, prop, body):
        return Lamda(Hyp(DI(str(name)), str(prop)), body)

    def admal(self, name, prop, body):
        return Admal(Pyh(ID(str(name)), str(prop)), body)

    def mu(self, name, prop, command):
        term, context = command
        return Mu(ID(str(name)), str(prop), term, context)

    def mutilde(self, name, prop, command):
        term, context = command
        return Mutilde(DI(str(name)), str(prop), term, context)

    def command(self, term, context):
        return term, context

    def pairfo(self, witness, term):
        return TermsPairFO(witness, term)

    def destruct(self, name, sort, context):
        return DestructTermsPairFO(str(name), sort, context)

    def goal(self, number, prop=None):
        return Goal(number, _text(prop))

    def deleg(self, number, prop=None):
        return Deleg(number, _text(prop))

    def laog(self, number, prop=None):
        return Laog(number, _text(prop))

    def geled(self, number, prop=None):
        return Geled(number, _text(prop))

    def di(self, name, prop=None):
        return DI(str(name), _text(prop))

    def id(self, name, prop=None):
        return ID(str(name), _text(prop))

    def number(self, *parts):
        return ".".join(str(part) for part in parts)


def _text(prop):
    return None if prop is None else str(prop)


def parse_proof_term(text: str, *, category: str | None = None):
    """Parse a proof term.

    ``category`` forces the top-level reading.  Left unset it is taken from the
    leading binder: ``μ'`` opens a context and anything else a term, which is
    how Fellowship prints antitheorems and theorems respectively.
    """
    if category is None:
        stripped = text.lstrip()
        category = (
            "context"
            if stripped.startswith("μ'") or stripped.startswith(";:'")
            else "term"
        )
    return parse(text, category)
