"""Structured propositions, first-order terms and sorts.

Propositions used to be plain Python strings, built by concatenation and
compared with ``==``.  That works for a propositional calculus but not for a
first-order one: universal instantiation needs capture-avoiding substitution
``P[t/x]``, and ``forall x, P x`` must compare equal to ``forall y, P y``.
Neither is expressible on strings.

This module mirrors Fellowship's own ``type prop`` (``core.ml:198-207``),
restricted to the fragment AIDA supports: implication, subtraction, negation,
the two units, predicate application and the two quantifiers.  Conjunction and
disjunction are deliberately absent -- see :class:`BinOp`.

Three renderings, deliberately distinct
---------------------------------------

``str(prop)``
    Fellowship's *printed* form, the one that arrives in the machine payload:
    juxtaposed application (``P x y``), no space around connectives.
    ``Prop.parse`` is its exact inverse.

``prop.to_command()``
    Fellowship's *input* form: bracketed application (``P [x] [y]``).

The two share one precedence table, the conventional ordering
``/\\`` > ``\\/`` > ``->`` > ``-``.  They differ only in how application is
spelled and in spacing.  (Fellowship's printer and parser used to disagree
here, so the prover printed ``A-B->C`` for ``A-(B->C)`` and read that same
string back as ``(A-B)->C``.  Both sides now use this ordering.)

Quantifier scope
----------------

A quantifier body extends as far right as it can, so a quantifier in
subformula position must be parenthesised or its scope is lost:
``(forall x:A, Q x) -> C`` and ``forall x:A, (Q x -> C)`` would otherwise print
alike.  ``pretty_prop``'s quantifier branch used to ignore its ``parentprio``
argument and did exactly that; it now guards on it like every other
connective.  The renderer here mirrors that guard, so ``parse`` stays an exact
inverse of the prover's output.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from typing import Dict, Set


class PropError(ValueError):
    """Raised when a proposition cannot be parsed or rendered."""


# ---------------------------------------------------------------------------
#  Sorts
# ---------------------------------------------------------------------------


class Sort:
    """A Fellowship sort: ``type``, ``bool``, a declared sort, or an arrow."""

    __slots__ = ()

    def __str__(self) -> str:  # pragma: no cover - overridden
        raise NotImplementedError


@dataclass(frozen=True)
class SSet(Sort):
    """The sort of sorts, spelled ``type``."""

    def __str__(self) -> str:
        return "type"


@dataclass(frozen=True)
class SProp(Sort):
    """The sort of propositions, spelled ``bool``."""

    def __str__(self) -> str:
        return "bool"


@dataclass(frozen=True)
class SSym(Sort):
    """A declared sort, e.g. ``N``."""

    name: str

    def __str__(self) -> str:
        return self.name


@dataclass(frozen=True)
class SArr(Sort):
    """A function sort ``s -> s'``."""

    left: Sort
    right: Sort

    def __str__(self) -> str:
        # Mirrors core.ml:120-126: only a left operand that is itself an arrow
        # needs parentheses, since -> is right associative.
        lhs = f"({self.left})" if isinstance(self.left, SArr) else str(self.left)
        return f"{lhs}->{self.right}"


def sort_result(sort: Sort) -> Sort:
    """The result sort of an arrow chain: ``N->N->bool`` gives ``bool``."""
    while isinstance(sort, SArr):
        sort = sort.right
    return sort


# ---------------------------------------------------------------------------
#  First-order terms
# ---------------------------------------------------------------------------


class Term:
    """A first-order term (``core.ml:157-159``)."""

    __slots__ = ()

    def free_vars(self) -> Set[str]:
        raise NotImplementedError

    def subst(self, name: str, replacement: "Term") -> "Term":
        raise NotImplementedError


@dataclass(frozen=True)
class TSym(Term):
    """A variable or constant."""

    name: str

    def __str__(self) -> str:
        return self.name

    def free_vars(self) -> Set[str]:
        return {self.name}

    def subst(self, name: str, replacement: Term) -> Term:
        return replacement if self.name == name else self


@dataclass(frozen=True)
class TApp(Term):
    """Application of a term to a term, e.g. ``S O``."""

    fun: Term
    arg: Term

    def __str__(self) -> str:
        # core.ml:161-166: a nested application in argument position is
        # parenthesised, a bare symbol is not.
        arg = f"({self.arg})" if isinstance(self.arg, TApp) else str(self.arg)
        return f"{self.fun} {arg}"

    def free_vars(self) -> Set[str]:
        return self.fun.free_vars() | self.arg.free_vars()

    def subst(self, name: str, replacement: Term) -> Term:
        return TApp(self.fun.subst(name, replacement), self.arg.subst(name, replacement))


# ---------------------------------------------------------------------------
#  Propositions
# ---------------------------------------------------------------------------


class BinOp(Enum):
    """Binary connectives AIDA supports.

    Fellowship also has ``Conj`` and ``Disj``; they are omitted here because
    AIDA has no proof-term constructors for them, so a conjunction could be
    represented but never proved.  Adding them later needs only new members
    plus their precedences in :data:`_PRIORITY`.
    """

    IMP = "->"
    MINUS = "-"


#: Precedence, mirroring core.ml's pretty_prop and parser.mly, which now
#: agree.  Higher binds tighter: subtraction is the loosest binary connective,
#: then implication, then disjunction (8) and conjunction (10) should those
#: ever be added.
_PRIORITY: Dict[BinOp, int] = {BinOp.IMP: 6, BinOp.MINUS: 4}

#: Per operand, whether the same priority may appear without parentheses.
#: Implication is right associative, subtraction left associative.
_ASSOC: Dict[BinOp, tuple] = {BinOp.IMP: (False, True), BinOp.MINUS: (True, False)}

#: A quantifier body extends maximally, so a quantifier appearing in any
#: operand position needs parentheses.  Matches core.ml's Quant guard, which
#: keys off "is there a parent at all" rather than a priority number.
_NEG_PRIORITY = 12
#: Binds tightest.  Only reachable with an ill-sorted compound predicate head,
#: which Fellowship's type checker rejects; parenthesising is the safe choice.
_APP_PRIORITY = 14


class Quantifier(Enum):
    FORALL = "forall"
    EXISTS = "exists"


class Prop:
    """A proposition.

    Equality and hashing are up to alpha-equivalence, so a quantified
    proposition can be used as a dict key and compared across renamings.
    """

    __slots__ = ()

    # -- rendering ---------------------------------------------------------

    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        """Render at a given precedence.

        One traversal serves both spellings.  ``command=False`` reproduces
        Fellowship's printed form; ``command=True`` produces command syntax,
        which differs only in bracketing application and in spacing.
        """
        raise NotImplementedError

    def __str__(self) -> str:
        return self._render()

    def to_command(self) -> str:
        """Render for Fellowship's command parser: ``P [x] [y]``."""
        return self._render(command=True)

    # -- variables ---------------------------------------------------------

    def free_vars(self) -> Set[str]:
        """Free *first-order* variables.  Predicate symbols are not included."""
        raise NotImplementedError

    def subst(self, name: str, replacement: Term) -> "Prop":
        """Capture-avoiding substitution of a first-order term for a variable.

        This is what universal instantiation needs: eliminating
        ``forall x:S, P`` against a term ``t`` yields ``P[t/x]``.
        """
        raise NotImplementedError

    # -- alpha equivalence -------------------------------------------------

    def _canonical(self, env: Dict[str, int], depth: int) -> str:
        """Render with bound variables replaced by their binding depth."""
        raise NotImplementedError

    def canonical(self) -> str:
        return self._canonical({}, 0)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Prop):
            return NotImplemented
        return self.canonical() == other.canonical()

    def __hash__(self) -> int:
        return hash(self.canonical())

    # -- construction ------------------------------------------------------

    @staticmethod
    def parse(text: str) -> "Prop":
        """Parse Fellowship's printed proposition syntax."""
        return _Parser(text).parse_whole()


def _paren(text: str, condition: bool) -> str:
    return f"({text})" if condition else text


@dataclass(frozen=True, eq=False)
class PTrue(Prop):
    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        return "true"

    def free_vars(self) -> Set[str]:
        return set()

    def subst(self, name: str, replacement: Term) -> Prop:
        return self

    def _canonical(self, env, depth) -> str:
        return "true"


@dataclass(frozen=True, eq=False)
class PFalse(Prop):
    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        return "false"

    def free_vars(self) -> Set[str]:
        return set()

    def subst(self, name: str, replacement: Term) -> Prop:
        return self

    def _canonical(self, env, depth) -> str:
        return "false"


@dataclass(frozen=True, eq=False)
class PSym(Prop):
    """A propositional atom or a predicate symbol."""

    name: str

    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        return self.name

    def free_vars(self) -> Set[str]:
        # A predicate symbol is not a first-order variable.
        return set()

    def subst(self, name: str, replacement: Term) -> Prop:
        return self

    def _canonical(self, env, depth) -> str:
        return f"S:{self.name}"


@dataclass(frozen=True, eq=False)
class PApp(Prop):
    """Predicate application, e.g. ``P x y``."""

    pred: Prop
    arg: Term

    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        pred = self.pred._render(_APP_PRIORITY, command=command)
        if command:
            # The command parser requires brackets; the printer juxtaposes.
            return f"{pred}[{self.arg}]"
        # core.ml:226-228, mirroring pretty_term's argument rule.
        arg = f"({self.arg})" if isinstance(self.arg, TApp) else str(self.arg)
        return f"{pred} {arg}"

    def free_vars(self) -> Set[str]:
        return self.pred.free_vars() | self.arg.free_vars()

    def subst(self, name: str, replacement: Term) -> Prop:
        return PApp(self.pred.subst(name, replacement), self.arg.subst(name, replacement))

    def _canonical(self, env, depth) -> str:
        return f"A({self.pred._canonical(env, depth)},{_term_canonical(self.arg, env)})"


@dataclass(frozen=True, eq=False)
class PNeg(Prop):
    body: Prop

    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        return f"~{self.body._render(_NEG_PRIORITY, command=command)}"

    def free_vars(self) -> Set[str]:
        return self.body.free_vars()

    def subst(self, name: str, replacement: Term) -> Prop:
        return PNeg(self.body.subst(name, replacement))

    def _canonical(self, env, depth) -> str:
        return f"N({self.body._canonical(env, depth)})"


@dataclass(frozen=True, eq=False)
class PBin(Prop):
    left: Prop
    op: BinOp
    right: Prop

    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        priority = _PRIORITY[self.op]
        assoc_left, assoc_right = _ASSOC[self.op]
        left = self.left._render(priority, assoc_left, command=command)
        right = self.right._render(priority, assoc_right, command=command)
        # Commands space out the arrow, matching how these read in a script.
        operator = " -> " if command and self.op is BinOp.IMP else self.op.value
        body = f"{left}{operator}{right}"
        return _paren(body, priority < parent_priority or (parent_priority == priority and not assoc))

    def free_vars(self) -> Set[str]:
        return self.left.free_vars() | self.right.free_vars()

    def subst(self, name: str, replacement: Term) -> Prop:
        return PBin(
            self.left.subst(name, replacement), self.op, self.right.subst(name, replacement)
        )

    def _canonical(self, env, depth) -> str:
        return (
            f"B({self.left._canonical(env, depth)}"
            f",{self.op.value},"
            f"{self.right._canonical(env, depth)})"
        )


@dataclass(frozen=True, eq=False)
class PQuant(Prop):
    """``forall x,y:S, body`` or ``exists x,y:S, body``.

    Fellowship allows several variables to share one sort in a single binder
    (``core.ml:206``), and prints them comma-separated.
    """

    quantifier: Quantifier
    names: tuple
    sort: Sort
    body: Prop

    def __post_init__(self):
        if not self.names:
            raise PropError("a quantifier must bind at least one variable")
        object.__setattr__(self, "names", tuple(self.names))

    def _render(self, parent_priority: int = 0, assoc: bool = False, *, command: bool = False) -> str:
        # Mirrors core.ml's Quant branch: a quantifier body extends maximally,
        # so a quantifier in subformula position is parenthesised to keep its
        # scope.  Without this, (forall x,Q x)->C and forall x,(Q x->C) would
        # render identically and the reading would be lost.
        separator = ", " if command else ","
        # The body sits in maximal-extent position, so nothing in it needs
        # parentheses -- not even another quantifier.  core.ml passes the
        # default priority here for the same reason.
        rendered_body = self.body._render(command=command)
        body = (
            f"{self.quantifier.value} {','.join(self.names)}:{self.sort}"
            f"{separator}{rendered_body}"
        )
        return _paren(body, parent_priority > 0)

    def free_vars(self) -> Set[str]:
        return self.body.free_vars() - set(self.names)

    def subst(self, name: str, replacement: Term) -> Prop:
        if name in self.names:
            return self  # shadowed: no free occurrence below here
        captured = set(self.names) & replacement.free_vars()
        if not captured:
            return PQuant(self.quantifier, self.names, self.sort, self.body.subst(name, replacement))
        # Rename the binders that would capture a variable of the replacement.
        taken = replacement.free_vars() | self.body.free_vars() | {name}
        renamed = []
        body = self.body
        for bound in self.names:
            if bound in captured:
                fresh = _fresh(bound, taken)
                taken.add(fresh)
                body = body.subst(bound, TSym(fresh))
                renamed.append(fresh)
            else:
                renamed.append(bound)
        return PQuant(self.quantifier, tuple(renamed), self.sort, body.subst(name, replacement))

    def _canonical(self, env, depth) -> str:
        inner = dict(env)
        for offset, bound in enumerate(self.names):
            inner[bound] = depth + offset
        body = self.body._canonical(inner, depth + len(self.names))
        return f"Q({self.quantifier.value},{len(self.names)},{self.sort},{body})"

    # -- convenience -------------------------------------------------------

    def instantiate(self, term: Term) -> Prop:
        """Eliminate the outermost bound variable against ``term``.

        ``forall x,y:S, P`` instantiated at ``t`` gives ``forall y:S, P[t/x]``.
        """
        head, rest = self.names[0], self.names[1:]
        body = self.body.subst(head, term)
        if rest:
            return PQuant(self.quantifier, rest, self.sort, body)
        return body


def _fresh(prefix: str, taken: Set[str]) -> str:
    """A name based on ``prefix`` that is not in ``taken``."""
    if prefix not in taken:
        return prefix
    index = 2
    while f"{prefix}{index}" in taken:
        index += 1
    return f"{prefix}{index}"


def _term_canonical(term: Term, env: Dict[str, int]) -> str:
    if isinstance(term, TSym):
        if term.name in env:
            return f"#{env[term.name]}"
        return f"t:{term.name}"
    if isinstance(term, TApp):
        return f"@({_term_canonical(term.fun, env)},{_term_canonical(term.arg, env)})"
    raise PropError(f"unknown term node: {term!r}")


# ---------------------------------------------------------------------------
#  Parsing
# ---------------------------------------------------------------------------

# Order matters: "->" must be tried before "-".
_TOKEN = re.compile(
    r"""\s*(?:
          (?P<arrow>->)
        | (?P<punct>[()\[\],:~-])
        | (?P<name>[^\s()\[\],:~>-][^\s()\[\],:~-]*)
    )""",
    re.VERBOSE,
)

_KEYWORDS = {"forall", "exists", "true", "false"}


def _tokenize(text: str) -> list:
    tokens = []
    index = 0
    length = len(text)
    while index < length:
        if text[index].isspace():
            index += 1
            continue
        match = _TOKEN.match(text, index)
        if match is None or match.end() == index:
            raise PropError(f"cannot tokenize proposition near {text[index:]!r}")
        tokens.append(match.group(match.lastgroup))
        index = match.end()
    return tokens


class _Parser:
    """Recursive-descent parser for Fellowship's *printed* proposition syntax.

    Precedence follows the printer (core.ml), not the parser (parser.mly):
    ``-`` is looser than ``->``, negation binds tightest, and a quantifier body
    extends as far to the right as it can.
    """

    def __init__(self, text: str):
        self.text = text
        self.tokens = _tokenize(text)
        self.pos = 0

    # -- plumbing ----------------------------------------------------------

    def peek(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def next(self):
        token = self.peek()
        if token is None:
            raise PropError(f"unexpected end of proposition in {self.text!r}")
        self.pos += 1
        return token

    def expect(self, token: str) -> str:
        found = self.next()
        if found != token:
            raise PropError(f"expected {token!r}, found {found!r} in {self.text!r}")
        return found

    # -- entry -------------------------------------------------------------

    def parse_whole(self) -> Prop:
        prop = self.parse_prop()
        if self.peek() is not None:
            raise PropError(f"trailing input {self.peek()!r} in {self.text!r}")
        return prop

    # -- levels ------------------------------------------------------------

    def parse_prop(self) -> Prop:
        """Loosest level.  A quantifier here swallows everything to its right."""
        if self.peek() in ("forall", "exists"):
            return self.parse_quant()
        return self.parse_minus()

    def parse_quant(self) -> Prop:
        quantifier = Quantifier(self.next())
        names = [self.parse_name()]
        while self.peek() == ",":
            self.next()
            names.append(self.parse_name())
        self.expect(":")
        sort = self.parse_sort()
        self.expect(",")
        return PQuant(quantifier, tuple(names), sort, self.parse_prop())

    def parse_minus(self) -> Prop:
        """Subtraction: the loosest binary connective, left associative."""
        left = self.parse_imp()
        while self.peek() == "-":
            self.next()
            if self.peek() in ("forall", "exists"):
                # Older transcripts print `B-forall x:S,P` unparenthesised.
                return PBin(left, BinOp.MINUS, self.parse_quant())
            left = PBin(left, BinOp.MINUS, self.parse_imp())
        return left

    def parse_imp(self) -> Prop:
        """Implication: right associative, tighter than subtraction."""
        left = self.parse_neg()
        if self.peek() == "->":
            self.next()
            if self.peek() in ("forall", "exists"):
                return PBin(left, BinOp.IMP, self.parse_quant())
            return PBin(left, BinOp.IMP, self.parse_imp())
        return left

    def parse_neg(self) -> Prop:
        if self.peek() == "~":
            self.next()
            if self.peek() in ("forall", "exists"):
                # core.ml prints ~forall x:A,Q x without parentheses.
                return PNeg(self.parse_quant())
            return PNeg(self.parse_neg())
        return self.parse_application()

    def parse_application(self) -> Prop:
        prop = self.parse_atom()
        while True:
            token = self.peek()
            if token is None or token in {"->", "-", ",", ":", ")", "]", "~"}:
                break
            if token == "(":
                # After a predicate head, a parenthesised group is a term.
                self.next()
                prop = PApp(prop, self.parse_term())
                self.expect(")")
                continue
            if token == "[":
                # Tolerate the bracketed input spelling as well as the printed
                # juxtaposition, so a hand-written proposition also parses.
                self.next()
                prop = PApp(prop, self.parse_term())
                self.expect("]")
                continue
            prop = PApp(prop, TSym(self.parse_name()))
        return prop

    def parse_atom(self) -> Prop:
        token = self.next()
        if token == "true":
            return PTrue()
        if token == "false":
            return PFalse()
        if token == "(":
            inner = self.parse_prop()
            self.expect(")")
            return inner
        if token in {"->", "-", "~", ")", ",", ":", "[", "]"}:
            raise PropError(f"expected a proposition, found {token!r} in {self.text!r}")
        return PSym(token)

    # -- terms and sorts ---------------------------------------------------

    def parse_term(self) -> Term:
        term = self.parse_term_atom()
        while True:
            token = self.peek()
            if token is None or token in {")", "]", ",", ":", "->", "-", "~"}:
                break
            term = TApp(term, self.parse_term_atom())
        return term

    def parse_term_atom(self) -> Term:
        if self.peek() == "(":
            self.next()
            inner = self.parse_term()
            self.expect(")")
            return inner
        return TSym(self.parse_name())

    def parse_sort(self) -> Sort:
        left = self.parse_sort_atom()
        if self.peek() == "->":
            self.next()
            return SArr(left, self.parse_sort())
        return left

    def parse_sort_atom(self) -> Sort:
        if self.peek() == "(":
            self.next()
            inner = self.parse_sort()
            self.expect(")")
            return inner
        if self.peek() == "[":
            # `declare f : [termf -> form] -> form.` uses brackets to group.
            self.next()
            inner = self.parse_sort()
            self.expect("]")
            return inner
        name = self.parse_name()
        if name == "type":
            return SSet()
        if name == "bool":
            return SProp()
        return SSym(name)

    def parse_name(self) -> str:
        token = self.next()
        if token in {"->", "-", "~", "(", ")", "[", "]", ",", ":"}:
            raise PropError(f"expected a name, found {token!r} in {self.text!r}")
        return token


def parse_sort(text: str) -> Sort:
    """Parse a sort such as ``type``, ``bool`` or ``N->N->bool``."""
    parser = _Parser(text)
    sort = parser.parse_sort()
    if parser.peek() is not None:
        raise PropError(f"trailing input {parser.peek()!r} in sort {text!r}")
    return sort


def parse_term(text: str) -> Term:
    """Parse a first-order term such as ``O`` or ``S (S O)``."""
    parser = _Parser(text)
    term = parser.parse_term()
    if parser.peek() is not None:
        raise PropError(f"trailing input {parser.peek()!r} in term {text!r}")
    return term


def term_to_command(term: Term) -> str:
    """Render a first-order term for a Fellowship argument, e.g. ``[S O]``.

    Fellowship prints terms juxtaposed and parses them the same way inside
    brackets, so the only difference from ``str(term)`` is the brackets.
    """
    return f"[{term}]"
