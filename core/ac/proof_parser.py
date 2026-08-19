"""A deterministic parser for Fellowship proof terms.

This replaces a Lark/Earley grammar.  The reason is not taste: Earley resolves
ambiguity by silently picking one parse, and the proof-term syntax is genuinely
ambiguous when read as a context-free grammar.  ``λx:A.y*z`` has three parses
under the old grammar -- ``Admal(x:A, Cons(y,z))`` and
``Sonc(Admal(x:A,y), z)`` and ``Cons(Lamda(x:A,y), z)`` -- and which one you
got was an implementation detail of the parser.  Commit b3ac7ab fixed one
instance of this class of bug by changing Fellowship's printer; the rest are
fixed here by parsing deterministically.

Two properties of the concrete syntax make a hand-written parser
straightforward:

**Slots decide categories.**  Fellowship's syntax is not one grammar but two,
mutually recursive: terms and contexts.  ``X*Y`` in a context slot is
``Cons(term X, context Y)`` while in a term slot it is
``Sonc(context X, term Y)``.  A bare name is a ``DI`` in a term slot and an
``ID`` in a context slot.  ``(...)`` is an existential introduction in a term
slot and an existential elimination in a context slot.  Parsing with the
target category in hand removes the ambiguity rather than resolving it.

**Propositions contain no full stop.**  Fellowship identifiers are
``letter (letter | digit | '_')*`` (``lexer.mll:75-78``), so a proposition can
never contain ``.``.  That makes ``.`` a reliable terminator for the
annotation in ``μx:PROP.`` and ``λx:PROP.``, and lets annotations be handed to
:func:`core.ac.prop.Prop.parse` as opaque text instead of being re-specified
in a second grammar.

Binders extend maximally
------------------------

``λ`` and ``(x:S).`` swallow everything to their right, so neither is ever the
left operand of ``*``.  This is the reading that makes the printer's output
recoverable, and it matches how the quantifier body already behaves.  ``μ`` and
``μ'`` are self-delimiting -- they end at ``>`` -- so they *may* be a ``*``
operand, and are handled accordingly.

Ambiguity that remains
----------------------

``λx:ANN.t`` and ``h*c`` cannot be classified as first-order or propositional
from syntax alone; that needs the prover's declaration table.  This parser
emits the propositional reading and :mod:`core.ac.resolve` reclassifies.

``λh:R.b*c`` was once ambiguous between ``Cons(Lamda(h,b), c)`` and
``Admal(h, Cons(b,c))``.  It is no longer: a lambda binds tighter than ``*``,
so that spelling is the first, and the second is written ``λh:R.(b*c)``.
Fellowship's printer emits those parentheses -- it previously could not, which
is what made the reading unrecoverable rather than merely undeclared.
"""

from __future__ import annotations

import re

from core.ac.ast import (
    Admal,
    Cons,
    ConsFO,
    Context,
    DI,
    Deleg,
    DestructTermsPairFO,
    Geled,
    Goal,
    Hyp,
    ID,
    Laog,
    Lamda,
    Mu,
    Mutilde,
    ProofTerm,
    Pyh,
    Sonc,
    Term,
    TermsPairFO,
)
from core.ac.prop import (
    Prop,
    PropError,
    TApp,
    TSym,
    parse_sort,
    parse_term as parse_fo_term,
)

__all__ = ["ProofTermSyntaxError", "parse_proof_term"]


class ProofTermSyntaxError(ValueError):
    """Raised when a proof term cannot be parsed.

    ``offset`` is how far into the input the parser got.  When two readings
    are attempted and both fail, the one that got further is the more
    informative diagnosis, so it is the one reported.
    """

    def __init__(self, message: str, offset: int = -1):
        super().__init__(message)
        self.offset = offset


_NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_NUMBER = re.compile(r"[0-9]+(?:\.[0-9]+)*")

#: An annotation ends at the first of these that appears at parenthesis depth
#: zero.  Propositions contain none of them: `.` cannot occur in an identifier,
#: and the rest are proof-term punctuation.  A comma is deliberately absent --
#: `forall x,y:N,P x` puts commas inside an annotation, and no annotation is
#: ever followed by one.  `>` is special-cased below because of `->`.
_ANNOTATION_END = set(".*|<>?!")

#: Both spellings of each binder; the ASCII forms survive in older transcripts
#: and in hand-written `register` commands.
_MU = ("μ",)
_MUTILDE = ("μ'",)
_LAMBDA = ("λ", "\\")


def parse_proof_term(text: str, *, category: str | None = None) -> ProofTerm:
    """Parse a proof term.

    ``category`` forces the top-level reading to ``"term"`` or ``"context"``.
    Left unset, it is taken from the leading binder: ``μ'`` opens a context and
    anything else a term, which is how Fellowship prints theorems and
    antitheorems respectively.
    """
    parser = _Parser(text)
    if category is None:
        category = "context" if parser.looking_at(_MUTILDE) else "term"
    node = parser.parse_term() if category == "term" else parser.parse_context()
    parser.expect_end()
    return node


class _Parser:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0

    # -- scanning ----------------------------------------------------------

    def skip_space(self) -> None:
        while self.pos < len(self.text) and self.text[self.pos].isspace():
            self.pos += 1

    def looking_at(self, options) -> bool:
        self.skip_space()
        return any(self.text.startswith(option, self.pos) for option in options)

    def peek(self) -> str | None:
        self.skip_space()
        return self.text[self.pos] if self.pos < len(self.text) else None

    def eat(self, literal: str) -> bool:
        self.skip_space()
        if self.text.startswith(literal, self.pos):
            self.pos += len(literal)
            return True
        return False

    def expect(self, literal: str) -> None:
        if not self.eat(literal):
            raise self.error(f"expected {literal!r}")

    def expect_end(self) -> None:
        self.skip_space()
        if self.pos != len(self.text):
            raise self.error("unexpected trailing input")

    def error(self, message: str) -> ProofTermSyntaxError:
        return ProofTermSyntaxError(
            f"{message} at offset {self.pos} in {self.text!r} "
            f"(near {self.text[self.pos:self.pos + 24]!r})",
            offset=self.pos,
        )

    def match(self, pattern: re.Pattern) -> str | None:
        self.skip_space()
        found = pattern.match(self.text, self.pos)
        if found is None:
            return None
        self.pos = found.end()
        return found.group(0)

    def take_name(self) -> str:
        name = self.match(_NAME)
        if name is None:
            raise self.error("expected an identifier")
        return name

    # -- annotations -------------------------------------------------------

    def take_annotation_text(self, *, stop_at_close_paren: bool = False) -> str:
        """Raw annotation text, up to a terminator at parenthesis depth zero.

        Propositions may contain parentheses -- ``(true-A)-B`` -- so the scan
        tracks depth rather than stopping at the first ``)``.
        """
        self.skip_space()
        start = self.pos
        depth = 0
        while self.pos < len(self.text):
            char = self.text[self.pos]
            if char in "([":
                depth += 1
            elif char in ")]":
                if depth == 0 and stop_at_close_paren:
                    break
                depth -= 1
            elif depth == 0 and char in _ANNOTATION_END:
                # `>` closes a command, but it also ends an arrow.  Only the
                # former terminates an annotation.
                if char == ">" and self.pos > start and self.text[self.pos - 1] == "-":
                    self.pos += 1
                    continue
                break
            self.pos += 1
        text = self.text[start:self.pos].strip()
        if not text:
            raise self.error("expected a type annotation")
        return text

    def take_prop(self, **kwargs) -> str:
        """A binder annotation, validated and rendered canonically.

        Returned as a string rather than a :class:`Prop`: node annotations are
        still strings everywhere downstream, and threading structured
        propositions through the AST is a separate change.  Parsing here still
        earns something -- a malformed annotation fails at the parse, not
        later in a renderer.
        """
        text = self.take_annotation_text(**kwargs)
        try:
            return str(Prop.parse(text))
        except PropError as error:
            raise ProofTermSyntaxError(
                f"cannot read the proposition {text!r} in {self.text!r}: {error}"
            ) from error

    def take_sort(self):
        text = self.take_annotation_text(stop_at_close_paren=True)
        try:
            return parse_sort(text)
        except PropError as error:
            raise ProofTermSyntaxError(
                f"cannot read the sort {text!r} in {self.text!r}: {error}"
            ) from error

    def optional_prop(self):
        if self.peek() == ":":
            self.pos += 1
            return self.take_prop()
        return None

    # -- terms -------------------------------------------------------------

    def parse_term(self) -> Term:
        # X*Y in a term slot is Sonc(context X, term Y), so the left operand
        # is a context even though the slot is a term.  Which category a
        # lambda or a group belongs to is only known once a `*` is seen past
        # it, hence the single speculative attempt.
        save = self.pos
        deferred = None
        try:
            left = self.parse_tight("context")
            if self.peek() == "*":
                self.pos += 1
                return Sonc(left, self.parse_term())
        except ProofTermSyntaxError as error:
            deferred = error
        self.pos = save
        return self.finish(lambda: self.parse_tight("term"), deferred)

    def parse_context(self) -> Context:
        # An unparenthesised juxtaposition can only be a first-order term, so
        # this case is unambiguous and needs no speculation.
        fo_head = self.try_fo_head()
        if fo_head is not None:
            self.expect("*")
            return ConsFO(fo_head, self.parse_context())

        # X*Y in a context slot is Cons(term X, context Y).
        save = self.pos
        deferred = None
        try:
            left = self.parse_tight("term")
            if self.peek() == "*":
                self.pos += 1
                return Cons(left, self.parse_context())
        except ProofTermSyntaxError as error:
            deferred = error
        self.pos = save
        return self.finish(lambda: self.parse_tight("context"), deferred)

    # -- one head, binding tighter than `*` --------------------------------
    #
    # A lambda binds tighter than `*`: its body stops before one.  A lambda
    # *over* an application is written with parentheses, which is exactly what
    # Fellowship's printer emits for that case.  Groups, the two pair forms
    # and the command binders are all self-delimiting.

    def parse_tight(self, category: str):
        if self.looking_at(_LAMBDA):
            return self.parse_lambda(category)

        if self.peek() == "(":
            if self.looks_like_pair():
                if category != "term":
                    raise self.error("an existential introduction is a term")
                self.pos += 1
                witness = self.take_fo_term()
                self.expect(",")
                body = self.parse_term()
                self.expect(")")
                return TermsPairFO(witness, body)
            if self.looks_like_destructor():
                if category != "context":
                    raise self.error("an existential elimination is a context")
                self.pos += 1
                var = self.take_name()
                self.expect(":")
                sort = self.take_sort()
                self.expect(")")
                self.expect(".")
                return DestructTermsPairFO(var, sort, self.parse_context())
            self.expect("(")
            inner = self.parse_term() if category == "term" else self.parse_context()
            self.expect(")")
            return inner

        head, found = self.parse_delimited()
        return (
            self.as_term(head, found) if category == "term" else self.as_context(head, found)
        )

    def finish(self, attempt, deferred):
        """Run the fallback reading, reporting the better of the two failures."""
        try:
            return attempt()
        except ProofTermSyntaxError as error:
            # On a tie, prefer the speculative failure: it explains the
            # category mismatch, where the fallback only reports the
            # punctuation it tripped over.
            if deferred is not None and deferred.offset >= error.offset:
                raise deferred
            raise

    def parse_lambda(self, category: str):
        self.pos += 1  # the lambda itself
        name = self.take_name()
        self.expect(":")
        annotation = self.take_prop()
        self.expect(".")
        body = self.parse_tight(category)
        if category == "term":
            return Lamda(Hyp(DI(name), annotation), body)
        return Admal(Pyh(ID(name), annotation), body)

    def try_fo_head(self):
        """A juxtaposed first-order application, or None with nothing consumed."""
        save = self.pos
        self.skip_space()
        if self.match(_NAME) is None:
            self.pos = save
            return None
        name = self.text[save:self.pos].strip()
        if not self.starts_fo_argument():
            self.pos = save
            return None
        return self.continue_fo_application(TSym(name))

    # -- telling the three uses of `(` apart -------------------------------

    def matching_paren(self, start: int) -> int:
        depth = 0
        index = start
        while index < len(self.text):
            if self.text[index] == "(":
                depth += 1
            elif self.text[index] == ")":
                depth -= 1
                if depth == 0:
                    return index
            index += 1
        raise self.error("unclosed '('")

    def looks_like_destructor(self) -> bool:
        close = self.matching_paren(self.pos)
        inside = self.text[self.pos + 1 : close]
        after = self.text[close + 1 : close + 2]
        return after == "." and re.match(r"\s*[A-Za-z_][A-Za-z0-9_]*\s*:", inside) is not None

    def looks_like_pair(self) -> bool:
        close = self.matching_paren(self.pos)
        depth = 0
        for index in range(self.pos + 1, close):
            char = self.text[index]
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
            elif char == "," and depth == 0:
                return True
        return False

    # -- heads that do not extend to the right -----------------------------

    def parse_delimited(self):
        """Parse a self-delimiting head; return it with its category.

        The category is ``"term"``, ``"context"``, or ``"either"`` for a bare
        leaf, whose reading the caller's slot decides.
        """
        if self.looking_at(_MUTILDE):
            self.pos += 2
            return self.parse_command_binder(Mutilde, DI), "context"
        if self.looking_at(_MU):
            self.pos += 1
            return self.parse_command_binder(Mu, ID), "term"

        if self.peek() == "?":
            self.pos += 1
            return Goal(self.take_number(), self.optional_prop()), "term"
        if self.peek() == "!":
            self.pos += 1
            return Deleg(self.take_number(), self.optional_prop()), "term"

        number = self.match(_NUMBER)
        if number is not None:
            prop = self.optional_prop()
            if self.eat("?"):
                return Laog(number, prop), "context"
            if self.eat("!"):
                return Geled(number, prop), "context"
            raise self.error("expected '?' or '!' after a placeholder number")

        name = self.take_name()
        if self.peek() == ":":
            return (name, self.optional_prop()), "either"
        if self.starts_fo_argument():
            # Juxtaposition can only be a first-order application: proof terms
            # are never written that way.  `S (S O)*c` is a ConsFO whose head
            # needs no resolution, unlike the bare-name case.
            return self.continue_fo_application(TSym(name)), "fo"
        return (name, None), "either"

    def parse_command_binder(self, node_type, binder_type):
        name = self.take_name()
        self.expect(":")
        prop = self.take_prop()
        self.expect(".")
        self.expect("<")
        term = self.parse_term()
        self.expect("||")
        context = self.parse_context()
        self.expect(">")
        return node_type(binder_type(name), prop, term, context)

    def starts_fo_argument(self) -> bool:
        char = self.peek()
        return char is not None and (char == "(" or char.isalpha() or char == "_")

    def continue_fo_application(self, head):
        while self.starts_fo_argument():
            head = TApp(head, self.take_fo_argument())
        return head

    def take_fo_argument(self):
        if self.eat("("):
            inner = self.continue_fo_application(TSym(self.take_name()))
            self.expect(")")
            return inner
        return TSym(self.take_name())

    def take_number(self) -> str:
        number = self.match(_NUMBER)
        if number is None:
            raise self.error("expected a placeholder number")
        return number

    def take_fo_term(self):
        """The witness of an existential introduction, up to its comma."""
        self.skip_space()
        start = self.pos
        depth = 0
        while self.pos < len(self.text):
            char = self.text[self.pos]
            if char == "(":
                depth += 1
            elif char == ")":
                if depth == 0:
                    break
                depth -= 1
            elif char == "," and depth == 0:
                break
            self.pos += 1
        text = self.text[start:self.pos].strip()
        if not text:
            raise self.error("expected a first-order witness")
        try:
            return parse_fo_term(text)
        except PropError as error:
            raise ProofTermSyntaxError(
                f"cannot read the witness {text!r} in {self.text!r}: {error}"
            ) from error

    # -- turning a parsed head into the category the slot needs ------------

    def as_term(self, head, category) -> Term:
        if category == "either":
            name, prop = head
            return DI(name, prop)
        if category == "fo":
            raise self.error(
                "a first-order term can only head a universal instantiation, "
                "which is a context"
            )
        if category == "term":
            return head
        raise self.error(
            f"a {type(head).__name__} is a context and cannot stand where a term is expected"
        )

    def as_context(self, head, category) -> Context:
        if category == "either":
            name, prop = head
            return ID(name, prop)
        if category == "fo":
            raise self.error("a first-order term must be followed by '*'")
        if category == "context":
            return head
        raise self.error(
            f"a {type(head).__name__} is a term and cannot stand where a context is expected"
        )

