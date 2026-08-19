"""Reclassify the proof-term constructs Fellowship prints ambiguously.

Three of Fellowship's four first-order constructors are spelled exactly like a
propositional one (``core.ml:437-478``).  Two of those three survive into
AIDA's fragment:

===================  ==================  =============================
printed              propositional       first-order
===================  ==================  =============================
``λx:ANN.t``         ``Lamda``           ``LamdaFO``
``h*c``              ``Cons``            ``ConsFO``
===================  ==================  =============================

(The third, ``(t,u)``, would collide with conjunction introduction, which AIDA
does not support, so the parser reads it as ``TermsPairFO`` outright.  A
compound head such as ``S (S O)*c`` is likewise unambiguous, because a proof
term is never written by juxtaposition.)

Neither case is decidable from syntax: ``λx:A.t`` and ``λx:N.t`` differ only in
whether the annotation names a proposition or a sort, and ``ax*c`` and ``O*c``
differ only in whether the head is a proof or a first-order constant.  Both are
decidable from the prover's declaration table, which tags every name with its
kind, together with the binders in scope.

The parser emits the propositional reading, and this pass promotes it where the
evidence says otherwise.  With no declarations there is nothing to promote and
the term is returned unchanged, which is exactly the pre-first-order behaviour.
"""

from __future__ import annotations

from typing import Mapping

from core.ac.ast import (
    Admal,
    Cons,
    ConsFO,
    DI,
    DestructTermsPairFO,
    Hyp,
    ID,
    Lamda,
    LamdaFO,
    Mu,
    Mutilde,
    ProofTerm,
    Sonc,
    TermsPairFO,
)
from core.ac.prop import (
    PApp,
    PBin,
    PNeg,
    PQuant,
    PSym,
    Prop,
    PropError,
    SProp,
    SSet,
    TSym,
    parse_sort,
    sort_result,
)

__all__ = ["ResolutionError", "resolve"]


class ResolutionError(ValueError):
    """Raised when a construct cannot be classified from the declarations."""


def resolve(
    node: ProofTerm,
    declarations: Mapping[str, object] | None = None,
    *,
    env: Mapping[str, object] | None = None,
    strict: bool = True,
) -> ProofTerm:
    """Promote first-order constructs in ``node``, in place.

    ``declarations`` is the prover's table, whose values carry a ``kind`` of
    ``sort``, ``prop`` or ``moxia`` (:class:`core.ac.signature.Declaration`).
    ``env`` is the goal's first-order variable context, which names variables
    bound outside the fragment being resolved.

    With ``strict``, a name in an ambiguous position that the declarations do
    not account for raises rather than being guessed.  That matters most for
    ``register NAME : TYPE := TERM``, where the term is user-supplied and a
    silent misreading would produce a plausible but wrong tactic script.
    """
    if not declarations:
        return node
    scope = _Scope(declarations, fo=set(env or ()), strict=strict)
    return scope.visit(node)


class _Scope:
    """A resolving traversal that tracks which variables are first-order."""

    def __init__(self, declarations, fo, strict):
        self.declarations = declarations
        self.fo = set(fo)
        self.proof: set[str] = set()
        self.strict = strict

    # -- traversal ---------------------------------------------------------

    def visit(self, node):
        if node is None:
            return None

        if isinstance(node, Lamda):
            name = node.di.di.name
            if self._annotation_is_sort(node.di.prop):
                sort = self._as_sort(node.di.prop)
                with self._bind(fo=name):
                    return LamdaFO(name, sort, self.visit(node.term))
            with self._bind(proof=name):
                node.term = self.visit(node.term)
            return node

        if isinstance(node, Admal):
            with self._bind(proof=node.id.id.name):
                node.context = self.visit(node.context)
            return node

        if isinstance(node, Mu):
            with self._bind(proof=node.id.name):
                node.term = self.visit(node.term)
                node.context = self.visit(node.context)
            return node

        if isinstance(node, Mutilde):
            with self._bind(proof=node.di.name):
                node.term = self.visit(node.term)
                node.context = self.visit(node.context)
            return node

        if isinstance(node, LamdaFO):
            with self._bind(fo=node.var):
                node.term = self.visit(node.term)
            return node

        if isinstance(node, DestructTermsPairFO):
            with self._bind(fo=node.var):
                node.context = self.visit(node.context)
            return node

        if isinstance(node, Cons):
            head = node.term
            if isinstance(head, DI) and head.prop is None and self._is_first_order(head.name):
                return ConsFO(TSym(head.name), self.visit(node.context))
            node.term = self.visit(node.term)
            node.context = self.visit(node.context)
            return node

        # Everything else: recurse through whichever slots it has.
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, self.visit(child))
        return node

    # -- scope -------------------------------------------------------------

    def _bind(self, *, fo: str | None = None, proof: str | None = None):
        return _Binding(self, fo, proof)

    # -- classification ----------------------------------------------------

    def _declared_sort(self, name: str):
        """The declared sort of ``name``, or None if it is not a sort entry."""
        declaration = self.declarations.get(name)
        if declaration is None or getattr(declaration, "kind", None) != "sort":
            return None
        try:
            return parse_sort(str(declaration))
        except PropError:
            return None

    def _names_a_sort(self, name: str) -> bool:
        """Whether ``name`` is the name of a sort, as in ``declare N: type.``"""
        if name in ("bool", "type"):
            return True
        return isinstance(self._declared_sort(name), SSet)

    def _is_first_order(self, name: str) -> bool:
        """Whether ``name`` denotes a first-order term rather than a proof."""
        if name in self.fo:
            return True
        if name in self.proof:
            return False

        declaration = self.declarations.get(name)
        if declaration is None:
            if self.strict:
                raise ResolutionError(
                    f"{name!r} heads an application but is neither bound nor declared, "
                    f"so it cannot be told apart from a first-order term; declare it "
                    f"or pass strict=False to read it as a proof"
                )
            return False

        kind = getattr(declaration, "kind", None)
        if kind in ("prop", "moxia"):
            return False
        if kind == "sort":
            sort = self._declared_sort(name)
            # `O : N` and `S : N->N` are first-order; `A : bool` and
            # `P : N->bool` are propositions, and `N : type` is a sort.  Only
            # the first group can head a universal instantiation.
            return sort is not None and not isinstance(
                sort_result(sort), (SProp, SSet)
            )
        return False

    def _annotation_is_sort(self, annotation) -> bool:
        """Whether a binder's annotation is a sort rather than a proposition.

        Only positive evidence promotes: an annotation naming a declared sort,
        or an arrow built from such names.  Anything with a quantifier,
        application, negation or subtraction is a proposition outright.
        """
        if annotation is None:
            return False
        if isinstance(annotation, str):
            try:
                annotation = Prop.parse(annotation)
            except PropError:
                return False

        if isinstance(annotation, PSym):
            if self._names_a_sort(annotation.name):
                return True
            if annotation.name in self.declarations:
                return False
            if self.strict:
                raise ResolutionError(
                    f"the binder annotation {annotation.name!r} is neither a declared "
                    f"sort nor a declared proposition, so the binder cannot be told "
                    f"apart from a first-order one; declare it or pass strict=False"
                )
            return False

        if isinstance(annotation, PBin):
            # An arrow of sorts is a sort; anything else is a proposition.
            from core.ac.prop import BinOp

            if annotation.op is not BinOp.IMP:
                return False
            return self._annotation_is_sort(annotation.left) and self._annotation_is_sort(
                annotation.right
            )

        # PApp, PNeg, PQuant, PTrue, PFalse are propositions by construction.
        return False

    def _as_sort(self, annotation):
        return parse_sort(str(annotation))


class _Binding:
    """Adds a binder to the scope for the duration of a subtree."""

    def __init__(self, scope: _Scope, fo: str | None, proof: str | None):
        self.scope = scope
        self.fo = fo
        self.proof = proof
        self.had_fo = False
        self.had_proof = False

    def __enter__(self):
        if self.fo is not None:
            self.had_fo = self.fo in self.scope.fo
            self.scope.fo.add(self.fo)
            # A first-order binder shadows a proof variable of the same name.
            self.scope.proof.discard(self.fo)
        if self.proof is not None:
            self.had_proof = self.proof in self.scope.proof
            self.scope.proof.add(self.proof)
            self.scope.fo.discard(self.proof)
        return self

    def __exit__(self, *exc):
        if self.fo is not None and not self.had_fo:
            self.scope.fo.discard(self.fo)
        if self.proof is not None and not self.had_proof:
            self.scope.proof.discard(self.proof)
        return False
