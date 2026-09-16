"""The legacy reducer's shape predicate.  NOT an acceptance labelling.

``LegacyShapeClassifier.classify`` answers green / red / yellow from the
SHAPE of a term - "no attack scaffold present" / "attack scaffold present"
/ "open leaf" - and nothing else.  The M5 audit (2026-08-31, six-case
corpus, recorded in tasks.org under aida-renderers-grounded-labels) showed
it reads a supported argument as defeated and an unfilled obligation as
accepted, so it was retired from every presentation surface on
2026-09-16: acceptance questions are answered by core/comp/adf_label.py
and the `label` / `evaluate` commands.

It survives here for one consumer only: ``core.comp.reduce._is_red``, the
defeat test inside the legacy ArgumentTermReducer's support and defence
rules, which the reducer's own semantics depends on.  It goes when that
reducer goes (tasks.org, aida-legacy-reducer-findings).  Nothing else may
import this module.
"""

from copy import deepcopy
from typing import Optional
from pres.gen import ProofTermGenerationVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Admal, Sonc, Deleg, Geled, FirstOrderNotSupported, first_order_node


class LegacyShapeClassifier:
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self._memo_unattacked: dict[int, bool] = {}
        self._memo_color: dict[int, Optional[str]] = {}

    # utilities
    def _is_term_open(self, node: ProofTerm) -> bool:
        return isinstance(node, (Goal, Deleg))

    def _is_context_open(self, node: ProofTerm) -> bool:
        return isinstance(node, (Laog, Geled))

    def _node_pres(self, n: ProofTerm) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def _var_occurs(self, name: str, node: Optional[ProofTerm]) -> bool:
        """Return True iff variable `name` occurs (free w.r.t. outer binder) in node.
        Shadowing by λ/μ/μ′ with the same name stops descent."""
        if node is None:
            return False
        if isinstance(node, (ID, DI)):
            return node.name == name
        # stop under shadowing binders that re-bind the same name
        if isinstance(node, Lamda) and node.di.di.name == name:
            return False
        if isinstance(node, Admal) and node.id.id.name == name:
            return False
        if isinstance(node, Mu) and node.id.name == name:
            return False
        if isinstance(node, Mutilde) and node.di.name == name:
            return False
        # recurse
        for child in getattr(node, 'term', None), getattr(node, 'context', None):
            if child is not None and self._var_occurs(name, child):
                return True
        return False

    def _non_affine_goal_side(self, n: Mu) -> bool:
        """μ with an open term-side is non-affine for attacks iff id occurs in context."""
        return self._is_term_open(n.term) and self._var_occurs(n.id.name, n.context)

    def _non_affine_laog_side(self, n: Mutilde) -> bool:
        """μ′ with an open context-side is non-affine for attacks iff di occurs in term."""
        return self._is_context_open(n.context) and self._var_occurs(n.di.name, n.term)

    def _has_open(self, n: ProofTerm) -> bool:
        if self._is_term_open(n) or self._is_context_open(n):
            return True
        for child in getattr(n, 'term', None), getattr(n, 'context', None):
            if child is not None and self._has_open(child):
                return True
        return False

    def _unattacked(self, n: ProofTerm) -> bool:
        mid = id(n)
        hit = self._memo_unattacked.get(mid)
        if hit is not None:
            return hit
        if not self._has_open(n):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Lamda) and self._is_term_open(n.term):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Cons) and self._is_term_open(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Cons) and self._unattacked(n.term) and self._is_context_open(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Lamda) and self._unattacked(n.term):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Cons) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Admal) and self._is_context_open(n.context):
            self._memo_unattacked[mid] = True
            return True

        if isinstance(n, Sonc) and self._is_term_open(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Sonc) and self._unattacked(n.term) and self._is_context_open(n.context):
            self._memo_unattacked[mid] = True
            return True

        if isinstance(n, Admal) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True

        if isinstance(n, Sonc) and self._unattacked(n.context) and self._unattacked(n.term):
            self._memo_unattacked[mid] = True
            return True

        if isinstance(n, Mu) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Mutilde) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        # Non-affine binders do not constitute attacks on immediate open leaves.
        if isinstance(n, Mu) and self._non_affine_goal_side(n):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Mutilde) and self._non_affine_laog_side(n):
            self._memo_unattacked[mid] = True
            return True
        self._memo_unattacked[mid] = False
        return False

    def classify(self, n: ProofTerm) -> Optional[str]:
        mid = id(n)
        memo = self._memo_color.get(mid)
        if memo is not None:
            return memo
        found = first_order_node(n)
        if found is not None:
            # Checked before anything else: `_unattacked` answers True for a
            # node it does not recognise, so a first-order term would be
            # reported green -- accepted, without having been examined.
            raise FirstOrderNotSupported("Acceptance colouring", found)
        if self._is_term_open(n) or self._is_context_open(n):
            self._memo_color[mid] = "yellow"
            return "yellow"
        if self._unattacked(n):
            self._memo_color[mid] = "green"
            return "green"
        if isinstance(n, Lamda):
            c = self.classify(n.term)
            self._memo_color[mid] = c
            return c
        if isinstance(n, Cons):
            if self._is_term_open(n.term):
                c2 = self.classify(n.context)
                self._memo_color[mid] = c2
                return c2
            if self._is_context_open(n.context):
                c1 = self.classify(n.term)
                self._memo_color[mid] = c1
                return c1
            c1, c2 = self.classify(n.term), self.classify(n.context)
            if c1 == "green" and c2 == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c1 == "red" and c2 in {"green", "yellow"}) or (c2 == "red" and c1 in {"green", "yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c1 == "yellow" and c2 == "green") or (c1 == "green" and c2 == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            if c1 == "red" and c2 == "red":
                self._memo_color[mid] = "red"; return "red"
            if c1 == "yellow" and c2 == "yellow":
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Cons node {self._node_pres(n)}: "
                f"left={c1}, right={c2}"
            )
        if isinstance(n, Mu):
            if self._is_term_open(n.term):
                # Non-affine μ cannot attack the open term side (assumption is discharged)
                if self._non_affine_goal_side(n):
                    self._memo_color[mid] = "green"
                    return "green"
                c_c = self.classify(n.context)
                if c_c == "red":    self._memo_color[mid] = "green";  return "green"
                if c_c == "green":  self._memo_color[mid] = "red";    return "red"
                if c_c == "yellow": self._memo_color[mid] = "yellow"; return "yellow"
                raise ValueError(
                    f"Acceptance coloring incomplete for Mu node {self._node_pres(n)}: "
                    f"term is open; context_color={c_c}"
                )
            c_t = self.classify(n.term)
            c_c = self.classify(n.context)
            if c_t == "green" and c_c == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c_t == "red" and c_c in {"green","yellow"}) or (c_c == "red" and c_t in {"green","yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c_t == "yellow" and c_c == "green") or (c_t == "green" and c_c == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            if c_t == "red" and c_c == "red":
                self._memo_color[mid] = "red"; return "red"
            if c_t == "yellow" and c_c == "yellow":
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Mu node {self._node_pres(n)}: "
                f"term_color={c_t}, context_color={c_c}"
            )
        if isinstance(n, Mutilde):
            if self._is_context_open(n.context):
                # Non-affine μ′ cannot attack the open context side (assumption is discharged)
                if self._non_affine_laog_side(n):
                    self._memo_color[mid] = "green"
                    return "green"
                c_t = self.classify(n.term)
                if c_t == "red":    self._memo_color[mid] = "green";  return "green"
                if c_t == "green":  self._memo_color[mid] = "red";    return "red"
                if c_t == "yellow": self._memo_color[mid] = "yellow"; return "yellow"
                raise ValueError(
                    f"Acceptance coloring incomplete for Mutilde node {self._node_pres(n)}: "
                    f"context is open; term_color={c_t}"
                )
            c_t = self.classify(n.term)
            c_c = self.classify(n.context)
            if c_t == "green" and c_c == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c_t == "red" and c_c in {"green","yellow"}) or (c_c == "red" and c_t in {"green","yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c_t == "yellow" and c_c == "green") or (c_t == "green" and c_c == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            if c_t == "red" and c_c == "red":
                self._memo_color[mid] = "red"; return "red"
            if c_t == "yellow" and c_c == "yellow":
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Mutilde node {self._node_pres(n)}: "
                f"term_color={c_t}, context_color={c_c}"
            )
        if isinstance(n, Admal):
            c = self.classify(n.context)
            self._memo_color[mid] = c
            return c

        if isinstance(n, Sonc):
            # mirror Cons but with (context*term) orientation
            if self._is_term_open(n.term):
                c2 = self.classify(n.context)
                self._memo_color[mid] = c2
                return c2
            if self._is_context_open(n.context):
                c1 = self.classify(n.term)
                self._memo_color[mid] = c1
                return c1
            c1, c2 = self.classify(n.term), self.classify(n.context)
            if c1 == "green" and c2 == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c1 == "red" and c2 in {"green", "yellow"}) or (c2 == "red" and c1 in {"green", "yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c1 == "yellow" and c2 == "green") or (c1 == "green" and c2 == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            if c1 == "red" and c2 == "red":
                self._memo_color[mid] = "red"; return "red"
            if c1 == "yellow" and c2 == "yellow":
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Sonc node {self._node_pres(n)}: "
                f"term_color={c1}, context_color={c2}"
            )
        raise ValueError(
            f"Acceptance coloring incomplete for leaf {type(n).__name__} {self._node_pres(n)}"
        )

