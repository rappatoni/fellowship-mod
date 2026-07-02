from copy import deepcopy
from dataclasses import dataclass
from typing import Optional
from pres.gen import ProofTermGenerationVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Admal, Sonc


@dataclass(frozen=True)
class DebateNodeLabel:
    status: str
    inherited_status: Optional[str] = None
    open_site: Optional[str] = None


class DebateTermLabeller:
    """Label debate terms via a simple two-pass attribute grammar.

    The current implementation separates the bottom-up computation of a node's
    local acceptance status from the top-down propagation of inherited status.
    It intentionally stays close to the existing AcceptanceColoringVisitor
    semantics so downstream renderers can migrate incrementally.
    """

    VALID_STATUSES = {"green", "red", "yellow"}

    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self._synth: dict[int, str] = {}
        self._inherited: dict[int, Optional[str]] = {}
        self._open_site: dict[int, Optional[str]] = {}

    def label(self, node: ProofTerm) -> dict[int, DebateNodeLabel]:
        self._synth.clear()
        self._inherited.clear()
        self._open_site.clear()
        self._compute_synthesized(node)
        self._propagate_inherited(node, None)
        return {
            nid: DebateNodeLabel(
                status=status,
                inherited_status=self._inherited.get(nid),
                open_site=self._open_site.get(nid),
            )
            for nid, status in self._synth.items()
        }

    def status_of(self, node: ProofTerm) -> str:
        nid = id(node)
        if nid not in self._synth:
            self.label(node)
        return self._synth[nid]

    def inherited_of(self, node: ProofTerm) -> Optional[str]:
        nid = id(node)
        if nid not in self._synth:
            self.label(node)
        return self._inherited.get(nid)

    def label_of(self, node: ProofTerm) -> DebateNodeLabel:
        nid = id(node)
        if nid not in self._synth:
            self.label(node)
        return DebateNodeLabel(
            status=self._synth[nid],
            inherited_status=self._inherited.get(nid),
            open_site=self._open_site.get(nid),
        )

    def _combine_pair(self, left: str, right: str, node: ProofTerm) -> str:
        if left == "green" and right == "green":
            return "green"
        if (left == "red" and right in {"green", "yellow", "red"}) or (
            right == "red" and left in {"green", "yellow", "red"}
        ):
            return "red"
        if (left == "yellow" and right == "green") or (left == "green" and right == "yellow"):
            return "yellow"
        if left == "yellow" and right == "yellow":
            return "yellow"
        raise ValueError(
            f"DebateTermLabeller incomplete for {type(node).__name__} {self._node_pres(node)}: "
            f"left={left}, right={right}"
        )

    def _invert_open(self, other: str) -> str:
        if other == "red":
            return "green"
        if other == "green":
            return "red"
        return "yellow"

    def _compute_synthesized(self, node: Optional[ProofTerm]) -> Optional[str]:
        if node is None:
            return None
        nid = id(node)
        if nid in self._synth:
            return self._synth[nid]

        if isinstance(node, Goal):
            status = "yellow"
            self._open_site[nid] = "term"
        elif isinstance(node, Laog):
            status = "yellow"
            self._open_site[nid] = "context"
        elif isinstance(node, (ID, DI)):
            status = "green"
            self._open_site[nid] = None
        elif isinstance(node, Lamda):
            status = self._compute_synthesized(node.term)
            self._open_site[nid] = self._open_site.get(id(node.term))
        elif isinstance(node, Admal):
            status = self._compute_synthesized(node.context)
            self._open_site[nid] = self._open_site.get(id(node.context))
        elif isinstance(node, Cons):
            if isinstance(node.term, Goal):
                status = self._compute_synthesized(node.context)
                self._open_site[nid] = "term"
            elif isinstance(node.context, Laog):
                status = self._compute_synthesized(node.term)
                self._open_site[nid] = "context"
            else:
                left = self._compute_synthesized(node.term)
                right = self._compute_synthesized(node.context)
                status = self._combine_pair(left, right, node)
                self._open_site[nid] = None
        elif isinstance(node, Sonc):
            if isinstance(node.term, Goal):
                status = self._compute_synthesized(node.context)
                self._open_site[nid] = "term"
            elif isinstance(node.context, Laog):
                status = self._compute_synthesized(node.term)
                self._open_site[nid] = "context"
            else:
                left = self._compute_synthesized(node.term)
                right = self._compute_synthesized(node.context)
                status = self._combine_pair(left, right, node)
                self._open_site[nid] = None
        elif isinstance(node, Mu):
            if isinstance(node.term, Goal):
                other = self._compute_synthesized(node.context)
                status = self._invert_open(other)
                self._open_site[nid] = "term"
            else:
                left = self._compute_synthesized(node.term)
                right = self._compute_synthesized(node.context)
                status = self._combine_pair(left, right, node)
                self._open_site[nid] = None
        elif isinstance(node, Mutilde):
            if isinstance(node.context, Laog):
                other = self._compute_synthesized(node.term)
                status = self._invert_open(other)
                self._open_site[nid] = "context"
            else:
                left = self._compute_synthesized(node.term)
                right = self._compute_synthesized(node.context)
                status = self._combine_pair(left, right, node)
                self._open_site[nid] = None
        else:
            status = "green"
            self._open_site[nid] = None

        self._synth[nid] = status
        return status

    def _propagate_inherited(self, node: Optional[ProofTerm], inherited_status: Optional[str]) -> None:
        if node is None:
            return
        nid = id(node)
        self._inherited[nid] = inherited_status

        child_term = getattr(node, "term", None)
        child_context = getattr(node, "context", None)

        if isinstance(node, Lamda):
            self._propagate_inherited(child_term, self._synth[nid])
            return
        if isinstance(node, Admal):
            self._propagate_inherited(child_context, self._synth[nid])
            return
        if isinstance(node, Cons):
            if child_term is not None:
                self._propagate_inherited(child_term, self._synth[nid] if isinstance(child_term, Goal) else inherited_status)
            if child_context is not None:
                self._propagate_inherited(child_context, self._synth[nid] if isinstance(child_context, Laog) else inherited_status)
            return
        if isinstance(node, Sonc):
            if child_context is not None:
                self._propagate_inherited(child_context, self._synth[nid] if isinstance(child_context, Laog) else inherited_status)
            if child_term is not None:
                self._propagate_inherited(child_term, self._synth[nid] if isinstance(child_term, Goal) else inherited_status)
            return
        if isinstance(node, Mu):
            if child_term is not None:
                self._propagate_inherited(child_term, self._synth[nid] if isinstance(child_term, Goal) else inherited_status)
            if child_context is not None:
                self._propagate_inherited(child_context, inherited_status)
            return
        if isinstance(node, Mutilde):
            if child_term is not None:
                self._propagate_inherited(child_term, inherited_status)
            if child_context is not None:
                self._propagate_inherited(child_context, self._synth[nid] if isinstance(child_context, Laog) else inherited_status)
            return

        if child_term is not None:
            self._propagate_inherited(child_term, inherited_status)
        if child_context is not None:
            self._propagate_inherited(child_context, inherited_status)

    def _node_pres(self, n: ProofTerm) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))


class AcceptanceColoringVisitor:
    ANSI = {
        "green": "\x1b[32m",
        "red": "\x1b[31m",
        "yellow": "\x1b[33m",
        "reset": "\x1b[0m",
    }
    def __init__(self, verbose: bool = False):
        super().__init__()
        self.verbose = verbose
        self._memo_unattacked: dict[int, bool] = {}
        self._memo_color: dict[int, Optional[str]] = {}

    # public API
    def render(self, node: ProofTerm) -> str:
        return self.visit(node)

    # utilities
    def _wrap(self, color: Optional[str], s: str) -> str:
        if not color:
            return s
        return f"{self.ANSI[color]}{s}{self.ANSI['reset']}"

    def _ansi(self, color: Optional[str]) -> tuple[str, str]:
        if not color:
            return "", ""
        return self.ANSI[color], self.ANSI["reset"]

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
        """μ with Goal on term-side is non-affine for attacks iff id occurs in context."""
        return isinstance(n.term, Goal) and self._var_occurs(n.id.name, n.context)

    def _non_affine_laog_side(self, n: Mutilde) -> bool:
        """μ′ with Laog on context-side is non-affine for attacks iff di occurs in term."""
        return isinstance(n.context, Laog) and self._var_occurs(n.di.name, n.term)

    def _has_open(self, n: ProofTerm) -> bool:
        if isinstance(n, (Goal, Laog)):
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
        if isinstance(n, Lamda) and isinstance(n.term, Goal):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Cons) and isinstance(n.term, Goal) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Cons) and self._unattacked(n.term) and isinstance(n.context, Laog):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Lamda) and self._unattacked(n.term):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Cons) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Admal) and isinstance(n.context, Laog):
            self._memo_unattacked[mid] = True
            return True

        if isinstance(n, Sonc) and isinstance(n.term, Goal) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Sonc) and self._unattacked(n.term) and isinstance(n.context, Laog):
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
        # Non-affine binders do not constitute attacks on immediate Goal/Laog
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
        if self._unattacked(n):
            self._memo_color[mid] = "green"
            return "green"
        if isinstance(n, Lamda):
            c = self.classify(n.term)
            self._memo_color[mid] = c
            return c
        if isinstance(n, Cons):
            if isinstance(n.term, Goal):
                c2 = self.classify(n.context)
                self._memo_color[mid] = c2
                return c2
            if isinstance(n.context, Laog):
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
            if isinstance(n.term, Goal):
                # Non-affine μ cannot attack the Goal side (assumption is discharged)
                if self._non_affine_goal_side(n):
                    self._memo_color[mid] = "green"
                    return "green"
                c_c = self.classify(n.context)
                if c_c == "red":    self._memo_color[mid] = "green";  return "green"
                if c_c == "green":  self._memo_color[mid] = "red";    return "red"
                if c_c == "yellow": self._memo_color[mid] = "yellow"; return "yellow"
                raise ValueError(
                    f"Acceptance coloring incomplete for Mu node {self._node_pres(n)}: "
                    f"term is Goal; context_color={c_c}"
                )
            c_t = self.classify(n.term)
            c_c = self.classify(n.context)
            if c_t == "green" and c_c == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c_t == "red" and c_c in {"green","yellow"}) or (c_c == "red" and c_t in {"green","yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c_t == "yellow" and c_c == "green") or (c_t == "green" and c_c == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Mu node {self._node_pres(n)}: "
                f"term_color={c_t}, context_color={c_c}"
            )
        if isinstance(n, Mutilde):
            if isinstance(n.context, Laog):
                # Non-affine μ′ cannot attack the Laog side (assumption is discharged)
                if self._non_affine_laog_side(n):
                    self._memo_color[mid] = "green"
                    return "green"
                c_t = self.classify(n.term)
                if c_t == "red":    self._memo_color[mid] = "green";  return "green"
                if c_t == "green":  self._memo_color[mid] = "red";    return "red"
                if c_t == "yellow": self._memo_color[mid] = "yellow"; return "yellow"
                raise ValueError(
                    f"Acceptance coloring incomplete for Mutilde node {self._node_pres(n)}: "
                    f"context is Laog; term_color={c_t}"
                )
            c_t = self.classify(n.term)
            c_c = self.classify(n.context)
            if c_t == "green" and c_c == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c_t == "red" and c_c in {"green","yellow"}) or (c_c == "red" and c_t in {"green","yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c_t == "yellow" and c_c == "green") or (c_t == "green" and c_c == "yellow"):
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
            if isinstance(n.term, Goal):
                c2 = self.classify(n.context)
                self._memo_color[mid] = c2
                return c2
            if isinstance(n.context, Laog):
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

    def visit(self, node):
        if node is None:
            return ""
        if isinstance(node, Mu):
            return self.visit_Mu(node)
        if isinstance(node, Mutilde):
            return self.visit_Mutilde(node)
        if isinstance(node, Lamda):
            return self.visit_Lamda(node)
        if isinstance(node, Admal):
            return self.visit_Admal(node)
        if isinstance(node, Cons):
            return self.visit_Cons(node)
        if isinstance(node, Sonc):
            return self.visit_Sonc(node)
        if isinstance(node, Goal):
            return self.visit_Goal(node)
        if isinstance(node, Laog):
            return self.visit_Laog(node)
        if isinstance(node, ID):
            return self.visit_ID(node)
        if isinstance(node, DI):
            return self.visit_DI(node)
        return self.visit_unhandled(node)

    def visit_Mu(self, node: Mu):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        t_str = self.visit(node.term)
        c_str = self.visit(node.context)
        if isinstance(node.term, Goal):
            t_str = f"{openP}{t_str}{reset}"
        if isinstance(node.context, Laog):
            c_str = f"{openP}{c_str}{reset}"
        s = (
            f"{openP}μ{node.id.name}:{node.prop}.<"
            f"{reset}{t_str}"
            f"{openP}||"
            f"{reset}{c_str}"
            f"{openP}>"
            f"{self.ANSI['reset']}"
        )
        return s

    def visit_Mutilde(self, node: Mutilde):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        t_str = self.visit(node.term)
        c_str = self.visit(node.context)
        if isinstance(node.term, Goal):
            t_str = f"{openP}{t_str}{reset}"
        if isinstance(node.context, Laog):
            c_str = f"{openP}{c_str}{reset}"
        s = (
            f"{openP}μ'{node.di.name}:{node.prop}.<"
            f"{reset}{t_str}"
            f"{openP}||"
            f"{reset}{c_str}"
            f"{openP}>"
            f"{self.ANSI['reset']}"
        )
        return s

    def visit_Lamda(self, node: Lamda):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        inner = self.visit(node.term)
        if isinstance(node.term, Goal):
            inner = f"{openP}{inner}{reset}"
        s = (
            f"{openP}λ{node.di.di.name}:{node.di.prop}."
            f"{reset}{inner}"
            f"{openP}{self.ANSI['reset']}"
        )
        return s

    def visit_Admal(self, node: Admal):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        inner = self.visit(node.context)
        if isinstance(node.context, Laog):
            inner = f"{openP}{inner}{reset}"
        s = (
            f"{openP}λ{node.id.id.name}:{node.id.prop}."
            f"{reset}{inner}"
            f"{openP}{self.ANSI['reset']}"
        )
        return s

    def visit_Cons(self, node: Cons):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        left  = self.visit(node.term)
        right = self.visit(node.context)
        if isinstance(node.term, Goal):
            left = f"{openP}{left}{reset}"
        if isinstance(node.context, Laog):
            right = f"{openP}{right}{reset}"
        s = f"{left}{openP}*{self.ANSI['reset']}{right}"
        return s

    def visit_Sonc(self, node: Sonc):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        left  = self.visit(node.context)
        right = self.visit(node.term)
        if isinstance(node.term, Goal):
            right = f"{openP}{right}{reset}"
        if isinstance(node.context, Laog):
            left = f"{openP}{left}{reset}"
        s = f"{left}{openP}*{self.ANSI['reset']}{right}"
        return s

    def visit_Goal(self, node: Goal):
        return f"{node.number}:{node.prop}"

    def visit_Laog(self, node: Laog):
        return f"{node.number}:{node.prop}"

    def visit_ID(self, node: ID):
        s = f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
        return self._wrap(self.classify(node), s)

    def visit_DI(self, node: DI):
        s = f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
        return self._wrap(self.classify(node), s)

    def visit_unhandled(self, node):
        return self._wrap(self.classify(node), repr(node))

def pretty_colored_proof_term(pt: ProofTerm, verbose: bool = False) -> str:
    return AcceptanceColoringVisitor(verbose=verbose).render(pt)


def label_debate_term(pt: ProofTerm, verbose: bool = False) -> dict[int, DebateNodeLabel]:
    return DebateTermLabeller(verbose=verbose).label(pt)
