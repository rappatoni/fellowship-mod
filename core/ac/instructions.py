from copy import deepcopy
import collections
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc, Goal, Laog, Deleg, Geled, ID, DI,
    LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO,
)
from core.ac.prop import term_to_command
from core.ac.prop_render import prop_to_command
from pres.gen import ProofTermGenerationVisitor

# Normalize logical symbols to the prover's ASCII syntax
ASCII_REPLACEMENTS = {
    '⊥': 'false',
    '¬': '~',
    '→': '->',
    '∧': '/\\',
    '∨': '\\/',
}
def to_ascii_logic(s: str) -> str:
    for k, v in ASCII_REPLACEMENTS.items():
        s = s.replace(k, v)
    return s

def fn(negated_prop: str):
    return negated_prop.replace("¬", "~")

#: Falsum reaches this module under three spellings: `false` is what the
#: prover prints and what a parsed proposition renders back to, `⊥` is what
#: `Argument._normalize_pt_to_unicode` rewrites it to, and `_F_` is the name
#: of the eliminator leaf.
_FALSUM_SPELLINGS = ('⊥', 'false', '_F_')


def is_falsum_prop(p: str) -> bool:
    if not isinstance(p, str):
        return False
    return p.strip() in _FALSUM_SPELLINGS

def is_primitive_negation_prop(p: str) -> bool:
    """Whether p is a primitive negation, as opposed to an arrow into falsum.

    Fellowship treats the two differently: eliminating ¬A consumes the falsum
    in the same step, while eliminating A->false leaves the falsum as an open
    goal.  A test that accepts both shapes cannot decide whether a falsum
    still needs discharging.
    """
    if not isinstance(p, str):
        return False
    ps = p.replace(" ", "")
    return ps.startswith("¬") or ps.startswith("~")

class InstructionsGenerationVisitor(ProofTermVisitor):  # TODO: make purely functional later
    def __init__(self, *, root_name: str | None = None):
        self.instructions = collections.deque('')
        self._autoclosed_falsum = set()  # id()s of ⊥ leaves already discharged by ¬-elim
        self.root_name = root_name

    def _is_synthetic_root_name(self, name: str | None) -> bool:
        return bool(name) and name == "thesis"

    def _collect_autoclosed_falsum(self, node: ProofTerm):
        # A ⊥ leaf closing an application chain normally needs its own elim to
        # discharge it.  The exception is negation elimination: eliminating a
        # primitive ¬A consumes the falsum in the same step, so emitting a
        # command for that ⊥ would leave the replay one elim ahead of the
        # prover.  Record those leaves so instruction generation skips them.
        if isinstance(node, (Mu, Mutilde)) and isinstance(getattr(node, "context", None), Cons):
            tail = node.context
            while isinstance(tail, Cons):
                tail = tail.context
            if isinstance(tail, (ID, DI)) and getattr(tail, "name", None) == "_F_":
                head = getattr(node, "term", None)
                if is_primitive_negation_prop(getattr(head, "prop", None)):
                    self._autoclosed_falsum.add(id(tail))
        for child in (getattr(node, 'term', None), getattr(node, 'context', None)):
            if child is not None:
                self._collect_autoclosed_falsum(child)

    def _node_pres(self, n: ProofTerm) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def return_instructions(self, proofterm):
        self.instructions.clear()
        # Pre-scan to find ⊥ leaves that ¬-elim already discharged
        self._autoclosed_falsum.clear()
        self._collect_autoclosed_falsum(proofterm)
        self.visit(proofterm)
        # Ensure ASCII-only syntax and no trailing dot (execute() appends '.')
        sanitized = []
        for instr in self.instructions:
            s = instr.strip()
            if s.endswith('.'):
                s = s[:-1].strip()
            s = to_ascii_logic(s)
            sanitized.append(s)
        # A trailing `next` can never do useful work: it exists to move the
        # prover to the goal the following instructions address, and nothing
        # follows.  When the last open site is also the last goal, Fellowship
        # refuses it ("There is only one goal, impossible to switch"), so
        # drop it here rather than let Argument.execute swallow the error
        # (tasks.org, aida-trailing-next-warning).
        if sanitized and sanitized[-1] == "next":
            sanitized.pop()
        self.instructions = collections.deque(sanitized)
        return self.instructions

    def visit_Mu(self, node: Mu):
        # Exact neg-elim scaffold on the right:
        # μ H1:¬A . < λ H2:A . μ H3:⊥ . < H2 || X > || H1 >
        if not self._is_synthetic_root_name(node.id.name) and isinstance(getattr(node, "term", None), Lamda):
            lam = node.term
            h2_name = getattr(getattr(lam, "di", None), "di", None)
            h2_name = getattr(h2_name, "name", None)
            inner = getattr(lam, "term", None)
            if h2_name and isinstance(inner, Mu) and is_falsum_prop(getattr(inner, "prop", "")):
                left = getattr(inner, "term", None)
                if isinstance(left, (ID, DI)) and getattr(left, "name", None) == h2_name:
                    # Pattern matched: visit X first, then place elim at the front
                    x = getattr(inner, "context", None)
                    if x is not None:
                        self.visit(x)
                    self.instructions.appendleft(f"elim {node.id.name}")
                    return node
        # Default traversal and instruction
        node = super().visit_Mu(node)
        if self._is_synthetic_root_name(node.id.name):
            return node
        if node.contr:
            self.instructions.appendleft(f"cut ({prop_to_command(fn(node.contr))}) {node.id.name}.")
            return node
        raise Exception(f"Could not identify cut proposition for Mu node {self._node_pres(node)}")

    def _is_negation_elim_wrapper(self, node: Mutilde) -> bool:
        # Fellowship encodes context-side negation elimination as
        #   μ′ H:¬A . < H || chain*_F_ >
        # The wrapper is representation rather than a cut anyone performed.
        if self._is_synthetic_root_name(node.di.name):
            return False
        left = getattr(node, "term", None)
        right = getattr(node, "context", None)
        if not isinstance(left, (ID, DI)) or getattr(left, "name", None) != node.di.name:
            return False
        if not isinstance(right, Cons):
            return False
        if not is_primitive_negation_prop(getattr(node, "prop", None)):
            return False
        tail = right
        while isinstance(tail, Cons):
            tail = tail.context
        return isinstance(tail, (ID, DI)) and getattr(tail, "name", None) == "_F_"

    def visit_Mutilde(self, node: Mutilde):
        if self._is_negation_elim_wrapper(node):
            # The chain already supplies the elim; emit nothing for the
            # wrapper and skip its self-reference.
            self.visit(node.context)
            return node
        node = super().visit_Mutilde(node)
        if self._is_synthetic_root_name(node.di.name):
            return node
        if node.contr:
            self.instructions.appendleft(f"cut ({prop_to_command(fn(node.contr))}) {node.di.name}.")
            return node
        raise Exception(f"Could not identify cut proposition for Mutilde node {self._node_pres(node)}")

    def visit_Lamda(self, node: Lamda):
        node = super().visit_Lamda(node)
        name = getattr(getattr(node, "di", None), "di", None)
        name = getattr(name, "name", None)
        if name:
            self.instructions.appendleft(f'elim {name}.')
        else:
            raise Exception(f"Missing hypothesis name for Lamda node {self._node_pres(node)}")
        return node

    def visit_Admal(self, node: Admal):
        # Mirror Lamda: emit named elim for context lambda
        node = super().visit_Admal(node)
        name = getattr(getattr(node, "id", None), "id", None)
        name = getattr(name, "name", None)
        if name:
            self.instructions.appendleft(f'elim {name}.')
        else:
            raise Exception(f"Missing variable name for Admal node {self._node_pres(node)}")
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        self.instructions.appendleft(f'elim.')
        return node

    def visit_Sonc(self, node: Sonc):
        # Mirror Cons: emit plain elim for context*term
        node = super().visit_Sonc(node)
        self.instructions.appendleft('elim.')
        return node

    # -- first-order eliminations ------------------------------------------
    #
    # Each first-order node came from one `elim` (tactics.ml:502-607).  The two
    # that bind a variable name it; the two that supply a witness bracket it.
    #
    # The bracketing is not cosmetic.  Fellowship *prints* a first-order term
    # juxtaposed -- `Even O`, `S (S O)` -- but only *accepts* it bracketed, so
    # the witness has to be re-serialised on the way back out.

    def visit_LamdaFO(self, node: LamdaFO):
        """Universal introduction: `elim` naming the fresh eigenvariable."""
        node = super().visit_LamdaFO(node)
        self.instructions.appendleft(f'elim {node.var}.')
        return node

    def visit_DestructTermsPairFO(self, node: DestructTermsPairFO):
        """Existential elimination: likewise names its variable."""
        node = super().visit_DestructTermsPairFO(node)
        self.instructions.appendleft(f'elim {node.var}.')
        return node

    def visit_ConsFO(self, node: ConsFO):
        """Universal instantiation: `elim [t]` with the witness."""
        node = super().visit_ConsFO(node)
        self.instructions.appendleft(f'elim {term_to_command(node.fo_term)}.')
        return node

    def visit_TermsPairFO(self, node: TermsPairFO):
        """Existential introduction: also `elim [t]`, with the witness."""
        node = super().visit_TermsPairFO(node)
        self.instructions.appendleft(f'elim {term_to_command(node.witness)}.')
        return node

    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        self.instructions.appendleft(f'next.')
        return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        self.instructions.appendleft(f'next.')
        return node

    def visit_Deleg(self, node: Deleg):
        node = super().visit_Deleg(node)
        self.instructions.appendleft(f'next.')
        self.instructions.appendleft(f'by default.')
        return node

    def visit_Geled(self, node: Geled):
        node = super().visit_Geled(node)
        self.instructions.appendleft(f'next.')
        self.instructions.appendleft(f'by default.')
        return node
    

    def _emit_unit_elim(self, node) -> None:
        # Discharge the ⊥ or ⊤ closing an application chain.  appendleft, not
        # append: instructions are built right-to-left, and visit_Cons reaches
        # the context before the term, so this lands immediately after the
        # chain's own commands rather than at the end of the whole replay.
        #
        # Only ⊥ has an exception: eliminating a primitive ¬A consumes it in
        # the same step.  There is no primitive co-negation — Neg is the only
        # unary connective — so every ⊤ needs its elim.
        if getattr(node, "flag", None) == "Falsum" and id(node) in self._autoclosed_falsum:
            return
        self.instructions.appendleft('elim.')

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        if node.name:
            if self._is_synthetic_root_name(node.name):
                return node
            if node.flag in ("Falsum", "Truth"):
                self._emit_unit_elim(node)
                return node
            elif getattr(node, "cites", None):
                # A citation regenerates as one (core/dc/cite.py).
                self.instructions.appendleft(f'cite {node.cites}.')
            else:
                self.instructions.appendleft(f'moxia {node.name}.')
        else:
            raise Exception(f"Axiom name missing for ID node {self._node_pres(node)}")
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        if node.name:
            if self._is_synthetic_root_name(node.name):
                return node
            if node.flag in ("Falsum", "Truth"):
                self._emit_unit_elim(node)
                return node
            elif getattr(node, "cites", None):
                # A citation regenerates as one (core/dc/cite.py).
                self.instructions.appendleft(f'cite {node.cites}.')
            else:
                self.instructions.appendleft(f'axiom {node.name}.')
        else:
            raise Exception(f"Axiom name missing for DI node {self._node_pres(node)}")
        return node

    def visit_unhandled(self, node):
        raise Exception(f"Unhandled node type: {type(node).__name__} {self._node_pres(node)}")
