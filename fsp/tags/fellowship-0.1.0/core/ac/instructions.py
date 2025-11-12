from copy import deepcopy
import collections
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI
from pres.gen import ProofTermGenerationVisitor

# Normalize logical symbols to the prover's ASCII syntax
ASCII_REPLACEMENTS = {
    '⊥': '_F_',
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

def is_falsum_prop(p: str) -> bool:
    if not isinstance(p, str):
        return False
    return p.strip() in ('⊥', '_F_')

def is_negation_prop(p: str) -> bool:
    if not isinstance(p, str):
        return False
    ps = p.replace(" ", "")
    if ps.startswith("¬") or ps.startswith("~"):
        return True
    if "->" in ps:
        rhs = ps.split("->", 1)[1]
        return rhs in ("⊥", "_F_")
    return False

class InstructionsGenerationVisitor(ProofTermVisitor):  # TODO: make purely functional later
    def __init__(self):
        self.instructions = collections.deque('')
        self._neg_bound_names = set()  # names bound by λ in ¬-elim scaffolds

    def _collect_neg_bound_names(self, node: ProofTerm):
        # Detect λ H:A . μ _:⊥ . < H || … > scaffolds and record H
        if isinstance(node, Lamda):
            # binder name
            name = getattr(getattr(node, "di", None), "di", None)
            name = getattr(name, "name", None)
            body = getattr(node, "term", None)
            if name and isinstance(body, Mu) and is_falsum_prop(getattr(body, "prop", "")):
                left = getattr(body, "term", None)  # μ’s term (left of ||)
                if isinstance(left, (ID, DI)) and getattr(left, "name", None) == name:
                    self._neg_bound_names.add(name)
        # Recurse into children
        if isinstance(node, Mu) or isinstance(node, Mutilde) or isinstance(node, Cons):
            for child in (getattr(node, "term", None), getattr(node, "context", None)):
                if child is not None:
                    self._collect_neg_bound_names(child)
        elif isinstance(node, Lamda):
            child = getattr(node, "term", None)
            if child is not None:
                self._collect_neg_bound_names(child)

    def _node_pres(self, n: ProofTerm) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def return_instructions(self, proofterm):
        self.instructions.clear()
        # Pre-scan to find bound-negation lambda binders (H2 in ¬-elim)
        self._neg_bound_names.clear()
        self._collect_neg_bound_names(proofterm)
        self.visit(proofterm)
        # Ensure ASCII-only syntax and no trailing dot (execute() appends '.')
        sanitized = []
        for instr in self.instructions:
            s = instr.strip()
            if s.endswith('.'):
                s = s[:-1].strip()
            s = to_ascii_logic(s)
            sanitized.append(s)
        self.instructions = collections.deque(sanitized)
        return self.instructions

    def visit_Mu(self, node: Mu):
        # Exact neg-elim scaffold on the right:
        # μ H1:¬A . < λ H2:A . μ H3:⊥ . < H2 || X > || H1 >
        if node.id.name != 'thesis' and isinstance(getattr(node, "term", None), Lamda):
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
        if node.id.name == 'thesis':
            return node
        if is_falsum_prop(getattr(node, "prop", "")):
            return node
        if node.contr:
            self.instructions.appendleft(f"cut ({fn(node.contr)}) {node.id.name}.")
            return node
        raise Exception(f"Could not identify cut proposition for Mu node {self._node_pres(node)}")

    def visit_Mutilde(self, node: Mutilde):
        # Exact neg-elim scaffold on the left (unique orientation in our AST/printing):
        # μ′ H1:¬A . < H1 || adapter*_F_ >
        # Left of ||: DI named H1; Right of ||: Cons with falsum on term side and adapter on context side.
        left = getattr(node, "term", None)       # H1 is on the left side of ||
        right = getattr(node, "context", None)   # adapter*_F_ sits on the right side of ||
        if node.di.name != 'thesis' and isinstance(left, DI) and getattr(left, "name", None) == getattr(node.di, "name", None):
            if isinstance(right, Cons):
                falsum_term = getattr(right, "term", None)      # ⊥ / _F_ must be on term side
                if getattr(falsum_term, "flag", None) == "Falsum" or is_falsum_prop(getattr(falsum_term, "prop", "")):
                    adapter_ctx = getattr(right, "context", None)  # adapter is the context side
                    if adapter_ctx is not None:
                        # Visit adapter first (emits 'axiom adapter'), then prepend 'elim H1'
                        self.visit(adapter_ctx)
                        self.instructions.appendleft(f"elim {node.di.name}")
                        return node
        # Default traversal and instruction
        node = super().visit_Mutilde(node)
        if node.di.name == 'thesis':
            return node
        if node.contr:
            self.instructions.appendleft(f"cut ({fn(node.contr)}) {node.di.name}.")
            return node
        raise Exception(f"Could not identify cut proposition for Mutilde node {self._node_pres(node)}")

    def visit_Lamda(self, node: Lamda):
        node = super().visit_Lamda(node)
        # Skip the inner lambda used by negation-elimination scaffolding
        name = getattr(getattr(node, "di", None), "di", None)
        name = getattr(name, "name", None)
        if name and name in self._neg_bound_names:
            return node
        if name:
            self.instructions.appendleft(f'elim {name}.')
        else:
            raise Exception(f"Missing hypothesis name for Lamda node {self._node_pres(node)}")
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        self.instructions.appendleft(f'elim.')
        return node

    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        self.instructions.appendleft(f'next.')
        return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        self.instructions.appendleft(f'next.')
        return node

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        if node.name:
            if node.name == "thesis":
                return node
            if getattr(node, "flag", None) == "bound negation" or node.name in self._neg_bound_names:
                return node
            if node.flag == "Falsum":
                return node
            else:
                self.instructions.appendleft(f'moxia {node.name}.')
        else:
            raise Exception(f"Axiom name missing for ID node {self._node_pres(node)}")
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        if node.name:
            if node.name == "thesis":
                return node
        if node.name:
            if getattr(node, "flag", None) == "bound negation" or node.name in self._neg_bound_names:
                return node
            if node.flag == "Falsum":
                return node
            else:
                self.instructions.appendleft(f'axiom {node.name}.')
        else:
            raise Exception(f"Axiom name missing for DI node {self._node_pres(node)}")
        return node

    def visit_unhandled(self, node):
        raise Exception(f"Unhandled node type: {type(node).__name__} {self._node_pres(node)}")
        return node
