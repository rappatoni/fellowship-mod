from typing import Set, Dict
from copy import deepcopy
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Goal, Laog, ID, DI


def _collect_binder_names(node: ProofTerm, names=None):
    """Return the set of all µ / µ̃ / λ binder names occurring in node."""
    if names is None:
        names = set()
    if isinstance(node, Mu):
        names.add(node.id.name)
    elif isinstance(node, Mutilde):
        names.add(node.di.name)
    elif isinstance(node, Lamda):
        names.add(node.di.di.name)  # di holds a Hyp → DI → name
    elif isinstance(node, Admal):
        names.add(node.id.id.name)
    # recurse
    for child in (getattr(node, 'term', None), getattr(node, 'context', None)):
        if child is not None:
            _collect_binder_names(child, names)
    return names


def _fresh(prefix: str, taken: set[str]) -> str:
    """Generate a fresh variable starting with prefix not in taken."""
    if prefix not in taken:
        return prefix
    i = 2
    while f"{prefix}{i}" in taken:
        i += 1
    return f"{prefix}{i}"


class _AlphaRename(ProofTermVisitor):
    """Capture-avoiding α-renamer. mapping is old-name → new-name."""
    def __init__(self, mapping: dict[str, str]):
        super().__init__()
        self.mapping = dict(mapping)

    # binders – rename & recurse
    def visit_Mu(self, node: Mu):
        if node.id.name in self.mapping:
            node.id.name = self.mapping[node.id.name]
        return super().visit_Mu(node)

    def visit_Mutilde(self, node: Mutilde):
        if node.di.name in self.mapping:
            node.di.name = self.mapping[node.di.name]
        return super().visit_Mutilde(node)

    def visit_Lamda(self, node: Lamda):
        if node.di.di.name in self.mapping:
            node.di.di.name = self.mapping[node.di.di.name]
        return super().visit_Lamda(node)

    def visit_Admal(self, node: Admal):
        # rename context lambda binder if mapped, then recurse into context
        if node.id.id.name in self.mapping:
            node.id.id.name = self.mapping[node.id.id.name]
        return super().visit_Admal(node)

    # leaves – replace occurrences
    def visit_ID(self, node: ID):
        if node.name in self.mapping:
            node.name = self.mapping[node.name]
        return node

    def visit_DI(self, node: DI):
        if node.name in self.mapping:
            node.name = self.mapping[node.name]
        return node


class FreshenBinderNames(ProofTermVisitor):
    """
    Capture-avoiding binder freshener with local active-scope freshness:
      - Traverses the proof term while tracking only names that are currently active
        in the enclosing binder stack, plus any externally reserved `taken` names.
      - A binder is freshened only when reusing its name would clash with one of
        those currently active / reserved names.
      - This models Fellowship's local visible-context freshness more closely than
        a global "every binder name in the whole AST must be unique" policy.
      - Affine binder '_' is exempt and is never freshened.
    """
    def __init__(self, taken: set[str] | None = None):
        super().__init__()
        self.taken: set[str] = set(taken or ())

    def _visit_binder(self, node, *, get_name, set_name, recurse_term: bool, recurse_context: bool):
        old = get_name(node)
        new = old
        scope_taken = set(self.taken)
        if old != "_" and old in scope_taken:
            new = _fresh(old, scope_taken)
            ren = _AlphaRename({old: new})
            set_name(node, new)
            if recurse_term and getattr(node, "term", None) is not None:
                node.term = ren.visit(node.term)
            if recurse_context and getattr(node, "context", None) is not None:
                node.context = ren.visit(node.context)
        prev_taken = self.taken
        next_taken = set(prev_taken)
        if new != "_":
            next_taken.add(new)
        self.taken = next_taken
        try:
            if recurse_term and getattr(node, "term", None) is not None:
                node.term = self.visit(node.term)
            if recurse_context and getattr(node, "context", None) is not None:
                node.context = self.visit(node.context)
        finally:
            self.taken = prev_taken
        return node

    def visit_Mu(self, node: Mu):
        return self._visit_binder(
            node,
            get_name=lambda n: n.id.name,
            set_name=lambda n, value: setattr(n.id, "name", value),
            recurse_term=True,
            recurse_context=True,
        )

    def visit_Mutilde(self, node: Mutilde):
        return self._visit_binder(
            node,
            get_name=lambda n: n.di.name,
            set_name=lambda n, value: setattr(n.di, "name", value),
            recurse_term=True,
            recurse_context=True,
        )

    def visit_Lamda(self, node: Lamda):
        return self._visit_binder(
            node,
            get_name=lambda n: n.di.di.name,
            set_name=lambda n, value: setattr(n.di.di, "name", value),
            recurse_term=True,
            recurse_context=False,
        )

    def visit_Admal(self, node: Admal):
        return self._visit_binder(
            node,
            get_name=lambda n: n.id.id.name,
            set_name=lambda n, value: setattr(n.id.id, "name", value),
            recurse_term=False,
            recurse_context=True,
        )

    # Other nodes: default traversal
    def visit_Cons(self, node: Cons):
        return super().visit_Cons(node)

    def visit_Goal(self, node: Goal):
        return node

    def visit_Laog(self, node: Laog):
        return node

    def visit_ID(self, node: ID):
        return node

    def visit_DI(self, node: DI):
        return node