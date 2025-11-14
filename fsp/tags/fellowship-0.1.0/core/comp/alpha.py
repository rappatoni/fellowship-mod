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
    Capture-avoiding global freshener:
      - Traverses the proof term, keeping a set of already-seen binder names.
      - When encountering a binder whose name collides with an already-seen name,
        generate a fresh name and rename the binder together with all its bound
        occurrences within its scope (term/context).
      - Leaves non-binder leaves untouched.
    """
    def __init__(self, taken: set[str] | None = None):
        super().__init__()
        self.seen: set[str] = set()
        self.taken: set[str] = set(taken or ())

    def visit_Mu(self, node: Mu):
        # possibly rename binder id and bound occurrences in scope
        old = node.id.name
        new = old
        if old in self.seen or old in self.taken:
            new = _fresh(old, self.seen | self.taken)
            ren = _AlphaRename({old: new})
            node.id.name = new
            node.term = ren.visit(node.term)
            node.context = ren.visit(node.context)
        self.seen.add(new); self.taken.add(new)
        node.term = self.visit(node.term)
        node.context = self.visit(node.context)
        return node

    def visit_Mutilde(self, node: Mutilde):
        old = node.di.name
        new = old
        if old in self.seen or old in self.taken:
            new = _fresh(old, self.seen | self.taken)
            ren = _AlphaRename({old: new})
            node.di.name = new
            node.term = ren.visit(node.term)
            node.context = ren.visit(node.context)
        self.seen.add(new); self.taken.add(new)
        node.term = self.visit(node.term)
        node.context = self.visit(node.context)
        return node

    def visit_Lamda(self, node: Lamda):
        old = node.di.di.name
        new = old
        if old in self.seen or old in self.taken:
            new = _fresh(old, self.seen | self.taken)
            ren = _AlphaRename({old: new})
            node.di.di.name = new
            node.term = ren.visit(node.term)
        self.seen.add(new); self.taken.add(new)
        node.term = self.visit(node.term)
        return node

    def visit_Admal(self, node: Admal):
        old = node.id.id.name
        new = old
        if old in self.seen or old in self.taken:
            new = _fresh(old, self.seen | self.taken)
            ren = _AlphaRename({old: new})
            node.id.id.name = new
            node.context = ren.visit(node.context)
        self.seen.add(new); self.taken.add(new)
        node.context = self.visit(node.context)
        return node

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
