from typing import Set, Dict
from copy import deepcopy
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI


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
    Placeholder refactor target that applies alpha-renaming to freshen binders.
    Replace with the verbatim implementation from parser.py.
    """

    def visit(self, node):
        # No-op pass-through until real implementation is moved here.
        return node
