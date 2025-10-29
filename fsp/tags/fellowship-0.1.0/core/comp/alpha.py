from typing import Set, Dict
from copy import deepcopy
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI


def _collect_binder_names(node) -> Set[str]:
    """
    Placeholder refactor target: collect all binder names in the AST.
    Replace with the verbatim implementation from parser.py.
    """
    return set()


def _fresh(existing: Set[str], base: str) -> str:
    """
    Placeholder refactor target: generate a fresh binder name not in `existing`.
    Replace with the verbatim implementation from parser.py.
    """
    if base not in existing:
        return base
    i = 1
    while f"{base}_{i}" in existing:
        i += 1
    return f"{base}_{i}"


class _AlphaRename(ProofTermVisitor):
    """
    Placeholder refactor target for alpha-renaming visitor.
    Replace with the verbatim implementation from parser.py.
    """

    def __init__(self, renamings: Dict[str, str]) -> None:
        super().__init__()
        self.renamings = dict(renamings)

    def visit(self, node):
        # No-op pass-through until real implementation is moved here.
        return node


class FreshenBinderNames(ProofTermVisitor):
    """
    Placeholder refactor target that applies alpha-renaming to freshen binders.
    Replace with the verbatim implementation from parser.py.
    """

    def visit(self, node):
        # No-op pass-through until real implementation is moved here.
        return node
