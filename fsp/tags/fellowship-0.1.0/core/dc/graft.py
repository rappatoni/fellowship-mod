from typing import Optional, Dict, Any
from copy import deepcopy
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI
from core.comp.alpha import _AlphaRename  # if used


class _CapturingSubst(ProofTermVisitor):
    """
    Placeholder refactor target for a substitution visitor that captures mappings.
    Replace with the verbatim implementation from parser.py, if used.
    """
    def __init__(self, subst: Dict[str, Any]) -> None:
        super().__init__()
        self.subst = dict(subst)

    def visit(self, node):
        return node


class _GraftVisitor(ProofTermVisitor):
    """
    Placeholder refactor target for grafting visitor.
    Replace with the verbatim implementation from parser.py.
    """
    def __init__(self, target, replacement) -> None:
        super().__init__()
        self.target = target
        self.replacement = replacement

    def visit(self, node):
        return node


def graft_single(target, into):
    """
    Placeholder refactor target.
    Graft `target` into `into` at a single appropriate site.
    Replace with the verbatim implementation from parser.py.
    """
    return deepcopy(into)


def graft_uniform(target, into):
    """
    Placeholder refactor target.
    Uniformly graft `target` into `into`.
    Replace with the verbatim implementation from parser.py.
    """
    return deepcopy(into)
