from copy import deepcopy
from typing import Optional

# Import AST nodes and the base visitor from parser to avoid circular deps
# while parser still owns the AST and base visitor.
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc, Goal, Laog, Deleg, Geled, ID, DI
from core.comp.visitor import ProofTermVisitor

class ProofTermGenerationVisitor(ProofTermVisitor):
    """Generate a proof term from an (enriched or rewritten) argument body.

    verbosity levels:
      0 / False: enriched, non-verbose proof term (historical default)
      1 / True: enriched, verbose proof term with contraction labels
     -1: replay-oriented proof term that strips ordinary leaf annotations while
         preserving binder and open-placeholder annotations needed for replay
    """
    def __init__(self, verbose: bool | int = False):
        if isinstance(verbose, bool):
            self.verbosity = 1 if verbose else 0
        else:
            self.verbosity = int(verbose)
        self.verbose = self.verbosity > 0

    def _typed_leaf(self, name, prop):
        if self.verbosity < 0:
            return f'{name}'
        return f'{name}:{prop}' if prop else f'{name}'

    def _open_term(self, prefix, number, prop):
        if prop:
            return f'{prefix}{number}:{prop}'
        return f'{prefix}{number}'

    def _open_context(self, number, prop, suffix):
        if prop:
            return f'{number}:{prop}{suffix}'
        return f'{number}{suffix}'

    def visit_Mu(self, node: Mu):
        node = super().visit_Mu(node)
        if self.verbose:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>'
        else:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>'
        return node

    def visit_Mutilde(self, node: Mutilde):
        node = super().visit_Mutilde(node)
        if self.verbose:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>"
        else:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>"
        return node

    def visit_Lamda(self, node: Lamda):
        node = super().visit_Lamda(node)
        node.pres = f'λ{node.di.di.name}:{node.di.prop}.{node.term.pres}'
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        node.pres = f'{node.term.pres}*{node.context.pres}'
        return node

    def visit_Admal(self, node: Admal):
        node = super().visit_Admal(node)
        node.pres = f'λ{node.id.id.name}:{node.id.prop}.{node.context.pres}'
        return node

    def visit_Sonc(self, node: Sonc):
        node = super().visit_Sonc(node)
        node.pres = f'{node.context.pres}*{node.term.pres}'
        return node

    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        node.pres = self._open_term('?', node.number, node.prop)
        return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        node.pres = self._open_context(node.number, node.prop, '?')
        return node

    def visit_Deleg(self, node: Deleg):
        node = super().visit_Deleg(node)
        node.pres = self._open_term('!', node.number, node.prop)
        return node

    def visit_Geled(self, node: Geled):
        node = super().visit_Geled(node)
        node.pres = self._open_context(node.number, node.prop, '!')
        return node

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        node.pres = self._typed_leaf(node.name, node.prop)
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        node.pres = self._typed_leaf(node.name, node.prop)
        return node

    def visit_unhandled(self, node):
        return node
