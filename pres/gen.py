from copy import deepcopy
from typing import Optional

# Import AST nodes and the base visitor from parser to avoid circular deps
# while parser still owns the AST and base visitor.
from parser import (
    Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI,
    ProofTerm, ProofTermVisitor,
)

class ProofTermGenerationVisitor(ProofTermVisitor):
    """Generate a proof term from an (enriched or rewritten) argument body."""
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        pass

    def visit_Mu(self, node: Mu):
        node = super().visit_Mu(node)
        if self.verbose == True:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>'
        else:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>'
        return node

    def visit_Mutilde(self, node: Mutilde):
        node = super().visit_Mutilde(node)
        if self.verbose == True:
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

    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        node.pres = f'{node.number}:{node.prop}'
        return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        node.pres = f'{node.number}:{node.prop}'
        return node

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        node.pres = f'{node.name}:{node.prop}' if node.prop else f'{node.name}'
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        node.pres = f'{node.name}:{node.prop}' if node.prop else f'{node.name}'
        return node

    def visit_unhandled(self, node):
        return node
