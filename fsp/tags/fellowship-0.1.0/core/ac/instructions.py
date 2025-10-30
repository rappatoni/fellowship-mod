from copy import deepcopy
import collections
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI
from pres.gen import ProofTermGenerationVisitor

def fn(negated_prop: str):
    return negated_prop.replace("¬", "~")

class InstructionsGenerationVisitor(ProofTermVisitor):  # TODO: make purely functional later
    def __init__(self):
        self.instructions = collections.deque('')
        pass

    def _node_pres(self, n: ProofTerm) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def return_instructions(self, proofterm):
        self.instructions.clear()
        self.visit(proofterm)
        return self.instructions

    def visit_Mu(self, node: Mu):
        node = super().visit_Mu(node)
        if node.id.name == 'thesis':
            pass
        else:
            if node.contr:
                if (node.prop.startswith("¬") or node.prop.startswith("~")) and node.prop == node.contr:
                    for _ in range(3):
                        self.instructions.popleft()
                    self.instructions.appendleft(f"elim {node.id.name}")
                else:
                    self.instructions.appendleft(f"cut ({fn(node.contr)}) {node.id.name}.")
            else:
                raise Exception(f"Could not identify cut proposition for Mu node {self._node_pres(node)}")
        return node

    def visit_Mutilde(self, node: Mutilde):
        node = super().visit_Mutilde(node)
        if node.contr:
            if (node.prop.startswith("¬") or node.prop.startswith("~")) and node.prop == node.contr:
                for _ in range(2):
                    self.instructions.popleft()
                self.instructions.appendleft(f"elim {node.di.name}")
            else:
                self.instructions.appendleft(f"cut ({fn(node.contr)}) {node.di.name}.")
        else:
            raise Exception(f"Could not identify cut proposition for Mutilde node {self._node_pres(node)}")
        return node

    def visit_Lamda(self, node: Lamda):
        node = super().visit_Lamda(node)
        if node.di.di.name:
            self.instructions.appendleft(f'elim {node.di.di.name}.')
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
            if node.flag == "bound negation":
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
            if node.flag == "bound negation":
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
