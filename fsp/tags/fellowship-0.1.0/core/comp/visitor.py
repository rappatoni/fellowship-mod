from core.ac.ast import Mu, Mutilde, Lamda, Admal, Cons, Sonc, Goal, Laog, ID, DI

class ProofTermVisitor:
    def visit(self, node):
        if node is None:
            return None
        if isinstance(node, Mu):
            return self.visit_Mu(node)
        elif isinstance(node, Mutilde):
            return self.visit_Mutilde(node)
        elif isinstance(node, Lamda):
            return self.visit_Lamda(node)
        elif isinstance(node, Cons):
            return self.visit_Cons(node)
        elif isinstance(node, Goal):
            return self.visit_Goal(node)
        elif isinstance(node, Laog):
            return self.visit_Laog(node)
        elif isinstance(node, ID):
            return self.visit_ID(node)
        elif isinstance(node, DI):
            return self.visit_DI(node)
        elif isinstance(node, Admal):
            return self.visit_Admal(node)
        elif isinstance(node, Sonc):
            return self.visit_Sonc(node)
        else:
            return self.visit_unhandled(node)

    def visit_Mu(self, node: Mu):
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)
        return node

    def visit_Mutilde(self, node: Mutilde):
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)
        return node

    def visit_Lamda(self, node: Lamda):
        node.term = self.visit(node.term)
        return node

    def visit_Cons(self, node: Cons):
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)
        return node

    def visit_Admal(self, node: Admal):
        node.context = self.visit(node.context)
        return node

    def visit_Sonc(self, node: Sonc):
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)
        return node

    def visit_Goal(self, node: Goal):
        return node

    def visit_Laog(self, node: Laog):
        return node

    def visit_ID(self, node: ID):
        return node

    def visit_DI(self, node: DI):
        return node

    def visit_unhandled(self, node):
        return node
