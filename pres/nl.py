import re
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI

def pretty_natural(proof_term: "ProofTerm", semantic: "Rendering_Semantics") -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0)
    return '\n'.join(lines)

class Rendering_Semantics:
    def __init__(self, indentation, Mu, Mutilde, Lamda, Cons, Goal, ID, DI):
        self.indentation = indentation
        self.Mu = Mu
        self.Mutilde = Mutilde
        self.Lamda = Lamda
        #self.Hyp = Hyp
        self.Cons = Cons
        self.Goal = Goal
        #self.Done = Done
        self.ID = ID
        self.DI = DI

natural_language_rendering = Rendering_Semantics('   ', "we need to prove ", f"we proved ", f"assume ", f"and", f"? ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', "Assume a refutation of ", f"Assume a proof of  ", f"assume ", f"and", f"? ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], [f"We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"by default ", f"done ", f"by ")

from core.comp.visitor import ProofTermVisitor


class _NLVisitor(ProofTermVisitor):
    def __init__(self, semantic: Rendering_Semantics, lines: list[str], indent: int = 0):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent

    def _indent_str(self) -> str:
        return self.semantic.indentation * self.indent

    def _with_indent(self, delta: int, node):
        old = self.indent
        self.indent = old + delta
        try:
            return self.visit(node)
        finally:
            self.indent = old

    def visit_Mu(self, term: Mu):
        indent_str = self._indent_str()
        if term.id.name == "stash":
            self.lines.append(f"{indent_str}" + self.semantic.Mu[1] + f"{term.prop}" + " in")
            self._with_indent(1, term.term)
            return term
        if term.id.name == "support":
            self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}")
            self._with_indent(1, term.term)
            self.lines.append(f"{indent_str}" + self.semantic.Mu[2])
            self._with_indent(1, term.context)
            return term
        if re.compile(r"alt\d").match(term.id.name):
            self.visit(term.term)
            self.visit(term.context)
            return term
        if term.id.name == "undercut":
            # Note that the result is somewhat ugly due to the need for an adapter to deal with Fellowship's treatment of negation.
            self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
            self._with_indent(1, term.context)
            self.lines.append(f"{indent_str}" + self.semantic.Mu[3])
            self._with_indent(1, term.term)
            return term

        self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
        self._with_indent(1, term.term)
        self._with_indent(1, term.context)
        return term

    def visit_Mutilde(self, term: Mutilde):
        indent_str = self._indent_str()
        if term.di.name == "issue":
            self.lines.append(f"{indent_str}" + self.semantic.Mutilde[0] + f"{term.prop}")
            self.visit(term.term)
            self.lines.append(f"{indent_str}" + self.semantic.Mutilde[1])
            self.visit(term.context)
            return term
        if term.di.name == "adapter":
            self.visit(term.term)
            self.lines.append(f"{indent_str}".removesuffix(self.semantic.indentation) + self.semantic.Mutilde[2])
            return term
        if re.compile(r"alt\d").match(term.di.name):
            self.visit(term.term)
            self.visit(term.context)
            return term

        self.lines.append(f"{indent_str}" + self.semantic.Mutilde[0] + f"{term.prop} " + f"({term.di.name})")
        self._with_indent(1, term.term)
        self._with_indent(1, term.context)
        return term

    def visit_Lamda(self, term: Lamda):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Lamda + f"{term.di.prop}" + f"({term.di.di.name})")
        self.visit(term.term)
        return term

    def visit_Cons(self, term: Cons):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Cons)
        self.visit(term.term)
        self.visit(term.context)
        return term

    def visit_Goal(self, term: Goal):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Goal + f"{term.number}")
        return term

    def visit_DI(self, term: DI):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.DI + f"{term.name}")
        return term

    def visit_ID(self, term: ID):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}".removesuffix(self.semantic.indentation) + self.semantic.ID)
        return term

    def visit_Sonc(self, term):
        # Preserve legacy behavior: Sonc was not handled by the old traversal and printed as unhandled.
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}Unhandled term type: {type(term)}")
        return term

    def visit_unhandled(self, term):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}Unhandled term type: {type(term)}")
        return term


def traverse_proof_term(semantic, term, lines, indent):
    _NLVisitor(semantic, lines, indent=indent).visit(term)
