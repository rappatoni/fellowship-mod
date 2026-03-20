import re
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Sonc, Goal, Laog, ID, DI

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

# Vanilla rendering: preserves the full proof-term syntax, only adds indentation/line breaks.
# Implemented as a dedicated semantics object plus a visitor special-case.
vanilla_rendering = Rendering_Semantics('   ', "", "", "", "", "", "", "")

from core.comp.visitor import ProofTermVisitor


class _NLVisitor(ProofTermVisitor):
    def _is_vanilla(self) -> bool:
        return self.semantic is vanilla_rendering

    def _emit(self, s: str) -> None:
        # Emit into the current line buffer.
        self._cur.append(s)

    def _newline(self) -> None:
        # Flush current line.
        self.lines.append(self.semantic.indentation * self.indent + "".join(self._cur))
        self._cur = []

    def _vanilla_visit(self, node) -> None:
        # Syntax-preserving pretty-printer: emit the term exactly once, only whitespace differs.
        if isinstance(node, Mu):
            self._emit(f"μ{node.id.name}:{node.prop}.<")
            self.indent += 1
            self._newline()
            self._vanilla_visit(node.term)
            self._emit("||")
            self._newline()
            self._vanilla_visit(node.context)
            self.indent -= 1
            self._emit(">")
            return
        if isinstance(node, Mutilde):
            self._emit(f"μ'{node.di.name}:{node.prop}.<")
            self.indent += 1
            self._newline()
            self._vanilla_visit(node.term)
            self._emit("||")
            self._newline()
            self._vanilla_visit(node.context)
            self.indent -= 1
            self._emit(">")
            return
        if isinstance(node, Lamda):
            self._emit(f"λ{node.di.di.name}:{node.di.prop}.")
            self.indent += 1
            self._newline()
            self._vanilla_visit(node.term)
            self.indent -= 1
            return
        if isinstance(node, Cons):
            self._vanilla_visit(node.term)
            self._emit("*")
            self._vanilla_visit(node.context)
            return
        if isinstance(node, Sonc):
            self._vanilla_visit(node.context)
            self._emit("*")
            self._vanilla_visit(node.term)
            return
        if isinstance(node, Goal):
            self._emit(f"{node.number}:{node.prop}")
            return
        if isinstance(node, Laog):
            self._emit(f"{node.number}:{node.prop}")
            return
        if isinstance(node, ID):
            self._emit(f"{node.name}:{node.prop}" if node.prop else f"{node.name}")
            return
        if isinstance(node, DI):
            self._emit(f"{node.name}:{node.prop}" if node.prop else f"{node.name}")
            return
        # Fallback: preserve old behavior as a line.
        self._emit(f"Unhandled term type: {type(node)}")
        return
    def __init__(self, semantic: Rendering_Semantics, lines: list[str], indent: int = 0):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent
        self._cur: list[str] = []

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
        if self._is_vanilla():
            self._vanilla_visit(term)
            # flush any remaining buffer as final line
            if self._cur:
                self._newline()
            return term

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
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

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
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Lamda + f"{term.di.prop}" + f"({term.di.di.name})")
        self.visit(term.term)
        return term

    def visit_Cons(self, term: Cons):
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Cons)
        self.visit(term.term)
        self.visit(term.context)
        return term

    def visit_Goal(self, term: Goal):
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Goal + f"{term.number}")
        return term

    def visit_DI(self, term: DI):
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.DI + f"{term.name}")
        return term

    def visit_ID(self, term: ID):
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}".removesuffix(self.semantic.indentation) + self.semantic.ID)
        return term

    def visit_Sonc(self, term: Sonc):
        if self._is_vanilla():
            self._vanilla_visit(term)
            if self._cur:
                self._newline()
            return term

        # Treat Sonc like Cons (but order is context * term)
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Cons)
        self.visit(term.context)
        self.visit(term.term)
        return term

    def visit_unhandled(self, term):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}Unhandled term type: {type(term)}")
        return term


def traverse_proof_term(semantic, term, lines, indent):
    _NLVisitor(semantic, lines, indent=indent).visit(term)
