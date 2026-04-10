import re
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Sonc, Admal, Goal, Laog, ID, DI

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

natural_language_rendering = Rendering_Semantics('   ', ["we need to prove ", "we proved ", ""], ["we proved ", ""], f"assume ", f"and", f"? ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', ["Assume a refutation of ", "Assume a proof of  ", ""], ["Assume a proof of  ", ""], f"assume ", f"and", f"? ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], ["We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"by default ", f"done ", f"by ")

# Vanilla rendering: preserves the full proof-term syntax, only adds indentation/line breaks.
# Implemented as a dedicated semantics object plus a visitor special-case.
vanilla_rendering = Rendering_Semantics('   ', "", "", "", "", "", "", "")

from core.comp.visitor import ProofTermVisitor


class _VanillaVisitor(ProofTermVisitor):
    def __init__(self, semantic: Rendering_Semantics, lines: list[str], indent: int = 0):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent
        self._cur: list[str] = []

    def _emit(self, s: str) -> None:
        self._cur.append(s)

    def _newline(self) -> None:
        if self._cur:
            self.lines.append(self.semantic.indentation * self.indent + "".join(self._cur))
            self._cur = []

    def _emit_line(self, s: str, indent: int | None = None) -> None:
        level = self.indent if indent is None else indent
        self.lines.append(self.semantic.indentation * level + s)

    def render(self, node) -> None:
        self.visit(node)
        if self._cur:
            self._newline()

    def visit_Mu(self, node: Mu):
        opening_indent = self.indent
        child_indent = opening_indent + 1
        self._emit(f"μ{node.id.name}:{node.prop}.<")
        self._newline()

        term_lines: list[str] = []
        term_visitor = _VanillaVisitor(self.semantic, term_lines, indent=child_indent)
        term_visitor.render(node.term)

        context_lines: list[str] = []
        context_visitor = _VanillaVisitor(self.semantic, context_lines, indent=child_indent)
        context_visitor.render(node.context)

        if term_lines:
            self.lines.extend(term_lines[:-1])
            last_term = term_lines[-1]
            self.lines.append(f"{last_term}||")
        else:
            self._emit_line("||", indent=child_indent)
        self.lines.extend(context_lines)
        self._emit_line(">", indent=opening_indent)
        self._cur = []
        self.indent = opening_indent
        return node

    def visit_Mutilde(self, node: Mutilde):
        opening_indent = self.indent
        child_indent = opening_indent + 1
        self._emit(f"μ'{node.di.name}:{node.prop}.<")
        self._newline()

        term_lines: list[str] = []
        term_visitor = _VanillaVisitor(self.semantic, term_lines, indent=child_indent)
        term_visitor.render(node.term)

        context_lines: list[str] = []
        context_visitor = _VanillaVisitor(self.semantic, context_lines, indent=child_indent)
        context_visitor.render(node.context)

        if term_lines:
            self.lines.extend(term_lines[:-1])
            last_term = term_lines[-1]
            self.lines.append(f"{last_term}||")
        else:
            self._emit_line("||", indent=child_indent)
        self.lines.extend(context_lines)
        self._emit_line(">", indent=opening_indent)
        self._cur = []
        self.indent = opening_indent
        return node

    def visit_Lamda(self, node: Lamda):
        self._emit(f"λ{node.di.di.name}:{node.di.prop}.")
        self._newline()
        self.indent += 1
        self.visit(node.term)
        self.indent -= 1
        return node

    def visit_Cons(self, node: Cons):
        self.visit(node.term)
        self._emit("*")
        self.visit(node.context)
        return node

    def visit_Admal(self, node: Admal):
        self.visit(node.context)
        self._emit(f".{node.id.prop}:{node.id.id.name}λ")
        return node

    def visit_Sonc(self, node: Sonc):
        self.visit(node.context)
        self._emit("*")
        self.visit(node.term)
        return node

    def visit_Goal(self, node: Goal):
        self._emit(f"{node.number}:{node.prop}")
        return node

    def visit_Laog(self, node: Laog):
        self._emit(f"{node.number}:{node.prop}")
        return node

    def visit_ID(self, node: ID):
        self._emit(f"{node.name}:{node.prop}" if node.prop else f"{node.name}")
        return node

    def visit_DI(self, node: DI):
        self._emit(f"{node.name}:{node.prop}" if node.prop else f"{node.name}")
        return node

    def visit_unhandled(self, node):
        self._emit(f"Unhandled term type: {type(node)}")
        return node


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
        # if term.id.name == "stash":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[1] + f"{term.prop}" + " in")
        #     self._with_indent(1, term.term)
        #     return term
        # if term.id.name == "support":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}")
        #     self._with_indent(1, term.term)
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[2])
        #     self._with_indent(1, term.context)
        #     return term
        # if re.compile(r"alt\d").match(term.id.name):
        #     self.visit(term.term)
        #     self.visit(term.context)
        #     return term
        # if term.id.name == "undercut":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
        #     self._with_indent(1, term.context)
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[3])
        #     self._with_indent(1, term.term)
        #     return term

        self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
        self._with_indent(1, term.term)
        self._with_indent(1, term.context)
        return term

    def visit_Mutilde(self, term: Mutilde):
        indent_str = self._indent_str()
        # if term.di.name == "issue":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mutilde[0] + f"{term.prop}")
        #     self.visit(term.term)
        #     self.lines.append(f"{indent_str}" + self.semantic.Mutilde[1])
        #     self.visit(term.context)
        #     return term
        # if term.di.name == "adapter":
        #     self.visit(term.term)
        #     self.lines.append(f"{indent_str}".removesuffix(self.semantic.indentation) + self.semantic.Mutilde[2])
        #     return term
        # if re.compile(r"alt\d").match(term.di.name):
        #     self.visit(term.term)
        #     self.visit(term.context)
        #     return term

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

    def visit_Sonc(self, term: Sonc):
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
    if semantic is vanilla_rendering:
        _VanillaVisitor(semantic, lines, indent=indent).render(term)
    else:
        _NLVisitor(semantic, lines, indent=indent).visit(term)
