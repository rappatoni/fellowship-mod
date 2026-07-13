import re
from core.ac.ast import Deleg, ProofTerm, Mu, Mutilde, Lamda, Cons, Sonc, Admal, Goal, Laog, Deleg, Geled, ID, DI

def pretty_natural(proof_term: "ProofTerm", semantic: "Rendering_Semantics") -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0)
    return '\n'.join(lines)

class Rendering_Semantics:
    def __init__(self, indentation, Mu, Mutilde, Lamda, Cons, Goal, Laog, Deleg, Geled, ID, DI):
        self.indentation = indentation
        self.Mu = Mu
        self.Mutilde = Mutilde
        self.Lamda = Lamda
        #self.Hyp = Hyp
        self.Cons = Cons
        self.Goal = Goal
        self.Laog = Laog
        self.Deleg = Deleg
        self.Geled = Geled
        #self.Done = Done
        self.ID = ID
        self.DI = DI

natural_language_rendering = Rendering_Semantics('   ', ["we need to prove ", "we proved ", ""], ["we proved ", ""], f"assume ", f"and", f"? ", f" ?", f" !", f"! ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', ["Assume a refutation of ", "Assume a proof of  ", ""], ["Assume a proof of  ", ""], f"assume ", f"and", f"? ", f" ?", f" !", f"! ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], ["We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"? ", f" ?", f"by default!", f"by default!", f"done ", f"by ")

# Vanilla rendering: preserves the full proof-term syntax, only adds indentation/line breaks.
# Implemented as a dedicated semantics object plus a visitor special-case.
vanilla_rendering = Rendering_Semantics('   ', "", "", "", "", "", "", "", "", "", "")

from core.comp.visitor import ProofTermVisitor


class _VanillaVisitor(ProofTermVisitor):
    def __init__(self, semantic: Rendering_Semantics, lines: list[str], indent: int = 0, prefix: str = ""):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent
        self.prefix = prefix or (self.semantic.indentation * indent)
        self._cur: list[str] = []

    def _emit(self, s: str) -> None:
        self._cur.append(s)

    def _newline(self) -> None:
        if self._cur:
            self.lines.append(self.prefix + "".join(self._cur))
            self._cur = []

    def _emit_line(self, s: str, prefix: str | None = None) -> None:
        line_prefix = self.prefix if prefix is None else prefix
        self.lines.append(line_prefix + s)

    @staticmethod
    def _child_prefix(parent_prefix: str, is_last: bool) -> str:
        return parent_prefix + ("└─ " if is_last else "├─ ")

    @staticmethod
    def _continuation_prefix(parent_prefix: str, is_last: bool) -> str:
        return parent_prefix + ("   " if is_last else "│  ")

    @staticmethod
    def _with_prefix(lines: list[str], first_prefix: str, rest_prefix: str) -> list[str]:
        if not lines:
            return []
        return [first_prefix + lines[0], *(rest_prefix + line for line in lines[1:])]

    def _render_child(self, node, *, is_last: bool) -> list[str]:
        child_lines: list[str] = []
        child_visitor = _VanillaVisitor(self.semantic, child_lines)
        child_visitor.render(node)
        return self._with_prefix(
            child_lines,
            self._child_prefix(self.prefix, is_last),
            self._continuation_prefix(self.prefix, is_last),
        )

    def render(self, node) -> None:
        self.visit(node)
        if self._cur:
            self._newline()

    def visit_Mu(self, node: Mu):
        opening_prefix = self.prefix
        self._emit(f"μ{node.id.name}:{node.prop}.<")
        self._newline()

        term_lines = self._render_child(node.term, is_last=False)
        context_lines = self._render_child(node.context, is_last=True)

        if term_lines:
            self.lines.extend(term_lines[:-1])
            last_term = term_lines[-1]
            self.lines.append(f"{last_term}||")
        else:
            self._emit_line("├─ ||", prefix=opening_prefix)
        self.lines.extend(context_lines)
        self._emit_line(">", prefix=opening_prefix)
        self._cur = []
        return node

    def visit_Mutilde(self, node: Mutilde):
        opening_prefix = self.prefix
        self._emit(f"μ'{node.di.name}:{node.prop}.<")
        self._newline()

        term_lines = self._render_child(node.term, is_last=False)
        context_lines = self._render_child(node.context, is_last=True)

        if term_lines:
            self.lines.extend(term_lines[:-1])
            last_term = term_lines[-1]
            self.lines.append(f"{last_term}||")
        else:
            self._emit_line("├─ ||", prefix=opening_prefix)
        self.lines.extend(context_lines)
        self._emit_line(">", prefix=opening_prefix)
        self._cur = []
        return node

    def visit_Lamda(self, node: Lamda):
        self._emit(f"λ{node.di.di.name}:{node.di.prop}.")
        self._newline()
        old_prefix = self.prefix
        self.prefix = self._child_prefix(old_prefix, True)
        try:
            self.visit(node.term)
        finally:
            self.prefix = old_prefix
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
        self._emit(f"?{node.number}:{node.prop}")
        return node

    def visit_Laog(self, node: Laog):
        self._emit(f"{node.number}:{node.prop}?")
        return node

    def visit_Deleg(self, node: Deleg):
        self._emit(f"!{node.number}:{node.prop}")
        return node

    def visit_Geled(self, node: Geled):
        self._emit(f"{node.number}:{node.prop}!")
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
    
    def visit_Laog(self, term: Laog):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Laog + f"{term.number}")
        return term
    
    def visit_Deleg(self, term: Deleg):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Deleg + f"{term.number}")
        return term
    
    def visit_Geled(self, term: Geled):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Geled + f"{term.number}")
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
