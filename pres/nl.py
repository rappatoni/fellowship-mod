import re
from typing import Mapping
from pres.decorations import render_declaration, render_prop
from pres.pattern_render import (
    AlternativeCasesRenderer,
    AlternativeCounterexamplesRenderer,
    ApplicationRenderer,
    DefeasibleWarrantRenderer,
    DualApplicationRenderer,
    DualDefeasibleWarrantRenderer,
    PatternRenderContext,
    PatternRenderingRegistry,
)
from core.ac.ast import Deleg, ProofTerm, Mu, Mutilde, Lamda, Cons, Sonc, Admal, Goal, Laog, Deleg, Geled, ID, DI, LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO

def pretty_natural(
    proof_term: "ProofTerm",
    semantic: "Rendering_Semantics",
    *,
    declarations: Mapping[str, str] | None = None,
    decorations: Mapping[str, str] | None = None,
) -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0, declarations=declarations, decorations=decorations)
    if getattr(semantic, "tree_guides", False):
        lines = _with_tree_guides(lines, semantic.indentation)
    return '\n'.join(lines)


def _line_level(line: str, indentation: str) -> tuple[int, str]:
    level = 0
    rest = line
    while indentation and rest.startswith(indentation):
        level += 1
        rest = rest[len(indentation):]
    return level, rest


def _has_later_sibling(levels: list[int], index: int, level: int) -> bool:
    for later_level in levels[index + 1:]:
        if later_level < level:
            return False
        if later_level == level:
            return True
    return False


def _with_tree_guides(lines: list[str], indentation: str) -> list[str]:
    if not indentation:
        return lines
    parsed = [_line_level(line, indentation) for line in lines]
    levels = [level for level, _ in parsed]
    guided: list[str] = []
    for index, (level, text) in enumerate(parsed):
        if level == 0:
            guided.append(text)
            continue
        parts: list[str] = []
        for ancestor_level in range(1, level):
            parts.append("│  " if _has_later_sibling(levels, index, ancestor_level) else "   ")
        parts.append("├─ " if _has_later_sibling(levels, index, level) else "└─ ")
        guided.append("".join(parts) + text)
    return guided

class Rendering_Semantics:
    def __init__(self, indentation, Mu, Mutilde, Lamda, Cons, Goal, Laog, Deleg, Geled, ID, DI, *, pattern_renderers=None, connective_templates=None, tree_guides: bool = False,
                 LamdaFO_template="consider an arbitrary but fixed @var of type @sort",
                 DestructTermsPairFO_template="let @var of type @sort be such a thing",
                 ConsFO_template="instantiated at @witness",
                 TermsPairFO_template="witnessed by @witness"):
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
        self.connective_templates = dict(connective_templates or {})
        # Phrasing for the first-order nodes, keyword-only so the existing
        # positional constructions stay valid.  Defaults follow Fellowship's
        # own natural-language rendering (print.ml:63-129).
        self.LamdaFO = LamdaFO_template
        self.DestructTermsPairFO = DestructTermsPairFO_template
        self.ConsFO = ConsFO_template
        self.TermsPairFO = TermsPairFO_template
        self.tree_guides = tree_guides
        self.pattern_registry = PatternRenderingRegistry(pattern_renderers or [])

natural_language_rendering = Rendering_Semantics('   ', ["we need to prove ", "we proved ", ""], ["we proved ", ""], f"assume ", f"and", f"? ", f" ?", f" !", f"! ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', ["Assume a refutation of ", "Assume a proof of  ", ""], ["Assume a proof of  ", ""], f"assume ", f"and", f"? ", f" ?", f" !", f"! ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], ["We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"? ", f" ?", f"by default!", f"by default!", f"done ", f"by ", pattern_renderers=[AlternativeCasesRenderer(), AlternativeCounterexamplesRenderer(child_indent_delta=3), ApplicationRenderer(header_template="Für @prop ist hinreichend, dass @arg_prop", end_label=None), DualApplicationRenderer(header_template="Für @prop ist notwendig dass @condition_prop", warrant_indent_delta=3, end_label=None), DefeasibleWarrantRenderer(header_template="Für @prop spricht ", exception_indent_delta=3), DualDefeasibleWarrantRenderer(header_template="gegen @prop spricht ", support_indent_delta=3)])
pruefschema_rendering = Rendering_Semantics(
    '   ',
    ["Es genügt zu prüfen, dass "],
    ["Es ist notwendigerweise zu prüfen, ob "],
    "Angenommen ",
    "und",
    "Prüfung fehlgeschlagen: ",
    "Prüfung fehlgeschlagen: ",
    "sofern nichts entgegensteht: ",
    "sofern nichts anderes bekannt ist, ist zu verneinen: ",
    ["es ist ausgeschlossen, dass: ", "Prüfung abgeschlossen: "],
    ["es liegt vor: ", "Prüfung fehlgeschlagen: "],
    pattern_renderers=[
        AlternativeCasesRenderer(
            header_template="Die Prüfung, ob @prop gilt, zerfällt in folgende Fallgruppen:",
            first_case_label="Fallgruppe @index:",
            next_case_label="oder Fallgruppe @index:",
        ),
        AlternativeCounterexamplesRenderer(
            header_template="Zur Prüfung von @prop müssen folgende Prüfpunkte abgearbeitet werden:",
            first_condition_label="Prüfpunkt @index:",
            next_condition_label="und Prüfpunkt @index:",
        ),
        ApplicationRenderer(
            header_template="Für @prop (Prüfung: @binder) ist es hinreichend, dass @arg_prop gilt.",
            reason_label="weil",
            separator_label="und",
            end_label="Prüfung @binder abgeschlossen.",
        ),
        DualApplicationRenderer(
            header_template="Für @prop (Prüfung: @binder) ist notwendig, dass @condition_prop gilt.",
            reason_label="weil",
            end_label="Prüfung @binder abgeschlossen.",
        ),
        DefeasibleWarrantRenderer(
            header_template="Für @prop spricht:",
            exception_label="aber",
        ),
        DualDefeasibleWarrantRenderer(
            header_template="Gegen @prop spricht:",
            requirement_template="aber",
        ),
    ],
    connective_templates={"->": "@left impliziert @right", "-": "@left ohne @right"},
    tree_guides=True,
)

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

    # -- first-order nodes -------------------------------------------------

    def visit_LamdaFO(self, node: LamdaFO):
        self._emit(f"λ{node.var}:{node.sort}.")
        self._newline()
        old_prefix = self.prefix
        self.prefix = self._child_prefix(old_prefix, True)
        try:
            self.visit(node.term)
        finally:
            self.prefix = old_prefix
        return node

    def visit_ConsFO(self, node: ConsFO):
        self._emit(f"{node.fo_term}*")
        self.visit(node.context)
        return node

    def visit_TermsPairFO(self, node: TermsPairFO):
        self._emit(f"({node.witness},")
        self.visit(node.term)
        self._emit(")")
        return node

    def visit_DestructTermsPairFO(self, node: DestructTermsPairFO):
        self._emit(f"({node.var}:{node.sort}).")
        self.visit(node.context)
        return node

    def visit_unhandled(self, node):
        self._emit(f"Unhandled term type: {type(node)}")
        return node


def _fill(template: str, **values: str) -> str:
    """Substitute @name placeholders, as decorations and connectives do."""
    for name, value in values.items():
        template = template.replace(f"@{name}", value)
    return template


class _NLVisitor(ProofTermVisitor):
    def __init__(
        self,
        semantic: Rendering_Semantics,
        lines: list[str],
        indent: int = 0,
        *,
        declarations: Mapping[str, str] | None = None,
        decorations: Mapping[str, str] | None = None,
        bound_ids: set[str] | None = None,
        bound_dis: set[str] | None = None,
    ):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent
        self.declarations = declarations or {}
        self.decorations = decorations or {}
        self.bound_ids = set(bound_ids or ())
        self.bound_dis = set(bound_dis or ())

    def _indent_str(self) -> str:
        return self.semantic.indentation * self.indent

    def _render_prop(self, prop: str | None) -> str:
        return render_prop(
            prop,
            self.declarations,
            self.decorations,
            getattr(self.semantic, "connective_templates", {}),
        )

    def _render_declaration_or_prop(self, name: str, prop: str | None) -> str:
        if name in self.declarations or name in self.decorations:
            return render_declaration(
                name,
                self.declarations,
                self.decorations,
                getattr(self.semantic, "connective_templates", {}),
            )
        return self._render_prop(prop)

    @staticmethod
    def _leaf_prefix(prefix, index: int) -> str:
        if isinstance(prefix, (list, tuple)):
            return prefix[index]
        return prefix

    def _with_indent(self, delta: int, node):
        old = self.indent
        self.indent = old + delta
        try:
            return self.visit(node)
        finally:
            self.indent = old

    def visit(self, node):
        if self._try_pattern_render(node):
            return node
        return super().visit(node)

    def _try_pattern_render(self, node) -> bool:
        if node is None:
            return False
        registry = getattr(self.semantic, "pattern_registry", None)
        if registry is None or not registry.renderers:
            return False

        context = PatternRenderContext(
            indent=self.indent,
            indentation=self.semantic.indentation,
            declarations=self.declarations,
            decorations=self.decorations,
            render_prop=self._render_prop,
            render_node=self._render_node_lines,
        )
        result = registry.try_render(node, context)
        if result is None:
            return False

        self.lines.extend(result.lines)
        return True

    def _render_node_lines(self, node, indent_delta: int = 0) -> list[str]:
        child_lines: list[str] = []
        child_visitor = _NLVisitor(
            self.semantic,
            child_lines,
            indent=self.indent + indent_delta,
            declarations=self.declarations,
            decorations=self.decorations,
            bound_ids=self.bound_ids,
            bound_dis=self.bound_dis,
        )
        child_visitor.visit(node)
        return child_lines

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

        self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{self._render_prop(term.prop)}" + f"({term.id.name})")
        old_bound_ids = self.bound_ids
        self.bound_ids = set(old_bound_ids)
        self.bound_ids.add(term.id.name)
        try:
            self._with_indent(1, term.term)
            self._with_indent(1, term.context)
        finally:
            self.bound_ids = old_bound_ids
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

        self.lines.append(f"{indent_str}" + self.semantic.Mutilde[0] + f"{self._render_prop(term.prop)} " + f"({term.di.name})")
        old_bound_dis = self.bound_dis
        self.bound_dis = set(old_bound_dis)
        self.bound_dis.add(term.di.name)
        try:
            self._with_indent(1, term.term)
            self._with_indent(1, term.context)
        finally:
            self.bound_dis = old_bound_dis
        return term

    def visit_Lamda(self, term: Lamda):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Lamda + f"{self._render_prop(term.di.prop)}" + f"({term.di.di.name})")
        old_bound_dis = self.bound_dis
        self.bound_dis = set(old_bound_dis)
        self.bound_dis.add(term.di.di.name)
        try:
            self.visit(term.term)
        finally:
            self.bound_dis = old_bound_dis
        return term

    def visit_Admal(self, term: Admal):
        old_bound_ids = self.bound_ids
        self.bound_ids = set(old_bound_ids)
        self.bound_ids.add(term.id.id.name)
        try:
            self.visit(term.context)
        finally:
            self.bound_ids = old_bound_ids
        return term

    def visit_Cons(self, term: Cons):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Cons)
        self.visit(term.term)
        self.visit(term.context)
        return term

    # -- first-order nodes -------------------------------------------------
    #
    # A first-order variable is not a proof variable, so it does not join
    # bound_dis/bound_ids -- those decide whether a leaf reads as a reference
    # to an assumption, which an individual never is.

    def visit_LamdaFO(self, term: LamdaFO):
        self.lines.append(
            self._indent_str()
            + _fill(self.semantic.LamdaFO, var=term.var, sort=str(term.sort))
        )
        self.visit(term.term)
        return term

    def visit_DestructTermsPairFO(self, term: DestructTermsPairFO):
        self.lines.append(
            self._indent_str()
            + _fill(self.semantic.DestructTermsPairFO, var=term.var, sort=str(term.sort))
        )
        self.visit(term.context)
        return term

    def visit_ConsFO(self, term: ConsFO):
        self.lines.append(
            self._indent_str() + _fill(self.semantic.ConsFO, witness=str(term.fo_term))
        )
        self.visit(term.context)
        return term

    def visit_TermsPairFO(self, term: TermsPairFO):
        self.lines.append(
            self._indent_str() + _fill(self.semantic.TermsPairFO, witness=str(term.witness))
        )
        self.visit(term.term)
        return term

    def visit_Goal(self, term: Goal):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Goal + f"{self._render_prop(term.prop)}")
        return term
    
    def visit_Laog(self, term: Laog):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Laog + f"{self._render_prop(term.prop)}")
        return term
    
    def visit_Deleg(self, term: Deleg):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Deleg + f"{self._render_prop(term.prop)}")
        return term
    
    def visit_Geled(self, term: Geled):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Geled + f"{self._render_prop(term.prop)}")
        return term

    def visit_DI(self, term: DI):
        indent_str = self._indent_str()
        if term.name in self.bound_dis:
            if not getattr(self.semantic, "tree_guides", False):
                indent_str = indent_str.removesuffix(self.semantic.indentation)
            self.lines.append(f"{indent_str}" + self._leaf_prefix(self.semantic.DI, 1) + f"{term.name}")
        else:
            self.lines.append(f"{indent_str}" + self._leaf_prefix(self.semantic.DI, 0) + f"{self._render_prop(term.prop)}" + f" ({term.name})")
        return term

    def visit_ID(self, term: ID):
        indent_str = self._indent_str()
        if term.name in self.bound_ids:
            if not getattr(self.semantic, "tree_guides", False):
                indent_str = indent_str.removesuffix(self.semantic.indentation)
            self.lines.append(f"{indent_str}" + self._leaf_prefix(self.semantic.ID, 1) + f"{term.name}")
        else:
            self.lines.append(f"{indent_str}" + self._leaf_prefix(self.semantic.ID, 0) + f"{self._render_prop(term.prop)}" + f" ({term.name})")
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



def traverse_proof_term(semantic, term, lines, indent, *, declarations=None, decorations=None):
    if semantic is vanilla_rendering:
        _VanillaVisitor(semantic, lines, indent=indent).render(term)
    else:
        _NLVisitor(semantic, lines, indent=indent, declarations=declarations, decorations=decorations).visit(term)
