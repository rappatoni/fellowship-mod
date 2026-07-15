from dataclasses import dataclass
from typing import Callable, Mapping, Protocol

from core.ac.alt_structure import match_alt_structure
from core.ac.ast import ProofTerm, Term


@dataclass(frozen=True)
class PatternRenderResult:
    lines: list[str]


@dataclass(frozen=True)
class PatternRenderContext:
    indent: int
    indentation: str
    declarations: Mapping[str, str]
    decorations: Mapping[str, str]
    render_prop: Callable[[str | None], str]
    render_node: Callable[[ProofTerm, int], list[str]]

    def indent_str(self, delta: int = 0) -> str:
        return self.indentation * (self.indent + delta)


class PatternRenderer(Protocol):
    name: str

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        ...


class PatternRenderingRegistry:
    def __init__(self, renderers=()):
        self.renderers = list(renderers)

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        for renderer in self.renderers:
            result = renderer.try_render(node, context)
            if result is not None:
                return result
        return None


class AlternativeCasesRenderer:
    name = "alternative_cases"

    def __init__(
        self,
        *,
        header_template: str = "Hinreichend für @prop ist",
        first_case_label: str = "Fallgruppe:",
        next_case_label: str = "oder Fallgruppe:",
    ):
        self.header_template = header_template
        self.first_case_label = first_case_label
        self.next_case_label = next_case_label

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        alt = match_alt_structure(node)
        if alt is None:
            return None

        prop = context.render_prop(alt.prop)
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, binder=alt.binder_name)]

        for index, element in enumerate(alt.elements):
            label = self.first_case_label if index == 0 else self.next_case_label
            lines.append(context.indent_str(1) + self._apply_template(label, prop=prop, binder=alt.binder_name))
            child_lines = context.render_node(element, 2)
            lines.extend(child_lines)

        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, binder: str) -> str:
        return template.replace("@prop", prop).replace("@binder", binder)