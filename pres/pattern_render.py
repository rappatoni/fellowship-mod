from dataclasses import dataclass
from typing import Callable, Mapping, Protocol

from core.ac.alt_structure import (
    match_alt_structure,
    match_alternative_counterexample_structure,
    match_application_structure,
    match_defeasible_warrant_structure,
    match_dual_application_structure,
    match_dual_defeasible_warrant_structure,
)
from core.ac.ast import ProofTerm


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
        header_template: str = "Die Prüfung, ob @prop zerfällt in folgende Fallgruppen:",
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
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, binder=alt.binder_name, index=0)]

        for index, element in enumerate(alt.elements, start=1):
            label = self.first_case_label if index == 1 else self.next_case_label
            lines.append(context.indent_str(1) + self._apply_template(label, prop=prop, binder=alt.binder_name, index=index))
            child_lines = context.render_node(element, 2)
            lines.extend(child_lines)

        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, binder: str, index: int) -> str:
        return template.replace("@prop", prop).replace("@binder", binder).replace("@index", str(index)).replace("@number", str(index))


class AlternativeCounterexamplesRenderer:
    name = "alternative_counterexamples"

    def __init__(
        self,
        *,
        header_template: str = "Für @prop ist notwendigerweise zu prüfen:",
        first_condition_label: str = "Prüfpunkt:",
        next_condition_label: str = "und Prüfpunkt:",
        child_indent_delta: int = 2,
    ):
        self.header_template = header_template
        self.first_condition_label = first_condition_label
        self.next_condition_label = next_condition_label
        self.child_indent_delta = child_indent_delta

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        alt = match_alternative_counterexample_structure(node)
        if alt is None:
            return None

        prop = context.render_prop(alt.prop)
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, binder=alt.binder_name, index=0)]

        for index, element in enumerate(alt.elements, start=1):
            label = self.first_condition_label if index == 1 else self.next_condition_label
            lines.append(context.indent_str(1) + self._apply_template(label, prop=prop, binder=alt.binder_name, index=index))
            lines.extend(context.render_node(element, self.child_indent_delta))

        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, binder: str, index: int) -> str:
        return template.replace("@prop", prop).replace("@binder", binder).replace("@index", str(index)).replace("@number", str(index))


class ApplicationRenderer:
    name = "application"

    def __init__(
        self,
        *,
        header_template: str = "Zur Prüfung von @prop (@binder) ist hinreichend, dass @arg_prop",
        reason_label: str = "weil",
        separator_label: str = "und",
        end_label: str | None = "Prüfung @binder abgeschlossen",
    ):
        self.header_template = header_template
        self.reason_label = reason_label
        self.separator_label = separator_label
        self.end_label = end_label

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        application = match_application_structure(node)
        if application is None:
            return None

        prop = context.render_prop(application.prop)
        arg_prop = context.render_prop(application.argument_prop)
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, arg_prop=arg_prop, binder=application.binder_name)]
        lines.append(context.indent_str(1) + self._apply_template(self.reason_label, prop=prop, arg_prop=arg_prop, binder=application.binder_name))
        lines.extend(context.render_node(application.function, 2))
        lines.append(context.indent_str(1) + self._apply_template(self.separator_label, prop=prop, arg_prop=arg_prop, binder=application.binder_name))
        lines.extend(context.render_node(application.argument, 2))
        if self.end_label is not None:
            lines.append(context.indent_str(1) + self._apply_template(self.end_label, prop=prop, arg_prop=arg_prop, binder=application.binder_name))
        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, arg_prop: str, binder: str) -> str:
        return template.replace("@prop", prop).replace("@A", prop).replace("@arg_prop", arg_prop).replace("@B", arg_prop).replace("@binder", binder)


class DualApplicationRenderer:
    name = "dual_application"

    def __init__(
        self,
        *,
        header_template: str = "Zur Prüfung von @prop (@binder) ist notwendig, dass @condition_prop",
        reason_label: str = "weil",
        end_label: str | None = "Prüfung @binder abgeschlossen.",
        warrant_indent_delta: int = 2,
        condition_indent_delta: int = 2,
    ):
        self.header_template = header_template
        self.reason_label = reason_label
        self.end_label = end_label
        self.warrant_indent_delta = warrant_indent_delta
        self.condition_indent_delta = condition_indent_delta

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        application = match_dual_application_structure(node)
        if application is None:
            return None

        prop = context.render_prop(application.prop)
        condition_prop = context.render_prop(application.condition_prop)
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, condition_prop=condition_prop, binder=application.binder_name)]
        lines.append(context.indent_str(1) + self._apply_template(self.reason_label, prop=prop, condition_prop=condition_prop, binder=application.binder_name))
        lines.extend(context.render_node(application.warrant, self.warrant_indent_delta))
        lines.extend(context.render_node(application.condition, self.condition_indent_delta))
        if self.end_label is not None:
            lines.append(context.indent_str(1) + self._apply_template(self.end_label, prop=prop, condition_prop=condition_prop, binder=application.binder_name))
        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, condition_prop: str, binder: str) -> str:
        return template.replace("@prop", prop).replace("@A", prop).replace("@condition_prop", condition_prop).replace("@B", condition_prop).replace("@binder", binder)


class DefeasibleWarrantRenderer:
    name = "defeasible_warrant"

    def __init__(
        self,
        *,
        header_template: str = "Für @prop spricht:",
        exception_label: str = "aber",
        support_indent_delta: int = 2,
        exception_indent_delta: int = 2,
    ):
        self.header_template = header_template
        self.exception_label = exception_label
        self.support_indent_delta = support_indent_delta
        self.exception_indent_delta = exception_indent_delta

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        warrant = match_defeasible_warrant_structure(node)
        if warrant is None:
            return None

        prop = context.render_prop(warrant.prop)
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, binder=warrant.binder_name)]
        lines.extend(context.render_node(warrant.support, self.support_indent_delta))
        lines.append(context.indent_str(1) + self._apply_template(self.exception_label, prop=prop, binder=warrant.binder_name))
        lines.extend(context.render_node(warrant.exception, self.exception_indent_delta))
        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, binder: str) -> str:
        return template.replace("@prop", prop).replace("@binder", binder)


class DualDefeasibleWarrantRenderer:
    name = "dual_defeasible_warrant"

    def __init__(
        self,
        *,
        header_template: str = "gegen @prop spricht:",
        requirement_template: str = "aber",
        requirement_indent_delta: int = 2,
        support_indent_delta: int = 2,
    ):
        self.requirement_template = requirement_template
        self.header_template = header_template
        self.requirement_indent_delta = requirement_indent_delta
        self.support_indent_delta = support_indent_delta

    def try_render(self, node: ProofTerm, context: PatternRenderContext) -> PatternRenderResult | None:
        warrant = match_dual_defeasible_warrant_structure(node)
        if warrant is None:
            return None

        prop = context.render_prop(warrant.prop)
        lines = [context.indent_str() + self._apply_template(self.header_template, prop=prop, binder=warrant.binder_name)]
        lines.extend(context.render_node(warrant.requirement, self.requirement_indent_delta))
        lines.append(context.indent_str(1) + self._apply_template(self.requirement_template, prop=prop, binder=warrant.binder_name))
        lines.extend(context.render_node(warrant.support, self.support_indent_delta))
        return PatternRenderResult(lines=lines)

    @staticmethod
    def _apply_template(template: str, *, prop: str, binder: str) -> str:
        return template.replace("@prop", prop).replace("@binder", binder)
