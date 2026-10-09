from __future__ import annotations

import json
import logging
import re
from typing import Mapping

from core.ac.prop import (
    PApp,
    PBin,
    PFalse,
    PNeg,
    PQuant,
    PSym,
    PTrue,
    Prop,
    PropError,
    TApp,
    TSym,
)

logger = logging.getLogger(__name__)

_DECORATE_RE = re.compile(r"^decorate\s+(?P<name>[^\s:]+)\s*:\s*(?P<text>.+?)\s*\.?$")
_ARG_RE = re.compile(r"@arg(\d+)")


class DecorationError(ValueError):
    """Raised when a wrapper-side decoration command is malformed."""


def parse_decorate_command(command: str) -> tuple[str, str]:
    """Parse ``decorate NAME : "template".`` wrapper commands.

    The command is intentionally wrapper-only; callers should store the result
    rather than forwarding it to Fellowship.  Templates are double-quoted (a
    single quote is an ordinary character, README, *Command syntax*), use
    JSON string escaping - importer-generated ``.fspy`` files write them
    with ``json.dumps`` - and may contain positional placeholders @arg1,
    @arg2, ... .
    """
    match = _DECORATE_RE.match(command.strip())
    if match is None:
        raise DecorationError('Invalid decorate command. Use: decorate NAME : "template".')
    name = match.group("name").strip()
    text = match.group("text").strip()
    if len(text) >= 2 and text[0] == text[-1] == '"':
        try:
            text = json.loads(text)
        except json.JSONDecodeError as e:
            raise DecorationError(f"Invalid JSON-quoted decoration template: {e}") from e
    elif len(text) >= 2 and text[0] == text[-1] == "'":
        raise DecorationError("Single-quoted templates are gone: double-quote it, "
                              f'decorate {name} : "{text[1:-1]}".')
    if not name or not text:
        raise DecorationError('Invalid decorate command. Use: decorate NAME : "template".')
    return name, text


def render_declaration(
    name: str,
    declarations: Mapping[str, str] | None,
    decorations: Mapping[str, str] | None,
    connective_templates: Mapping[str, str] | None = None,
) -> str:
    """Render a declaration name using explicit and compositional decorations."""
    declarations = declarations or {}
    decorations = decorations or {}
    if name in decorations:
        prop = declarations.get(name)
        args = _declaration_template_args(prop, declarations, decorations, connective_templates) if prop else []
        return _apply_template(decorations[name], args)
    prop = declarations.get(name)
    if prop:
        return render_prop(prop, declarations, decorations, connective_templates)
    return name


def render_prop(
    prop: str | None,
    declarations: Mapping[str, str] | None,
    decorations: Mapping[str, str] | None,
    connective_templates: Mapping[str, str] | None = None,
) -> str:
    """Render a proposition string through the wrapper decoration registry."""
    if prop is None:
        return ""
    text = prop.strip()
    if not text:
        return text
    ast = _parse(text)
    if ast is None:
        return text
    return _render_ast(ast, declarations or {}, decorations or {}, connective_templates or {})


def _parse(text: str) -> Prop | None:
    """Parse a proposition, or return None if it cannot be read.

    Rendering must not bring down a whole argument, so an unreadable
    proposition falls back to its raw text.  It is logged rather than dropped
    silently -- an unparseable proposition here means the renderer is showing
    undecorated output, which is easy to mistake for a missing decoration.
    """
    try:
        return Prop.parse(text)
    except PropError as error:
        logger.debug("Falling back to raw text for proposition %r: %s", text, error)
        return None


def _declaration_template_args(
    prop: str,
    declarations: Mapping[str, str],
    decorations: Mapping[str, str],
    connective_templates: Mapping[str, str] | None = None,
) -> list[str]:
    ast = _parse(prop)
    if ast is None:
        return []
    connective_templates = connective_templates or {}

    def render(node) -> str:
        return _render_ast(node, declarations, decorations, connective_templates)

    if isinstance(ast, PBin):
        return [render(ast.left), render(ast.right)]
    if isinstance(ast, PNeg):
        return [render(ast.body)]
    if isinstance(ast, PQuant):
        return [render(ast.body)]
    if isinstance(ast, PApp):
        _, args = _flatten_application(ast)
        return [_render_term(arg, decorations) for arg in args]
    return []


def _flatten_application(prop: PApp) -> tuple[str, list]:
    """Split ``P x y`` into its head name and its argument terms, in order."""
    args = []
    node: Prop = prop
    while isinstance(node, PApp):
        args.append(node.arg)
        node = node.pred
    args.reverse()
    return _atom_name(node), args


def _atom_name(prop: Prop) -> str:
    """The decoration-registry key for an atomic proposition."""
    if isinstance(prop, PSym):
        return prop.name
    if isinstance(prop, PTrue):
        return "true"
    if isinstance(prop, PFalse):
        return "false"
    # A compound predicate head is not well-sorted in Fellowship, but rendering
    # should still show something rather than raise.
    return str(prop)


def _render_ast(
    ast: Prop,
    declarations: Mapping[str, str],
    decorations: Mapping[str, str],
    connective_templates: Mapping[str, str] | None = None,
) -> str:
    connective_templates = connective_templates or {}

    def render(node: Prop) -> str:
        return _render_ast(node, declarations, decorations, connective_templates)

    if isinstance(ast, (PSym, PTrue, PFalse)):
        name = _atom_name(ast)
        if name in decorations:
            return _apply_template(decorations[name], [])
        return name

    if isinstance(ast, PApp):
        head, arg_terms = _flatten_application(ast)
        args = [_render_term(arg, decorations) for arg in arg_terms]
        if head in decorations:
            return _apply_template(decorations[head], args)
        return " ".join([head, *args])

    def render_operand(node: Prop) -> str:
        """An operand, bracketed when it is a quantifier.

        A quantifier body extends maximally, so `for all x, P x -> Q` reads as
        though the arrow were inside it.  The proposition printer parenthesises
        for the same reason; prose needs it at least as much.
        """
        rendered = render(node)
        return f"({rendered})" if isinstance(node, PQuant) else rendered

    if isinstance(ast, PNeg):
        body = render_operand(ast.body)
        if "~" in connective_templates:
            return _apply_named_template(connective_templates["~"], {"body": body, "arg": body})
        return f"not {body}"

    if isinstance(ast, PQuant):
        body = render(ast.body)
        variables = ", ".join(ast.names)
        key = ast.quantifier.value
        if key in connective_templates:
            return _apply_named_template(
                connective_templates[key],
                {"vars": variables, "sort": str(ast.sort), "body": body, "arg": body},
            )
        opening = "for all" if key == "forall" else "for some"
        return f"{opening} {variables} of type {ast.sort}, {body}"

    if isinstance(ast, PBin):
        left = render_operand(ast.left)
        right = render_operand(ast.right)
        op = ast.op.value
        if op in connective_templates:
            return _apply_named_template(
                connective_templates[op],
                {"left": left, "right": right, "A": left, "B": right},
            )
        if op == "->":
            return f"{left} -> {right}"
        return f"{left}-{right}"

    raise DecorationError(f"Cannot render proposition node {type(ast).__name__}")


def _render_term(term, decorations: Mapping[str, str]) -> str:
    """Render a first-order term, decorating every symbol it contains.

    Recurses into applications so that a decoration for ``O`` also applies in
    ``S O``; a decoration names a thing, wherever that thing occurs.
    """
    if isinstance(term, TSym):
        if term.name in decorations:
            return _apply_template(decorations[term.name], [])
        return term.name
    if isinstance(term, TApp):
        fun = _render_term(term.fun, decorations)
        arg = _render_term(term.arg, decorations)
        # Mirrors the term printer: a nested application in argument position
        # keeps its parentheses.
        return f"{fun} ({arg})" if isinstance(term.arg, TApp) else f"{fun} {arg}"
    return str(term)


def _apply_template(template: str, args: list[str]) -> str:
    def repl(match: re.Match[str]) -> str:
        index = int(match.group(1)) - 1
        if 0 <= index < len(args):
            return args[index]
        return match.group(0)

    return _ARG_RE.sub(repl, template)


def _apply_named_template(template: str, values: Mapping[str, str]) -> str:
    rendered = template
    for key, value in values.items():
        rendered = rendered.replace(f"@{key}", value)
    return rendered
