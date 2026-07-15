from __future__ import annotations

import json
import re
from dataclasses import dataclass
from typing import Mapping


_DECORATE_RE = re.compile(r"^decorate\s+(?P<name>[^\s:]+)\s*:\s*(?P<text>.+?)\s*\.?$")
_TOKEN_RE = re.compile(r"\s*(->|[()~\-]|[^\s()~\-]+)")
_ARG_RE = re.compile(r"@arg(\d+)")


class DecorationError(ValueError):
    """Raised when a wrapper-side decoration command is malformed."""


@dataclass(frozen=True)
class _Name:
    value: str


@dataclass(frozen=True)
class _App:
    head: str
    args: tuple[_Name, ...]


@dataclass(frozen=True)
class _Unary:
    op: str
    body: "_Prop"


@dataclass(frozen=True)
class _Binary:
    op: str
    left: "_Prop"
    right: "_Prop"


_Prop = _Name | _App | _Unary | _Binary


def parse_decorate_command(command: str) -> tuple[str, str]:
    """Parse ``decorate NAME : 'template'.`` wrapper commands.

    The command is intentionally wrapper-only; callers should store the result
    rather than forwarding it to Fellowship.  Templates may be quoted with
    single or double quotes and may contain positional placeholders @arg1,
    @arg2, ... .  Double-quoted templates use JSON string escaping because
    importer-generated ``.fspy`` files render them with ``json.dumps``.
    """
    match = _DECORATE_RE.match(command.strip())
    if match is None:
        raise DecorationError("Invalid decorate command. Use: decorate NAME : 'template'.")
    name = match.group("name").strip()
    text = match.group("text").strip()
    if len(text) >= 2 and text[0] == text[-1] == '"':
        try:
            text = json.loads(text)
        except json.JSONDecodeError as e:
            raise DecorationError(f"Invalid JSON-quoted decoration template: {e}") from e
    elif len(text) >= 2 and text[0] == text[-1] == "'":
        text = text[1:-1]
    if not name or not text:
        raise DecorationError("Invalid decorate command. Use: decorate NAME : 'template'.")
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
    try:
        parser = _Parser(text)
        ast = parser.parse()
        parser.expect_end()
    except DecorationError:
        return text
    return _render_ast(ast, declarations or {}, decorations or {}, connective_templates or {})


def _declaration_template_args(
    prop: str,
    declarations: Mapping[str, str],
    decorations: Mapping[str, str],
    connective_templates: Mapping[str, str] | None = None,
) -> list[str]:
    try:
        parser = _Parser(prop)
        ast = parser.parse()
        parser.expect_end()
    except DecorationError:
        return []
    connective_templates = connective_templates or {}
    if isinstance(ast, _Binary) and ast.op == "->":
        return [_render_ast(ast.left, declarations, decorations, connective_templates), _render_ast(ast.right, declarations, decorations, connective_templates)]
    if isinstance(ast, _Binary) and ast.op == "-":
        return [_render_ast(ast.left, declarations, decorations, connective_templates), _render_ast(ast.right, declarations, decorations, connective_templates)]
    if isinstance(ast, _Unary):
        return [_render_ast(ast.body, declarations, decorations, connective_templates)]
    if isinstance(ast, _App):
        return [_render_term(arg.value, decorations) for arg in ast.args]
    return []


def _render_ast(
    ast: _Prop,
    declarations: Mapping[str, str],
    decorations: Mapping[str, str],
    connective_templates: Mapping[str, str] | None = None,
) -> str:
    connective_templates = connective_templates or {}
    if isinstance(ast, _Name):
        if ast.value in decorations:
            return _apply_template(decorations[ast.value], [])
        return ast.value
    if isinstance(ast, _App):
        args = [_render_term(arg.value, decorations) for arg in ast.args]
        if ast.head in decorations:
            return _apply_template(decorations[ast.head], args)
        return " ".join([ast.head, *args])
    if isinstance(ast, _Unary):
        body = _render_ast(ast.body, declarations, decorations, connective_templates)
        if ast.op in connective_templates:
            return _apply_named_template(connective_templates[ast.op], {"body": body, "arg": body})
        if ast.op == "~":
            return f"not {body}"
        return f"{ast.op}{body}"
    left = _render_ast(ast.left, declarations, decorations, connective_templates)
    right = _render_ast(ast.right, declarations, decorations, connective_templates)
    if ast.op in connective_templates:
        return _apply_named_template(connective_templates[ast.op], {"left": left, "right": right, "A": left, "B": right})
    if ast.op == "->":
        return f"{left} -> {right}"
    return f"{left}-{right}"


def _render_term(name: str, decorations: Mapping[str, str]) -> str:
    if name in decorations:
        return _apply_template(decorations[name], [])
    return name


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


class _Parser:
    def __init__(self, text: str):
        self.tokens = self._tokenize(text)
        self.pos = 0

    def _tokenize(self, text: str) -> list[str]:
        tokens: list[str] = []
        index = 0
        while index < len(text):
            match = _TOKEN_RE.match(text, index)
            if match is None:
                raise DecorationError(f"Cannot tokenize proposition near: {text[index:]!r}")
            tokens.append(match.group(1))
            index = match.end()
        return tokens

    def parse(self) -> _Prop:
        return self._parse_implication()

    def expect_end(self) -> None:
        if self._peek() is not None:
            raise DecorationError(f"Unexpected proposition token: {self._peek()!r}")

    def _peek(self) -> str | None:
        if self.pos >= len(self.tokens):
            return None
        return self.tokens[self.pos]

    def _consume(self, token: str | None = None) -> str:
        current = self._peek()
        if current is None:
            raise DecorationError("Unexpected end of proposition")
        if token is not None and current != token:
            raise DecorationError(f"Expected {token!r}, got {current!r}")
        self.pos += 1
        return current

    def _parse_implication(self) -> _Prop:
        left = self._parse_minus()
        if self._peek() == "->":
            self._consume("->")
            right = self._parse_implication()
            return _Binary("->", left, right)
        return left

    def _parse_minus(self) -> _Prop:
        left = self._parse_unary()
        if self._peek() == "-":
            self._consume("-")
            right = self._parse_unary()
            return _Binary("-", left, right)
        return left

    def _parse_unary(self) -> _Prop:
        if self._peek() == "~":
            self._consume("~")
            return _Unary("~", self._parse_unary())
        if self._peek() == "(":
            self._consume("(")
            inner = self._parse_implication()
            self._consume(")")
            return inner
        return self._parse_application()

    def _parse_application(self) -> _Prop:
        names: list[str] = []
        while True:
            token = self._peek()
            if token is None or token in {"->", "-", "~", "(", ")"}:
                break
            names.append(self._consume())
        if not names:
            raise DecorationError(f"Expected proposition atom, got {self._peek()!r}")
        if len(names) == 1:
            return _Name(names[0])
        return _App(names[0], tuple(_Name(arg) for arg in names[1:]))