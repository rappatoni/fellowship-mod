from __future__ import annotations

import re


_PROP_TOKEN_RE = re.compile(r"\s*(->|[()~\-]|[^\s()~\-]+)")


class PropRenderError(ValueError):
    """Raised when an internal proposition string cannot be rendered."""


def prop_to_command(prop: str | None) -> str | None:
    """Render an internal AC proposition for Fellowship command syntax.

    Internally, proof terms and Fellowship machine output render predicate
    applications with whitespace, e.g. ``Child Bob`` or ``Father Rich Bob``.
    The command parser requires bracketed applications, e.g. ``Child[Bob]``.
    This renderer preserves boolean/propositional atoms and converts simple
    predicate applications while respecting ``~``, ``->``, ``-`` and grouping.
    """
    if prop is None:
        return None
    text = prop.strip()
    if not text:
        return text
    parser = _PropCommandRenderer(text)
    rendered = parser.parse()
    parser.expect_end()
    return rendered


class _PropCommandRenderer:
    def __init__(self, text: str):
        self.text = text
        self.tokens = self._tokenize(text)
        self.pos = 0

    def _tokenize(self, text: str) -> list[str]:
        tokens: list[str] = []
        index = 0
        while index < len(text):
            match = _PROP_TOKEN_RE.match(text, index)
            if match is None:
                raise PropRenderError(f"Cannot tokenize proposition near: {text[index:]!r}")
            token = match.group(1)
            tokens.append(token)
            index = match.end()
        return tokens

    def parse(self) -> str:
        return self._parse_implication()

    def expect_end(self) -> None:
        if self._peek() is not None:
            raise PropRenderError(f"Unexpected proposition token: {self._peek()!r}")

    def _peek(self) -> str | None:
        if self.pos >= len(self.tokens):
            return None
        return self.tokens[self.pos]

    def _consume(self, token: str | None = None) -> str:
        current = self._peek()
        if current is None:
            raise PropRenderError("Unexpected end of proposition")
        if token is not None and current != token:
            raise PropRenderError(f"Expected {token!r}, got {current!r}")
        self.pos += 1
        return current

    def _parse_implication(self) -> str:
        left = self._parse_minus()
        if self._peek() == "->":
            self._consume("->")
            right = self._parse_implication()
            return f"{left} -> {right}"
        return left

    def _parse_minus(self) -> str:
        left = self._parse_unary()
        if self._peek() == "-":
            self._consume("-")
            right = self._parse_unary()
            return f"{left}-{right}"
        return left

    def _parse_unary(self) -> str:
        if self._peek() == "~":
            self._consume("~")
            return f"~{self._parse_unary()}"
        if self._peek() == "(":
            self._consume("(")
            inner = self._parse_implication()
            self._consume(")")
            return f"({inner})"
        return self._parse_application()

    def _parse_application(self) -> str:
        names: list[str] = []
        while True:
            token = self._peek()
            if token is None or token in {"->", "-", "~", "(", ")"}:
                break
            names.append(self._consume())
        if not names:
            raise PropRenderError(f"Expected proposition atom, got {self._peek()!r}")
        if len(names) == 1:
            return names[0]
        head, args = names[0], names[1:]
        return head + "".join(f"[{arg}]" for arg in args)