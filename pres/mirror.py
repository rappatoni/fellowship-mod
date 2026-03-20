"""Mirror rendering for proof terms.

Goal (step 1): mirror context syntax so contexts read left-to-right and mirror all angle brackets.
No whitespace is inserted.

This module intentionally does NOT mutate `node.pres` and does not depend on `pres.gen`.
"""

from __future__ import annotations

from core.ac.ast import (
    ProofTerm,
    Term,
    Context,
    Mu,
    Mutilde,
    Lamda,
    Admal,
    Cons,
    Sonc,
    Goal,
    Laog,
    ID,
    DI,
)


def render_mirror_context_linear(node: Context) -> str:
    """Render a Context, mirroring syntax and reading direction.

    In context position, typed variable occurrences (pyh-like) are read right-to-left,
    so they render as NAME:PROP (not PROP:NAME).
    """
    if isinstance(node, Mutilde):
        return (
            f">{render_mirror_linear(node.term)}||{render_mirror_context_linear(node.context)}"
            f"<.{node.di.name}:{node.prop}μ"
        )

    if isinstance(node, Admal):
        pyh = f"{node.id.id.name}:{node.id.prop}" if node.id.prop is not None else node.id.id.name
        return f"{render_mirror_context_linear(node.context)}.{pyh}λ"

    if isinstance(node, Cons):
        return f"{render_mirror_linear(node.term)}*{render_mirror_context_linear(node.context)}"

    if isinstance(node, Sonc):
        return f"{render_mirror_context_linear(node.context)}*{render_mirror_linear(node.term)}"

    if isinstance(node, Laog):
        return f"{node.number}:{node.prop}" if node.prop is not None else f"?{node.number}"

    if isinstance(node, ID):
        return f"{node.name}:{node.prop}" if node.prop is not None else node.name

    # Fallback: use term renderer.
    return render_mirror_linear(node)


def render_mirror_linear(node: ProofTerm) -> str:
    """Render proof term with mirrored context syntax and mirrored angle brackets (<->>)."""
    if isinstance(node, Mu):
        # mirror all angles: <...> becomes >...<
        # Binder ID prints like a pyh when typed: PROP:NAME
        binder = f"{node.prop}:{node.id.name}" if node.prop is not None else node.id.name
        return f"μ{binder}.>{render_mirror_linear(node.term)}||{render_mirror_context_linear(node.context)}<"

    if isinstance(node, Mutilde):
        # Context-mirrored syntax:
        #   mutilde: ">" term "||" context "<" "." pyh "μ"
        # and μ' becomes μ (read right-to-left).
        return (
            f">{render_mirror_linear(node.term)}||{render_mirror_linear(node.context)}"
            f"<.{node.di.name}:{node.prop}μ"
        )

    if isinstance(node, Lamda):
        return f"λ{node.di.di.name}:{node.di.prop}.{render_mirror_linear(node.term)}"

    # Context syntax mirroring (left-to-right)
    if isinstance(node, Admal):
        # Original syntax: λ pyh . context
        # Mirrored syntax: context . pyh λ
        # pyh prints as PROP:NAME
        return f"{render_mirror_linear(node.context)}.{node.id.prop}:{node.id.id.name}λ"

    if isinstance(node, Cons):
        return f"{render_mirror_linear(node.term)}*{render_mirror_linear(node.context)}"

    if isinstance(node, Sonc):
        # term-level sonc stays context*term
        return f"{render_mirror_linear(node.context)}*{render_mirror_linear(node.term)}"

    # leaves
    if isinstance(node, Goal):
        return f"{node.number}:{node.prop}" if node.prop is not None else f"?{node.number}"

    if isinstance(node, Laog):
        return f"{node.number}:{node.prop}" if node.prop is not None else f"?{node.number}"

    if isinstance(node, ID):
        return f"{node.name}:{node.prop}" if node.prop is not None else node.name

    if isinstance(node, DI):
        return f"{node.name}:{node.prop}" if node.prop is not None else node.name

    raise TypeError(f"Unhandled node type for mirror render: {type(node)}")

# --- mirror-tree ---
# Symmetric renderer (step 2): build a two-sided, centered layout.
# NOTE: This is a first implementation pass; it deliberately prefers correctness
# and non-overlap over perfect aesthetics.

class _SBox:
    """A text box with a designated anchor column (0..width)."""

    def __init__(self, lines: list[str], anchor: int):
        self.lines = lines
        self.width = max((len(s) for s in lines), default=0)
        self.height = len(lines)
        self.anchor = max(0, min(anchor, self.width))

    def normalized(self) -> "_SBox":
        """Pad lines to self.width."""
        if self.height == 0:
            return self
        if all(len(s) == self.width for s in self.lines):
            return self
        return _SBox([s + (" " * (self.width - len(s))) for s in self.lines], self.anchor)


def _pad_height(box: _SBox, h: int) -> _SBox:
    box = box.normalized()
    if box.height >= h:
        return box
    return _SBox(box.lines + [" " * box.width for _ in range(h - box.height)], box.anchor)


def _trim_left_common(lines: list[str]) -> list[str]:
    """Remove common leading whitespace from all non-empty lines."""
    nonempty = [ln for ln in lines if ln.strip()]
    if not nonempty:
        return [ln.rstrip() for ln in lines]
    m = min(len(ln) - len(ln.lstrip(" ")) for ln in nonempty)
    return [ln[m:].rstrip() for ln in lines]


def _compose_center(left: _SBox, mid: str, right: _SBox, *, gap: int = 1) -> _SBox:
    """Compose left and right around a center marker string.

    The returned box uses its anchor at the start of `mid`.
    """
    h = max(left.height, right.height)
    left = _pad_height(left, h).normalized()
    right = _pad_height(right, h).normalized()

    # Ensure we have at least `gap` spaces between (left, mid, right).
    # We model the center as: left + spaces + mid + spaces + right.
    spacer = " " * gap
    lines: list[str] = []
    for i in range(h):
        ln = left.lines[i].rstrip() + spacer + mid + spacer + right.lines[i].lstrip()
        lines.append(ln.rstrip())

    # anchor is after left + gap
    anchor = max((len(left.lines[i].rstrip()) for i in range(h)), default=0) + gap
    return _SBox(lines, anchor)


def _leaf_box(node: ProofTerm) -> _SBox:
    s = render_mirror_linear(node)
    return _SBox([s], anchor=0)


def _tree_box(node: ProofTerm) -> _SBox:
    # Atomic-ish nodes: single line.
    if isinstance(node, (Goal, Laog, ID, DI, Cons, Sonc, Lamda, Admal)):
        return _leaf_box(node)

    if isinstance(node, Mutilde):
        # Same-line opposition: left=context, right=term, mid="< >" (mirrored angle marker).
        left = _tree_box(node.context)
        right = _tree_box(node.term)
        # Use a visible separator reminiscent of the earlier sketch.
        return _compose_center(left, "< >", right, gap=1)

    if isinstance(node, Mu):
        binder = f"{node.prop}:{node.id.name}" if node.prop is not None else node.id.name
        head = _SBox([f"μ{binder}."], anchor=0)

        # Mirror μ body delimiters: > term || ctx < ; but tree places ctx left, term right.
        left = _tree_box(node.context)
        right = _tree_box(node.term)
        body = _compose_center(left, "> <", right, gap=1)

        # Stack head above body, keeping anchor aligned at 0.
        lines = head.lines + body.lines
        return _SBox(lines, anchor=0)

    raise TypeError(f"Unhandled node type for mirror-tree render: {type(node)}")


def render_mirror_tree(node: ProofTerm) -> str:
    """Symmetric tree-like view for mirrored proof terms."""
    box = _tree_box(node)
    return "\n".join(_trim_left_common(box.lines)).rstrip()
