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
    """Render a Context, mirroring syntax.

    IMPORTANT: In the original grammar, pyh := id:prop and is printed as PROP:NAME.
    In mirror-tree we keep pyhs as PROP:NAME on the context side.
    """
    if isinstance(node, Mutilde):
        return (
            f">{render_mirror_linear(node.term)}||{render_mirror_context_linear(node.context)}"
            f"<.{node.di.name}:{node.prop}μ"
        )

    if isinstance(node, Admal):
        pyh = f"{node.id.prop}:{node.id.id.name}" if node.id.prop is not None else node.id.id.name
        return f"{render_mirror_context_linear(node.context)}.{pyh}λ"

    if isinstance(node, Cons):
        return f"{render_mirror_linear(node.term)}*{render_mirror_context_linear(node.context)}"

    if isinstance(node, Sonc):
        return f"{render_mirror_context_linear(node.context)}*{render_mirror_linear(node.term)}"

    if isinstance(node, Laog):
        return f"{node.number}:{node.prop}" if node.prop is not None else f"?{node.number}"

    if isinstance(node, ID):
        # In context position, an ID leaf stands for a pyh-like variable: PROP:NAME
        return f"{node.prop}:{node.name}" if node.prop is not None else node.name

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

    This enforces a *shared* center column across all composed lines by padding the left
    side to the maximum left width.

    The returned box uses its anchor at the start of `mid`.
    """
    h = max(left.height, right.height)
    left = _pad_height(left, h).normalized()
    right = _pad_height(right, h).normalized()

    # Shared null-column: use the maximum visible left width across lines.
    left_w = max((len(ln.rstrip()) for ln in left.lines), default=0)

    spacer = " " * gap
    lines: list[str] = []
    for i in range(h):
        l = left.lines[i].rstrip()
        l = l + (" " * (left_w - len(l)))
        r = right.lines[i].lstrip()
        ln = l + spacer + mid + spacer + r
        lines.append(ln.rstrip())

    anchor = left_w + gap
    return _SBox(lines, anchor)


def _leaf_box(node: ProofTerm) -> _SBox:
    s = render_mirror_linear(node)
    return _SBox([s], anchor=0)


def _fmt_pyh(prop: str | None, name: str) -> str:
    # pyh := prop:id  => PROP:NAME
    return f"{prop}:{name}" if prop is not None else name


def _fmt_hyp(name: str, prop: str | None) -> str:
    # hyp := di:prop (term-side), allow NAME:PROP
    return f"{name}:{prop}" if prop is not None else name


def _cmd_row(left: str, right: str) -> str:
    # Command row representation: | left < > right |
    return f"| {left} < > {right} |"


def _ctx_box(ctx: Context) -> _SBox:
    """Render a Context for mirror-tree.

    Context nodes may include binders (μ~) and pyh suffixes (Admal) which must appear
    as their own lines, *above* the command row they bind in the underlying calculus.
    """
    if isinstance(ctx, Mutilde):
        # Binder line for μ~ is shown first: .PROP:NAMEμ
        binder = f".{_fmt_pyh(ctx.prop, ctx.di.name)}μ"

        # μ~ binds a command whose *context* is ctx.context and whose *term* is ctx.term
        ctxb = _ctx_box(ctx.context)
        termb = _term_box(ctx.term)
        term0 = termb.lines[0] if termb.lines else ""
        ctx0 = ctxb.lines[0] if ctxb.lines else ""

        # Command row is always: | context < > term |
        row = _cmd_row(ctx0, term0)

        # Append tails from both sides.
        tail_ctx = ctxb.lines[1:]
        tail_term = termb.lines[1:]
        tail_h = max(len(tail_ctx), len(tail_term))
        tail: list[str] = []
        for i in range(tail_h):
            l = tail_ctx[i] if i < len(tail_ctx) else ""
            r = tail_term[i] if i < len(tail_term) else ""
            if l.strip() and r.strip():
                tail.append(f"  {l}   {r}")
            elif l.strip():
                tail.append(f"  {l}")
            elif r.strip():
                tail.append(f"  {r}")

        return _SBox([binder, row] + tail, anchor=0)

    if isinstance(ctx, Admal):
        # Mirrored syntax: context . pyh λ
        # For mirror-tree (step 2), treat Admal as the context-side analogue of Lamda:
        # emit the binder suffix line first, then a newline, then the underlying context.
        base = _ctx_box(ctx.context)
        pyh = _fmt_pyh(ctx.id.prop, ctx.id.id.name)
        return _SBox([f".{pyh}λ"] + base.lines, anchor=0)

    return _SBox([render_mirror_context_linear(ctx)], anchor=0)



def _term_box(t: Term) -> _SBox:
    """Render a Term for mirror-tree."""
    if isinstance(t, Mu):
        binder = f"μ{_fmt_pyh(t.prop, t.id.name)}."

        ctxb = _ctx_box(t.context)
        termb = _term_box(t.term)

        ctx0 = ctxb.lines[0] if ctxb.lines else ""
        term0 = termb.lines[0] if termb.lines else ""
        row = _cmd_row(ctx0, term0)

        # Append tails (remaining lines from either side) below the command row.
        tail_ctx = ctxb.lines[1:]
        tail_term = termb.lines[1:]
        tail_h = max(len(tail_ctx), len(tail_term))
        tail: list[str] = []
        for i in range(tail_h):
            l = tail_ctx[i] if i < len(tail_ctx) else ""
            r = tail_term[i] if i < len(tail_term) else ""
            if l.strip() and r.strip():
                tail.append(f"  {l}   {r}")
            elif l.strip():
                tail.append(f"  {l}")
            elif r.strip():
                tail.append(f"  {r}")

        return _SBox([binder, row] + tail, anchor=0)

    # Default: single-line via linear mirror.
    return _SBox([render_mirror_linear(t)], anchor=0)

def _tree_box(node: ProofTerm) -> _SBox:
    """Entry point for mirror-tree rendering.

    Mirror-tree is a binder+command view:
    - μ (Mu) is a Term binder over a command.
    - μ~ (Mutilde) is a Context binder over a command.
    """
    if isinstance(node, Mu):
        return _term_box(node)

    if isinstance(node, Mutilde):
        return _ctx_box(node)

    if isinstance(node, Term):
        return _term_box(node)

    if isinstance(node, Context):
        return _ctx_box(node)

    # Fallback: single-line.
    return _leaf_box(node)


def render_mirror_tree(node: ProofTerm) -> str:
    """Symmetric tree-like view for mirrored proof terms."""
    box = _tree_box(node)
    return "\n".join(_trim_left_common(box.lines)).rstrip()
