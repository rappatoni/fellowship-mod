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
    """A text box with a designated anchor column.

    For mirror-tree, `anchor` is the column where the '<' of the command marker '< >'
    should be placed/aligned.
    """

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


def _shift_lines(lines: list[str], n: int) -> list[str]:
    if n <= 0:
        return lines
    return [(" " * n) + ln if ln else ln for ln in lines]


def _shift_box(box: _SBox, n: int) -> _SBox:
    if n <= 0:
        return box
    return _SBox(_shift_lines(box.lines, n), anchor=box.anchor + n)


def _align_box_anchor(box: _SBox, target_anchor: int) -> _SBox:
    """Shift box so that box.anchor == target_anchor (only shifts right)."""
    return _shift_box(box, max(0, target_anchor - box.anchor))

def _compose_tail(ctx_lines: list[str], term_lines: list[str], anchor_col: int) -> list[str]:
    """Compose continuation lines under a *fixed* anchor column.

    Key property: must NOT introduce additional global shifts; it must keep the anchor
    stable across nesting.

    Strategy:
    - Left part (context-ish) is right-aligned to end at anchor_col-1.
    - Right part (term-ish) starts at anchor_col+3 (roughly after '< >').
    """
    h = max(len(ctx_lines), len(term_lines))
    out: list[str] = []
    for i in range(h):
        l = (ctx_lines[i] if i < len(ctx_lines) else "").rstrip()
        r = (term_lines[i] if i < len(term_lines) else "").lstrip()
        if not l and not r:
            out.append("")
            continue

        # Left side: place so that it ends at anchor_col-1.
        if l:
            left_start = max(0, (anchor_col - 1) - len(l) + 1)
            left_part = (" " * left_start) + l
        else:
            left_part = ""

        # Ensure we have at least anchor_col spaces before the right side starts.
        min_len = anchor_col + 3  # skip over where '< >' would visually live
        if len(left_part) < min_len:
            left_part = left_part + (" " * (min_len - len(left_part)))

        line = (left_part + r).rstrip()
        out.append(line)

    return out

def _compose_command_row(ctx0: str, term0: str, anchor_col: int) -> tuple[str, int, int]:
    """Return (row, shift, anchor_out) where row contains '<' at anchor_out.

    We build: "| {ctx0} < > {term0} |" and shift the whole row right if ctx0 is too wide.
    """
    left_prefix = f"| {ctx0} "
    shift = max(0, len(left_prefix) - anchor_col)
    pad = max(0, anchor_col + shift - len(left_prefix))
    row = left_prefix + (" " * pad) + f"< > {term0} |"
    anchor_out = anchor_col + shift
    return row, shift, anchor_out


def _align_binder_and_row(binder: str, row: str, *, dot_col: int, lt_col: int) -> tuple[str, str]:
    """Pad binder so that its '.' sits above row's '<'."""
    if dot_col < 0 or lt_col < 0:
        return binder, row
    if dot_col == lt_col:
        return binder, row
    if dot_col < lt_col:
        return (" " * (lt_col - dot_col)) + binder, row
    # dot_col > lt_col: shift row right
    return binder, (" " * (dot_col - lt_col)) + row


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
    # Legacy/simple command row (no anchor alignment)
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

        # Desired global anchor is the '.' column of this binder.
        dot_col = binder.find(".")

        # First, align children to this anchor.
        ctxb = _align_box_anchor(ctxb, dot_col)
        termb = _align_box_anchor(termb, dot_col)
        ctx0 = ctxb.lines[0] if ctxb.lines else ""
        term0 = termb.lines[0] if termb.lines else ""

        row, shift, anchor_out = _compose_command_row(ctx0, term0, dot_col)
        if shift:
            binder = (" " * shift) + binder
            ctxb = _shift_box(ctxb, shift)
            termb = _shift_box(termb, shift)

        dot_col = dot_col + shift

        # Append tails from both sides, aligned to our resulting anchor.
        ctxb = _align_box_anchor(ctxb, anchor_out)
        termb = _align_box_anchor(termb, anchor_out)

        tail = _compose_tail(ctxb.lines[1:], termb.lines[1:], anchor_out)
        return _SBox([binder, row] + tail, anchor=anchor_out)

    if isinstance(ctx, Admal):
        # Mirrored syntax: context . pyh λ
        # For mirror-tree (step 2), treat Admal as the context-side analogue of Lamda:
        # emit the binder suffix line first, then a newline, then the underlying context.
        base = _ctx_box(ctx.context)
        pyh = _fmt_pyh(ctx.id.prop, ctx.id.id.name)
        return _SBox([f".{pyh}λ"] + base.lines, anchor=base.anchor)

    return _SBox([render_mirror_context_linear(ctx)], anchor=0)



def _term_box(t: Term) -> _SBox:
    """Render a Term for mirror-tree."""
    if isinstance(t, Mu):
        binder = f"μ{_fmt_pyh(t.prop, t.id.name)}."

        ctxb = _ctx_box(t.context)
        termb = _term_box(t.term)

        ctx0 = ctxb.lines[0] if ctxb.lines else ""
        term0 = termb.lines[0] if termb.lines else ""

        # Desired global anchor is the '.' column of this binder.
        dot_col = binder.rfind(".")

        # First, align children to this anchor.
        ctxb = _align_box_anchor(ctxb, dot_col)
        termb = _align_box_anchor(termb, dot_col)
        ctx0 = ctxb.lines[0] if ctxb.lines else ""
        term0 = termb.lines[0] if termb.lines else ""

        row, shift, anchor_out = _compose_command_row(ctx0, term0, dot_col)
        if shift:
            binder = (" " * shift) + binder
            ctxb = _shift_box(ctxb, shift)
            termb = _shift_box(termb, shift)
            dot_col = dot_col + shift
        else:
            dot_col = dot_col

        # Append tails (remaining lines from either side) below the command row, aligned to our anchor.
        ctxb = _align_box_anchor(ctxb, anchor_out)
        termb = _align_box_anchor(termb, anchor_out)

        tail = _compose_tail(ctxb.lines[1:], termb.lines[1:], anchor_out)
        return _SBox([binder, row] + tail, anchor=anchor_out)

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
