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


def render_mirror_linear(node: ProofTerm) -> str:
    """Render proof term with mirrored context syntax and mirrored angle brackets (<->>)."""
    if isinstance(node, Mu):
        # mirror all angles: <...> becomes >...<
        # Binder ID prints like a pyh when typed: PROP:NAME
        binder = f"{node.prop}:{node.id.name}" if node.prop is not None else node.id.name
        return f"μ{binder}.>{render_mirror_linear(node.term)}||{render_mirror_linear(node.context)}<"

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
