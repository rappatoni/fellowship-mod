"""Debate-graph compilation: Layers 2-3 of debate-graph-spec.org.

This module is being built along propositional-fragment-plan.org:

- M2 (this section): canonical proposition identity.  Node identity in the
  debate graph is the parsed Prop modulo alpha-equivalence, with negation
  unfolded: ``~A`` is definitionally ``A -> false`` in the calculus, and
  the graph must give both spellings one node (the NAF fixtures write the
  implication form; contrary-detection needs them identified).  Keys are
  internal to this module: they are stable identifiers, not display text.
- M3 (to come): the DebateGraph structure and the compiler from
  theta-normal terms.
"""

from core.ac.prop import (
    Prop, PTrue, PFalse, PSym, PApp, PNeg, PBin, PQuant, BinOp, PropError,
)


def _unfold_neg(p: Prop) -> Prop:
    """Structurally rewrite every ``PNeg(b)`` into ``b -> false``."""
    if isinstance(p, PNeg):
        return PBin(_unfold_neg(p.body), BinOp.IMP, PFalse())
    if isinstance(p, PBin):
        return PBin(_unfold_neg(p.left), p.op, _unfold_neg(p.right))
    if isinstance(p, PApp):
        return PApp(_unfold_neg(p.pred), p.arg)
    if isinstance(p, PQuant):
        return PQuant(p.quantifier, p.names, p.sort, _unfold_neg(p.body))
    # PTrue, PFalse, PSym carry no subpropositions.
    return p


def canonical_prop(text: str) -> str:
    """The graph-node identity key for a proposition string.

    Parse (Fellowship's printed syntax), unfold negation, and take the
    alpha-invariant canonical rendering.  Whitespace and parenthesisation
    differences, bound-variable names, and the ``~A`` / ``A -> false``
    spelling all map to one key; genuinely different propositions (e.g.
    ``A-(B->C)`` vs ``(A-B)->C``) map to different keys.

    Raises PropError on unparsable input: an unreadable proposition must
    never silently become its own node.
    """
    return _unfold_neg(Prop.parse(text)).canonical()


def display_prop(text: str) -> str:
    """The display normalization: parse and re-render with the printer.

    Unlike ``canonical_prop`` this keeps the author's negation spelling;
    it only normalizes whitespace and parenthesisation.
    """
    return str(Prop.parse(text))
