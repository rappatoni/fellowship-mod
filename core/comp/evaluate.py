"""Label-guided evaluation of debate terms (M6 of
propositional-fragment-plan.org; Layer 4 of call-by-onus.org).

The pipeline is the corrected Layer-4 picture: the debate term is compiled
(M3) and labelled (M4) *first*; evaluation is then plain reduction, with
the witness labelling sigma resolving exactly the critical pairs that the
support/attack scaffolds are.

A scaffold  mu alt:A.< mu _:A.<ORIG || alt> || mu'_:A.<... SCION ...> >
(mirrored on the context side) holds the critical pair
< Mu(affine) || Mutilde(affine) >:

- (mu<) discards the scion wing and keeps ORIG - the CBV choice;
- (>mu) discards the ORIG wing and keeps the scion - the CBN choice.

``resolve_scaffolds`` applies the choice sigma dictates at every scaffold
(the discarded wing's binder is affine by construction - the M8 side
condition of the critique - and the eta step afterwards is valid exactly
when the catch variable is uncaught, which is checked, not assumed).  The
result is fully normalized (normalize_strong: the four standard rules
under a fixed leftmost-outermost congruence order) under the base
strategy and classified by the spec-derived NF classifier.

Wing choice (site of proposition A; sigma consulted at the scion's target
statement):

    supporter IN            -> keep scion        attacker IN   -> keep attack wing
    supporter OUT           -> keep ORIG         attacker OUT  -> keep ORIG
    supporter UNDEC         -> credulous: scion  attacker UNDEC-> credulous: ORIG
                               skeptical: ORIG                    skeptical: attack wing

v1 limitation, recorded: when an attacker wins, the kept attack wing is
mu alt.< ?g:A || ATTACKER > - the stolen-value obligation stays open
rather than being replaced by the abort term, so a defeated debate
normalizes to an open-or-exception term, not necessarily to a pure
exception.  Adequacy is asserted as IN -> value, OUT -> not a value.
"""

from copy import deepcopy

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, ID, DI,
    first_order_node, FirstOrderNotSupported,
)
from core.comp.adf_label import grounded_labels
from core.comp.oracle_terms import (
    normalize_strong, classify_nf, _occurs,
)
from core.dc.debate_graph import (
    DebateGraph, canonical_prop, compile_debate, _match_scaffold,
    DebateCompileError,
)


class EvaluationRefused(DebateCompileError):
    """A case the fragment evaluator does not decide; refuse, never guess."""


_MODES = ("skeptical", "credulous")


def _wing_choice(role: str, label: str, mode: str) -> str:
    """"scion" or "orig" per the table in the module docstring."""
    if role == "supporter":
        if label == "IN":
            return "scion"
        if label == "OUT":
            return "orig"
        return "scion" if mode == "credulous" else "orig"
    if role == "attacker":
        if label == "IN":
            return "scion"
        if label == "OUT":
            return "orig"
        return "orig" if mode == "credulous" else "scion"
    raise EvaluationRefused(f"Unknown scaffold role {role!r}")


def _keep_orig(node, orig, alt):
    """(mu<) then eta: valid only if the catch variable is uncaught in ORIG."""
    caught = (
        _occurs(orig, ID, alt) if isinstance(node, Mu) else _occurs(orig, DI, alt)
    )
    if caught:
        raise EvaluationRefused(
            f"Scaffold catch variable '{alt}' occurs in the kept original "
            f"wing: a capture pattern, outside the acyclic fragment."
        )
    return deepcopy(orig)


def _keep_scion_support(node, scion, alt):
    """(>mu) then eta for a support scaffold: scion must not catch alt."""
    caught = (
        _occurs(scion, ID, alt) if isinstance(node, Mu) else _occurs(scion, DI, alt)
    )
    if caught:
        raise EvaluationRefused(
            f"Scaffold catch variable '{alt}' occurs in the supporter wing: "
            f"a capture pattern, outside the acyclic fragment."
        )
    return deepcopy(scion)


def _keep_attack_wing(node):
    """(>mu) only: the attack wing catches alt, so the binder stays.

    For a term-side scaffold mu alt.<W1 || mu'_.<g || CTX>> this yields
    mu alt.< g || CTX >; dually for the context side.
    """
    result = deepcopy(node)
    if isinstance(node, Mu):
        wing = node.context  # Mutilde(_, A, goal, scion_ctx)
        result.term = deepcopy(wing.term)
        result.context = deepcopy(wing.context)
    else:
        wing = node.term  # Mu(_, A, scion_t, laog)
        result.term = deepcopy(wing.term)
        result.context = deepcopy(wing.context)
    return result


def resolve_scaffolds(body: ProofTerm, labels, mode: str) -> ProofTerm:
    """Replace every scaffold by its sigma-chosen wing, innermost-last."""
    if mode not in _MODES:
        raise ValueError(f"mode must be one of {_MODES}, got {mode!r}")

    def walk(node):
        if not isinstance(node, ProofTerm):
            return node
        match = _match_scaffold(node)
        if match is not None:
            role, site_side, prop, orig, scion_raw, alt, scion_kind = match
            target_side = (
                site_side if role == "supporter"
                else ("context" if site_side == "term" else "term")
            )
            statement = (canonical_prop(prop), target_side)
            label = labels.get(statement)
            if label is None:
                raise EvaluationRefused(
                    f"No label for scaffold issue {statement}; the labelling "
                    f"and the term disagree about the debate's shape."
                )
            choice = _wing_choice(role, label, mode)
            if choice == "orig":
                return walk(_keep_orig(node, orig, alt))
            if role == "supporter":
                return walk(_keep_scion_support(node, scion_raw, alt))
            return walk(_keep_attack_wing(node))
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child))
        return node

    return walk(deepcopy(body))


def evaluate_debate(
    body: ProofTerm,
    name: str,
    *,
    strict_names=None,
    mode: str = "skeptical",
    base: str = "cbn",
):
    """Compile, label, resolve, normalize, classify.

    Returns (normal_form, nf_class, labels, graph).  ``base`` is the
    strategy for critical pairs sigma does not decide.
    """
    found = first_order_node(body)
    if found is not None:
        raise FirstOrderNotSupported("Debate evaluation", found)
    graph = compile_debate(body, name, strict_names=strict_names)
    labels = grounded_labels(graph)
    resolved = resolve_scaffolds(body, labels, mode)
    normal_form = normalize_strong(resolved, strategy=base)
    return normal_form, classify_nf(normal_form), labels, graph
