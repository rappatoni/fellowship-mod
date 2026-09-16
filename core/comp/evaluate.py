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

Which sigma (the witness-labelling discipline, tasks.org
aida-credulous-witness-labelling, replacing the earlier per-scaffold
policy):

    semantics S in {grounded, complete, preferred, stable}; default for
    evaluation is "preferred", the classical home of credulous/skeptical.

    credulous : sigma := a labelling of S in which the ISSUE (the
                debate's root statement) is IN.  Which one only affects
                the normal form (which supporters and presumptions
                survive), never the verdict.  By default the first such
                labelling in the canonical numbering `label ARG S`
                prints; ``witness=N`` picks labelling N of that numbering
                (refused if its issue is not IN); ``evaluate_witnesses``
                evaluates under every one.  Under a two-valued semantics
                every scaffold is then decided by sigma and no tiebreak
                is used.  If no labelling of S makes the issue IN, the
                issue is credulously rejected: sigma := grounded and the
                residue is resolved skeptically - it must NOT be resolved
                with the credulous tiebreak, which is exactly the unsound
                local policy (see TestLocalPolicyWasUnsound).
    skeptical : sigma := the statement-wise intersection of all labellings
                of S (UNDEC where they disagree); residue resolved
                skeptically.

Wing choice at a scaffold (site of proposition A; sigma consulted at the
scion's target statement); the UNDEC rows are the tiebreak, reached only
when sigma itself leaves the statement undecided:

    supporter IN            -> keep scion        attacker IN   -> keep attack wing
    supporter OUT           -> keep ORIG         attacker OUT  -> keep ORIG
    supporter UNDEC         -> credulous: scion  attacker UNDEC-> credulous: ORIG
                               skeptical: ORIG                    skeptical: attack wing

What a defeated site holds: when an attacker wins, the kept attack wing
is mu b.< ?g:A || ATTACKER > with b unused - the winning refutation
facing the site's own indeterminate under an affine binder.  That is
the correct term, not a limitation (see debate-graph-spec.org, Semantics
subsection, corrected 2026-09-16): the site is not instantiated, and a
defeated presumption has no value to put in the slot.  Consequently the
adequacy claim on the OUT side is "not a value"; "a closed exception at
the root" would need the throw to propagate through axiom-headed spines
(task aida-abort-propagation), which the standard rules do not do.
"""

from copy import deepcopy

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, ID, DI,
    first_order_node, FirstOrderNotSupported,
)
from core.comp.adf_label import (
    grounded_labels, labellings, intersection_labelling, SEMANTICS,
)
from core.comp.oracle_terms import (
    normalize_strong, classify_nf, _occurs, check_conservativity,
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


def resolve_scaffolds(body: ProofTerm, labels, mode: str, strict_names=(),
                      trace=None) -> ProofTerm:
    """Replace every scaffold by its sigma-chosen wing, innermost-last.

    ``mode`` is the TIEBREAK for statements ``labels`` leaves UNDEC; the
    choice of ``labels`` itself is ``witness_labelling``'s job.
    ``strict_names`` lets the matcher recognise the primitive contrariness
    term.  ``trace``, if a list, receives (statement, label) for every
    scaffold consulted - tests use it to check that under a two-valued
    witness no tiebreak was needed."""
    if mode not in _MODES:
        raise ValueError(f"mode must be one of {_MODES}, got {mode!r}")
    strict_names = set(strict_names or ())

    def walk(node):
        if not isinstance(node, ProofTerm):
            return node
        match = _match_scaffold(node, strict_names)
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
            if trace is not None:
                trace.append((statement, label))
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


def issue_of(body: ProofTerm):
    """The debate's issue: the root statement (proposition, side)."""
    if isinstance(body, Mu):
        return (canonical_prop(body.prop), "term")
    if isinstance(body, Mutilde):
        return (canonical_prop(body.prop), "context")
    raise EvaluationRefused(
        f"Cannot read the issue off a {type(body).__name__} root; expected a mu/mu' binder."
    )


def _candidates(graph, semantics):
    if semantics not in SEMANTICS:
        raise ValueError(f"semantics must be one of {SEMANTICS}, got {semantics!r}")
    candidates = labellings(graph, semantics)
    if not candidates:
        raise EvaluationRefused(
            f"No labelling exists under {semantics} semantics for this debate; "
            f"choose another semantics."
        )
    return candidates


def accepting_witnesses(graph, issue, semantics: str = "preferred"):
    """[(number, sigma)] for every labelling of ``semantics`` with the
    issue IN, numbered as `label ARG semantics` numbers them (1-based)."""
    return [(i, sigma) for i, sigma in enumerate(_candidates(graph, semantics), 1)
            if sigma.get(issue) == "IN"]


def witness_labelling(graph, issue, mode: str, semantics: str = "preferred",
                      witness=None):
    """Choose sigma per the module docstring.  Returns (sigma, tiebreak).

    ``witness``: None for the default choice, or the 1-based number of a
    labelling in the canonical numbering (credulous mode only)."""
    if mode not in _MODES:
        raise ValueError(f"mode must be one of {_MODES}, got {mode!r}")
    candidates = _candidates(graph, semantics)
    if mode == "skeptical":
        if witness is not None:
            raise EvaluationRefused(
                "A witness number only applies to credulous evaluation; "
                "skeptical evaluation uses the intersection of all labellings."
            )
        return intersection_labelling(candidates), "skeptical"
    if witness is not None:
        if not 1 <= witness <= len(candidates):
            raise EvaluationRefused(
                f"Witness {witness} does not exist: {semantics} has "
                f"{len(candidates)} labelling(s); see `label` for the numbering."
            )
        sigma = candidates[witness - 1]
        if sigma.get(issue) != "IN":
            raise EvaluationRefused(
                f"Labelling {witness} does not accept the issue "
                f"({sigma.get(issue)}); a credulous witness must label it IN."
            )
        return sigma, "credulous"
    for sigma in candidates:
        if sigma.get(issue) == "IN":
            return sigma, "credulous"
    # Credulously rejected: no extension accepts the issue.  Fall back to
    # the grounded labelling and resolve the residue skeptically - never
    # with the credulous tiebreak, which would manufacture acceptance.
    return grounded_labels(graph), "skeptical"


def evaluate_debate(
    body: ProofTerm,
    name: str,
    *,
    strict_names=None,
    strict_kinds=None,
    mode: str = "skeptical",
    base: str = "cbn",
    semantics: str = "preferred",
    witness=None,
):
    """Compile, label, choose the witness sigma, resolve, normalize,
    classify.

    Returns (normal_form, nf_class, sigma, graph).  ``base`` is the
    strategy for critical pairs sigma does not decide; ``semantics`` the
    labelling semantics the modes range over; ``witness`` the number of
    the credulous witness to use (default: the first accepting one).
    """
    graph = _compile_for_evaluation(body, name, strict_names, strict_kinds)
    sigma, tiebreak = witness_labelling(graph, issue_of(body), mode, semantics, witness)
    normal_form = _evaluate_under(body, name, sigma, tiebreak, strict_names, base)
    return normal_form, classify_nf(normal_form), sigma, graph


def evaluate_witnesses(
    body: ProofTerm,
    name: str,
    *,
    strict_names=None,
    strict_kinds=None,
    base: str = "cbn",
    semantics: str = "preferred",
):
    """Credulous evaluation under every accepting witness.

    Returns (results, graph) with results = [(number, normal_form,
    nf_class, sigma)] for each labelling of ``semantics`` whose issue is
    IN, in the canonical numbering; empty if the issue is credulously
    rejected.  The verdict is the same for all (a value); the normal
    forms differ in which supporters and presumptions survive.
    """
    graph = _compile_for_evaluation(body, name, strict_names, strict_kinds)
    results = []
    for number, sigma in accepting_witnesses(graph, issue_of(body), semantics):
        nf = _evaluate_under(body, name, sigma, "credulous", strict_names, base)
        results.append((number, nf, classify_nf(nf), sigma))
    return results, graph


def _compile_for_evaluation(body, name, strict_names, strict_kinds):
    found = first_order_node(body)
    if found is not None:
        raise FirstOrderNotSupported("Debate evaluation", found)
    return compile_debate(body, name, strict_names=strict_names,
                          strict_kinds=strict_kinds)


def _evaluate_under(body, name, sigma, tiebreak, strict_names, base):
    resolved = resolve_scaffolds(body, sigma, tiebreak, strict_names=strict_names)
    normal_form = normalize_strong(resolved, strategy=base)
    check_conservativity(body, normal_form, operation=f"evaluate_debate('{name}')")
    return normal_form
