"""Label-guided evaluation of debate terms (M6 of
propositional-fragment-plan.org; Layer 4 of call-by-onus.org).

The pipeline is the corrected Layer-4 picture: the debate term is compiled
(M3) and labelled (M4) *first*; evaluation is then plain reduction, with
the witness labelling sigma resolving exactly the critical pairs that the
support/attack scaffolds are.

A scaffold is the exposure  mu alpha:A.< t || [A:] >  with the slot
filled by an alternative between two contexts (COMMA 2026, Definitions
Support and Attack; mirrored on the context side):

    support:  mu alpha.< t || mu'beta.< mu _.<beta||alpha> || mu'_.<t2||alpha> > >
    attack:   mu alpha.< t || mu'beta.< mu _.<beta||e2>    || mu'_.<beta||alpha> > >

The inner command is the critical pair < Mu(affine) || Mutilde(affine) >:
on the term side (mu<) keeps the original at a support and defeats it at
an attack, (>mu) the reverse - credulous is uniformly CBN there and
skeptical uniformly CBV; the context side is the mirror.  The outer pair
< t || mu'beta.c > is resolved together with the inner one (beta := t),
so no scaffold residue reaches the base strategy.  The legacy M3 shapes
the term-level verbs still build are recognised too (debate_graph.py).

Evaluation has two phases, as the paper's call-by-onus: first the
strict phase on the term (core/dc/strict.py: a scaffold whose one wing
is strict in the debate and the other is not is decided by strictness;
this is where a sprung trap - Peirce's law - becomes a closed proof),
then sigma for every scaffold strictness delayed.  The issue graph the
labelling runs on is the framework of the term plus the strict edges
the term contributes, so sigma never contradicts a strict decision.

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

Wing choice at a scaffold (site of proposition A) is decided by the
scion's DERIVATION STATUS under sigma (``derivation_status``): IN iff the
scion's target statement and all of its sources are IN, OUT iff any of
them is OUT, UNDEC otherwise.  Consulting the target statement alone was
unsound: Q[t] IN says some derivation of Q is live, not that this
supporter's is (aida-supporter-derivation-status, 2026-09-16).  The
UNDEC rows are the tiebreak, reached only when sigma leaves the status
undecided:

    supporter IN            -> keep scion        attacker IN   -> keep attack wing
    supporter OUT           -> keep ORIG         attacker OUT  -> keep ORIG
    supporter UNDEC         -> credulous: scion  attacker UNDEC-> credulous: ORIG
                               skeptical: ORIG                    skeptical: attack wing

What a defeated site holds: mu alpha.< t || e2 > - the original facing
the winning refutation under an affine binder, the paper's abort
("uncatchable exception").  When the original was a bare obligation this
is the clash < ?g || e2 > the legacy shapes always left.  classify_nf
reports a term containing an uncatchable clash as an exception; a clash
nested under an axiom head is not propagated by the standard rules
(task aida-abort-propagation), the classifier sees it anyway.
"""

import logging
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
    DebateCompileError, binder_statements, scion_record,
)
from core.logging_util import TRACE, artifact
from core.dc.strict import (
    strict_resolve, compile_issue,
    keep_orig as _keep_orig, keep_scion_support as _keep_scion_support,
    keep_attack_wing as _keep_attack_wing,
)


logger = logging.getLogger(__name__)


def _show(statement, prop=None) -> str:
    """``Prop[t]`` / ``Prop[c]`` for a log line; the canonical key is the
    fallback when the caller has no surface spelling."""
    key, side = statement
    return f"{prop or key}[{side[0]}]"


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


def derivation_status(records, labels) -> str:
    """The status of a scion's OWN derivation under sigma, read off the
    edge records the compiler builds for it (``scion_record``), one per
    alternative derivation:

    IN    iff some alternative has its target statement and every one of
          its sources IN;
    OUT   iff every alternative has its target or some source OUT;
    UNDEC otherwise.

    The sources are the ones the graph has for that edge - open sites,
    lambda subarguments, and whatever absorbed subarguments contributed.
    This is the statement-level labelling read at edge level; the target
    label alone says that *some* derivation of the statement is live,
    not that this one is (tasks.org, aida-supporter-derivation-status).
    """
    statuses = []
    for record in records:
        found = [labels.get((record.target_key, record.target_side))]
        found += [labels.get((s.key, s.side)) for s in record.sources]
        if any(label is None for label in found):
            raise EvaluationRefused(
                f"No label for a source of the scion '{record.name}'; the "
                f"labelling and the term disagree about the debate's shape."
            )
        if all(label == "IN" for label in found):
            statuses.append("IN")
        elif any(label == "OUT" for label in found):
            statuses.append("OUT")
        else:
            statuses.append("UNDEC")
    if "IN" in statuses:
        return "IN"
    if all(s == "OUT" for s in statuses):
        return "OUT"
    return "UNDEC"


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

    def walk(node, env):
        if not isinstance(node, ProofTerm):
            return node
        match = _match_scaffold(node, strict_names)
        if match is not None:
            role, site_side, prop, orig, scion_raw, alt, scion_kind = match
            records = scion_record(match, env, strict_names)
            statement = (records[0].target_key, records[0].target_side)
            if labels.get(statement) is None:
                raise EvaluationRefused(
                    f"No label for scaffold issue {statement}; the labelling "
                    f"and the term disagree about the debate's shape."
                )
            status = derivation_status(records, labels)
            if trace is not None:
                trace.append((statement, status))
            choice = _wing_choice(role, status, mode)
            if logger.isEnabledFor(logging.DEBUG):
                kept = ("original" if choice == "orig"
                        else ("supporter" if role == "supporter" else "attacker (the clash)"))
                # Which wing is thrown away, and what goes with it.  Resolution
                # recurses only into the wing it keeps, so every scaffold inside
                # the other one is never consulted - that is how a whole debate
                # can collapse to a single site in one decision.
                dropped = None
                if role == "supporter":
                    dropped = ("supporter", scion_raw) if choice == "orig" else ("original", orig)
                elif choice == "orig":
                    dropped = ("attacker", scion_raw)   # attacker wins -> the clash keeps both
                inside = _count_scaffolds(dropped[1], strict_names) if dropped else 0
                logger.debug("sigma: %s %s is %s -> keep the %s%s%s",
                             role, _show(statement, prop), status, kept,
                             " (%s tiebreak)" % mode if status == "UNDEC" else "",
                             "" if dropped is None else
                             ", dropping the %s%s" % (
                                 dropped[0],
                                 " and the %d scaffold(s) inside it" % inside if inside else ""))
            if choice == "orig":
                return walk(_keep_orig(node, orig, alt), env)
            if role == "supporter":
                return walk(_keep_scion_support(node, scion_raw, alt), env)
            return walk(_keep_attack_wing(node, match), env)
        inner = {**env, **binder_statements(node)}
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child, inner))
        return node

    return walk(deepcopy(body), {})


def _count_scaffolds(node, strict_names) -> int:
    """Scaffolds in a subterm: what a dropped wing takes with it."""
    if not isinstance(node, ProofTerm):
        return 0
    found = 1 if _match_scaffold(node, strict_names) is not None else 0
    for slot in ("term", "context"):
        found += _count_scaffolds(getattr(node, slot, None), strict_names)
    return found


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
    shown = _show(issue, graph.nodes.get(issue[0]))
    candidates = _candidates(graph, semantics)
    if mode == "skeptical":
        if witness is not None:
            raise EvaluationRefused(
                "A witness number only applies to credulous evaluation; "
                "skeptical evaluation uses the intersection of all labellings."
            )
        logger.debug("witness: skeptical over %d %s labelling(s)", len(candidates), semantics)
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
        logger.debug("witness: [%d] of %d chosen explicitly (credulous, issue %s IN)",
                     witness, len(candidates), shown)
        return sigma, "credulous"
    for number, sigma in enumerate(candidates, 1):
        if sigma.get(issue) == "IN":
            logger.debug("witness: [%d] of %d chosen, the first %s labelling accepting %s",
                         number, len(candidates), semantics, shown)
            return sigma, "credulous"
    # Credulously rejected: no extension accepts the issue.  Fall back to
    # the grounded labelling and resolve the residue skeptically - never
    # with the credulous tiebreak, which would manufacture acceptance.
    # INFO, not DEBUG: this contradicts the mode the caller asked for and
    # changes the answer, so it must be visible without raising the level.
    logger.info("No %s labelling accepts %s; evaluating against the grounded "
                "labelling, resolved skeptically.", semantics, shown)
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
    issue = issue_of(body)
    logger.debug("evaluate: '%s' on %s (%s, %s, base %s)",
                 name, _show(issue, graph.nodes.get(issue[0])), mode, semantics, base)
    sigma, tiebreak = witness_labelling(graph, issue, mode, semantics, witness)
    normal_form = _evaluate_under(body, name, sigma, tiebreak, strict_names, base)
    nf_class = classify_nf(normal_form)
    logger.debug("evaluate: '%s' is %s; the issue %s is %s", name, nf_class.upper(),
                 _show(issue, graph.nodes.get(issue[0])), sigma.get(issue))
    return normal_form, nf_class, sigma, graph


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
    """The issue graph: the framework plus the strict edges the term
    contributes (core/dc/strict.py)."""
    found = first_order_node(body)
    if found is not None:
        raise FirstOrderNotSupported("Debate evaluation", found)
    return compile_issue(body, name, strict_names=strict_names, strict_kinds=strict_kinds)


def _evaluate_under(body, name, sigma, tiebreak, strict_names, base):
    """The paper's two phases: strictness on the term first (strict
    redundancy and defeat), sigma for what it delays."""
    verbose = logger.isEnabledFor(logging.DEBUG)
    if verbose:
        from pres.gen import pres_str
        artifact(logger, "evaluate: the term going in", pres_str(body))
        artifact(logger, "evaluate: the witness labelling sigma",
                 "  ".join(f"{_show(st)}={v}" for st, v in sigma.items()) or "empty")
    # Phase 1: strictness on the term.  NOTE this is the SECOND strict_resolve
    # of an evaluation - compile_issue ran one to collect the strict edges and
    # threw the rewritten term away (tasks.org, aida-pipeline-logging).
    strict_trace = []
    strict_body, _ = strict_resolve(body, strict_names or (), trace=strict_trace)
    # Phase 2: sigma decides what strictness delayed.
    sigma_trace = []
    resolved = resolve_scaffolds(strict_body, sigma, tiebreak, strict_names=strict_names,
                                 trace=sigma_trace)
    logger.debug("evaluate: '%s' - %d scaffold(s) decided by strictness, %d by sigma "
                 "(%s tiebreak, base %s)",
                 name, len(strict_trace), len(sigma_trace), tiebreak, base)
    if verbose:
        from pres.gen import pres_str
        artifact(logger, "evaluate: the term after both phases, going into normalisation",
                 pres_str(resolved))
    normal_form = normalize_strong(resolved, strategy=base)
    check_conservativity(body, normal_form, operation=f"evaluate_debate('{name}')")
    return normal_form
