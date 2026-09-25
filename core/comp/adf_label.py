"""Grounded ADF labelling of debate graphs (M4 of
propositional-fragment-plan.org; Layer 3 of debate-graph-spec.org).

The compilation maps every referenced (proposition, side) pair of a
DebateGraph to an ADF statement and builds its acceptance condition:

    AC(K, side) =   [OR over strict edges deriving (K, side): AND sources]
                  OR ( [OR over defeasible edges: AND sources]
                       AND NOT (K, contrary side)   <- only if the contrary
                                                       side is itself derived )
                  OR ( true-if-presumption-marker
                       AND NOT (K, contrary side) )

- An obligation marker contributes nothing: the side stays OUT unless an
  edge derives it (default OUT).
- A presumption marker contributes a true disjunct: the side is IN unless
  the contrary side is accepted (default IN, defeasible).
- The NOT-contrary guard is how attack enters: accepting a proof of K
  defeats the challenge to K and vice versa.  Strict derivations bypass
  the guard - per the spec, strict edges are only countered by strict
  contradiction, which ``strict_contradictions`` reports.

The guard is asymmetric (author's decision, 2026-09-25): *a derivation is
contested only by a derivation*.  A presumption delegates the onus to the
other side, so a side that is nothing but a default marker never guards
back against an argument for the contrary; the argument defeats it
outright.  A presumption keeps its own guard, so a bare default is still
defeated by whatever the other side accepts, and two contrary derivations
still attack each other.  Consequences:

- An undercut of a presumed premise is decisive: in
  tests/multiple_undercuts.fspy the document grounds two-valued instead
  of leaving the whole contest UNDEC.
- The guard is dropped per DISJUNCT, not per statement.  A statement that
  is presumed *and* carries an argument keeps the guard on its
  presumption disjunct, so a failed argument cannot decide a contest
  between two defaults by merely being present.
- A defeasible argument now defeats a bare presumption exactly as a
  strict one does.  Strictness still matters between two derivations.

Contrariness with a default marker on BOTH sides is an onus conflict:
each side delegates the burden to the other, and neither can discharge
it.  ``opposing_presumptions`` reports the propositions where that
happens and ``compile_conditions`` warns (OpposingPresumptions).  Making
it an error and tracking the onus properly is task
aida-onus-delegation-polarity; until then those graphs still compile, and
both sides keep their guards, which is the old mutual-rebut behaviour.

Consequence worth stating (recorded as a plan correction): the guard makes
contested nodes - grounds on both sides - a two-statement negation cycle
even when the debate graph is propositionally acyclic, so such nodes can
ground to UNDEC (mutual rebut).  "Acyclic implies two-valued" holds only
for uncontested graphs.

Three implementations of grounded labelling (trust order decided by the
author, 2026-09-16, task aida-adf-bdd-primary):

- ``grounded_labels``: the PRODUCTION path - the third-party adf-bdd
  solver, run on the exported acceptance conditions.  It implements the
  ADF semantics directly and is trusted against specification drift in
  our own code.  It refuses to run without the binary; there is no
  fallback.
- ``grounded_labels_kleene``: the in-tree Kleene-fixpoint iteration, kept
  as a cross-check.  Sound only on *unipolar* conditions (each statement
  with one polarity per condition); it asserts unipolarity and raises
  NonUnipolarConditions otherwise instead of answering.  The compiler can
  produce a non-unipolar condition in exactly one way - an edge whose
  source is the contrary of its own target, Q[t] <- Q[c], *while the
  contrary side is itself derived*, so that the guard survives and
  Q[t] = (Q[c] & ~Q[c]).  On that shape Kleene is wrong (UNDEC where the
  definition says OUT).  With a bare default on the contrary side the
  guard is now dropped and the shape is unipolar.
- ``grounded_labels_via_oracle``: the naive enumeration oracle, correct
  by construction and limited to 14 statements; the second cross-check.

Transposal note: the transposal closure of debate-graph-spec.org is NOT
implemented.  It currently has nothing to act on because the compiler
fuses an implication with the premise it is applied to into one edge, so
no implication is ever an edge of its own.  That fusion is a defect (task
aida-strict-layer): a refutation of a conclusion cannot reach the premise
by contraposition, and tests/rationality/modus_tollens.fspy shows Q
staying IN where modus tollens requires OUT.  Design decision recorded
in that task: every implication contraposes, delegated (defeasible) ones
included; strictness affects default status only, never closure.
"""

import warnings

from core.comp.oracle import (
    ADF, const, var, neg, conj, disj,
    eval_formula, grounded_interpretation, run_adf_bdd,
)
from core.dc.debate_graph import DebateGraph

#: Separator between the canonical proposition key and the side in an ADF
#: statement name.  \x01 cannot occur in canonical renderings.
_SEP = "\x01"

_LABEL = {True: "IN", False: "OUT", None: "UNDEC"}


def _stmt(key: str, side: str) -> str:
    return f"{key}{_SEP}{side}"


def split_statement(stmt: str):
    key, side = stmt.rsplit(_SEP, 1)
    return key, side


def _contrary(side: str) -> str:
    return "context" if side == "term" else "term"


def referenced_statements(graph: DebateGraph):
    """All (key, side) pairs the graph mentions, in a stable order."""
    seen = []

    def add(key, side):
        if (key, side) not in seen:
            seen.append((key, side))

    for edge in graph.edges:
        add(edge.target_key, edge.target_side)
        for source in edge.sources:
            add(source.key, source.side)
    for key, side in graph.defaults:
        add(key, side)
    return seen


class OpposingPresumptions(UserWarning):
    """Both sides of one proposition carry a default marker.  Each side
    delegates the onus of refutation to the other, so neither holds it;
    see ``opposing_presumptions`` and task aida-onus-delegation-polarity."""


def opposing_presumptions(graph: DebateGraph):
    """Propositions presumed on the term side AND on the context side.

    A presumption delegates the burden of refutation to the other side.
    Both sides delegating is an onus conflict and should eventually be
    refused when the presentation is built, not merely warned about here.
    """
    return sorted(
        key for (key, side), kinds in graph.defaults.items()
        if side == "term" and "presumption" in kinds
        and "presumption" in graph.defaults.get((key, "context"), ())
    )


def _warn_opposing_presumptions(graph: DebateGraph):
    clash = opposing_presumptions(graph)
    if not clash:
        return
    names = ", ".join(graph.nodes.get(key, key) for key in clash)
    warnings.warn(
        f"opposing presumptions on {names}: both sides delegate the onus of "
        f"refutation, so neither side holds it; the contest is decided by "
        f"the contrariness guard alone",
        OpposingPresumptions, stacklevel=3,
    )


def compile_conditions(graph: DebateGraph):
    """Acceptance conditions for every referenced statement."""
    _warn_opposing_presumptions(graph)
    statements = referenced_statements(graph)
    materialized = {(k, s) for (k, s) in statements}
    derived = {(e.target_key, e.target_side) for e in graph.edges}
    conditions = {}
    for key, side in statements:
        strict_disjuncts = []
        defeasible_disjuncts = []
        for edge in graph.edges:
            if edge.target_key != key or edge.target_side != side:
                continue
            body = conj(*[var(_stmt(s.key, s.side)) for s in edge.sources])
            (strict_disjuncts if edge.strict else defeasible_disjuncts).append(body)
        kinds = graph.defaults.get((key, side), ())
        presumed = "presumption" in kinds
        contrary = (key, _contrary(side))
        guarded = contrary in materialized
        # A derivation is contested only by a derivation: a contrary side
        # that is nothing but a default marker has delegated the onus and
        # does not guard back.  bool(), because `and` returns the list and
        # the list is mutated below.
        unguarded_derivations = bool(
            guarded and defeasible_disjuncts and contrary not in derived
        )
        if unguarded_derivations and presumed:
            # The presumption disjunct keeps its guard even though the
            # derivations lose theirs, so a dead argument cannot decide a
            # contest between two defaults.
            core = disj(disj(*defeasible_disjuncts),
                        conj(const(True), neg(var(_stmt(*contrary)))))
        else:
            if presumed:
                defeasible_disjuncts.append(const(True))
            core = disj(*defeasible_disjuncts)
            if guarded and defeasible_disjuncts and not unguarded_derivations:
                core = conj(core, neg(var(_stmt(*contrary))))
        condition = disj(*strict_disjuncts, core) if strict_disjuncts else core
        conditions[_stmt(key, side)] = condition
    return conditions


def graph_to_adf(graph: DebateGraph, *, guard: bool = True) -> ADF:
    """The compiled ADF.  With the guard (default) it is limited to the
    oracle's 14 statements; the production path passes ``guard=False``
    because adf-bdd has no such limit - and must never hand an unguarded
    ADF to the enumeration functions."""
    conditions = compile_conditions(graph)
    return ADF(list(conditions), conditions, guard=guard)


# ---------------------------------------------------------------------------
# Unipolarity: the precondition of the Kleene cross-check
# ---------------------------------------------------------------------------

class NonUnipolarConditions(ValueError):
    """A compiled condition mentions some statement in both polarities; the
    Kleene fixpoint is not sound on it and refuses rather than answer."""


def _polarities(f, sign: bool, acc: dict) -> dict:
    tag = f[0]
    if tag == "var":
        acc.setdefault(f[1], set()).add(sign)
    elif tag == "not":
        _polarities(f[1], not sign, acc)
    elif tag in ("and", "or"):
        for g in f[1]:
            _polarities(g, sign, acc)
    return acc


def check_unipolar(conditions) -> None:
    """Raise NonUnipolarConditions if any condition is not unipolar."""
    for stmt, condition in conditions.items():
        bad = [v for v, signs in _polarities(condition, True, {}).items() if len(signs) > 1]
        if bad:
            key, side = split_statement(stmt)
            offenders = ", ".join(f"{split_statement(v)[0]}[{split_statement(v)[1]}]" for v in bad)
            raise NonUnipolarConditions(
                f"Condition of {key}[{side}] mentions {offenders} in both "
                f"polarities; the Kleene cross-check is not sound here (an "
                f"edge whose source is the contrary of its target). Use the "
                f"primary labeller."
            )


# ---------------------------------------------------------------------------
# Production grounded labelling: Kleene fixpoint
# ---------------------------------------------------------------------------

def kleene_eval(f, valuation):
    """Three-valued (strong Kleene) evaluation under a partial valuation."""
    tag = f[0]
    if tag == "const":
        return f[1]
    if tag == "var":
        return valuation[f[1]]
    if tag == "not":
        inner = kleene_eval(f[1], valuation)
        return None if inner is None else not inner
    if tag == "and":
        result = True
        for g in f[1]:
            v = kleene_eval(g, valuation)
            if v is False:
                return False
            if v is None:
                result = None
        return result
    if tag == "or":
        result = False
        for g in f[1]:
            v = kleene_eval(g, valuation)
            if v is True:
                return True
            if v is None:
                result = None
        return result
    raise ValueError(f"Unknown formula tag: {tag!r}")


#: The labelling semantics the labeller can be asked for.  "preferred" is
#: derived from "complete" (its <=_i-maximal members); the other three are
#: adf-bdd modes.  Semantics is a parameter because credulous/skeptical
#: acceptance only exists relative to a multi-extension semantics
#: (tasks.org, aida-credulous-witness-labelling).
SEMANTICS = ("grounded", "complete", "preferred", "stable")


def _to_labels(interpretation) -> dict:
    return {split_statement(s): _LABEL[v] for s, v in interpretation.items()}


def _leq_information(v: dict, w: dict) -> bool:
    return all(v[s] == "UNDEC" or v[s] == w[s] for s in v)


def labellings(graph: DebateGraph, semantics: str = "grounded"):
    """All labellings of the graph under ``semantics``, as a list of
    {(key, side): label} dicts, computed by adf-bdd (the production path;
    raises AdfBddNotFound when the solver is missing, no fallback).

    grounded -> exactly one; complete/stable -> possibly many; preferred
    -> the <=_i-maximal complete ones.  Always in the canonical order of
    ``labelling_key``, which is the numbering `label` prints.  A
    semantics with no labelling (stable on an odd cycle) returns [].
    """
    if semantics not in SEMANTICS:
        raise ValueError(f"semantics must be one of {SEMANTICS}, got {semantics!r}")
    adf = graph_to_adf(graph, guard=False)
    mode = "complete" if semantics == "preferred" else semantics
    found = [_to_labels(v) for v in run_adf_bdd(adf, mode)]
    if semantics == "grounded" and len(found) != 1:
        raise RuntimeError(
            f"adf-bdd returned {len(found)} grounded interpretations; expected exactly one"
        )
    if semantics == "preferred":
        found = [v for v in found if not any(w != v and _leq_information(v, w) for w in found)]
    return sorted(found, key=labelling_key)


_LABEL_RANK = {"IN": 0, "OUT": 1, "UNDEC": 2}


def labelling_key(labels: dict) -> tuple:
    """Canonical sort key: statements in sorted order, IN < OUT < UNDEC.

    adf-bdd's enumeration order is an artifact of its BDDs and differs
    between modes; sorting makes "labelling number N" mean the same
    labelling across runs, modes and semantics, so `label` and `evaluate`
    agree on the numbering."""
    return tuple((s, _LABEL_RANK[labels[s]]) for s in sorted(labels))


def intersection_labelling(labels_list):
    """The statement-wise agreement of several labellings: a label where
    all agree, UNDEC where they differ.  The skeptical reading."""
    if not labels_list:
        raise ValueError("no labellings to intersect")
    out = {}
    for statement in labels_list[0]:
        values = {lab[statement] for lab in labels_list}
        out[statement] = values.pop() if len(values) == 1 else "UNDEC"
    return out


def grounded_labels(graph: DebateGraph):
    """Grounded labels {(key, side): "IN" | "OUT" | "UNDEC"} - the
    production path, computed by adf-bdd.  Raises AdfBddNotFound when the
    solver is not installed; there is no fallback."""
    return labellings(graph, "grounded")[0]


def grounded_labels_kleene(graph: DebateGraph):
    """Grounded labels by the in-tree Kleene fixpoint - a CROSS-CHECK, not
    the production path.  Refuses non-unipolar conditions."""
    conditions = compile_conditions(graph)
    check_unipolar(conditions)
    valuation = {stmt: None for stmt in conditions}
    changed = True
    while changed:
        changed = False
        for stmt, condition in conditions.items():
            if valuation[stmt] is not None:
                continue
            value = kleene_eval(condition, valuation)
            if value is not None:
                valuation[stmt] = value
                changed = True
    return {split_statement(s): _LABEL[v] for s, v in valuation.items()}


def grounded_labels_via_oracle(graph: DebateGraph):
    """The same labels through the naive M0 oracle - the test criterion."""
    adf = graph_to_adf(graph)
    interpretation = grounded_interpretation(adf)
    return {split_statement(s): _LABEL[v] for s, v in interpretation.items()}


# ---------------------------------------------------------------------------
# Strict contradictions
# ---------------------------------------------------------------------------

def strict_contradictions(graph: DebateGraph):
    """Proposition keys strict-derived on both sides (CONTR candidates).

    Syntactic check on edges: a strict edge for K and a strict edge
    against K.  Confined CONTR propagation is future Layer-3 work; the
    fragment only reports the set.
    """
    strict_for = {e.target_key for e in graph.edges if e.strict and e.target_side == "term"}
    strict_against = {e.target_key for e in graph.edges if e.strict and e.target_side == "context"}
    return strict_for & strict_against
