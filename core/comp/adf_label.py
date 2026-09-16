"""Grounded ADF labelling of debate graphs (M4 of
propositional-fragment-plan.org; Layer 3 of debate-graph-spec.org).

The compilation maps every referenced (proposition, side) pair of a
DebateGraph to an ADF statement and builds its acceptance condition:

    AC(K, side) =   [OR over strict edges deriving (K, side): AND sources]
                  OR (
                        ( [OR over defeasible edges: AND sources]
                          OR true-if-presumption-marker )
                        AND NOT (K, contrary side)
                     )

- An obligation marker contributes nothing: the side stays OUT unless an
  edge derives it (default OUT).
- A presumption marker contributes a true disjunct: the side is IN unless
  the contrary side is accepted (default IN, defeasible).
- The NOT-contrary guard is how attack enters: accepting a proof of K
  defeats the challenge to K and vice versa.  Strict derivations bypass
  the guard - per the spec, strict edges are only countered by strict
  contradiction, which ``strict_contradictions`` reports.

Consequence worth stating (recorded as a plan correction): the guard makes
contested nodes - grounds on both sides - a two-statement negation cycle
even when the debate graph is propositionally acyclic, so such nodes can
ground to UNDEC (mutual rebut).  "Acyclic implies two-valued" holds only
for uncontested graphs.

Two implementations of grounded labelling:

- ``grounded_labels``: the production path - Kleene-fixpoint iteration.
  Sound against the ADF (completion-based) semantics because every
  compiled condition is unipolar: each statement occurs with only one
  polarity in any condition (sources positively, the contrary negatively),
  and on unipolar formulas Kleene evaluation coincides with the
  supervaluation that gamma computes.
- ``grounded_labels_via_oracle``: the same labels through the naive M0
  oracle (enumeration); the correctness criterion in tests.

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

from core.comp.oracle import (
    ADF, const, var, neg, conj, disj,
    eval_formula, grounded_interpretation,
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


def compile_conditions(graph: DebateGraph):
    """Acceptance conditions for every referenced statement."""
    statements = referenced_statements(graph)
    materialized = {(k, s) for (k, s) in statements}
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
        if "presumption" in kinds:
            defeasible_disjuncts.append(const(True))
        core = disj(*defeasible_disjuncts)
        contrary = (key, _contrary(side))
        if contrary in materialized and defeasible_disjuncts:
            core = conj(core, neg(var(_stmt(*contrary))))
        condition = disj(*strict_disjuncts, core) if strict_disjuncts else core
        conditions[_stmt(key, side)] = condition
    return conditions


def graph_to_adf(graph: DebateGraph) -> ADF:
    """The compiled ADF, for the oracle path and the adf-bdd cross-check.

    Subject to the oracle's statement-count guard; the production path
    (``grounded_labels``) has no such limit.
    """
    conditions = compile_conditions(graph)
    return ADF(list(conditions), conditions)


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


def grounded_labels(graph: DebateGraph):
    """Grounded labels {(key, side): "IN" | "OUT" | "UNDEC"} - production path."""
    conditions = compile_conditions(graph)
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
