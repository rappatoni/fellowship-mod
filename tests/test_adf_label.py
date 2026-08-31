"""M4: grounded ADF labelling of debate graphs.

Expectations are hand-derived from the acceptance-condition semantics in
core/comp/adf_label.py's docstring; the production Kleene-fixpoint path is
checked against the naive M0 oracle on every graph, and against the
third-party adf-bdd solver where installed.
"""

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Deleg, ID, DI
from core.comp.adf_label import (
    grounded_labels, grounded_labels_via_oracle, graph_to_adf,
    strict_contradictions, compile_conditions,
)
from core.comp.oracle import find_adf_bdd, run_adf_bdd, grounded_interpretation
from core.dc.debate_graph import (
    DebateGraph, Edge, Source, canonical_prop, compile_debate,
)

P, Q, RF = canonical_prop("P"), canonical_prop("Q"), canonical_prop("R->false")


def edge(name, target, side, sources=(), strict=False, role="argument"):
    return Edge(name=name, target_key=target, target_side=side,
                sources=tuple(sources), strict=strict, role=role)


def src(key, side, kind):
    return Source(key=key, side=side, kind=kind, site="s")


def graph_support_chain(leaf_kind):
    """pArg: P <- (Q, term); qArg: Q <- (R->false, term); leaf site of the
    given kind on R->false."""
    g = DebateGraph()
    for text in ("P", "Q", "R->false"):
        g.add_node(text)
    g.add_edge(edge("pArg", P, "term", [src(Q, "term", "obligation")]))
    g.add_edge(edge("qArg", Q, "term", [src(RF, "term", leaf_kind)], role="supporter"))
    return g


def graph_contested_presumptions():
    """Presumption for Q on both sides: mutual defeasible rebut."""
    g = DebateGraph()
    g.add_node("Q")
    g.mark_default(Q, "term", "presumption")
    g.mark_default(Q, "context", "presumption")
    return g


def graph_strict_challenge_vs_presumption():
    """Presumption for Q (term side) against a strict refutation of Q."""
    g = DebateGraph()
    g.add_node("Q")
    g.mark_default(Q, "term", "presumption")
    g.add_edge(edge("nQ", Q, "context", [], strict=True, role="attacker"))
    return g


def graph_defeasible_challenge_vs_presumption():
    """Presumption for Q against a challenge grounded in its own presumption."""
    g = DebateGraph()
    g.add_node("Q")
    g.add_node("S")
    g.mark_default(Q, "term", "presumption")
    g.add_edge(edge("nQ", Q, "context",
                    [src(canonical_prop("S"), "term", "presumption")],
                    role="attacker"))
    return g


ALL_GRAPHS = [
    graph_support_chain("obligation"),
    graph_support_chain("presumption"),
    graph_contested_presumptions(),
    graph_strict_challenge_vs_presumption(),
    graph_defeasible_challenge_vs_presumption(),
]


class TestHandWorkedLabels:
    def test_unsupported_obligation_chain_is_out(self):
        labels = grounded_labels(graph_support_chain("obligation"))
        assert labels[(RF, "term")] == "OUT"
        assert labels[(Q, "term")] == "OUT"
        assert labels[(P, "term")] == "OUT"

    def test_presumption_leaf_lifts_the_chain(self):
        labels = grounded_labels(graph_support_chain("presumption"))
        assert labels[(RF, "term")] == "IN"
        assert labels[(Q, "term")] == "IN"
        assert labels[(P, "term")] == "IN"

    def test_contested_presumptions_are_undec(self):
        # The plan-correction case: propositionally acyclic, yet UNDEC -
        # mutual rebut is a two-statement negation cycle.
        labels = grounded_labels(graph_contested_presumptions())
        assert labels[(Q, "term")] == "UNDEC"
        assert labels[(Q, "context")] == "UNDEC"

    def test_strict_challenge_defeats_presumption(self):
        labels = grounded_labels(graph_strict_challenge_vs_presumption())
        assert labels[(Q, "context")] == "IN"
        assert labels[(Q, "term")] == "OUT"

    def test_defeasible_challenge_yields_mutual_undec(self):
        labels = grounded_labels(graph_defeasible_challenge_vs_presumption())
        assert labels[(canonical_prop("S"), "term")] == "IN"
        assert labels[(Q, "term")] == "UNDEC"
        assert labels[(Q, "context")] == "UNDEC"


class TestProductionAgainstOracle:
    @pytest.mark.parametrize("graph", ALL_GRAPHS, ids=lambda g: ",".join(sorted(g.nodes.values())))
    def test_kleene_fixpoint_equals_oracle_grounded(self, graph):
        assert grounded_labels(graph) == grounded_labels_via_oracle(graph)

    def test_unipolarity_of_compiled_conditions(self):
        # The soundness argument for the Kleene path requires every
        # statement to occur with a single polarity per condition.
        def polarities(f, sign, acc):
            tag = f[0]
            if tag == "var":
                acc.setdefault(f[1], set()).add(sign)
            elif tag == "not":
                polarities(f[1], not sign, acc)
            elif tag in ("and", "or"):
                for g in f[1]:
                    polarities(g, sign, acc)
            return acc

        for graph in ALL_GRAPHS:
            for stmt, condition in compile_conditions(graph).items():
                for var_name, signs in polarities(condition, True, {}).items():
                    assert len(signs) == 1, (stmt, var_name)


needs_adf_bdd = pytest.mark.skipif(
    find_adf_bdd() is None,
    reason="adf-bdd binary not installed (cargo install adf-bdd-bin)",
)


@needs_adf_bdd
class TestThirdPartyCrossCheck:
    @pytest.mark.parametrize("graph", ALL_GRAPHS, ids=lambda g: ",".join(sorted(g.nodes.values())))
    def test_adf_bdd_agrees_on_grounded(self, graph):
        adf = graph_to_adf(graph)
        theirs = run_adf_bdd(adf, "grounded")
        assert len(theirs) == 1
        assert theirs[0] == grounded_interpretation(adf)


class TestStrictContradictions:
    def test_detected(self):
        g = DebateGraph()
        g.add_node("Q")
        g.add_edge(edge("ax1", Q, "term", [], strict=True))
        g.add_edge(edge("ax2", Q, "context", [], strict=True))
        assert strict_contradictions(g) == {Q}
        # Both sides ground IN: the strict disjunct bypasses the guard.
        labels = grounded_labels(g)
        assert labels[(Q, "term")] == "IN"
        assert labels[(Q, "context")] == "IN"

    def test_empty_without_double_derivation(self):
        assert strict_contradictions(graph_support_chain("obligation")) == set()


class TestEndToEndFromTerm:
    def test_compiled_support_debate_labels(self):
        # The dsup replica from test_debate_graph, straight through
        # compile_debate -> grounded_labels.
        def eta(name, prop, inner):
            return Mu(ID(name, prop), prop, inner, ID(name, prop))

        qarg = eta("qArg", "Q",
                   Mu(ID("rule2", "Q"), "Q",
                      DI("qRule", "(R->false)->Q"),
                      Cons(Deleg("2.1", "R->false"), ID("rule2", "Q"))))
        scaffold = Mu(ID("alt", "Q"), "Q",
                      Mu(ID("_", "Q"), "Q", Goal("1", "Q"), ID("alt", "Q")),
                      Mutilde(DI("_", "Q"), "Q", qarg, ID("alt", "Q")))
        body = eta("pArg", "P",
                   Mu(ID("rule", "P"), "P",
                      DI("pRule", "Q->P"),
                      Cons(scaffold, ID("rule", "P"))))
        graph = compile_debate(body, "dsup", strict_names={"pRule", "qRule"})
        labels = grounded_labels(graph)
        # qArg rests on a presumption -> everything grounds IN.
        assert labels[(RF, "term")] == "IN"
        assert labels[(Q, "term")] == "IN"
        assert labels[(P, "term")] == "IN"
        assert grounded_labels_via_oracle(graph) == labels
