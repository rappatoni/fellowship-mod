"""M7: rationality-postulate checks at graph level (V4 of
propositional-fragment-plan.org).

Expected labellings are derived from the acceptance-condition semantics
and validated three ways: the production Kleene labeller, the naive M0
oracle, and (where installed) the third-party adf-bdd solver.  No
expectation here was captured from experimental-algorithm output.
"""

import pytest

from core.comp.adf_label import (
    grounded_labels, grounded_labels_via_oracle, graph_to_adf,
)
from core.comp.oracle import find_adf_bdd, run_adf_bdd, grounded_interpretation
from core.dc.debate_graph import DebateGraph, Edge, Source, canonical_prop


def edge(name, target, side, sources=(), strict=False, role="argument"):
    return Edge(name=name, target_key=target, target_side=side,
                sources=tuple(sources), strict=strict, role=role)


def src(text, side, kind):
    return Source(key=canonical_prop(text), side=side, kind=kind, site="s")


K = canonical_prop


def married_bachelor() -> DebateGraph:
    """Caminada's consistency scenario, premise-defeasibility encoding.

    wears_ring (presumption) => married; parties (presumption) => bachelor;
    strict: bachelor -> not married, and - the strict layer being closed
    under transposition - married -> not bachelor.  "not X" is the
    context side of X, so the strict rules appear as context-side edges
    with the contrary conclusion among the sources.
    """
    g = DebateGraph()
    for text in ("WearsRing", "Parties", "Married", "Bachelor"):
        g.add_node(text)
    g.add_edge(edge("argM", K("Married"), "term",
                    [src("WearsRing", "term", "presumption")]))
    g.add_edge(edge("argB", K("Bachelor"), "term",
                    [src("Parties", "term", "presumption")]))
    # Strict bridge, both transposition directions.  The edges are strict
    # (their only source is the contrary conclusion, not an open site), so
    # `strict` reflects "no obligation/presumption source" - but the
    # source itself is another node's term side, so acceptance still flows
    # through it.
    g.add_edge(edge("bNotM", K("Married"), "context",
                    [Source(key=K("Bachelor"), side="term", kind="assumption", site="b")],
                    strict=True, role="attacker"))
    g.add_edge(edge("mNotB", K("Bachelor"), "context",
                    [Source(key=K("Married"), side="term", kind="assumption", site="m")],
                    strict=True, role="attacker"))
    return g


def contamination_pair():
    """A healthy component with and without an isolated strict
    contradiction alongside it."""

    def healthy(g: DebateGraph) -> DebateGraph:
        g.add_node("P")
        g.add_node("Q")
        g.add_edge(edge("argP", K("P"), "term",
                        [src("Q", "term", "presumption")]))
        return g

    clean = healthy(DebateGraph())
    dirty = healthy(DebateGraph())
    dirty.add_node("X")
    dirty.add_edge(edge("axX", K("X"), "term", [], strict=True))
    dirty.add_edge(edge("axNotX", K("X"), "context", [], strict=True))
    return clean, dirty


def support_chain(depth: int, leaf_kind: str) -> DebateGraph:
    """A0 <- A1 <- ... <- A_depth with the leaf site of the given kind."""
    g = DebateGraph()
    for i in range(depth + 1):
        g.add_node(f"A{i}")
    for i in range(depth):
        kind = leaf_kind if i == depth - 1 else "obligation"
        g.add_edge(edge(f"arg{i}", K(f"A{i}"), "term",
                        [src(f"A{i + 1}", "term", kind)],
                        role="argument" if i == 0 else "supporter"))
    return g


class TestMarriedBachelor:
    def test_grounded_is_mutual_undec(self):
        # Both defeasible positions are internally grounded and rebut each
        # other through the strict bridge: grounded refuses to decide -
        # and, critically, never accepts both.
        labels = grounded_labels(married_bachelor())
        assert labels[(K("WearsRing"), "term")] == "IN"
        assert labels[(K("Parties"), "term")] == "IN"
        assert labels[(K("Married"), "term")] == "UNDEC"
        assert labels[(K("Bachelor"), "term")] == "UNDEC"

    def test_direct_consistency_in_every_two_valued_model(self):
        # The consistency postulate, model-checked: no two-valued model
        # accepts Married and Bachelor together.
        adf = graph_to_adf(married_bachelor())
        from core.comp.oracle import two_valued_models
        for model in two_valued_models(adf):
            married = model.get(f"{K('Married')}\x01term")
            bachelor = model.get(f"{K('Bachelor')}\x01term")
            assert not (married and bachelor)

    def test_oracle_agreement(self):
        g = married_bachelor()
        assert grounded_labels(g) == grounded_labels_via_oracle(g)


class TestNonInterference:
    def test_isolated_contradiction_does_not_contaminate(self):
        clean, dirty = contamination_pair()
        clean_labels = grounded_labels(clean)
        dirty_labels = grounded_labels(dirty)
        for statement, label in clean_labels.items():
            assert dirty_labels[statement] == label
        # The contradiction itself is visible where it lives...
        assert dirty_labels[(K("X"), "term")] == "IN"
        assert dirty_labels[(K("X"), "context")] == "IN"
        # ...and reported.
        from core.comp.adf_label import strict_contradictions
        assert strict_contradictions(dirty) == {K("X")}
        assert strict_contradictions(clean) == set()

    def test_oracle_agreement(self):
        for g in contamination_pair():
            assert grounded_labels(g) == grounded_labels_via_oracle(g)


class TestSupportChains:
    @pytest.mark.parametrize("depth", [1, 3, 5])
    def test_presumption_leaf_lifts_the_whole_chain(self, depth):
        labels = grounded_labels(support_chain(depth, "presumption"))
        for i in range(depth + 1):
            assert labels[(K(f"A{i}"), "term")] == "IN"

    @pytest.mark.parametrize("depth", [1, 3, 5])
    def test_obligation_leaf_sinks_the_whole_chain(self, depth):
        labels = grounded_labels(support_chain(depth, "obligation"))
        for i in range(depth + 1):
            assert labels[(K(f"A{i}"), "term")] == "OUT"

    def test_oracle_agreement(self):
        for kind in ("presumption", "obligation"):
            g = support_chain(3, kind)
            assert grounded_labels(g) == grounded_labels_via_oracle(g)


needs_adf_bdd = pytest.mark.skipif(
    find_adf_bdd() is None,
    reason="adf-bdd binary not installed (cargo install adf-bdd-bin)",
)


@needs_adf_bdd
class TestThirdPartyAgreement:
    @pytest.mark.parametrize("make", [
        married_bachelor,
        lambda: contamination_pair()[1],
        lambda: support_chain(3, "presumption"),
    ], ids=["married-bachelor", "contamination", "support-chain"])
    def test_adf_bdd_agrees(self, make):
        adf = graph_to_adf(make())
        theirs = run_adf_bdd(adf, "grounded")
        assert len(theirs) == 1
        assert theirs[0] == grounded_interpretation(adf)


class TestObjectNegationConsistency:
    """Direct consistency with negation in the object language.

    Q and ~Q are distinct graph nodes, and nothing relates them until the
    strict layer adds the negation bridge (task aida-strict-layer). Until
    then both are accepted. Strict xfail: this flips when the bridge exists.
    """

    @pytest.mark.xfail(strict=True, reason="strict layer / negation bridge not implemented (aida-strict-layer)")
    def test_q_and_not_q_are_not_both_accepted(self):
        g = DebateGraph()
        g.add_node("Q")
        g.add_node("~Q")
        g.mark_default(K("Q"), "term", "presumption")
        g.mark_default(K("~Q"), "term", "presumption")
        labels = grounded_labels(g)
        assert not (labels[(K("Q"), "term")] == "IN"
                    and labels[(K("~Q"), "term")] == "IN")


class TestModusTollensFixture:
    """The compiled form of tests/rationality/modus_tollens.fspy.

    Today the rule Q->P is fused into the defeasible edge and no
    transposal exists, so the strict refutation of P never reaches Q.
    Strict xfail: flips when aida-strict-layer lands.
    """

    def _compiled_graph(self):
        from core.ac.ast import Mu, Cons, Deleg, ID, DI
        from core.dc.debate_graph import compile_debate
        tp = Mu(ID("tp", "P"), "P",
                Mu(ID("r", "P"), "P", DI("pRule", "Q->P"),
                   Cons(Deleg("1", "Q"), ID("r", "P"))),
                ID("tp", "P"))
        g = compile_debate(tp, "tp", strict_names={"pRule"})
        g.add_edge(edge("t", K("P"), "context", [], strict=True, role="attacker"))
        return g

    def test_rule_is_not_yet_its_own_edge(self):
        g = self._compiled_graph()
        strict_with_sources = [e for e in g.edges if e.strict and e.sources]
        assert strict_with_sources == []          # the fusion, pinned

    @pytest.mark.xfail(strict=True, reason="rule/premise fusion; no transposal (aida-strict-layer)")
    def test_refuting_p_refutes_q(self):
        labels = grounded_labels(self._compiled_graph())
        assert labels[(K("Q"), "term")] == "OUT"
