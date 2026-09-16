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


class TestCurriedAttack:
    """The compiled form of tests/rationality/curried_attack.fspy.

    A challenge to B->C cannot attack an argument that derives C from A
    and B via A->(B->C): the intermediate conclusion B->C occurs only as
    a context type in the elimination stack. Strict xfail; flips when
    aida-curried-intermediate-conclusions lands.
    """

    def _host(self, prover):
        from core.dc.argument import Argument
        c = Argument(prover, 'cArg', 'C',
                     ['cut (A->(B->C)) r', 'axiom abc', 'elim', 'next',
                      'elim', 'by default', 'next', 'axiom r'])
        c.execute()
        return c

    @pytest.fixture
    def curried_prover(self):
        from wrap.cli import setup_prover
        p = setup_prover()
        p.send_command('lk.')
        p.send_command('declare A,B,C:bool.')
        p.send_command('declare abc : (A->(B->C)).')
        yield p
        p.close()

    def test_intermediate_conclusion_is_context_only(self, curried_prover):
        """Pins the cause: no term-side node is typed B->C."""
        from core.ac.ast import ProofTerm, Term
        host = self._host(curried_prover)
        term_props = set()

        def walk(n):
            if not isinstance(n, ProofTerm):
                return
            if isinstance(n, Term) and getattr(n, "prop", None):
                term_props.add(n.prop)
            for slot in ("term", "context"):
                walk(getattr(n, slot, None))

        walk(host.body)
        assert "B->C" not in term_props

    @pytest.mark.xfail(strict=True, reason="curried intermediate conclusions not exposed (aida-curried-intermediate-conclusions)")
    def test_challenge_to_intermediate_conclusion_attacks(self, curried_prover):
        from core.dc.argument import Argument
        host = self._host(curried_prover)
        challenge = Argument(curried_prover, 'test', 'B->C', ['by default'], is_anti=True)
        challenge.execute()
        challenge.attack(host, name='d6')   # raises today


class TestCaptureIsACycle:
    """A captured presumption is a derivation cycle, decided by the labelling.

    Rootstock t argues A from a lemma B; attacker a refutes B on a
    presumption that A is refuted, which grafting binds to t's outer
    binder. On the graph that is A[t] <- B[t], B[c] <- A[c], a cycle
    through contrariness. No term-level inference is needed to settle it.
    """

    def _graph(self, t_strict: bool):
        from core.dc.debate_graph import DebateGraph
        g = DebateGraph()
        g.add_node("A"); g.add_node("B")
        if t_strict:
            g.add_edge(edge("t", K("A"), "term",
                            [Source(K("B"), "term", "assumption", "b")], strict=True))
            g.add_edge(edge("bax", K("B"), "term", [], strict=True, role="supporter"))
        else:
            g.add_edge(edge("t", K("A"), "term", [src("B", "term", "presumption")]))
        g.add_edge(edge("a", K("B"), "context",
                        [src("A", "context", "presumption")], role="attacker"))
        return g

    def test_it_is_a_derivation_cycle(self):
        assert not self._graph(False).is_acyclic()
        assert not self._graph(True).is_acyclic()

    def test_strict_rootstock_is_a_proof_by_contradiction(self):
        """The caught wing (a) is OUT and discarded; t stands. One model."""
        g = self._graph(True)
        labels = grounded_labels(g)
        assert labels[(K("A"), "term")] == "IN"
        assert labels[(K("B"), "context")] == "OUT"
        assert labels[(K("A"), "context")] == "OUT"
        from core.comp.oracle import two_valued_models
        assert len(two_valued_models(graph_to_adf(g))) == 1
        assert grounded_labels(g) == grounded_labels_via_oracle(g)

    def test_defeasible_rootstock_is_credulously_contested(self):
        """Grounded UNDEC; some model accepts t, another accepts a."""
        g = self._graph(False)
        assert set(grounded_labels(g).values()) == {"UNDEC"}
        from core.comp.oracle import two_valued_models
        models = two_valued_models(graph_to_adf(g))
        at, bc = f"{K('A')}\x01term", f"{K('B')}\x01context"
        assert any(m[at] for m in models)       # t credulously accepted
        assert any(m[bc] for m in models)       # a credulously accepted
        assert not any(m[at] and m[bc] for m in models)
