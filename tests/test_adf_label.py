"""M4: grounded ADF labelling of debate graphs.

Expectations are hand-derived from the acceptance-condition semantics in
core/comp/adf_label.py's docstring; the production Kleene-fixpoint path is
checked against the naive M0 oracle on every graph, and against the
third-party adf-bdd solver where installed.
"""

import warnings

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Deleg, ID, DI
from core.comp.adf_label import (
    grounded_labels, grounded_labels_kleene, grounded_labels_via_oracle,
    graph_to_adf, strict_contradictions, compile_conditions,
    NonUnipolarConditions, OpposingPresumptions, opposing_presumptions,
    _stmt,
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
    """Presumption for Q on both sides: mutual defeasible rebut.

    This is the onus conflict of aida-onus-delegation-polarity - both
    sides delegate the burden of refutation - so compiling it warns.  It
    is kept as a fixture because the warning, not a refusal, is what the
    compiler does today.
    """
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

    def test_defeasible_challenge_defeats_a_bare_presumption(self):
        # Q[term] is a bare default: it delegated the onus, so it does not
        # guard back against the challenge, and the challenge decides.
        # Same verdict as the strict challenge above; strictness separates
        # the two only when both sides are derived.
        labels = grounded_labels(graph_defeasible_challenge_vs_presumption())
        assert labels[(canonical_prop("S"), "term")] == "IN"
        assert labels[(Q, "context")] == "IN"
        assert labels[(Q, "term")] == "OUT"


class TestProductionAgainstOracle:
    @pytest.mark.parametrize("graph", ALL_GRAPHS, ids=lambda g: ",".join(sorted(g.nodes.values())))
    def test_primary_adf_bdd_equals_oracle_grounded(self, graph):
        assert grounded_labels(graph) == grounded_labels_via_oracle(graph)

    @pytest.mark.parametrize("graph", ALL_GRAPHS, ids=lambda g: ",".join(sorted(g.nodes.values())))
    def test_kleene_cross_check_equals_oracle_grounded(self, graph):
        assert grounded_labels_kleene(graph) == grounded_labels_via_oracle(graph)

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

    def test_detected_from_a_compiled_term(self):
        """CONTR reached from a term, not a hand-built graph: the primitive
        contrariness term with a declared proof and a denied refutation."""
        term = Mu(ID("att", "Q"), "Q",
                  Mu(ID("_", "Q"), "Q", DI("t", "Q"), ID("att", "Q")),
                  Mutilde(DI("_", "Q"), "Q", DI("t", "Q"), ID("nq", "Q")))
        g = compile_debate(term, "clash", strict_names={"t", "nq"},
                           strict_kinds={"t": "prop", "nq": "moxia"})
        assert strict_contradictions(g) == {Q}
        assert grounded_labels(g) == grounded_labels_via_oracle(g)


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


class TestNonUnipolarShape:
    """The one condition shape the compiler can emit where the Kleene
    fixpoint diverges from the definition: an edge whose source is the
    contrary of its target, Q[t] <- Q[c], giving Q[t] = (Q[c] & ~Q[c]).

    The Kleene fixpoint is wrong on it (a false UNDEC where the definition
    says OUT); adf-bdd, the primary labeller, and the oracle are right and
    agree, and the Kleene cross-check refuses the shape (lesson 8 item 15,
    decided 2026-09-16).

    Since the asymmetric guard (2026-09-25) the shape needs the contrary
    side to be DERIVED as well, otherwise the guard is dropped and the
    condition is unipolar - see ``test_bare_contrary_makes_the_shape_unipolar``.
    """

    def _graph(self):
        g = DebateGraph()
        g.add_node("Q")
        g.add_node("S")
        g.add_edge(edge("byContra", Q, "term",
                        [src(Q, "context", "obligation")]))
        g.add_edge(edge("nQ", Q, "context",
                        [src(canonical_prop("S"), "term", "presumption")],
                        role="attacker"))
        return g

    def test_primary_path_is_right_and_kleene_refuses(self):
        g = self._graph()
        labels = grounded_labels(g)                                   # adf-bdd
        assert labels[(Q, "term")] == "OUT" and labels[(Q, "context")] == "IN"
        assert grounded_labels_via_oracle(g) == labels                # the definition
        with pytest.raises(NonUnipolarConditions, match="both"):
            grounded_labels_kleene(g)                                 # refuses, never answers

    def test_adf_bdd_agrees_with_the_definition(self):
        from core.comp.oracle import grounded_interpretation
        adf = graph_to_adf(self._graph())
        theirs = run_adf_bdd(adf, "grounded")
        assert len(theirs) == 1
        assert theirs[0] == grounded_interpretation(adf)

    def test_bare_contrary_makes_the_shape_unipolar(self):
        """Q[t] <- Q[c] with Q[c] a bare presumption: the guard is dropped,
        so Q[t] = Q[c] and all three labellers agree.  The reductio is then
        an odd loop through the contrariness link and grounds UNDEC."""
        g = DebateGraph()
        g.add_node("Q")
        g.add_edge(edge("byContra", Q, "term",
                        [src(Q, "context", "presumption")]))
        rendered = {s: c for s, c in compile_conditions(g).items()}
        assert rendered[_stmt(Q, "term")] == ("or", (("and", (("var", _stmt(Q, "context")),)),))
        labels = grounded_labels(g)
        assert labels[(Q, "term")] == "UNDEC" and labels[(Q, "context")] == "UNDEC"
        assert grounded_labels_via_oracle(g) == labels
        assert grounded_labels_kleene(g) == labels        # no longer refuses


class TestAsymmetricGuard:
    """A derivation is contested only by a derivation (2026-09-25).

    A presumption delegates the onus of refutation to the other side, so a
    side that is nothing but a default marker does not guard back against
    an argument for the contrary.
    """

    def _presumed_vs_argued(self, dead_premise=False, presume_term=True):
        """Q presumed on the term side; an argument for Q[context] from S.
        With dead_premise, Q[term] additionally carries an argument whose
        own premise is an undischarged obligation."""
        g = DebateGraph()
        for text in ("Q", "S", "D"):
            g.add_node(text)
        if presume_term:
            g.mark_default(Q, "term", "presumption")
        g.add_edge(edge("nQ", Q, "context",
                        [src(canonical_prop("S"), "term", "presumption")],
                        role="attacker"))
        if dead_premise:
            g.add_edge(edge("weak", Q, "term",
                            [src(canonical_prop("D"), "term", "obligation")]))
        return g

    def test_argument_beats_a_bare_presumption(self):
        labels = grounded_labels(self._presumed_vs_argued())
        assert labels[(Q, "context")] == "IN"
        assert labels[(Q, "term")] == "OUT"

    def test_the_presumption_keeps_its_guard(self):
        """The dropped guard is on the DERIVATIONS of a side, not on the
        side.  A dead argument must not decide a contest between two bare
        defaults, so Q[term]'s presumption disjunct stays guarded."""
        g = self._presumed_vs_argued()
        g.defaults.pop((canonical_prop("S"), "term"), None)   # S now unsupported
        g.mark_default(canonical_prop("S"), "term", "obligation")
        labels = grounded_labels(g)
        assert labels[(canonical_prop("S"), "term")] == "OUT"
        assert labels[(Q, "context")] == "OUT"      # the challenge fails
        assert labels[(Q, "term")] == "IN"          # so the presumption stands

    def test_a_failed_argument_does_not_decide_a_contest_of_defaults(self):
        """Q presumed on both sides, plus a dead argument for Q[term].  The
        dead argument must change nothing: without the per-disjunct guard
        it would silently win the contest."""
        g = self._presumed_vs_argued(dead_premise=True)
        g.defaults.pop((canonical_prop("S"), "term"), None)
        g.mark_default(canonical_prop("S"), "term", "obligation")
        g.mark_default(Q, "context", "presumption")
        with pytest.warns(OpposingPresumptions):
            labels = grounded_labels(g)
        assert labels[(canonical_prop("D"), "term")] == "OUT"
        assert labels[(Q, "term")] == "UNDEC" and labels[(Q, "context")] == "UNDEC"

    def test_two_derivations_still_attack_each_other(self):
        """Both sides derived: the guard survives on both, so the mutual
        rebut is preserved and grounds UNDEC."""
        g = self._presumed_vs_argued(presume_term=False)
        g.add_edge(edge("qArg", Q, "term",
                        [src(canonical_prop("D"), "term", "presumption")]))
        labels = grounded_labels(g)
        assert labels[(Q, "term")] == "UNDEC" and labels[(Q, "context")] == "UNDEC"
        assert grounded_labels_via_oracle(g) == labels

    def test_undercut_of_a_presumed_premise_is_decisive(self):
        """The motivating case (tests/multiple_undercuts.fspy): B argued
        from a presumed A, three counterarguments to A of which one stands.
        The document grounds two-valued instead of leaving the whole
        contest UNDEC, and the derivation cycle A[t] -> B[t] -> A[c] ~ A[t]
        stops mattering for the labels."""
        g = DebateGraph()
        for text in ("A", "B", "C", "D"):
            g.add_node(text)
        C_, D_ = canonical_prop("C"), canonical_prop("D")
        g.add_edge(edge("arg1", canonical_prop("B"), "term",
                        [src(canonical_prop("A"), "term", "presumption")]))
        g.add_edge(edge("counter1", canonical_prop("A"), "context",
                        [src(canonical_prop("B"), "term", "obligation")], role="attacker"))
        g.add_edge(edge("counter2", canonical_prop("A"), "context",
                        [src(C_, "term", "presumption")], role="attacker"))
        g.add_edge(edge("counter3", canonical_prop("A"), "context",
                        [src(D_, "term", "obligation")], role="attacker"))
        assert not g.is_acyclic()
        labels = grounded_labels(g)
        assert labels == {
            (canonical_prop("B"), "term"): "OUT",
            (canonical_prop("A"), "term"): "OUT",
            (canonical_prop("A"), "context"): "IN",
            (C_, "term"): "IN",
            (D_, "term"): "OUT",
        }
        assert grounded_labels_via_oracle(g) == labels

    def test_strict_derivation_counts_as_a_derivation(self):
        """The contrary side is 'derived' if it has ANY edge, strict edges
        included; a strict refutation is not weakened by the new rule."""
        labels = grounded_labels(graph_strict_challenge_vs_presumption())
        assert labels[(Q, "context")] == "IN" and labels[(Q, "term")] == "OUT"


class TestOpposingPresumptions:
    """Both sides delegating the onus is an onus conflict; the compiler
    warns and still labels (task aida-onus-delegation-polarity)."""

    def test_reported_and_warned(self):
        g = graph_contested_presumptions()
        assert opposing_presumptions(g) == [Q]
        with pytest.warns(OpposingPresumptions, match="delegate the onus"):
            compile_conditions(g)

    def test_no_warning_when_the_onus_sits_on_one_side(self):
        for g in (graph_strict_challenge_vs_presumption(),
                  graph_defeasible_challenge_vs_presumption(),
                  graph_support_chain("presumption")):
            assert opposing_presumptions(g) == []
            with warnings.catch_warnings():
                warnings.simplefilter("error", OpposingPresumptions)
                compile_conditions(g)

    def test_opposing_obligations_are_not_a_conflict(self):
        """Nobody delegated, so there is no onus clash: both sides simply
        stay OUT until an argument arrives."""
        g = DebateGraph()
        g.add_node("Q")
        g.mark_default(Q, "term", "obligation")
        g.mark_default(Q, "context", "obligation")
        assert opposing_presumptions(g) == []
        with warnings.catch_warnings():
            warnings.simplefilter("error", OpposingPresumptions)
            labels = grounded_labels(g)
        assert labels[(Q, "term")] == "OUT" and labels[(Q, "context")] == "OUT"


class TestNoFallback:
    def test_missing_binary_refuses(self):
        from core.comp.oracle import run_adf_bdd, AdfBddNotFound
        adf = graph_to_adf(graph_support_chain("presumption"))
        with pytest.raises(AdfBddNotFound):
            run_adf_bdd(adf, "grounded", binary="/nonexistent/adf-bdd")

    def test_primary_path_is_not_kleene(self):
        """The guard against silent drift: the primary labeller must not be
        the in-tree fixpoint.  It is exercised by the non-unipolar shape,
        where the two disagree; see TestNonUnipolarShape."""
        from core.comp import adf_label
        assert adf_label.grounded_labels is not adf_label.grounded_labels_kleene


class TestEmptyGraph:
    """An empty document graph - a fresh session, or right after `lk.` resets
    the document - makes adf-bdd panic on the empty input it would receive
    (exit 101, parsing error).  The solver boundary answers itself: one
    labelling, the empty one, under every semantics."""

    @pytest.mark.parametrize("semantics", ["grounded", "complete", "preferred", "stable"])
    def test_one_empty_labelling(self, semantics):
        from core.comp.adf_label import labellings
        assert labellings(DebateGraph(), semantics) == [{}]

    def test_grounded_labels_of_nothing(self):
        assert grounded_labels(DebateGraph()) == {}

    def test_graph_document_show_on_an_empty_document(self, prover, caplog):
        """`graph document show` before anything is registered.  The autouse
        store reset leaves this test's document empty."""
        import logging
        from wrap.cli import graph_argument_cmd
        caplog.set_level(logging.INFO)
        graph_argument_cmd(prover, "document", show=True)
        assert any("0 nodes, 0 edges" in r.getMessage() for r in caplog.records)
