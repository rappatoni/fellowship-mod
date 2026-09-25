"""Pins every value quoted in minicourse.org.

The course quotes real transcripts.  If the implementation changes, these
assertions fail and the lesson text must be updated -- the material
cannot silently go stale.  Each test names the lesson and the claim.

The .fspy fixtures are replayed inside tmp_path because `graph ... show`
writes an image into the working directory.
"""

import pytest

from core.ac.ast import (
    Mu, Mutilde, Cons, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.comp.adf_label import (
    compile_conditions, grounded_labels, grounded_labels_via_oracle,
    split_statement,
)
from core.comp.evaluate import evaluate_debate
from core.comp.oracle import (
    ADF, var, neg, gamma, two_valued_models, complete_interpretations,
    grounded_interpretation, diamond_export,
)
from core.dc.debate_graph import (
    DebateGraph, Edge, Source, canonical_prop, compile_debate,
)
from wrap.cli import execute_script, setup_prover

from coursekit import readable  # course helper, same directory

K = canonical_prop
A, B, C, P, Q = K("A"), K("B"), K("C"), K("P"), K("Q")


# --- builders mirroring the lesson terms ------------------------------

def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def parg(site):
    """Lesson 1/4/7 host: P from one site via the axiom pRule."""
    return eta("pArg", "P",
               Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                  Cons(site, ID("rule", "P"))))


def carg(a_site, b_site):
    """Lesson 2/5 host: C from two sites via the axiom abc."""
    return eta("cArg", "C",
               Mu(ID("r", "C"), "C", DI("abc", "A->B->C"),
                  Cons(a_site, Cons(b_site, ID("r", "C")))))


def t_att(prop, orig, scion_ctx):
    # paper T-ATT: mu alt.< orig || mu'b.< mu_.<b||scion> || mu'_.<b||alt> > >
    return Mu(ID("alt", prop), prop, orig,
              Mutilde(DI("b", prop), prop,
                      Mu(ID("_", prop), prop, DI("b", prop), scion_ctx),
                      Mutilde(DI("_", prop), prop, DI("b", prop), ID("alt", prop))))


# --- Lesson 1 ---------------------------------------------------------

class TestLesson1Statements:
    def test_obligation_graph_statements(self):
        g = compile_debate(parg(Goal("1", "Q")), "pArg", strict_names={"pRule"})
        assert [(g.nodes[k], s) for k, s in g.statements()] == [("P", "term"), ("Q", "term")]

    def test_contested_graph_adds_the_context_side(self):
        challenge = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))
        g = compile_debate(parg(t_att("Q", Deleg("1", "Q"), challenge)),
                           "datt", strict_names={"pRule"})
        assert [(g.nodes[k], s) for k, s in g.statements()] == [
            ("P", "term"), ("Q", "term"), ("Q", "context")]

    def test_negation_spellings_share_a_key(self):
        assert K("~A") == K("A->false")

    def test_alpha_invariance_but_free_variables_matter(self):
        assert K("forall x:iota, Q x") == K("forall y:iota, Q y")
        assert K("Q x") != K("Q y")


# --- Lesson 3 ---------------------------------------------------------

class TestLesson3ADF:
    def even_loop(self):
        return ADF(["p", "q"], {"p": neg(var("q")), "q": neg(var("p"))})

    def test_even_loop_values(self):
        adf = self.even_loop()
        assert grounded_interpretation(adf) == {"p": None, "q": None}
        assert gamma(adf, {"p": None, "q": None}) == {"p": None, "q": None}
        assert gamma(adf, {"p": True, "q": False}) == {"p": True, "q": False}
        assert two_valued_models(adf) == [
            {"p": True, "q": False}, {"p": False, "q": True}]
        assert len(complete_interpretations(adf)) == 3

    def test_even_loop_export(self):
        text, _ = diamond_export(self.even_loop())
        assert text == "s(s1).\ns(s2).\nac(s1,neg(s2)).\nac(s2,neg(s1)).\n"

    def test_self_attack_solution(self):
        adf = ADF(["p"], {"p": neg(var("p"))})
        assert gamma(adf, {"p": None}) == {"p": None}
        assert two_valued_models(adf) == []
        assert complete_interpretations(adf) == [{"p": None}]
        assert grounded_interpretation(adf) == {"p": None}


# --- Lesson 4 ---------------------------------------------------------

class TestLesson4Conditions:
    def conditions(self, graph):
        return {
            f"{graph.nodes[split_statement(s)[0]]}[{split_statement(s)[1][0]}]":
                readable(c, graph)
            for s, c in compile_conditions(graph).items()
        }

    def test_obligation_site(self):
        g = compile_debate(parg(Goal("1", "Q")), "pArg", strict_names={"pRule"})
        assert self.conditions(g) == {"P[t]": "Q[t]", "Q[t]": "false"}

    def test_presumption_site(self):
        g = compile_debate(parg(Deleg("1", "Q")), "pArg", strict_names={"pRule"})
        assert self.conditions(g) == {"P[t]": "Q[t]", "Q[t]": "true"}

    def test_contested_is_a_negation_two_cycle(self):
        challenge = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))
        g = compile_debate(parg(t_att("Q", Deleg("1", "Q"), challenge)),
                           "datt", strict_names={"pRule"})
        assert self.conditions(g) == {
            "P[t]": "Q[t]",
            "Q[t]": "(true & not Q[c])",
            "Q[c]": "(true & not Q[t])",
        }
        assert {(g.nodes[k], s): v for (k, s), v in grounded_labels(g).items()} == {
            ("P", "term"): "UNDEC", ("Q", "term"): "UNDEC", ("Q", "context"): "UNDEC"}

    def test_lesson4_exercise_d2_conditions(self):
        g = DebateGraph()
        for text in ("A", "B", "C"):
            g.add_node(text)
        g.add_edge(Edge("d2", C, "term",
                        (Source(A, "term", "obligation", "s"),
                         Source(B, "term", "obligation", "s")), False, "argument"))
        g.add_edge(Edge("bArg", B, "term", (), True, "supporter"))
        assert self.conditions(g) == {
            "C[t]": "(A[t] & B[t])", "A[t]": "false", "B[t]": "(true | false)"}
        assert {(g.nodes[k], s): v for (k, s), v in grounded_labels(g).items()} == {
            ("C", "term"): "OUT", ("A", "term"): "OUT", ("B", "term"): "IN"}


# --- Lesson 5 ---------------------------------------------------------

class TestLesson5Labels:
    def labels(self, graph):
        return {(graph.nodes[k], s): v for (k, s), v in grounded_labels(graph).items()}

    def test_conjunction_of_sources(self):
        """One IN source does not carry the conclusion."""
        g = compile_debate(carg(Goal("1", "A"), Deleg("2", "B")),
                           "cArg", strict_names={"abc"})
        assert self.labels(g) == {
            ("C", "term"): "OUT", ("A", "term"): "OUT", ("B", "term"): "IN"}

    def test_exercise_both_kinds(self):
        both_pres = compile_debate(carg(Deleg("1", "A"), Deleg("2", "B")),
                                   "cArg", strict_names={"abc"})
        assert set(self.labels(both_pres).values()) == {"IN"}
        both_obli = compile_debate(carg(Goal("1", "A"), Goal("2", "B")),
                                   "cArg", strict_names={"abc"})
        assert set(self.labels(both_obli).values()) == {"OUT"}

    def test_production_agrees_with_oracle(self):
        g = compile_debate(carg(Goal("1", "A"), Deleg("2", "B")),
                           "cArg", strict_names={"abc"})
        assert grounded_labels(g) == grounded_labels_via_oracle(g)


# --- Lesson 7 ---------------------------------------------------------

class TestLesson7Evaluation:
    def test_obligation_site_is_out_and_open(self):
        nf, cls, labels, _ = evaluate_debate(parg(Goal("1", "Q")), "pOpen",
                                             strict_names={"pRule"})
        assert labels[(P, "term")] == "OUT"
        assert cls == "open"

    def test_presumption_site_is_in_and_a_value(self):
        nf, cls, labels, _ = evaluate_debate(parg(Deleg("1", "Q")), "pPres",
                                             strict_names={"pRule"})
        assert labels[(P, "term")] == "IN"
        assert cls == "value"

    def test_contested_modes_diverge(self):
        challenge = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))
        body = parg(t_att("Q", Deleg("1", "Q"), challenge))
        _, credulous, _, _ = evaluate_debate(body, "datt", strict_names={"pRule"},
                                             mode="credulous")
        _, skeptical, _, _ = evaluate_debate(body, "datt", strict_names={"pRule"},
                                             mode="skeptical")
        assert credulous == "value"
        assert skeptical != "value"

    def test_exercise_obligation_backed_challenge_decides(self):
        """L7 exercise 1: an obligation-backed challenge is OUT, so the
        guard releases Q[term] and both modes agree on a value."""
        challenge = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Laog("l", "Q"))
        body = parg(t_att("Q", Deleg("1", "Q"), challenge))
        g = compile_debate(body, "datt", strict_names={"pRule"})
        assert {(g.nodes[k], s): v for (k, s), v in grounded_labels(g).items()} == {
            ("P", "term"): "IN", ("Q", "term"): "IN", ("Q", "context"): "OUT"}
        for mode in ("skeptical", "credulous"):
            _, cls, _, _ = evaluate_debate(body, "datt", strict_names={"pRule"},
                                           mode=mode)
            assert cls == "value"


# --- fixtures replay ---------------------------------------------------

@pytest.mark.parametrize("script", [
    "lesson2_graph.fspy", "lesson5_labels.fspy", "lesson7_evaluate.fspy",
])
def test_lesson_fixture_replays(script, tmp_path, monkeypatch):
    """The course's .fspy fixtures run end to end.

    Replayed from tmp_path: `graph ... show` writes an image into the
    working directory, which must not land in the repository.
    """
    from pathlib import Path
    source = Path(__file__).parent / script
    monkeypatch.chdir(tmp_path)
    prover = setup_prover()
    try:
        execute_script(prover, str(source), strict=True)
    finally:
        prover.close()


class TestLesson3GammaIteration:
    """Pins the Gamma-iteration table added to Lesson 3."""

    def test_iteration_reaches_the_grounded_fixed_point(self):
        """Lesson 3 uses bare statement names: sides belong to lesson 4."""
        from core.comp.oracle import const
        adf = ADF(["a", "b"], {"a": var("b"), "b": const(True)})
        v0 = {"a": None, "b": None}
        v1 = gamma(adf, v0)
        v2 = gamma(adf, v1)
        v3 = gamma(adf, v2)
        assert v1 == {"a": None, "b": True}
        assert v2 == {"a": True, "b": True}
        assert v3 == v2                       # fixed point
        assert grounded_interpretation(adf) == v2

    def test_even_loop_is_stationary_at_all_undecided(self):
        loop = ADF(["p", "q"], {"p": neg(var("q")), "q": neg(var("p"))})
        v0 = {"p": None, "q": None}
        assert gamma(loop, v0) == v0


class TestLesson4GuardSideConditions:
    """The guard's two side conditions, which the lesson spec states.

    Pinned because the lesson originally presented the guard as
    unconditional, which does not match what compile_conditions emits.
    """

    def conditions(self, graph):
        from coursekit import conditions_of
        return conditions_of(graph)

    def build(self, edges, markers):
        from coursekit import graph_from_edges
        return graph_from_edges(["Q", "R"], edges, markers)

    def test_guard_dropped_when_defeasible_part_is_empty(self):
        g = self.build(
            [("qStrict", "Q", "term", [], True, "supporter")],
            [("Q", "term", "obligation"), ("Q", "context", "presumption")],
        )
        # Not (true | (false & not Q[c])): the guard is skipped.
        assert self.conditions(g)["Q[t]"] == "(true | false)"

    def test_no_guard_and_no_outer_or_without_strict_edges(self):
        g = self.build(
            [], [("Q", "term", "obligation"), ("Q", "context", "presumption")]
        )
        assert self.conditions(g)["Q[t]"] == "false"

    def test_guard_present_when_the_contrary_is_derived(self):
        g = self.build(
            [("e", "Q", "term", [("R", "term", "presumption")], False, "argument"),
             ("c", "Q", "context", [("R", "term", "presumption")], False, "attacker")],
            [],
        )
        assert self.conditions(g)["Q[t]"] == "(R[t] & not Q[c])"

    def test_guard_dropped_when_the_contrary_is_a_bare_default(self):
        """The asymmetric guard (2026-09-25): a side that is nothing but a
        default marker has delegated the onus and does not guard back."""
        g = self.build(
            [("e", "Q", "term", [("R", "term", "presumption")], False, "argument")],
            [("Q", "context", "presumption")],
        )
        assert self.conditions(g)["Q[t]"] == "R[t]"
        assert self.conditions(g)["Q[c]"] == "(true & not Q[t])"

    def test_a_presumption_keeps_its_guard_next_to_a_dropped_one(self):
        """The drop is per disjunct: Q[t] is presumed AND argued, so its
        presumption stays guarded while its derivation does not."""
        g = self.build(
            [("e", "Q", "term", [("R", "term", "obligation")], False, "argument")],
            [("Q", "term", "presumption"), ("Q", "context", "presumption")],
        )
        assert self.conditions(g)["Q[t]"] == "(R[t] | (true & not Q[c]))"

    def test_no_guard_when_the_contrary_is_not_materialised(self):
        g = self.build(
            [("e", "Q", "term", [("R", "term", "presumption")], False, "argument")],
            [],
        )
        assert self.conditions(g)["Q[t]"] == "R[t]"


class TestLesson2RoleIsDiagnostic:
    """A strict edge can carry role 'supporter': the two are independent.

    Pinned because the lesson 4 exercise on conjunctive support depends on
    'supporter' not being a synonym for 'defeasible edge'.
    """

    def test_strict_edge_can_be_a_supporter(self):
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["A", "B", "C"],
            [("d2", "C", "term",
              [("A", "term", "obligation"), ("B", "term", "obligation")],
              False, "argument"),
             ("bArg", "B", "term", [], True, "supporter")],
        )
        by_name = {e.name: e for e in g.edges}
        assert by_name["bArg"].role == "supporter" and by_name["bArg"].strict
        assert by_name["d2"].role == "argument" and not by_name["d2"].strict

    def test_conditions_do_not_depend_on_role(self):
        """Flipping every role leaves the acceptance conditions identical."""
        from dataclasses import replace
        from coursekit import conditions_of, graph_from_edges
        g = graph_from_edges(
            ["A", "B", "C"],
            [("d2", "C", "term",
              [("A", "term", "obligation"), ("B", "term", "obligation")],
              False, "argument"),
             ("bArg", "B", "term", [], True, "supporter")],
        )
        before = conditions_of(g)
        g.edges = [replace(e, role="attacker") for e in g.edges]
        assert conditions_of(g) == before


class TestLesson5EdgeAcyclicButUndecided:
    """The minimal witness for 'acyclic edges, still UNDEC' (lesson 5)."""

    def test_zero_edges_still_grounds_undecided(self):
        from coursekit import conditions_of, graph_from_edges
        g = graph_from_edges(
            ["Q"], [],
            [("Q", "term", "presumption"), ("Q", "context", "presumption")],
        )
        assert g.edges == []
        assert g.is_acyclic()                      # trivially: no edges
        assert conditions_of(g) == {
            "Q[t]": "(true & not Q[c])", "Q[c]": "(true & not Q[t])"}
        assert set(grounded_labels(g).values()) == {"UNDEC"}

    def test_undec_propagates_through_an_acyclic_edge(self):
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["P", "Q"],
            [("pArg", "P", "term", [("Q", "term", "presumption")], False, "argument")],
            [("Q", "context", "presumption")],
        )
        assert g.is_acyclic()
        assert set(grounded_labels(g).values()) == {"UNDEC"}


class TestLesson5KleeneVersusGamma:
    """Kleene and Gamma diverge on non-unipolar conditions (lesson 5)."""

    def test_tautology_witness(self):
        from core.comp.adf_label import kleene_eval
        from core.comp.oracle import disj
        adf = ADF(["p", "q"], {"p": disj(var("q"), neg(var("q"))), "q": const_true()})
        v0 = {"p": None, "q": None}
        assert gamma(adf, v0)["p"] is True
        assert kleene_eval(adf.ac["p"], v0) is None


def const_true():
    from core.comp.oracle import const
    return const(True)


class TestLesson5FragmentBoundary:
    """What is_acyclic refuses and admits, and its false-refusal defect."""

    def test_mutual_undercut_is_refused(self):
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["Pa", "Pb", "A", "B"],
            [("aArg", "A", "term", [("Pa", "term", "presumption")], False, "argument"),
             ("bArg", "B", "term", [("Pb", "term", "presumption")], False, "argument"),
             ("aUndercutsB", "Pb", "context", [("A", "term", "presumption")], False, "attacker"),
             ("bUndercutsA", "Pa", "context", [("B", "term", "presumption")], False, "attacker")],
        )
        assert not g.is_acyclic()

    def test_rebuttal_is_admitted_and_undecided(self):
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["Q"], [], [("Q", "term", "presumption"), ("Q", "context", "presumption")])
        assert g.is_acyclic()
        assert set(grounded_labels(g).values()) == {"UNDEC"}

    def test_transposal_pair_is_a_derivation_cycle(self):
        """Lesson 5: Q[t] <- P[t] with its transposal P[c] <- Q[c].

        The two edges share no statement, yet the arguments undermine each
        other through contrariness, so the refusal is correct. With
        presumption premises the conditions really are cyclic.
        """
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["P", "Q"],
            [("arg1", "Q", "term", [("P", "term", "presumption")], False, "argument"),
             ("arg2", "P", "context", [("Q", "context", "presumption")], False, "argument")],
        )
        assert not g.is_acyclic()
        assert set(grounded_labels(g).values()) == {"UNDEC"}

    def test_refused_even_when_no_guard_bites(self):
        """The structural notion: refused although the conditions of the
        obligation-premise version are acyclic and ground two-valued."""
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["P", "Q"],
            [("arg1", "Q", "term", [("P", "term", "obligation")], False, "argument"),
             ("arg2", "P", "context", [("Q", "context", "obligation")], False, "argument")],
        )
        assert not g.is_acyclic()
        assert set(grounded_labels(g).values()) == {"OUT"}

    def test_even_loop_closes_only_through_contrariness(self):
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["P", "Q"],
            [("pArg", "P", "term", [("Q", "context", "presumption")], False, "argument"),
             ("qArg", "Q", "term", [("P", "context", "presumption")], False, "argument")],
        )
        derivation_statements = [
            {(e.target_key, e.target_side)} | {(s.key, s.side) for s in e.sources}
            for e in g.edges
        ]
        assert not (derivation_statements[0] & derivation_statements[1])
        assert not g.is_acyclic()

    def test_proof_by_contradiction_edge_is_refused(self):
        """Q[t] <- Q[c]: a derivation edge from Q to Q. Refusing it keeps
        the compiled conditions unipolar for the Kleene labeller."""
        from coursekit import graph_from_edges
        g = graph_from_edges(
            ["Q"],
            [("byContra", "Q", "term", [("Q", "context", "presumption")], False, "argument")],
        )
        assert not g.is_acyclic()


class TestLesson8StrictRefutationRecorded:
    """Lesson 8 item 6, fixed (aida-strict-refutation-edges): a declared
    refutation in the stolen-value slot of the primitive contrariness term
    compiles to a strict context-side edge, so the clash is visible."""

    def _clash(self):
        return Mu(ID("att", "Q"), "Q",
                  Mu(ID("_", "Q"), "Q", DI("t", "Q"), ID("att", "Q")),
                  Mutilde(DI("_", "Q"), "Q", DI("t", "Q"), ID("t2", "Q")))

    def test_clash_term_compiles_to_two_strict_edges(self):
        from core.comp.adf_label import strict_contradictions
        g = compile_debate(self._clash(), "dbg", strict_names={"t", "t2"})
        sides = sorted((e.target_side, e.strict, e.sources) for e in g.edges)
        assert sides == [("context", True, ()), ("term", True, ())]
        assert strict_contradictions(g) == {Q}
        labels = grounded_labels(g)
        assert labels[(Q, "term")] == "IN" and labels[(Q, "context")] == "IN"   # CONTR

    def test_primitive_contrariness_shape_is_matched(self):
        from core.dc.debate_graph import _match_scaffold
        assert _match_scaffold(self._clash(), {"t", "t2"}) is not None
        assert _match_scaffold(self._clash()) is None      # without the names: not a scaffold

    def test_wrong_kind_is_refused(self):
        from core.dc.debate_graph import DebateCompileError
        # t2 is used as a refutation (context leaf) but declared as a proof.
        with pytest.raises(DebateCompileError, match="kind"):
            compile_debate(self._clash(), "dbg", strict_names={"t", "t2"},
                           strict_kinds={"t": "prop", "t2": "prop"})
        # and the correct kinds pass
        compile_debate(self._clash(), "dbg", strict_names={"t", "t2"},
                       strict_kinds={"t": "prop", "t2": "moxia"})

