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
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, Goal("g2", prop), scion_ctx))


def readable(condition, graph=None):
    """The notation the course prints conditions in."""
    tag = condition[0]
    if tag == "const":
        return "true" if condition[1] else "false"
    if tag == "var":
        key, side = split_statement(condition[1])
        name = graph.nodes[key] if graph else key
        return f"{name}[{side[0]}]"
    if tag == "not":
        return f"~{readable(condition[1], graph)}"
    if tag in ("and", "or"):
        empty, glue = ("true", " & ") if tag == "and" else ("false", " | ")
        parts = condition[1]
        if not parts:
            return empty
        if len(parts) == 1:
            return readable(parts[0], graph)
        return "(" + glue.join(readable(p, graph) for p in parts) + ")"
    raise AssertionError(condition)


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
            "Q[t]": "(true & ~Q[c])",
            "Q[c]": "(true & ~Q[t])",
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
