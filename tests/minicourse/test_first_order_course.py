"""Pins every value quoted in docs/minicourses/minicourse-first-order.org.
If the implementation changes, these fail and the lesson text must be
updated."""

import warnings
from pathlib import Path

import pytest

from pres.gen import pres_str
from wrap.cli import setup_prover
from wrap.service import Service

ROOT = Path(__file__).resolve().parents[2]
DISCOURSE = ROOT / "tests/discourse"
STEPS = (ROOT / "tests/minicourse/first_order_steps.fspy").read_text()


def fixture(name):
    return (DISCOURSE / name).read_text()


@pytest.fixture
def session():
    opened = []

    def open_(text):
        prover = setup_prover()
        svc = Service.of(prover)
        svc.set_pipeline("unfolded")
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            report = svc.check(text)
        assert report.ok
        opened.append(prover)
        return prover, svc

    yield open_
    for prover in opened:
        prover.close()


def evaluated(svc, target):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = svc.evaluate(svc.target(target))
    return result.nf_class, pres_str(result.normal_form)


def labels(svc, target):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        found = svc.label(svc.target(target))
    return [(found.graph.shown(k), s, v) for (k, s), v in found.labellings[0].items()]


def graph_text(svc, target):
    return svc.graph(svc.target(target)).graph.to_text()


# --- lesson 1 ----------------------------------------------------------

def test_lesson1_the_four_steps(session):
    prover, _svc = session(STEPS)
    term = {n: prover.get_argument(n).enriched_proof_term
            for n in ("everyone", "someone", "from_everyone", "from_someone")}
    assert "λz:Pub.!1.1:Dr z" in term["everyone"]
    assert "(c,!1.1:Dr c)" in term["someone"]
    assert "h:forall z:Pub,Dr z||c*1.1.2.1:Dr c?" in term["from_everyone"]
    assert "h:exists x:Pub,Dr x||(x:Pub).1.1.2.1:Dr x?" in term["from_someone"]


def test_lesson1_exercise2_an_open_choice_as_witness(session):
    text = (STEPS.replace("elim [c].\nby default.", "elim [!w].\nby default.")
            .replace("argument someone :", "argument someone : [!w:Pub]"))
    _prover, svc = session(text)
    assert evaluated(svc, "someone") == (
        "value", "μsomeone:exists x:Pub,Dr x.<(!w,!IN:Dr !w)||someone:exists x:Pub,Dr x>")


# --- lesson 2 ----------------------------------------------------------

def test_lesson2_weakening(session):
    _prover, svc = session(fixture("weakening.fspy"))
    text = graph_text(svc, "a")
    for line in ("`-- [y?:Pub] ⊢ Eff[term]  [obligation]",
                 "|-- weakening  (weakening, defeasible)",
                 "`-- contested by [y?:Pub] ⊢ Eff[context]"):
        assert line in text
    got = labels(svc, "a")
    assert ("[y?:Pub] ⊢ Eff", "term", "UNDEC") in got
    assert ("[y?:Pub] ⊢ Eff", "context", "UNDEC") in got
    assert ("forall y:Pub,Use", "term", "UNDEC") in got
    assert evaluated(svc, "a") == ("open", "?UNDEC:forall y:Pub,Use")


def test_lesson2_exercise2_without_the_counterargument(session):
    _prover, svc = session(fixture("weakening.fspy").split("\ncounterargument c :")[0])
    assert evaluated(svc, "a") == ("value", "!IN:forall y:Pub,Use")


# --- lesson 3 ----------------------------------------------------------

def test_lesson3_a_generic_argument_fills_a_site_under_a_binder(session):
    _prover, svc = session(fixture("peirce_fo.fspy"))
    text = graph_text(svc, "a")
    assert "`-- [?x:Pub] ⊢ Dr ?x->false[term]  [obligation]" in text
    assert "`-- [?x:Pub] ⊢ Dr ?x[context]  [presumption]" in text
    cls, nf = evaluated(svc, "a")
    assert cls == "value"
    assert "μsup:Dr z->false.<λp:Dr z.μm:false.<p:Dr z||g:Dr z>||sup:Dr z->false>*g:Dr z>" in nf


def test_lesson3_exercise1_the_sigil_does_not_matter_where_instantiated(session):
    _prover, svc = session(fixture("peirce_fo.fspy"))
    before = evaluated(svc, "a")
    _prover, svc = session(fixture("peirce_fo.fspy").replace("?x", "!x"))
    assert evaluated(svc, "a") == before


def test_lesson3_an_uninstantiated_parameter_owes_a_witness(session):
    _prover, svc = session(fixture("counter_instance.fspy"))
    got = labels(svc, "gen")
    assert ("a witness for ?z", "term", "OUT") in got
    assert ("[?z:iota] ⊢ P ?z", "term", "OUT") in got
    assert evaluated(svc, "gen") == ("open", "μx2:P ?z.<?OUT:P ?z||x2:P ?z>")


# --- lesson 4 ----------------------------------------------------------

def test_lesson4_a_closed_counterargument_contests_one_instance(session):
    _prover, svc = session(fixture("conrad_peter.fspy"))
    got = labels(svc, "brawl")
    for row in (("beat peter conrad", "term", "IN"), ("~beat peter conrad", "context", "IN"),
                ("beat conrad peter", "term", "UNDEC"), ("beat conrad peter", "context", "UNDEC"),
                ("beat conrad peter-~beat peter conrad", "term", "UNDEC")):
        assert row in got


def test_lesson4_the_lattice(session):
    _prover, svc = session(fixture("beat_lattice.fspy"))
    got = labels(svc, "fights")
    assert ("beat peter pedro", "term", "IN") in got
    assert ("beat peter conrad", "term", "UNDEC") in got
    assert ("beat conrad conrad", "term", "UNDEC") in got


# --- lesson 5 ----------------------------------------------------------

def test_lesson5_the_closed_universal_falls_as_one(session):
    _prover, svc = session(fixture("counter_instance.fspy"))
    got = labels(svc, "a")
    for row in (("forall x:iota,P x", "term", "OUT"), ("forall x:iota,P x", "context", "IN"),
                ("P c", "term", "OUT"), ("P c", "context", "IN"),
                ("[?z:iota] ⊢ P ?z", "term", "IN")):
        assert row in got
    assert evaluated(svc, "a") == (
        "exception",
        "μalt3:forall x:iota,P x.<?OUT:forall x:iota,P x||c*μ'b1:P c.<b1:P c||not_pc:P c>>")


def test_lesson5_exercise1_without_the_counterargument(session):
    _prover, svc = session(fixture("counter_instance.fspy").split("counterargument no")[0])
    assert evaluated(svc, "a") == ("value", "μa:forall x:iota,P x.<λx:iota.!IN:P x||a:forall x:iota,P x>")


# --- lesson 6 ----------------------------------------------------------

def test_lesson6_a_binder_answers_an_open_choice(session):
    _prover, svc = session(fixture("witness_import.fspy"))
    text = graph_text(svc, "a")
    assert "a witness for ?x of w[term]  [obligation]" in text
    assert "[?x:Pub] ⊢ Q ?x[term]  [obligation]" in text
    cls, nf = evaluated(svc, "a")
    assert cls == "value"
    assert "<λy:Pub.λh:Q y.μw:exists x:Pub,Q x.<(y,h:Q y)||w:exists x:Pub,Q x>||a:" in nf


def test_lesson6_a_constant_answers_and_a_question_is_never_offered(session):
    _prover, svc = session(fixture("open_choice.fspy"))
    cls, nf = evaluated(svc, "some_donkey")
    assert cls == "value" and "(pedro,!IN:donkey pedro)" in nf and "?u" not in nf
    _prover, svc = session(fixture("open_choice.fspy").split("argument pd")[0])
    assert evaluated(svc, "some_donkey") == ("open", "?OUT:exists y:Ind,donkey y")


def test_lesson6_the_sort_node(session):
    _prover, svc = session(fixture("sort_node.fspy"))
    assert ("exists x:iota,true", "term", "IN") in labels(svc, "some_farmer")
    cls, nf = evaluated(svc, "some_farmer")
    assert cls == "value" and "(!x,!IN:farmer !x)" in nf


# --- lesson 7 ----------------------------------------------------------

def test_lesson7_a_pronoun_keeps_one_reading(session):
    _prover, svc = session(fixture("anaphora.fspy"))
    got = labels(svc, "he_whistles")
    for row in (("exists x:iota,whistles x", "term", "IN"),
                ("a witness for ?c of he_whistles", "term", "OUT"),
                ("[!a:iota] ⊢ whistles !a", "term", "IN"),
                ("[!a_2:iota] ⊢ whistles !a_2", "term", "IN")):
        assert row in got
    assert evaluated(svc, "he_whistles") == (
        "value",
        "μhe_whistles:exists x:iota,whistles x.<(!a_2,!IN:whistles !a_2)||he_whistles:exists x:iota,whistles x>")


def test_lesson7_a_contested_reading_and_a_question_resolved(session):
    _prover, svc = session(fixture("anaphora_silent.fspy"))
    assert "(!a,!IN:whistles !a)" in evaluated(svc, "he_whistles")[1]
    _prover, svc = session(fixture("sort_node.fspy"))
    cls, nf = evaluated(svc, "some_farmer_owed")
    assert cls == "value" and "(!x,!IN:farmer !x)" in nf


def test_lesson7_exercises(session):
    text = fixture("anaphora.fspy")
    plumber = text[text.index("argument plumber_walks"):text.index("argument knight_walks")]
    knight = text[text.index("argument knight_walks"):text.index("argument he_whistles")]
    _prover, svc = session(text.replace(plumber, "@").replace(knight, plumber).replace("@", knight))
    assert "(!a_2,!IN:whistles !a_2)" in evaluated(svc, "he_whistles")[1]
    _prover, svc = session(text.replace("?c", "!c"))
    assert "(!c,!IN:whistles !c)" in evaluated(svc, "he_whistles")[1]


# --- lesson 8 ----------------------------------------------------------

def test_lesson8_a_label_and_the_term(session):
    _prover, svc = session(fixture("witness_import.fspy"))
    got = labels(svc, "a")
    assert ("forall y:Pub,Q y->(exists x:Pub,Q x)", "term", "IN") in got
    assert ("[y?:Pub] ⊢ exists x:Pub,Q x", "term", "OUT") in got
    assert evaluated(svc, "w") == ("open", "?OUT:exists x:Pub,Q x")
