"""The argument discipline of the pure level (core/dc/discipline.py;
tasks.org, aida-argument-lj-restriction): arguments linear,
counterarguments dual-linear, no debate scaffold in either; enforced at
`dixi.` and by `register`."""

import glob
import warnings
from pathlib import Path

import pytest

from conftest import DEFAULT_SCRIPTS
from core.ac.ast import Mu, Mutilde, Lamda, Hyp, Cons, Goal, Laog, Deleg, Geled, ID, DI
from core.dc.debate_graph import DebateGraph, canonical_prop as K
from core.dc.discipline import violations, without_negation_wrappers
from core.dc.unfold import Unfolder
from wrap.cli import setup_prover
from wrap.service import Service

TESTS = Path(__file__).parent


def cons(t, e, prop):
    n = Cons(t, e)
    n.prop = prop
    return n


def lam(x, xprop, body, prop):
    n = Lamda(Hyp(DI(x, xprop), xprop), body)
    n.prop = prop
    return n


def refusals(text):
    """(line, message) of every refused unit of a document check."""
    prover = setup_prover()
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            report = Service.of(prover).check(text)
        return [(e.span.line, e.message) for e in report.entries if e.status == "refused"], \
            {n: a for n, a in prover.arguments.items()}
    finally:
        prover.close()


# -- refused ------------------------------------------------------------------

PEIRCE = """lk.
declare P,Q:bool.
argument peirce : (((P->Q)->P)->P).
elim f.
cut ((P->Q)->P) a.
axiom f.
elim.
elim h.
cut (P) z.
axiom h.
axiom a.
axiom a.
dixi.
"""


def test_peirces_law_is_refused_as_an_argument():
    found, registered = refusals(PEIRCE)
    assert "peirce" not in registered
    assert found == [(13, "'peirce' is not a argument of the pure level: not linear: the continuation "
                          "'a' is used 2 times. An argument is linear: each continuation used once, "
                          "with no second continuation binder between. Debates, not arguments, may "
                          "be classical.")]


def test_a_lambda_bar_body_using_the_continuation_is_refused():
    text = ("lk.\ndeclare A,D:bool.\ndeclare r7: (A - D).\n"
            "argument admaltest : (A).\ncut (A-D) th.\naxiom r7.\nelim alpha.\naxiom th.\ndixi.\n")
    found, registered = refusals(text)
    assert "admaltest" not in registered
    assert len(found) == 1 and found[0][0] == 9
    assert "the continuation 'th' is used under a second continuation binder" in found[0][1]


def test_register_takes_the_same_discipline():
    prover = setup_prover()
    try:
        svc = Service.of(prover)
        svc.check("lk.\ndeclare P:bool.\n")
        from wrap.service import ContentRefused
        # the four scaffold shapes over open sites, typed as terms
        u = Unfolder(DebateGraph())
        u.graph.add_node("P")
        P, Pc = (K("P"), "term"), (K("P"), "context")
        shapes = [u._support(P, Deleg("1", "P"), Deleg("2", "P"), "alt", "b"),
                  u._attack(P, Deleg("1", "P"), Geled("2", "P"), "alt", "b"),
                  u._support(Pc, Geled("1", "P"), Geled("2", "P"), "alt", "b"),
                  u._attack(Pc, Geled("1", "P"), Deleg("2", "P"), "alt", "b")]
        from pres.gen import pres_str
        for i, shape in enumerate(shapes):
            with pytest.raises(ContentRefused, match="holds a debate scaffold"):
                svc.register(f"s{i}", "P", pres_str(shape), strict=False)
        assert not prover.arguments
    finally:
        prover.close()


def test_the_four_scaffold_shapes_are_refused_on_both_sides():
    u = Unfolder(DebateGraph())
    u.graph.add_node("P")
    P, Pc = (K("P"), "term"), (K("P"), "context")
    assert violations(u._support(P, Deleg("1", "P"), Deleg("2", "P"), "alt", "b"), False)
    assert violations(u._attack(P, Deleg("1", "P"), Geled("2", "P"), "alt", "b"), False)
    assert violations(u._support(Pc, Geled("1", "P"), Geled("2", "P"), "alt", "b"), True)
    assert violations(u._attack(Pc, Geled("1", "P"), Deleg("2", "P"), "alt", "b"), True)
    # the scaffold rule is what refuses the two shapes linearity lets through
    att = u._attack(P, Deleg("1", "P"), Geled("2", "P"), "alt", "b")
    assert violations(att, False) == ["holds a debate scaffold: the pure command under 'b' "
                                      "shares 'b' between its two alternatives"]


# -- accepted -----------------------------------------------------------------

def test_refutations_through_a_proved_negation_are_dual_linear():
    """Lesson 9's con and killC: one hypothesis each once the negation
    wrapper is seen through."""
    text = (TESTS / "minicourse" / "lesson8a_attackrule.fspy").read_text()
    found, registered = refusals(text)
    assert found == []
    assert {"pro", "con", "killC", "attackrule"} <= set(registered)
    con = registered["con"].body
    assert violations(con, True) == []
    from pres.gen import pres_str
    assert "H1" in pres_str(con) and "H1" not in pres_str(without_negation_wrappers(con))


def test_a_contraction_refutation_is_refused():
    """mu'h:A.<rr || h*h*?false>: the hypothesis used twice.  Fellowship's lj
    accepts it; dual-linearity (COMMA 2026: exactly once) does not, and the
    horn rule needs it so - with contraction a hypothesis can stand in both
    halves of its own command, and the spine is no longer single."""
    text = ("lj.\ndeclare A:bool.\ndeclare rr: (A -> A -> false).\n"
            "counterargument twice : (A).\ncut (A -> A -> false) h.\naxiom rr.\nelim.\naxiom.\nelim.\naxiom.\ndixi.\n")
    found, registered = refusals(text)
    assert "twice" not in registered
    assert len(found) == 1 and "the hypothesis 'h' is used 2 times" in found[0][1]


def test_intuitionistic_shapes_with_a_let_and_a_double_negation_pass():
    x = lambda: DI("x", "A")
    let_form = Mu(ID("out", "A->C"), "A->C", lam("x", "A", Mu(ID("al", "C"), "C",
                  Mu(ID("be", "B"), "B", DI("g", "A->B"), cons(x(), ID("be", "B"), "A->B")),
                  Mutilde(DI("y", "B"), "B", DI("h", "A->B->C"),
                          cons(x(), cons(DI("y", "B"), ID("al", "C"), "B->C"), "A->B->C"))), "A->C"),
                  ID("out", "A->C"))
    assert violations(let_form, False) == []
    PE = "((P->Q)->P)->P"
    KT = f"({PE})->Q"
    k = lambda: DI("k", KT)
    inner = lam("p", "P", Mu(ID("ga", "Q"), "Q", k(), cons(lam("u", "(P->Q)->P", DI("p", "P"), PE), ID("ga", "Q"), KT)), "P->Q")
    pk = lam("f", "(P->Q)->P", Mu(ID("be", "P"), "P", DI("f", "(P->Q)->P"), cons(inner, ID("be", "P"), "(P->Q)->P")), PE)
    nn = Mu(ID("out", f"({KT})->Q"), f"({KT})->Q", lam("k", KT, Mu(ID("al", "Q"), "Q", k(), cons(pk, ID("al", "Q"), KT)), f"({KT})->Q"), ID("out", f"({KT})->Q"))
    assert violations(nn, False) == []


def test_an_unused_continuation_is_allowed():
    """A proof by contradiction with an affine root (Peirce's s2 shape)."""
    s2 = Mu(ID("beta", "P"), "P", lam("h", "P", Goal("1", "Q"), "P->Q"), Laog("2", "P->Q"))
    assert violations(s2, False) == []


# -- the census --------------------------------------------------------------

FIXTURES = sorted({str(TESTS / s) for s in DEFAULT_SCRIPTS}
                  | set(glob.glob(str(TESTS / "minicourse" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "demo" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "rationality" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "*.fspy"))))

#: The two fixture arguments the pure level refuses, by design.
REFUSED = {("alternative_arguments.fspy", "admaltest"), ("marius_example.fspy", "arg0")}


@pytest.mark.parametrize("script", FIXTURES, ids=lambda p: str(Path(p).relative_to(TESTS)))
def test_fixture_census(script):
    found, _ = refusals(Path(script).read_text())
    discipline = [m for _, m in found if "of the pure level" in m]
    expected = {name for file, name in REFUSED if file == Path(script).name}
    assert {m.split("'")[1] for m in discipline} == expected
