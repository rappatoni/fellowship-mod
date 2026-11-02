"""What credulous evaluation does with a scaffold its witness labelling
leaves UNDEC (core/comp/evaluate.py, ``UNDECIDED``; tasks.org,
aida-credulous-tiebreak-grounded, decided by the author 2026-10-16).

By default ("witness") the residue is resolved as in skeptical mode: the
witness alone makes the credulous choices, so under grounded semantics
credulous and skeptical evaluation coincide.  The old credulous tiebreak
- keep an UNDEC supporter, drop an UNDEC attack - is the option
``undecided credulous``.  Fixtures: the literature evaluation's bug 3
(eval/literature/bug_credulous_tiebreak2.fspy and _value.fspy) and
tests/olon.fspy."""

import warnings
from pathlib import Path

import pytest

from pres.gen import pres_str
from wrap.cli import setup_prover
from wrap.service import Service, InvalidRequest

TESTS = Path(__file__).parent


def evaluated(script, target, mode, semantics="grounded", undecided=None):
    prover = setup_prover()
    svc = Service.of(prover)
    try:
        if undecided is not None:
            svc.set_undecided(undecided)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            svc.check((TESTS / script).read_text())
            result = svc.evaluate(svc.target(target), mode=mode, semantics=semantics)
        return result.nf_class, pres_str(result.normal_form)
    finally:
        prover.close()


CASES = [("undecided/undec_presumption.fspy", "pa"), ("undecided/undec_presumption.fspy", "sa"),
         ("undecided/undec_contested.fspy", "pa"), ("undecided/undec_contested.fspy", "sa"),
         ("olon.fspy", "first"), ("olon.fspy", "tester"), ("olon.fspy", "final"),
         ("olon.fspy", "olon")]


@pytest.mark.parametrize("script, target", CASES)
def test_under_grounded_credulous_is_skeptical(script, target):
    skeptical = evaluated(script, target, "skeptical")
    assert skeptical == ("value", "!IN:A")
    assert evaluated(script, target, "credulous") == skeptical


def test_the_old_tiebreak_kept_a_supporter_on_an_undecided_presumption():
    assert evaluated("undecided/undec_presumption.fspy", "pa", "credulous",
                     undecided="credulous") == ("open", "μsa:A.<b_a:B->A||?UNDEC:B*sa:A>")


def test_the_old_tiebreak_kept_a_derivation_grounded_does_not_accept():
    cls, nf = evaluated("undecided/undec_contested.fspy", "pa", "credulous", undecided="credulous")
    assert cls == "value" and "q_b" in nf          # pb's derivation of B, nb's attack dropped


def test_preferred_is_unchanged_here():
    for undecided in ("witness", "credulous"):
        assert evaluated("undecided/undec_presumption.fspy", "pa", "credulous", "preferred",
                         undecided) == ("value", "!IN:A")


def test_the_setting():
    prover = setup_prover()
    svc = Service.of(prover)
    try:
        assert prover.undecided == "witness"
        svc.set_undecided("credulous")
        assert prover.undecided == "credulous"
        with pytest.raises(InvalidRequest):
            svc.set_undecided("maybe")
        report = svc.check("lk.\nundecided witness.\n")
        assert [e.status for e in report.entries][-1] == "ok"
        assert prover.undecided == "witness"
    finally:
        prover.close()
