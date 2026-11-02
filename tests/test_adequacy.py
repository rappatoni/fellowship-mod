"""Adequacy, the IN half (debate-graph-spec.org, T3; tasks.org,
aida-judge-by-what-it-stands-for): if the witness labelling sigma labels
the issue IN, the evaluated debate term is a value.

Since 2026-10-16 sigma judges every expression by what it stands for in
the term, so a site is instantiated only by a founded derivation; a
statement IN without one (an unfounded loop accepted credulously) stays
an indeterminate, which the discharge names IN - still a value.  Checked
for every argument and statement of the fixtures, skeptically and under
every accepting preferred witness."""

import glob
import warnings
from pathlib import Path

import pytest

from conftest import DEFAULT_SCRIPTS
from core.dc.unfold import argument_edge
from pres.gen import pres_str
from wrap.cli import setup_prover
from wrap.service import LogicRefused, Service, TypeCheckRefused

TESTS = Path(__file__).parent
FIXTURES = sorted({str(TESTS / s) for s in DEFAULT_SCRIPTS}
                  | set(glob.glob(str(TESTS / "minicourse" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "demo" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "exposure" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "first_order" / "*.fspy")))
                  | {str(TESTS / "entrypoints.fspy"), str(TESTS / "circular_supporter.fspy"),
                     str(TESTS / "rationality" / "self_attack_lk.fspy"),
                     str(TESTS / "rationality" / "even_loop_lk.fspy")})


#: Known violations, (fixture, target, where): tests/test1.fspy is an
#: inconsistent document - two strict derivations of EfficientMetro and
#: ~EfficientMetro - and a strict argument facing a strict refutation is
#: the uncatchable exception, whatever the labels say (tasks.org,
#: aida-strict-layer, aida-uncatchable-exception-terminology).  Present
#: before 2026-10-16 as well.
KNOWN = {("test1.fspy", text, where)
         for text in ("arg1", "issue :UseMetro", "issue :EfficientMetro", "issue EfficientMetro:")
         for where in ("skeptical", "witness 1")}
#: tests/first_order/hypothesis_horn.fspy: b proves its statement by ex
#: falso under lambda-bound hypotheses, which classify_nf takes for an
#: uncatchable clash (tasks.org, aida-exfalso-under-lambda-exception).
KNOWN |= {("hypothesis_horn.fspy", text, where)
          for text in ("b", "issue :forall z:Pub,(Dr z->false)->"
                            "(exists x:Pub,Dr x->(forall y:Pub,Dr y))")
          for where in ("skeptical", "witness 1")}

#: The fixtures with nothing to check: their debates are refused (lj) or no
#: issue of theirs is IN.  Pinned, so that a fixture cannot drop out unnoticed.
NOTHING_TO_CHECK = {"exposure/lemma_scope_cases.fspy", "first_order/birds_fly_open.fspy",
                    "fo_exists_forall.fspy", "fo_forall.fspy", "rationality/even_loop.fspy",
                    "rationality/self_attack.fspy", "tactics.fspy"}


def targets(prover):
    found = [name for name in prover.arguments if argument_edge(prover.graph, name) is not None]
    for key, side in prover.graph.statements():
        prop = prover.graph.nodes.get(key, key)
        found.append(f"issue :{prop}" if side == "term" else f"issue {prop}:")
    return found


@pytest.mark.parametrize("script", FIXTURES, ids=lambda p: str(Path(p).relative_to(TESTS)))
def test_an_issue_in_under_the_witness_evaluates_to_a_value(script):
    prover = setup_prover()
    svc = Service.of(prover)
    name = Path(script).name
    checked, violations = 0, set()
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            svc.check(Path(script).read_text())
            for text in targets(prover):
                try:
                    target = svc.target(text)
                    skeptical = svc.evaluate(target, mode="skeptical")
                    witnesses = svc.evaluate(target, mode="credulous", witness="all")
                except (LogicRefused, TypeCheckRefused):
                    continue                  # refused (lj, first order, ...): not this check's business
                issue = svc.issue_term(target, want_term=False).issue
                if skeptical.sigma.get(issue) == "IN":
                    checked += 1
                    if skeptical.nf_class != "value":
                        violations.add((name, text, "skeptical"))
                for number, nf, cls, sigma in witnesses.results:
                    if sigma.get(issue) == "IN":
                        checked += 1
                        if cls != "value":
                            violations.add((name, text, f"witness {number}"))
    finally:
        prover.close()
    assert (not checked) == (str(Path(script).relative_to(TESTS)) in NOTHING_TO_CHECK)
    if not checked:
        pytest.skip("no debate of this fixture is evaluated (refused or none)")
    assert violations == {k for k in KNOWN if k[0] == name}
