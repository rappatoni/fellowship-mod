"""The demo files run end to end, %stop included, and end in the state the
narration promises.  ACDC_NO_OPEN keeps `graph ... show` from opening a
viewer."""

import os
import warnings
from pathlib import Path

import pytest

from wrap.cli import setup_prover, execute_script
from core.dc.debate_graph import canonical_prop, declaration_kinds
from core.dc.strict import compile_issue
from core.dc.unfold import unfold
from core.comp.adf_label import grounded_labels
from core.comp.evaluate import evaluate_debate

DEMOS = sorted(str(p) for p in Path("tests/demo").glob("0*.fspy"))


@pytest.fixture
def prover(monkeypatch):
    monkeypatch.setenv("ACDC_NO_OPEN", "1")
    p = setup_prover()
    yield p
    p.close()


def run(prover, script):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, script, strict=False, stop_on_error=False, isolate=False, stop_marker=False)


def issue_verdict(prover, name, mode="skeptical"):
    arg = prover.get_argument(name)
    term = unfold(prover.document, prover.issue_of(arg))
    sn, sk = prover.declarations.keys(), declaration_kinds(prover.declarations)
    _, cls, sigma, _ = evaluate_debate(term, name, strict_names=sn, strict_kinds=sk, mode=mode)
    return cls, sigma[prover.issue_of(arg)]


@pytest.mark.parametrize("script", DEMOS)
def test_demo_runs_end_to_end(prover, script):
    run(prover, script)


def test_02_verbs_and_refusal(prover):
    run(prover, "tests/demo/02_support_attack.fspy")
    assert [str(m) for m in prover.debates["dsup"].moves] == ["tweety.", "support wings tweety."]
    assert [str(m) for m in prover.debates["datt"].moves] == ["tweety.", "attack penguin tweety."]
    assert [str(m) for m in prover.debates["bad"].moves] == ["tweety."]   # the support is refused
    assert issue_verdict(prover, "tweety", "skeptical")[0] != "value"
    assert issue_verdict(prover, "tweety", "credulous")[0] == "value"


def test_04_document_contest(prover):
    run(prover, "tests/demo/04_document.fspy")
    # a strict refutation registered later grounds the presumed rule
    assert issue_verdict(prover, "tweety")[1] == "OUT"
    assert issue_verdict(prover, "tweety", "credulous")[0] != "value"


def test_05_even_loop_symmetric(prover):
    run(prover, "tests/demo/05_even_loop.fspy")
    for name in ("Pdefault", "Qdefault"):
        assert issue_verdict(prover, name, "skeptical")[0] != "value"
        assert issue_verdict(prover, name, "credulous")[0] == "value"


def test_06_peirce_is_a_theorem(prover):
    run(prover, "tests/demo/06_peirce.fspy")
    cls, label = issue_verdict(prover, "p1")
    assert (cls, label) == ("value", "IN")
    assert prover.typecheck_enabled
