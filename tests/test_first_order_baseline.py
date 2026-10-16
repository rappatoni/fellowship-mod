"""The state the first-order expansion starts from (tasks.org,
aida-first-order-reduction; pinned 2026-10-16 after
aida-fo-resolve-unwired).

- Every proof term AIDA parses goes through the resolution pass, so the
  first-order constructors are first-order nodes (``LamdaFO``,
  ``ConsFO``, ...), and the two first-order fixtures still replay.
- A quantified proposition used as a whole - presumed, owed, refuted,
  used through declared rules - labels and evaluates like any statement
  (copies of eval/literature2/fo_opaque.fspy and rio_fo.fspy).
- A quantifier step is refused with ``FirstOrderNotSupported``, naming
  the construct: at registration (the argument is left out of the
  document graph, with the warning on its `dixi` entry) and at every
  query.  Before the resolution pass was wired in, ``elim [tweety]`` was
  misread as a propositional cell and refused with "declared name
  'tweety' has kind 'sort'" (the literature evaluation's bug 8).
"""

import warnings
from pathlib import Path

import pytest

from core.ac.ast import ProofTerm
from wrap.cli import setup_prover
from wrap.service import Service, AidaError

TESTS = Path(__file__).parent
REFUSAL = "does not support first-order terms yet, and this one contains a ConsFO"


def kinds(node):
    found, stack = set(), [node]
    while stack:
        n = stack.pop()
        if isinstance(n, ProofTerm):
            found.add(type(n).__name__)
            stack.extend(v for v in vars(n).values() if isinstance(v, ProofTerm))
    return found


@pytest.fixture
def check():
    provers = []

    def run(script):
        prover = setup_prover()
        provers.append(prover)
        svc = Service.of(prover)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            report = svc.check((TESTS / script).read_text())
        return prover, svc, report

    yield run
    for prover in provers:
        prover.close()


@pytest.mark.parametrize("script, expected", [
    ("fo_forall.fspy", {"ConsFO"}),
    ("fo_exists_forall.fspy", {"LamdaFO", "ConsFO", "TermsPairFO", "DestructTermsPairFO"}),
])
def test_the_first_order_constructors_are_resolved_and_replay(check, script, expected):
    prover, _svc, report = check(script)
    assert report.ok
    body_kinds = set().union(*(kinds(a.body) for a in prover.arguments.values()))
    assert expected <= body_kinds


def test_a_quantified_statement_used_as_a_whole_labels_and_evaluates(check):
    prover, svc, report = check("first_order/opaque.fspy")
    assert report.ok
    assert {"pc", "refuted"} <= {e.name for e in prover.graph.edges}
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        classes = {name: svc.evaluate(svc.target(name)).nf_class for name in ("gen", "pc", "refuted")}
    # the generalisation is presumed and strictly refuted by the
    # counter-instance; its instance at c rests on it
    assert classes == {"gen": "exception", "pc": "open", "refuted": "value"}


def test_an_existential_presumed_as_a_whole_is_a_value(check):
    _prover, svc, report = check("first_order/opaque_existential.fspy")
    assert report.ok
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        assert svc.evaluate(svc.target("obliged")).nf_class == "value"


def test_a_quantifier_step_is_refused_as_first_order(check):
    prover, svc, report = check("fo_dropped.fspy")
    dixi, = [e for e in report.entries if e.kind == "dixi"]
    assert "ConsFO" in {k for k in kinds(prover.arguments["flies_tweety"].body)}
    assert any(REFUSAL in d.message for d in dixi.diagnostics)
    assert "flies_tweety" not in {e.name for e in prover.graph.edges}
    for query in (svc.graph, svc.label, svc.evaluate):
        with pytest.raises(AidaError, match="first-order terms"):
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                query(svc.target("flies_tweety"))


def test_register_refuses_a_name_it_cannot_resolve(check):
    _prover, svc, _report = check("first_order/opaque_existential.fspy")
    with pytest.raises(AidaError, match="neither bound nor declared"):
        svc.register("r", "Inform [e1] [ctrl] [subj]", "μthesis:Inform e1 ctrl subj.<ax||Zed*thesis>")
