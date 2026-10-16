"""An argument that applies a negation to an open site (tasks.org,
aida-negation-cell-horn; the literature evaluation's bug 1).

``cut (~A) g. next. elim. next.`` leaves the elimination cell
``?:A * _F_`` inside Fellowship's negation wrapper
``mu'H1:~A.< H1 || ?:A * _F_ >``.  The cell had no proposition (falsum is
flagged, not typed), so the compiler refused it and registration dropped
the argument from the document graph.  Now the cell takes the type of the
command it is cut against (core/comp/enrich.py), and off the spine the
wrapper, not the bare cell, is the horn (core/dc/debate_graph.py): the
bare cell is no refutation Fellowship can rebuild.  Fixture: a copy of
eval/literature/bug_falsum_cell.fspy."""

import warnings
from pathlib import Path

import pytest

from wrap.cli import setup_prover
from wrap.service import Service

TESTS = Path(__file__).parent
SCRIPT = TESTS / "exposure" / "negation_cell.fspy"


@pytest.fixture
def session():
    prover = setup_prover()
    svc = Service.of(prover)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        report = svc.check(SCRIPT.read_text())
    yield prover, svc, report
    prover.close()


def test_both_arguments_join_the_document_graph(session):
    prover, _svc, report = session
    assert not any("not added to the document graph" in str(d) for d in report.diagnostics)
    edges = {e.name: e for e in prover.graph.edges}
    assert {"x1", "x2", "x1.c1", "x2.c1", "na"} <= set(edges)
    for cell in ("x1.c1", "x2.c1"):
        edge = edges[cell]
        assert (prover.graph.nodes[edge.target_key], edge.target_side) == ("~A", "context")
        assert [(s.side, s.kind) for s in edge.sources] == [("term", "obligation")]
        assert prover.graph.nodes[edge.sources[0].key] == "A"
        assert type(edge.term).__name__ == "Mutilde"          # the wrapper, not the bare cell


@pytest.mark.parametrize("typecheck", ["on", "expanded"])
def test_the_debates_replay_and_evaluate(session, typecheck):
    _prover, svc, _report = session
    svc.set_typecheck(typecheck)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        for target in ("x1", "x2"):
            for mode in ("skeptical", "credulous"):
                result = svc.evaluate(svc.target(target), mode=mode)
                assert result.nf_class == "open", (target, mode)   # A is owed: ~A is not refuted
