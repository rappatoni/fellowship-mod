"""An argument for a negation that starts by introducing its hypothesis
keeps its name (tasks.org, aida-negation-edge-provenance; the literature
evaluation's bug 7).  ``elim h.`` on the goal ~A makes Fellowship wrap the
proof as ``mu H1:~A.< lambda h:A. ... || H1 >``; the edge was named after
the prover's binder H1, with no span for a document UI."""

import warnings
from pathlib import Path

from wrap.cli import setup_prover
from wrap.serialize import to_json
from wrap.service import Service

TESTS = Path(__file__).parent


def test_the_edge_is_named_after_the_argument_with_its_span():
    prover = setup_prover()
    svc = Service.of(prover)
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            svc.check((TESTS / "negation_edge_name.fspy").read_text())
            data = to_json(svc.graph(svc.target("document")), prover)
        assert [e.name for e in prover.graph.edges] == ["negA"]
        edge, = data["graph"]["edges"]
        assert edge["argument"] == "negA"
        assert edge["span"] == {"line": 6, "col": 1, "end_line": 9, "end_col": 5}
    finally:
        prover.close()
