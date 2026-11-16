"""The order of the arguments in a graph: each argument the user wrote is
numbered by its place in the document, or in a debate by the order of first
utterance, so the course of a debate can be read from the drawing.
Subarguments and declared axioms stay unnumbered."""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from core.dc.debate_graph import DebateGraph, Edge, Source  # noqa: E402
from wrap.service import Service  # noqa: E402


def _graph():
    g = DebateGraph()
    g.nodes.update({"P": "P", "Q": "Q", "R": "R"})
    g.add_edge(Edge("ax", "Q", "term", (), True, "subargument"))
    g.add_edge(Edge("first", "P", "term", (Source("Q", "term", "presumption", "1"),), False))
    g.add_edge(Edge("second", "P", "context",
                    (Source("Q", "term", "obligation", "2"), Source("R", "term", "obligation", "3")),
                    False, "attacker"))
    # an argument that only presumes its own conclusion: a marker, no edge
    g.add_edge(Edge("own", "R", "term", (Source("R", "term", "presumption", "4"),), False))
    return g


def test_to_dot_numbers_arrows_boxes_and_markers():
    dot = _graph().to_dot(order={"first": 1, "second": 2, "own": 3})
    assert 'label="1. first"' in dot                  # a labelled arrow
    assert 'label="2. second"' in dot                 # a small box
    assert 'label="ax"' in dot                        # a declared axiom: no number
    assert re.search(r'label="R\\n\[t:o t:p\]\\n3\. own"', dot)
    assert "the n-th argument of the document" in dot


def test_to_dot_without_order_is_unchanged():
    dot = _graph().to_dot()
    assert 'label="first"' in dot and "own" not in dot and "n. name" not in dot


def test_to_dot_debate_order_in_the_legend():
    dot = _graph().to_dot(order={"second": 1}, order_source="debate")
    assert 'label="1. second"' in dot and 'label="first"' in dot
    assert "the n-th argument moved in the debate" in dot


SCRIPT = """lk.
declare A, B, C : bool.
declare r : (A -> B).
argument b2 : (B).
cut (A -> B) h.
axiom r.
elim.
by default.
next.
axiom h.
dixi.
counterargument a1 : (A).
by default.
dixi.
argument a0 : (A).
by default.
dixi.
debate pro closed d : (B).
b2.
a0.
a1.
cedat tempus.
"""


def test_service_graph_orders_document_and_debate(prover):
    svc = Service.of(prover)
    assert svc.check(SCRIPT).ok
    doc = svc.graph(svc.target("document"))
    assert (doc.order, doc.order_source) == ({"b2": 1, "a1": 2, "a0": 3}, "document")
    deb = svc.graph(svc.target("d"))
    assert (deb.order, deb.order_source) == ({"b2": 1, "a0": 2, "a1": 3}, "debate")
    dot = deb.graph.to_dot(order=deb.order, order_source=deb.order_source)
    assert 'label="1. b2"' in dot
    # a0 and a1 only presume A and its refutation: they are markers on A
    assert re.search(r"2\. a0, 3\. a1", dot), dot
