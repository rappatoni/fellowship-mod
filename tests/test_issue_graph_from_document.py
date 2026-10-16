"""The issue graph taken from the document graph, checked against the one
compiled from the term (tasks.org, aida-discuss-issue-graph-from-document).

``document_issue_graph`` is the issue graph since 2026-10-15, and the only
one since 2026-10-16 (the `issue-graph term` setting is retired).
``compile_issue`` stays - for an argument the document refused, and here
as a check of the unfolder: the framework of every unfolded term agrees
with the document graph up to these classes of difference:

- ``markers``: the term has an obligation marker the document lacks -
  the unfolder's own roots (an issue nobody presumes is the obligation
  ?u1).  An obligation marker contributes nothing to an acceptance
  condition (core/comp/adf_label.py), so no label changes;
- ``kinds``: the same edge with a source of another kind - the term shows
  a site with the kind of its statement's root (a presumption if anybody
  presumes it), the document as the argument wrote it.  The labeller
  ignores source kinds;
- ``exposed`` (since 2026-10-16, the exposure of horns): the same edge,
  strict in the document and defeasible in the term - the debate about
  an exposed node has an obligation root (tasks.org,
  aida-discuss-non-site-attack-points, Q3), which the term's compiler
  reads as an obligation source where the document has the node's own
  strict edge as a ``subargument`` source.  The strict phase restores
  the strict edge where it decides that scaffold (``c1*``); where the
  decision is made further out (tests/test1.fspy: arg1 is decided whole,
  so the exposed strict node inconsistency.μ'1 under it gets no starred
  edge; and for the issue EfficientMetro[c] itself the attack's two
  wings are both strict, which phase 1 leaves to the labelling), the
  term route labels the node's statement without its strictness.  The
  labels may differ on such statements only (the inconsistent test1.fspy
  is the one fixture where they do);
- ``captured``: edges only the document has, for an exposed node whose
  statement was bound where the node stood: the term has the captured
  variable instead (decision 2 of aida-discuss-non-site-attack-points:
  without primacy, the captured variable alone).  No label changes.

Until 2026-10-16 the ``captured`` class was ``lambda``, and it changed
labels: the unfolder stacked no subargument edge, so a statement only a
lambda, a lemma or an axiom derived was a bare site in the term.  The
shared route is checked equal to the unfolded one in tests/test_share.py.
"""

import glob
import warnings
from pathlib import Path

import pytest

from conftest import DEFAULT_SCRIPTS
from core.comp.adf_label import labellings, referenced_statements
from core.dc.debate_graph import declaration_kinds
from core.dc.strict import compile_issue, document_issue_graph, strict_resolve
from core.dc.unfold import argument_edge, unfold
from test_share import random_document
from wrap.cli import setup_prover
from wrap.service import Service

TESTS = Path(__file__).parent
FIXTURES = sorted({str(TESTS / s) for s in DEFAULT_SCRIPTS}
                  | set(glob.glob(str(TESTS / "minicourse" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "demo" / "*.fspy")))
                  | set(glob.glob(str(TESTS / "rationality" / "*.fspy")))
                  | {str(TESTS / "entrypoints.fspy"), str(TESTS / "debates.fspy")})

SMALL = ("grounded", "preferred", "stable")
ACCEPTED = {"markers", "kinds", "exposed", "captured"}


def edge_key(edge, kinds=True, strict=True):
    """An edge up to its name and role: the term names occurrence copies
    apart and calls its edges supporters and attackers."""
    sources = sorted((s.key, s.side, s.kind) if kinds else (s.key, s.side) for s in edge.sources)
    return (edge.target_key, edge.target_side, edge.strict if strict else None, tuple(sources))


def exposed_only(term_graph, doc_graph) -> bool:
    """Whether the edges differ only by the term's being defeasible where
    the document's is strict (the ``exposed`` class)."""
    term_edges = {edge_key(e, False, False): e.strict for e in term_graph.edges}
    doc_edges = {edge_key(e, False, False): e.strict for e in doc_graph.edges}
    if set(term_edges) != set(doc_edges):
        return False
    return all(term_edges[k] == doc_edges[k] or (doc_edges[k] and not term_edges[k])
               for k in term_edges)


def classify(term_graph, doc_graph):
    """The classes of difference between the two issue graphs (empty if
    they agree on edges and default markers)."""
    found = set()
    term_edges = {edge_key(e) for e in term_graph.edges}
    doc_edges = {edge_key(e) for e in doc_graph.edges}
    if term_edges != doc_edges:
        if {edge_key(e, False) for e in term_graph.edges} == {edge_key(e, False) for e in doc_graph.edges}:
            found.add("kinds")
        elif exposed_only(term_graph, doc_graph):
            found.add("exposed")
        elif any(e.role == "subargument" for e in doc_graph.edges if edge_key(e) not in term_edges):
            found.add("captured")
        else:
            found.add("other edges")
    statements = set(term_graph.defaults) | set(doc_graph.defaults)
    for statement in statements:
        term_kinds = set(term_graph.defaults.get(statement, ()))
        doc_kinds = set(doc_graph.defaults.get(statement, ()))
        if term_kinds == doc_kinds:
            continue
        if (term_kinds - doc_kinds) <= {"obligation"} and (doc_kinds - term_kinds) <= {"obligation"}:
            found.add("markers")
        elif "captured" in found or "exposed" in found:
            pass                         # a subargument edge's own presumption markers
        else:
            found.add("other markers")
    return found


def labels_differ(term_graph, doc_graph, semantics):
    """The statements the two graphs share and label differently, under
    any of the semantics (every shared statement if the two give a
    different number of labellings)."""
    common = set(referenced_statements(term_graph)) & set(referenced_statements(doc_graph))
    if len(common) > 10:
        semantics = ("grounded",)        # the multi-extension solver is slow beyond that

    def projected(graph, chosen):
        return sorted(sorted((k, v) for k, v in labelling.items() if k in common)
                      for labelling in labellings(graph, chosen))

    differing = set()
    for s in semantics:
        left, right = projected(term_graph, s), projected(doc_graph, s)
        if len(left) != len(right):
            differing |= common
        else:
            for a, b in zip(left, right):
                differing |= {k for (k, v), (_, w) in zip(a, b) if v != w}
    return differing


def exposed_statements(term_graph, doc_graph):
    """The targets of the edges of the ``exposed`` class."""
    strict_in_doc = {edge_key(e, False, False) for e in doc_graph.edges if e.strict}
    return {(e.target_key, e.target_side) for e in term_graph.edges
            if not e.strict and edge_key(e, False, False) in strict_in_doc}


def entries(prover):
    """Every argument with an edge, every statement, every debate: (name,
    issue, the term's thunk, the graph the issue graph is taken from)."""
    found = []
    for argument in list(prover.arguments.values()):
        if argument_edge(prover.graph, argument.name) is not None:
            found.append((f"arg {argument.name}", prover.issue_of(argument),
                          lambda a=argument: prover.unfolded_term(a), prover.graph))
    for statement in prover.graph.statements():
        found.append((f"issue {prover.graph._show(*statement)}", statement,
                      lambda s=statement: prover.issue_term(s), prover.graph))
    for debate in prover.debates.values():
        found.append((f"debate {debate.name}", debate.issue,
                      lambda d=debate: prover.debate_term(d), prover.debate_graph(debate)))
    return found


def both(term, source, issue, strict_names, strict_kinds=None):
    term_graph = compile_issue(term, "x", strict_names=strict_names, strict_kinds=strict_kinds)
    _, strict_edges = strict_resolve(term, strict_names)
    return term_graph, document_issue_graph(source, issue, strict_edges)


@pytest.mark.parametrize("script", FIXTURES, ids=lambda p: str(Path(p).relative_to(TESTS)))
def test_fixture(script):
    name = str(Path(script).relative_to(TESTS))
    prover = setup_prover()
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            Service.of(prover).check(Path(script).read_text())
            strict_names = list(prover.declarations.keys())
            strict_kinds = declaration_kinds(prover.declarations)
            for label, issue, term_of, source in entries(prover):
                term_graph, doc_graph = both(term_of(), source, issue, strict_names, strict_kinds)
                classes = classify(term_graph, doc_graph)
                assert classes <= ACCEPTED, (label, classes)
                differing = labels_differ(term_graph, doc_graph, SMALL)
                if differing:
                    assert "exposed" in classes, (label, differing)
                    assert differing <= exposed_statements(term_graph, doc_graph), (label, differing)
    finally:
        prover.close()


@pytest.mark.parametrize("seed", range(60))
def test_random_documents(seed):
    doc = random_document(seed)
    strict = {f"ax{n}" for n in range(1, 40)}
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        for statement in doc.statements():
            term_graph, doc_graph = both(unfold(doc, statement), doc, statement, strict)
            classes = classify(term_graph, doc_graph)
            assert classes <= ACCEPTED, statement
            differing = labels_differ(term_graph, doc_graph, ("grounded", "complete"))
            if differing:
                assert "exposed" in classes, (statement, differing)
                assert differing <= exposed_statements(term_graph, doc_graph), (statement, differing)


def test_the_reference_route_is_retired():
    """`issue-graph term` selected this comparison's reference as the
    issue graph until 2026-10-16; the issue graph always comes from the
    document now (aida-rational-closure, step 4), and the comparison above
    stays as a check of the unfolder."""
    from wrap.syntax import parse
    assert parse("issue-graph term").kind == "opaque"
    prover = setup_prover()
    try:
        assert not hasattr(prover, "issue_graph_from_document")
        assert not hasattr(Service.of(prover), "set_issue_graph")
    finally:
        prover.close()
