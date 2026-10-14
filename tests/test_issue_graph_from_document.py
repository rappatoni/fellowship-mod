"""The issue graph taken from the document graph, checked against the one
compiled from the term (tasks.org, aida-discuss-issue-graph-from-document).

Nothing is switched yet: ``document_issue_graph`` is built beside
``compile_issue`` and every difference between the two is classified.
The classes found on 2026-10-14:

- ``markers``: the term has an obligation marker the document lacks -
  the unfolder's own roots (an issue nobody presumes is the obligation
  ?u1).  An obligation marker contributes nothing to an acceptance
  condition (core/comp/adf_label.py), so no label changes;
- ``kinds``: the same edge with a source of another kind - the term shows
  a site with the kind of its statement's root (a presumption if anybody
  presumes it), the document as the argument wrote it.  The labeller
  ignores source kinds;
- ``lambda``: edges only the document has, around a lambda subargument.
  The unfolder never stacks a lambda edge, so an issue whose statement
  only a lambda derives is a bare site in the term.  This is the one class
  that changes labels (``LABEL_CHANGES``).

Any other difference fails the test.  The shared route is checked equal
to the unfolded one in tests/test_share.py.
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

#: (fixture, issue) where the labels differ: an issue only a lambda derives.
LABEL_CHANGES = {
    ("demo/05_even_loop.fspy", "issue P->false[t]"), ("demo/05_even_loop.fspy", "issue Q->false[t]"),
    ("minicourse/lesson15_capture.fspy", "issue P->false[t]"),
    ("minicourse/lesson15_capture.fspy", "issue Q->false[t]"),
    ("rationality/even_loop.fspy", "issue P->false[t]"), ("rationality/even_loop.fspy", "issue Q->false[t]"),
    ("rationality/even_loop_lk.fspy", "issue P->false[t]"),
    ("rationality/even_loop_lk.fspy", "issue Q->false[t]"),
    ("rationality/self_attack.fspy", "issue P->false[t]"),
    ("rationality/self_attack_lk.fspy", "issue P->false[t]"),
}

SMALL = ("grounded", "preferred", "stable")


def edge_key(edge, kinds=True):
    """An edge up to its name and role: the term names occurrence copies
    apart and calls its edges supporters and attackers."""
    sources = sorted((s.key, s.side, s.kind) if kinds else (s.key, s.side) for s in edge.sources)
    return (edge.target_key, edge.target_side, edge.strict, tuple(sources))


def classify(term_graph, doc_graph):
    """The classes of difference between the two issue graphs (empty if
    they agree on edges and default markers)."""
    found = set()
    term_edges = {edge_key(e) for e in term_graph.edges}
    doc_edges = {edge_key(e) for e in doc_graph.edges}
    if term_edges != doc_edges:
        if {edge_key(e, False) for e in term_graph.edges} == {edge_key(e, False) for e in doc_graph.edges}:
            found.add("kinds")
        elif any(e.role == "subargument" for e in doc_graph.edges if edge_key(e) not in term_edges):
            found.add("lambda")
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
        elif "lambda" in found:
            pass                         # a lambda edge's own presumption markers
        else:
            found.add("other markers")
    return found


def labels_differ(term_graph, doc_graph, semantics):
    """Whether the two graphs label the statements they share differently."""
    common = set(referenced_statements(term_graph)) & set(referenced_statements(doc_graph))
    if len(common) > 10:
        semantics = ("grounded",)        # the multi-extension solver is slow beyond that

    def projected(graph, chosen):
        return sorted(sorted((k, v) for k, v in labelling.items() if k in common)
                      for labelling in labellings(graph, chosen))

    return [s for s in semantics if projected(term_graph, s) != projected(doc_graph, s)]


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
            changed = set()
            for label, issue, term_of, source in entries(prover):
                term_graph, doc_graph = both(term_of(), source, issue, strict_names, strict_kinds)
                classes = classify(term_graph, doc_graph)
                assert classes <= {"markers", "kinds", "lambda"}, (label, classes)
                if labels_differ(term_graph, doc_graph, SMALL):
                    assert "lambda" in classes, label
                    changed.add((name, label))
    finally:
        prover.close()
    assert changed == {c for c in LABEL_CHANGES if c[0] == name}


@pytest.mark.parametrize("seed", range(60))
def test_random_documents(seed):
    doc = random_document(seed)
    strict = {f"ax{n}" for n in range(1, 40)}
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        lambda_only = lambda_only_statements(doc)
        for statement in doc.statements():
            term_graph, doc_graph = both(unfold(doc, statement), doc, statement, strict)
            classes = classify(term_graph, doc_graph)
            assert classes <= {"markers", "kinds", "lambda"}, statement
            if labels_differ(term_graph, doc_graph, ("grounded", "complete")):
                assert "lambda" in classes and statement in lambda_only, statement


def lambda_only_statements(graph):
    """Statements only a lambda subargument derives: the unfolder stacks no
    lambda edge, so their issue is a bare site in the term."""
    by_role = {}
    for edge in graph.edges:
        by_role.setdefault((edge.target_key, edge.target_side), set()).add(edge.role == "subargument")
    return {statement for statement, roles in by_role.items() if roles == {True}}


def test_the_setting_selects_the_issue_graph():
    """`issue-graph document` (the default) and `issue-graph term` through
    the service, on the example of the lambda class: the document labels
    P->false UNDEC, the term (a bare site) OUT."""
    from wrap.service import InvalidRequest
    prover = setup_prover()
    try:
        svc = Service.of(prover)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            svc.check((TESTS / "rationality" / "self_attack_lk.fspy").read_text())
            assert prover.issue_graph_from_document
            target = svc.target("issue :P->false")
            from core.dc.debate_graph import canonical_prop
            key = (canonical_prop("P->false"), "term")
            assert svc.label(target, semantics="grounded").labellings[0][key] == "UNDEC"
            svc.set_issue_graph("term")
            assert not prover.issue_graph_from_document
            assert svc.label(target, semantics="grounded").labellings[0][key] == "OUT"
            with pytest.raises(InvalidRequest):
                svc.set_issue_graph("both")
    finally:
        prover.close()


def test_the_command_parses():
    from wrap.syntax import SyntaxRefused, parse
    assert parse("issue-graph term").args == {"mode": "term"}
    assert parse("issue-graph document").kind == "issue_graph"
    with pytest.raises(SyntaxRefused, match=r"Use: issue-graph document\|term\."):
        parse("issue-graph both")
