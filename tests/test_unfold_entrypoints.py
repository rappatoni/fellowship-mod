"""Unfolding from an entrypoint (tasks.org, aida-unfold-entrypoints).

An issue unfolds to its canonical term, an argument to the same shape with
itself on top of its supporter stack:

    issue term     ATT( SUP(ROOT, STACK), SUP~(ROOT_c, STACK_c) )
    STACK          SUP( SUP(P1, P2), P3 )        last registered outermost
    argument P1    SUP( ROOT, SUP( SUP(P2, P3), P1 ) )

The shape is presentation: the issue graph and its labellings are those of
the legacy term, whichever entrypoint the term is unfolded from.
"""

import warnings

import pytest

from core.comp.adf_label import labellings
from core.dc.debate_graph import declaration_kinds
from core.dc.strict import compile_issue
from core.dc.unfold import argument_edge, unfold, unfold_argument, unfold_legacy
from mod import store
from wrap.cli import execute_script, setup_prover

FIXTURES = [
    "tests/rationality/contested.fspy",
    "tests/rationality/two_witnesses.fspy",
    "tests/rationality/self_attack_lk.fspy",
    "tests/rationality/cyclic_undercut.fspy",
    "tests/peirces_law.fspy",
    "tests/naf_olon.fspy",
    "tests/multiple_undercuts.fspy",
    "tests/label_evaluate.fspy",
    "tests/statements_and_citations.fspy",
    "tests/demo/02_support_attack.fspy",
    "tests/demo/03_contested.fspy",
    "tests/demo/04_document.fspy",
    "tests/demo/05_even_loop.fspy",
    "tests/demo/06_peirce.fspy",
    "tests/minicourse/lesson9_evaluation.fspy",
    "tests/minicourse/lesson8a_cycle.fspy",
    "tests/minicourse/lesson15_capture.fspy",
]


@pytest.fixture
def fresh():
    store.arguments.clear()
    store.document.clear()
    prover = setup_prover()
    yield prover
    prover.close()


def run(prover, script):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, str(script), strict=False, stop_on_error=False, isolate=False,
                       render_files=False, stop_marker=False)


def edge_key(edge):
    """An edge up to its name (copies are named apart) and the kinds of its
    sources (an argument entrypoint's own sites keep the kind it wrote;
    acceptance conditions do not read source kinds)."""
    return (edge.target_key, edge.target_side, edge.strict,
            tuple(sorted((s.key, s.side) for s in edge.sources)))


def issue_graph(term, names, kinds):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return compile_issue(term, "x", strict_names=names, strict_kinds=kinds)


def presumed(graph):
    return {s for s, kinds in graph.defaults.items() if "presumption" in kinds}


def framework(graph):
    return {edge_key(e) for e in graph.edges if not e.name.endswith("*")}


def strict_edges(graph):
    return {edge_key(e) for e in graph.edges if e.name.endswith("*")}


#: Where the stacked term finds strict edges the legacy term missed, seen
#: from the contrary's side: the attacker wing is now the contrary's whole
#: debate, so a strict decision inside it is made from either side.  The
#: legacy term made it only from the statement's own side (self-attack:
#: SelfAttack* for P[t] but not for P[c]).  {script: {(prop, side)}}.
MORE_STRICT_EDGES = {}


def compare(script, statement, reference, graph, doc):
    assert framework(graph) == framework(reference)
    assert presumed(graph) == presumed(reference)
    assert set(graph.nodes) == set(reference.nodes)
    assert strict_edges(graph) >= strict_edges(reference)
    if strict_edges(graph) != strict_edges(reference):
        MORE_STRICT_EDGES.setdefault(script, set()).add((doc.nodes[statement[0]], statement[1]))
        return
    for semantics in ("grounded", "preferred"):
        assert labellings(graph, semantics) == labellings(reference, semantics), semantics


@pytest.mark.parametrize("script", FIXTURES)
def test_the_shape_does_not_change_the_framework(fresh, script, capsys):
    """For every statement: the canonical term, and the term of every
    argument for it, compile to the legacy term's framework (edges up to
    names and source kinds, presumption markers, nodes).  The strict edges
    can only grow (MORE_STRICT_EDGES); where they are equal, so are the
    labellings.  Obligation markers may differ: the labeller reads only
    presumptions."""
    run(fresh, script)
    doc = fresh.document
    names, kinds = list(fresh.declarations.keys()), declaration_kinds(fresh.declarations)
    for statement in doc.statements():
        reference = issue_graph(unfold_legacy(doc, statement), names, kinds)
        compare(script, statement, reference, issue_graph(unfold(doc, statement), names, kinds), doc)
        for edge in doc.edges:
            if (edge.target_key, edge.target_side) == statement and edge.role != "subargument":
                compare(script, statement, reference,
                        issue_graph(unfold_argument(doc, edge), names, kinds), doc)


# ---------------------------------------------------------------------------
# The shapes and what evaluation returns from each entrypoint
# (tests/entrypoints.fspy)
# ---------------------------------------------------------------------------

import copy                                                   # noqa: E402
import logging                                                # noqa: E402

from core.ac.ast import Deleg, Goal, Laog, Mu, Mutilde       # noqa: E402
from core.comp.evaluate import evaluate_debate                # noqa: E402
from core.dc.debate_graph import canonical_prop as K, stack_elements, _match_scaffold  # noqa: E402
from core.dc.typecheck import typecheck                       # noqa: E402
from pres.gen import pres_str                                 # noqa: E402

ENTRYPOINTS = "tests/entrypoints.fspy"


@pytest.fixture
def doc():
    store.arguments.clear()
    store.document.clear()
    prover = setup_prover()
    run(prover, ENTRYPOINTS)
    yield prover
    prover.close()


def options(prover):
    return dict(strict_names=list(prover.declarations.keys()),
                strict_kinds=declaration_kinds(prover.declarations))


def proof(prover, term, mode="credulous", **extra):
    """The class and the name of the argument whose proof the normal form is."""
    nf, cls, _sigma, _graph = evaluate_debate(copy.deepcopy(term), "x", mode=mode,
                                              **options(prover), **extra)
    name = nf.id.name if isinstance(nf, Mu) else nf.di.name if isinstance(nf, Mutilde) else None
    return cls, name


def stack_names(term, prover):
    """The names of the supporter stack under the root, bottom first."""
    match = _match_scaffold(term, set(prover.declarations))
    assert match is not None and match[0] == "supporter"
    return [e.id.name if isinstance(e, Mu) else e.di.name
            for e in stack_elements(match[4], set(prover.declarations))]


class TestShapes:
    def test_the_issue_term_has_the_root_on_top_and_the_stack_below(self, doc):
        term = unfold(doc.document, (K("B"), "term"))
        match = _match_scaffold(term, set(doc.declarations))
        assert isinstance(match[3], Goal)                    # ROOT: nobody presumes B
        assert stack_names(term, doc) == ["p1", "p2", "p3"]  # last registered outermost

    def test_an_argument_goes_on_top_of_its_stack(self, doc):
        for name, expected in (("p1", ["p2", "p3", "p1"]), ("p2", ["p1", "p3", "p2"]),
                               ("p3", ["p1", "p2", "p3"])):
            term = unfold_argument(doc.document, argument_edge(doc.document, name))
            assert stack_names(term, doc) == expected, name

    def test_the_argument_keeps_its_own_binder_names(self, doc):
        term = unfold_argument(doc.document, argument_edge(doc.document, "p1"))
        assert "μp1:B.<μth:B.<r1:A->B||!u2:A*th:B>||p1:B>" in pres_str(term)

    def test_a_counterargument_is_a_supporter_of_its_context_side_statement(self, doc):
        term = unfold_argument(doc.document, argument_edge(doc.document, "c1"))
        assert isinstance(term, Mutilde)
        match = _match_scaffold(term, set(doc.declarations))
        assert match[0] == "supporter" and isinstance(match[3], Laog)
        assert stack_names(term, doc) == ["c2", "c1"]

    def test_an_own_obligation_keeps_its_kind_with_the_presumption_at_the_bottom(self, doc):
        # q demands A; p1 presumes it.  In q's term the root of A's debate is
        # q's own obligation, and the presumption is A's only supporter.
        term = unfold_argument(doc.document, argument_edge(doc.document, "q"))
        assert "μalt2:A.<?u2:A||μ'b4:A.<μ_:A.<b4:A||alt2:A>||μ'_:A.<!u3:A||alt2:A>>>" in pres_str(term)
        # canonically A is presumed: the issue term of G has the presumption as A's root
        assert "!u2:A*th:G" in pres_str(unfold(doc.document, (K("G"), "term")))


class TestEvaluation:
    """The session's table: with every supporter IN the issue term yields
    the top of the stack, an argument's term the argument itself."""

    @pytest.mark.parametrize("mode", ["credulous", "skeptical"])
    def test_issue_and_argument_entrypoints(self, doc, mode):
        assert proof(doc, unfold(doc.document, (K("B"), "term")), mode) == ("value", "p3")
        for name in ("p1", "p2", "p3"):
            term = unfold_argument(doc.document, argument_edge(doc.document, name))
            assert proof(doc, term, mode) == ("value", name)

    def test_strictness_keeps_the_stack_order(self, doc):
        # two strict supporters: the last registered wins from the issue,
        # the argument itself from its own entrypoint
        assert proof(doc, unfold(doc.document, (K("E"), "term"))) == ("value", "s2")
        for name in ("s1", "s2"):
            term = unfold_argument(doc.document, argument_edge(doc.document, name))
            assert proof(doc, term) == ("value", name)

    def test_counterargument_entrypoints(self, doc):
        assert proof(doc, unfold(doc.document, (K("F"), "context"))) == ("value", "c2")
        for name in ("c1", "c2"):
            term = unfold_argument(doc.document, argument_edge(doc.document, name))
            assert proof(doc, term) == ("value", name)

    def test_every_argument_term_replays_through_fellowship(self, doc):
        for edge in doc.document.edges:
            if edge.role == "subargument":
                continue
            term = unfold_argument(doc.document, edge)
            typecheck(doc, term, f"entry_{edge.name}", doc.document.nodes[edge.target_key],
                      edge.target_side == "context")


def test_favour_prefers_a_witness_accepting_the_argument(fresh):
    # two_witnesses: Q is IN in both preferred labellings, but yQ's own
    # source S only in the second.  Without favour the first is taken and
    # the presumption of Q is what survives; with it, yQ's proof.
    run(fresh, "tests/rationality/two_witnesses.fspy")
    doc = fresh.document
    edge = argument_edge(doc, "yQ")
    term = unfold_argument(doc, edge)
    nf, cls, *_ = evaluate_debate(copy.deepcopy(term), "x", mode="credulous", **options(fresh))
    assert (pres_str(nf), cls) == ("!u1:Q", "value")
    nf, cls, *_ = evaluate_debate(copy.deepcopy(term), "x", mode="credulous", favour=edge,
                                  **options(fresh))
    assert cls == "value" and pres_str(nf).startswith("μyQ:Q.<sq:S->Q||")


# ---------------------------------------------------------------------------
# Terms as attributes, staleness, and the CLI
# ---------------------------------------------------------------------------

def test_an_unfolded_term_is_cached_until_the_document_changes(fresh, tmp_path):
    run(fresh, ENTRYPOINTS)
    p1 = fresh.get_argument("p1")
    first = fresh.unfolded_term(p1)
    assert fresh.unfolded_term(p1) is first                     # fresh: cached
    script = tmp_path / "more.fspy"
    script.write_text("declare H:bool.\n")
    run(fresh, script)
    after_declaration = fresh.unfolded_term(p1)
    assert after_declaration is not first                        # a declaration makes it stale
    script.write_text("start argument p4 B\ncut (A -> B) th.\naxiom r1.\nelim.\nby default.\n"
                      "next.\naxiom.\nend argument\n")
    run(fresh, script)
    term = fresh.unfolded_term(p1)
    assert term is not after_declaration and "p4" in pres_str(term)   # so does an argument


def test_the_cli(fresh, tmp_path, caplog):
    script = tmp_path / "cli.fspy"
    script.write_text(open(ENTRYPOINTS).read() + "\n".join([
        "unfold argument p1",
        "unfold issue :B",
        "unfold debate whatever",
        "evaluate p1 credulous",
        "evaluate issue :B credulous",
        "evaluate issue F: skeptical",
        "evaluate p1 credulous favour",
        "render p1 evaluated",
        "label issue :B",
    ]) + "\n")
    with caplog.at_level(logging.INFO):
        run(fresh, script)
    text = "\n".join(r.getMessage() for r in caplog.records)
    assert "Unfolded the debate term unfolded for 'p1'" in text
    assert "Unfolded the canonical debate term of issue :B" in text
    assert "unfold debate: not implemented yet" in text
    assert "Evaluated 'p1' (credulous, preferred, base cbn): VALUE" in text
    assert "normal form: μp1:B.<r1:A->B||!u2:A*p1:B>" in text
    assert "Evaluated 'issue :B' (credulous, preferred, base cbn): VALUE" in text
    assert "normal form: μp3:B.<r3:D->B||" in text
    assert "Evaluated 'issue F:' (skeptical, preferred, base cbn): VALUE" in text
    assert "Evaluated 'p1' (credulous, preferred, base cbn, favoured): VALUE" in text
    assert "Rendering p1, the normal form of the last evaluation:" in text
    assert "Grounded labelling for 'issue :B':" in text
