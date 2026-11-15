"""The check-list renderings (English ``checklist``, German ``pruefschema``),
the decorations of first-order terms, ``render ... shared`` and decorated
graph nodes (tasks.org, aida-render-text-styles, aida-render-fo-terms)."""

from __future__ import annotations

import warnings
from copy import deepcopy

import pytest

from core.ac.prop import parse_term
from core.ac.signature import Declaration
from core.ac.syntax import parse_proof_term
from core.ac.resolve import resolve
from core.comp.enrich import PropEnrichmentVisitor
from core.dc.argument import Argument
from pres.decorations import render_fo_term, render_prop
from pres.nl import checklist_rendering, pretty_natural, pruefschema_rendering
from wrap.cli import _decorated_text, execute_script, setup_prover
from wrap.service import RENDER_STYLES, InvalidRequest, Rendering, Service

DECORATIONS = {
    "interim": "an interim order to submit @arg1",
    "pair": "the pair of @arg1 and @arg2",
    "d_appl": "the application",
    "Lawful": "@arg1 is the lawful order",
}


# -- first-order terms --------------------------------------------------------

def test_a_function_symbol_is_filled_with_its_rendered_arguments():
    assert render_fo_term("interim d_appl", DECORATIONS) == "an interim order to submit the application"
    assert render_fo_term("pair d_appl (interim d_appl)", DECORATIONS) == \
        "the pair of the application and an interim order to submit the application"


def test_an_undecorated_function_symbol_keeps_its_printed_form():
    assert render_fo_term("f d_appl", DECORATIONS) == "f the application"
    assert render_fo_term("f (g c)", {}) == "f (g c)"
    assert render_fo_term(parse_term("f (g c)"), {"g": "g of @arg1"}) == "f (g of c)"


def test_unreadable_term_text_is_shown_as_it_is():
    assert render_fo_term("((", DECORATIONS) == "(("


def test_a_function_term_inside_a_proposition_is_decorated():
    assert render_prop("Lawful [interim d_appl]", {}, DECORATIONS) == \
        "an interim order to submit the application is the lawful order"


# -- the first-order lines of the check-list styles -------------------------------

DECLS = {"N": Declaration("type", "sort"), "P": Declaration("N->N->bool", "sort")}

EXISTS_FORALL = (
    ";:thesis:(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)."
    "<\\H:exists y:N,forall x:N,P x y.\\x:N."
    ";:th:exists y:N,P x y.<H||(y:N)."
    ";:'K:forall x:N,P x y.<(y,;:K2:P x y.<K||x*K2>)||th>>||thesis>"
)


def enriched():
    term = parse_proof_term(EXISTS_FORALL)
    resolve(term, DECLS)
    return PropEnrichmentVisitor(DECLS).visit(deepcopy(term))


@pytest.mark.parametrize("style, phrases", [
    (checklist_rendering, ["for an arbitrary but fixed x of type N", "let y of type N be such a thing",
                           "witnessed by y", "instantiating x with x", "for all x of type N, P x y"]),
    (pruefschema_rendering, ["für ein beliebiges, aber festes x vom Typ N", "sei y vom Typ N ein solches",
                             "belegt durch y", "eingesetzt für x: x", "für alle x vom Typ N gilt: P x y"]),
])
def test_each_first_order_step_has_a_line(style, phrases):
    text = pretty_natural(enriched(), style)
    for phrase in phrases:
        assert phrase in text, (phrase, text)


def test_witness_and_instantiation_lines_are_decorated():
    text = pretty_natural(enriched(), checklist_rendering, decorations={"y": "the witness", "x": "the instance"})
    assert "witnessed by the witness" in text
    assert "instantiating x with the instance" in text


def test_the_german_style_has_no_english_connective_left():
    text = pretty_natural(enriched(), pruefschema_rendering)
    for english in ("for all", "for some", "of type", "witnessed by", "instantiat"):
        assert english not in text


# -- label-aware phrases of an owed site ----------------------------------------

@pytest.fixture
def svc():
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, "tests/debates.fspy", strict=True, isolate=False)
    yield Service.of(p)
    p.close()


def test_the_bottom_alternative_of_a_scaffold_is_phrased_as_the_fallback(svc):
    text = svc.render("tweety", "unfolded", "checklist").text
    assert "otherwise not established: Flies" in text
    assert "It suffices to check that Flies. Starting check tweety:" in text
    assert "Check tweety completed (holds)." in text
    assert "Check penguin completed (refuted)." in text
    de = svc.render("tweety", "unfolded", "pruefschema").text
    assert "andernfalls nicht erwiesen: Flies" in de
    assert "Prüfung penguin abgeschlossen (verneint)." in de


@pytest.mark.parametrize("label, english, german", [
    ("OUT", "not established: ", "Prüfung fehlgeschlagen: "),
    ("UNDEC", "undecided: ", "offen: "),
    ("IN", "established: ", "es liegt vor: "),
    (None, "to be checked: ", "noch zu prüfen: "),
])
def test_an_owed_site_is_phrased_by_its_label(label, english, german):
    from core.ac.ast import Goal
    from pres.nl import _NLVisitor
    site = Goal("1.2", "A")
    site.label = label
    for style, phrase in ((checklist_rendering, english), (pruefschema_rendering, german)):
        visitor = _NLVisitor.__new__(_NLVisitor)
        visitor.semantic, visitor._fallback = style, None
        assert visitor._owed_phrase(site, "default") == phrase


# -- render ... shared --------------------------------------------------------------

def test_render_shared_with_and_without_a_style(svc):
    plain = svc.render("tweety", "shared", None)
    assert isinstance(plain, Rendering) and plain.which == "shared" and "SUP(" in plain.text
    text = svc.render("tweety", "shared", "checklist").text
    assert text.startswith("It suffices to check that Flies. Starting check ")
    assert svc.render("issue :Flies", "shared", "pruefschema").text.startswith("Es genügt zu prüfen")


def test_render_shared_refuses_a_debate(svc):
    svc.start_debate("d", "Flies")
    with pytest.raises(InvalidRequest, match="shared"):
        svc.render("d", "shared", "checklist")


def test_checklist_is_a_render_style():
    from wrap.syntax import RENDER_STYLES as PARSED
    assert "checklist" in RENDER_STYLES and "checklist" in PARSED


# -- decorated graph nodes ------------------------------------------------------------

def test_a_decorated_graph_shows_the_decorations_in_its_nodes(svc):
    p = svc.session
    p.decorations.update({"Flies": "Tweety flies", "Penguin": "Tweety is a penguin"})
    dot = svc.graph("tweety").graph.to_dot(node_text=_decorated_text(p))
    assert "Tweety flies" in dot and "Tweety is a penguin" in dot
    assert "Tweety flies" not in svc.graph("tweety").graph.to_dot()   # formulae by default
    assert "Tweety is a penguin implies it is not the case that Tweety flies" in dot        # English connectives


def test_a_failing_node_text_falls_back_to_the_formula(svc):
    def broken(_):
        raise RuntimeError("no")
    assert "Flies" in svc.graph("tweety").graph.to_dot(node_text=broken)


def test_an_instantiation_without_its_quantifier_keeps_the_anonymous_line():
    from core.ac.ast import ConsFO, ID
    from pres.nl import _instantiated_variable
    cell = ConsFO(parse_term("c"), ID("k", "A"))
    assert _instantiated_variable(cell) is None
    cell.prop = "forall d, e:N, P d e"
    assert _instantiated_variable(cell) == "d"
    cell.prop = "exists d:N, P d d"
    assert _instantiated_variable(cell) is None


def test_an_instantiation_after_an_implication_names_its_variable():
    """`r * h * c` against `A -> forall x:N, P x`: the instantiation is in no
    command; enrichment carries the type down the spine to it."""
    decls = {"N": Declaration("type", "sort"), "P": Declaration("N->bool", "sort"),
             "A": Declaration("bool", "sort"), "c": Declaration("N", "sort")}
    term = parse_proof_term(";:thesis:P c.<r:A->(forall x:N,P x)||h:A*c*thesis>")
    resolve(term, decls)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        term = PropEnrichmentVisitor(decls).visit(deepcopy(term))
    assert "instantiating x with c" in pretty_natural(term, checklist_rendering)


def _refuted_only_graph():
    """R is only refuted (by `against`, from a presumed E); E is proved and
    refuted; ~N is a negation, only refuted."""
    from core.dc.debate_graph import DebateGraph, Edge, Source
    g = DebateGraph()
    g.nodes.update({"R": "R", "E": "E", "~N": "~N"})
    g.edges.append(Edge("against", "R", "context",
                        (Source("E", "term", "presumption", "1"),), False, "attacker"))
    g.edges.append(Edge("doubt", "E", "context", (), True, "attacker"))
    g.edges.append(Edge("deny", "~N", "context", (), True, "attacker"))
    g.defaults[("E", "term")] = {"presumption"}
    labels = {("R", "context"): "IN", ("E", "term"): "IN", ("E", "context"): "OUT",
              ("~N", "context"): "OUT"}
    return g, labels


def test_prose_graph_words_the_markings_and_negates_a_refuted_only_node():
    g, labels = _refuted_only_graph()
    decorations = {"R": "the usufruct is registrable", "E": "the restraint excludes it", "N": "a need"}
    text = lambda shown: render_prop(shown, {}, decorations, checklist_rendering.connective_templates)
    dot = g.to_dot(labels=labels, node_text=text, prose=True)
    assert 'label="it is not the case that the usufruct is registrable\\nproof: IN"' in dot
    assert "the restraint excludes it\\nproof presumed\\nproof: IN · refutation: OUT" in dot
    # a negation is not negated again
    assert 'label="it is not the case that a need\\nrefutation: OUT"' in dot
    # the arrow into the negated node derives its proof: a filled head
    assert 'n1 -> n0 [color=red arrowhead=normal label="against"]' in dot
    assert "t=IN" not in dot and "[t:p]" not in dot


def test_the_formula_graph_is_unchanged_without_prose():
    g, labels = _refuted_only_graph()
    dot = g.to_dot(labels=labels)
    assert 'label="R\\nc=IN"' in dot and "[t:p]" in dot and "arrowhead=empty" in dot
