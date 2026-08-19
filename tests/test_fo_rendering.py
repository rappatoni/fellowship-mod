"""Rendering first-order proof terms.

`pres/gen.py` is the inverse of the grammar, so its output has to parse back to
the same tree.  That is not only about display: `reduce.py` compares subtrees by
their rendered string, so two different terms rendering alike would compare
equal.
"""

from copy import deepcopy

import pytest

from core.ac.ast import (
    Admal, Cons, ConsFO, DI, DestructTermsPairFO, Hyp, ID, Lamda, LamdaFO,
    Pyh, Sonc, TermsPairFO,
)
from core.ac.prop import SSym, TApp, TSym
from core.ac.syntax import parse_proof_term
from core.comp.enrich import PropEnrichmentVisitor
from core.ac.resolve import resolve
from core.ac.signature import Declaration
from core.dc.argument import Argument
from pres.decorations import render_prop
from pres.gen import ProofTermGenerationVisitor
from pres.nl import natural_language_rendering, pretty_natural, vanilla_rendering


DECLS = {"N": Declaration("type", "sort"), "P": Declaration("N->N->bool", "sort")}

EXISTS_FORALL = (
    ";:thesis:(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)."
    "<\\H:exists y:N,forall x:N,P x y.\\x:N."
    ";:th:exists y:N,P x y.<H||(y:N)."
    ";:'K:forall x:N,P x y.<(y,;:K2:P x y.<K||x*K2>)||th>>||thesis>"
)


def rendered(node):
    return ProofTermGenerationVisitor().visit(deepcopy(node)).pres


def shape(node):
    kids = [
        shape(getattr(node, slot))
        for slot in ("term", "context")
        if getattr(node, slot, None) is not None and hasattr(getattr(node, slot), "__dict__")
    ]
    return type(node).__name__ + (f"({','.join(kids)})" if kids else "")


# ---------------------------------------------------------------------------
#  The serialiser is the grammar's inverse
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "category,text",
    [
        ("term", "μt:A.<a||t>"),
        ("term", "μt:A.<a||x*y*t>"),
        ("term", "μt:A.<λh:A.a||t>"),
        ("term", "μt:A.<a||λh:R.b*c>"),
        ("term", "μt:A.<a||λh:R.(b*c)>"),
        ("term", "μt:A.<λh:R.(b*c)||t>"),
        ("term", "μt:A.<a||(y:N).b*c>"),
        ("term", "μt:A.<(y,a)||t>"),
        ("term", "μt:A.<a||S (S O)*t>"),
        ("term", "μt:A.<?1.2:A||1.3:B?>"),
        ("context", "μ't:A.<a||t>"),
    ],
)
def test_rendering_round_trips_through_the_grammar(category, text):
    node = parse_proof_term(text, category=category)
    again = parse_proof_term(rendered(node), category=category)
    assert shape(node) == shape(again)


def test_a_lambda_over_an_application_is_bracketed():
    """Without the brackets this renders as `λh:R.b*c`, which reads back as
    `Cons(Lamda(h,b), c)` -- a different term."""
    node = Admal(Pyh(ID("h"), "R"), Cons(DI("b"), ID("c")))
    assert rendered(node) == "λh:R.(b*c)"
    assert isinstance(parse_proof_term(rendered(node), category="context"), Admal)


def test_a_lambda_over_a_leaf_is_not_bracketed():
    node = Admal(Pyh(ID("h"), "R"), ID("c"))
    assert rendered(node) == "λh:R.c"


def test_a_destructor_body_needs_no_brackets():
    """A destructor is a context, so it can never be a `*` operand."""
    node = DestructTermsPairFO("y", SSym("N"), Cons(DI("b"), ID("c")))
    assert rendered(node) == "(y:N).b*c"


@pytest.mark.parametrize(
    "node,expected",
    [
        (LamdaFO("x", SSym("N"), DI("a")), "λx:N.a"),
        (ConsFO(TSym("O"), ID("t")), "O*t"),
        (ConsFO(TApp(TSym("S"), TSym("O")), ID("t")), "S O*t"),
        (TermsPairFO(TSym("y"), DI("a")), "(y,a)"),
        (DestructTermsPairFO("y", SSym("N"), ID("c")), "(y:N).c"),
    ],
)
def test_each_first_order_node_renders(node, expected):
    assert rendered(node) == expected


# ---------------------------------------------------------------------------
#  Natural language
# ---------------------------------------------------------------------------


def enriched():
    node = resolve(parse_proof_term(Argument._normalize_pt_to_unicode(EXISTS_FORALL)), DECLS)
    return PropEnrichmentVisitor().visit(node)


def test_vanilla_keeps_every_first_order_construct():
    text = pretty_natural(enriched(), vanilla_rendering)
    for construct in ("λx:N.", "(y:N).", "(y,", "x*K2"):
        assert construct in text


@pytest.mark.parametrize(
    "phrase",
    [
        "consider an arbitrary but fixed x of type N",   # universal introduction
        "let y of type N be such a thing",               # existential elimination
        "witnessed by y",                                # existential introduction
        "instantiated at x",                             # universal instantiation
    ],
)
def test_prose_names_each_first_order_step(phrase):
    assert phrase in pretty_natural(enriched(), natural_language_rendering)


def test_prose_brackets_a_quantified_operand():
    """`for all x, P x -> Q` reads as though the arrow were inside the
    quantifier, so an operand that is quantified is bracketed."""
    text = render_prop("(forall x:N,P x x)->A", {}, {})
    assert text.startswith("(for all x of type N")
    assert ") -> A" in text


def test_prose_does_not_bracket_what_needs_no_bracket():
    assert render_prop("A->B", {}, {}) == "A -> B"
