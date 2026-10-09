"""Decoration rendering after the move onto the shared proposition AST.

`pres/decorations.py` used to carry its own tokenizer and parser -- a near
duplicate of the one in `core/ac/prop_render.py`.  Both are gone; the module
now walks `core.ac.prop` nodes.  These tests pin the behaviour that had to be
preserved across that move, plus the quantifier support it gained.
"""

import pytest

from pres.decorations import DecorationError, parse_decorate_command, render_declaration, render_prop


DECLARATIONS = {"ax": "forall n:N,P n", "bird": "Bird Tweety"}
DECORATIONS = {"P": "@arg1 is prime", "O": "zero", "Bird": "@arg1 is a bird", "A": "it rains"}


def render(prop, templates=None):
    return render_prop(prop, DECLARATIONS, DECORATIONS, templates)


# ---------------------------------------------------------------------------
#  Behaviour preserved from the old parser
# ---------------------------------------------------------------------------


def test_atom_decoration():
    assert render("A") == "it rains"


def test_undecorated_atom_renders_as_itself():
    assert render("B") == "B"


def test_predicate_application_fills_positional_placeholders():
    assert render("Bird Tweety") == "Tweety is a bird"


def test_argument_decorations_apply_inside_an_application():
    assert render("P O") == "zero is prime"


def test_implication_default_rendering():
    assert render("A->B") == "it rains -> B"


def test_connective_template_is_applied():
    assert render("A->B", {"->": "@left implies @right"}) == "it rains implies B"


def test_negation_default_rendering():
    assert render("~A") == "not it rains"


def test_unparseable_proposition_falls_back_to_raw_text():
    assert render("A ->") == "A ->"


def test_empty_and_missing_propositions():
    assert render_prop(None, {}, {}) == ""
    assert render_prop("   ", {}, {}) == ""


def test_render_declaration_uses_the_declared_proposition():
    assert render_declaration("bird", DECLARATIONS, DECORATIONS) == "Tweety is a bird"


def test_render_declaration_of_an_unknown_name_is_the_name():
    assert render_declaration("nope", DECLARATIONS, DECORATIONS) == "nope"


# ---------------------------------------------------------------------------
#  Quantifiers -- new
# ---------------------------------------------------------------------------


def test_universal_default_rendering():
    assert render("forall n:N,P n") == "for all n of type N, n is prime"


def test_existential_default_rendering():
    assert render("exists y:N,P y") == "for some y of type N, y is prime"


def test_shared_binder_lists_every_variable():
    assert render("forall n,m:N,P n") == "for all n, m of type N, n is prime"


def test_quantifier_templates_expose_variables_sort_and_body():
    rendered = render(
        "forall n:N,P n", {"forall": "jedes @vars aus @sort erfuellt: @body"}
    )
    assert rendered == "jedes n aus N erfuellt: n is prime"


def test_existential_template_is_keyed_separately():
    templates = {"forall": "ALL @body", "exists": "SOME @body"}
    assert render("exists y:N,P y", templates) == "SOME y is prime"


def test_decorations_reach_under_a_quantifier():
    assert "is prime" in render("forall x:N,forall y:N,P x")


def test_quantified_declaration_renders():
    assert render_declaration("ax", DECLARATIONS, DECORATIONS) == (
        "for all n of type N, n is prime"
    )


def test_compound_first_order_argument_renders():
    assert render("P (S O)") == "S zero is prime"


# ---------------------------------------------------------------------------
#  The decorate command itself is untouched by the migration
# ---------------------------------------------------------------------------


def test_single_quoted_template_is_refused_with_its_replacement():
    with pytest.raises(DecorationError, match='double-quote it, decorate Bird : "@arg1 is a bird".'):
        parse_decorate_command("decorate Bird : '@arg1 is a bird'.")


def test_double_quoted_template_is_json_decoded():
    name, template = parse_decorate_command('decorate L : "@arg1 ist eine \\\\sn{Liste}".')
    assert name == "L"
    assert template == "@arg1 ist eine \\sn{Liste}"


def test_malformed_decorate_command_raises():
    with pytest.raises(DecorationError):
        parse_decorate_command("decorate")
