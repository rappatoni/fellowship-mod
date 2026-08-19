"""Type synthesis after moving it onto the proposition AST.

Enrichment used to build propositions by string concatenation, deciding
parenthesisation with `"->" in prop or "-" in prop`.  That is right often
enough to have survived, but it cannot see a quantifier, and it adds
parentheses the precedence does not need.  It now parses, builds and prints.
"""

import pytest

from core.ac.ast import ConsFO, DI, DestructTermsPairFO, ID, LamdaFO, TermsPairFO
from core.ac.prop import SSym, TSym
from core.ac.resolve import resolve
from core.ac.signature import Declaration
from core.ac.syntax import parse_proof_term
from core.comp.enrich import PropEnrichmentVisitor
from core.dc.argument import Argument


DECLARATIONS = {"N": Declaration("type", "sort"), "P": Declaration("N->N->bool", "sort")}

EXISTS_FORALL = (
    ";:thesis:(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)."
    "<\\H:exists y:N,forall x:N,P x y.\\x:N."
    ";:th:exists y:N,P x y.<H||(y:N)."
    ";:'K:forall x:N,P x y.<(y,;:K2:P x y.<K||x*K2>)||th>>||thesis>"
)


def enriched(text, declarations=DECLARATIONS, **kwargs):
    node = resolve(parse_proof_term(Argument._normalize_pt_to_unicode(text)), declarations)
    return PropEnrichmentVisitor(**kwargs).visit(node)


def find(node, wanted):
    if isinstance(node, wanted):
        return node
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if child is not None and hasattr(child, "__dict__"):
            found = find(child, wanted)
            if found is not None:
                return found
    return None


# ---------------------------------------------------------------------------
#  Building propositions with real precedence
# ---------------------------------------------------------------------------


def _visitor():
    return PropEnrichmentVisitor()


def test_a_quantified_antecedent_is_parenthesised():
    """The substring test could not see this: neither "->" nor "-" occurs in
    `forall n:N,P n`, so the antecedent used to be left bare and the arrow
    silently slid inside the quantifier."""
    assert _visitor()._mk_imp("forall n:N,P n", "Q") == "(forall n:N,P n)->Q"


def test_redundant_parentheses_are_not_added():
    # Subtraction is looser than implication, so this needs none.
    assert _visitor()._mk_minus("A", "B->C") == "A-B->C"


def test_an_arrow_antecedent_still_gets_its_parentheses():
    assert _visitor()._mk_imp("A->B", "C") == "(A->B)->C"


def test_propositions_outside_the_fragment_do_not_crash():
    """Conjunction is unsupported, so it cannot be parsed; enrichment declines
    rather than failing."""
    assert _visitor()._mk_imp("A/\\B", "C") is None


def test_declared_types_are_canonicalised():
    assert _visitor()._canonical("(B -> A)") == "B->A"
    assert _visitor()._canonical("(true-A)-B") == "true-A-B"


# ---------------------------------------------------------------------------
#  First-order rules
# ---------------------------------------------------------------------------


def test_universal_introduction_quantifies_the_body():
    node = enriched(EXISTS_FORALL)
    assert find(node, LamdaFO).prop == "forall x:N,exists y:N,P x y"


def test_existential_elimination_is_the_dual():
    node = enriched(EXISTS_FORALL)
    assert find(node, DestructTermsPairFO).prop == "exists y:N,forall x:N,P x y"


def test_the_root_keeps_the_theorem_it_was_proved_from():
    node = enriched(EXISTS_FORALL)
    assert node.prop == "(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)"


@pytest.mark.parametrize("node_type", [ConsFO, TermsPairFO])
def test_the_non_invertible_nodes_carry_no_proposition(node_type):
    """A universal instantiation's type cannot be rebuilt from its children --
    recovering P from P[t/x] is not determined -- and an existential
    introduction's binder is discarded by Fellowship's printer.  Both take
    their type from the enclosing command instead."""
    node = enriched(EXISTS_FORALL)
    assert find(node, node_type).prop is None


def test_the_command_takes_its_type_from_the_other_side():
    """Where one side carries no proposition, the cut proposition still
    resolves, because a command's two sides have the same type."""
    node = enriched(EXISTS_FORALL)
    innermost = find(find(node, TermsPairFO).term, DI)
    assert innermost.prop == "forall x:N,P x y"


def test_a_first_order_instantiation_needs_no_proposition_to_replay():
    node = enriched(";:t:P O.<ax||O*t>", declarations={
        "N": Declaration("type", "sort"),
        "O": Declaration("N", "sort"),
        "P": Declaration("N->bool", "sort"),
        "ax": Declaration("forall n:N,P n", "prop"),
    }, axiom_props={"ax": "forall n:N,P n"})
    assert isinstance(node.context, ConsFO)
    assert node.context.fo_term == TSym("O")
    assert node.contr == "forall n:N,P n"      # the cut proposition


# ---------------------------------------------------------------------------
#  Scope
# ---------------------------------------------------------------------------


def test_a_binder_does_not_outlive_its_subtree():
    """bound_vars was written and never unwound, so a binder's type leaked
    into its siblings and survived past its scope."""
    visitor = PropEnrichmentVisitor()
    visitor.visit(parse_proof_term("μt:A.<λh:B.a||t>"))
    assert "h" not in visitor.bound_vars


def test_a_shadowing_binder_restores_the_outer_one():
    visitor = PropEnrichmentVisitor(bound_vars={"h": "Outer"})
    visitor.visit(parse_proof_term("μt:A.<λh:B.a||t>"))
    assert visitor.bound_vars["h"] == "Outer"


def test_a_first_order_binder_is_scoped_too():
    visitor = PropEnrichmentVisitor()
    visitor.visit(resolve(parse_proof_term("μt:A.<λx:N.a||t>"), DECLARATIONS))
    assert "x" not in visitor.bound_vars


# ---------------------------------------------------------------------------
#  First-order variables are a separate namespace
# ---------------------------------------------------------------------------


def test_a_first_order_binder_does_not_give_a_proof_leaf_a_sort():
    """bound_vars maps a proof variable to its *proposition*.

    A first-order variable has a sort, so tracking it in the same table let a
    name collision hand a Sort to a proof leaf -- `inner x .prop` came out as
    SSym('N') rather than a proposition.
    """
    visitor = PropEnrichmentVisitor()
    node = visitor.visit(
        resolve(parse_proof_term("μt:A.<λx:N.μu:A.<x||u>||t>"), DECLARATIONS)
    )
    inner = node.term.term.term
    assert inner.prop is None
    assert not isinstance(inner.prop, SSym)


def test_a_shadowed_proof_variable_is_not_given_the_outer_proposition():
    """Reaching past the shadow would give the name someone else's type."""
    visitor = PropEnrichmentVisitor(bound_vars={"x": "Outer"})
    node = visitor.visit(
        resolve(parse_proof_term("μt:A.<λx:N.μu:A.<x||u>||t>"), DECLARATIONS)
    )
    assert node.term.term.term.prop is None
    assert visitor.bound_vars["x"] == "Outer"     # and the outer one survives


def test_a_shadowed_axiom_is_not_reached_through_either():
    visitor = PropEnrichmentVisitor(axiom_props={"x": "SomeAxiom"})
    node = visitor.visit(
        resolve(parse_proof_term("μt:A.<λx:N.μu:A.<x||u>||t>"), DECLARATIONS)
    )
    assert node.term.term.term.prop is None


def test_the_first_order_namespace_is_unwound():
    visitor = PropEnrichmentVisitor()
    visitor.visit(resolve(parse_proof_term("μt:A.<λx:N.a||t>"), DECLARATIONS))
    assert visitor.fo_vars == set()
    assert "x" not in visitor.bound_vars
