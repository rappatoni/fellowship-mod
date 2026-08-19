"""Reclassifying the constructs Fellowship prints ambiguously.

`λx:A.t` and `λx:N.t` differ only in whether the annotation names a
proposition or a sort; `ax*c` and `O*c` only in whether the head is a proof or
a first-order constant.  Neither is decidable from syntax, and both are
decidable from the declaration table plus the binders in scope.
"""

import pytest

from core.ac.ast import Cons, ConsFO, DI, Lamda, LamdaFO
from core.ac.prop import TSym
from core.ac.syntax import parse_proof_term
from core.ac.resolve import ResolutionError, resolve
from core.ac.signature import Declaration


DECLARATIONS = {
    "N": Declaration("type", "sort"),          # a sort
    "O": Declaration("N", "sort"),             # a first-order constant
    "S": Declaration("N->N", "sort"),          # a first-order function
    "Pred": Declaration("N->bool", "sort"),    # a predicate
    "A": Declaration("bool", "sort"),          # a propositional atom
    "ax": Declaration("forall n:N,Pred n", "prop"),   # an axiom
    "mA": Declaration("A", "moxia"),           # a denied proposition
}


def read(text, **kwargs):
    return resolve(parse_proof_term(text), DECLARATIONS, **kwargs)


# ---------------------------------------------------------------------------
#  Universal instantiation:  Cons -> ConsFO
# ---------------------------------------------------------------------------


def test_a_first_order_constant_heads_an_instantiation():
    node = read("μt:A.<ax||O*t>")
    assert isinstance(node.context, ConsFO)
    assert node.context.fo_term == TSym("O")


def test_an_axiom_heads_an_ordinary_application():
    node = read("μt:A.<a||ax*t>")
    assert isinstance(node.context, Cons)


def test_a_denied_proposition_heads_an_ordinary_application():
    assert isinstance(read("μt:A.<a||mA*t>").context, Cons)


def test_a_bound_proof_variable_heads_an_ordinary_application():
    node = read("μt:A.<λh:A.μu:A.<a||h*u>||t>")
    assert isinstance(node.term.term.context, Cons)


def test_a_predicate_does_not_head_an_instantiation():
    """`Pred : N->bool` is a proposition former, neither proof nor term."""
    assert isinstance(read("μt:A.<a||Pred*t>").context, Cons)


# ---------------------------------------------------------------------------
#  Universal introduction:  Lamda -> LamdaFO
# ---------------------------------------------------------------------------


def test_a_sort_annotation_makes_a_first_order_binder():
    node = read("μt:A.<λx:N.a||t>")
    assert isinstance(node.term, LamdaFO)
    assert node.term.var == "x"
    assert str(node.term.sort) == "N"


def test_a_propositional_annotation_stays_propositional():
    assert isinstance(read("μt:A.<λh:A.a||t>").term, Lamda)


def test_a_quantified_annotation_stays_propositional():
    assert isinstance(read("μt:A.<λh:forall n:N,Pred n.a||t>").term, Lamda)


def test_an_arrow_of_sorts_is_a_sort():
    node = read("μt:A.<λf:N->N.a||t>")
    assert isinstance(node.term, LamdaFO)
    assert str(node.term.sort) == "N->N"


def test_an_arrow_of_propositions_is_not_a_sort():
    assert isinstance(read("μt:A.<λh:A->A.a||t>").term, Lamda)


# ---------------------------------------------------------------------------
#  Scope
# ---------------------------------------------------------------------------


def test_a_variable_bound_first_order_heads_an_instantiation():
    node = read("μt:A.<λx:N.μu:A.<a||x*u>||t>")
    assert isinstance(node.term.term.context, ConsFO)


def test_an_existential_witness_variable_is_first_order():
    node = read("μt:A.<a||(y:N).μ'k:A.<μu:A.<a||y*u>||t>>")
    assert isinstance(node.context.context.term.context, ConsFO)


def test_a_first_order_binder_shadows_a_proof_variable():
    node = read("μt:A.<λx:A.λx:N.μu:A.<a||x*u>||t>")
    inner = node.term.term.term
    assert isinstance(inner.context, ConsFO)


def test_a_proof_binder_shadows_a_first_order_variable():
    node = read("μt:A.<λx:N.λx:A.μu:A.<a||x*u>||t>")
    inner = node.term.term.term
    assert isinstance(inner.context, Cons)


def test_a_binder_does_not_leak_past_its_subtree():
    """`x` is first-order only under its binder; the sibling must not see it."""
    node = read("μt:A.<λx:N.a||x*t>", strict=False)
    assert isinstance(node.term, LamdaFO)      # bound here
    assert isinstance(node.context, Cons)      # but not in scope here


def test_the_goal_environment_supplies_outer_binders():
    node = resolve(parse_proof_term("μt:A.<a||x*t>"), DECLARATIONS, env={"x": "N"})
    assert isinstance(node.context, ConsFO)


# ---------------------------------------------------------------------------
#  Validation
# ---------------------------------------------------------------------------


def test_an_undeclared_head_raises_rather_than_being_guessed():
    with pytest.raises(ResolutionError, match="neither bound nor declared"):
        read("μt:A.<a||mystery*t>")


def test_an_undeclared_annotation_raises():
    with pytest.raises(ResolutionError, match="neither a declared sort"):
        read("μt:A.<λq:Mystery.a||t>")


def test_strict_can_be_relaxed_to_the_propositional_reading():
    assert isinstance(read("μt:A.<a||mystery*t>", strict=False).context, Cons)


def test_without_declarations_nothing_is_promoted():
    """The pre-first-order behaviour, preserved for hand-built terms."""
    node = resolve(parse_proof_term("μt:A.<a||O*t>"), None)
    assert isinstance(node.context, Cons)


def test_unambiguous_first_order_nodes_need_no_declarations():
    """A compound witness is syntactically first-order already."""
    node = resolve(parse_proof_term("μt:A.<a||S (S O)*t>"), None)
    assert isinstance(node.context, ConsFO)
