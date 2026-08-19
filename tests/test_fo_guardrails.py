"""Reduction and the debate operations refuse first-order terms.

They are Milestone B: the first-order reduction rules for AC/DC are not
settled, and grafting into a first-order binder needs design work.  Until then
they must fail loudly, because each of them has a fall-through that would
otherwise be silent and wrong:

* grafting returns an unrecognised node unchanged, so a graft inside it would
  simply not happen;
* structural matching returns "not equal", so a first-order term would compare
  unequal to itself;
* acceptance colouring answers "green", reporting a term as accepted without
  having examined it.
"""

import warnings

import pytest

from core.ac.alt_structure import _same_tree
from core.ac.ast import FirstOrderNotSupported, first_order_node
from core.ac.resolve import resolve
from core.ac.signature import Declaration
from core.ac.syntax import parse_proof_term
from core.comp.color import AcceptanceColoringVisitor, DebateTermLabeller
from core.comp.reduce import ArgumentTermReducer, EtaReducer
from core.dc.graft import graft_single, graft_uniform
from core.dc.match_utils import match_trees


DECLS = {
    "N": Declaration("type", "sort"),
    "O": Declaration("N", "sort"),
    "P": Declaration("N->bool", "sort"),
}


def first_order(text="μt:P O.<a||O*t>"):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return resolve(parse_proof_term(text), DECLS)


def propositional(text="μt:A.<a||b*t>"):
    return parse_proof_term(text)


# ---------------------------------------------------------------------------
#  Detection
# ---------------------------------------------------------------------------


def test_a_propositional_term_has_no_first_order_node():
    assert first_order_node(propositional()) is None


def test_a_first_order_node_is_found_however_deep():
    assert first_order_node(first_order()) is not None


def test_the_error_names_the_operation_and_the_construct():
    with pytest.raises(FirstOrderNotSupported) as failure:
        ArgumentTermReducer().reduce(first_order())
    assert "Reduction" in str(failure.value)
    assert "ConsFO" in str(failure.value)


# ---------------------------------------------------------------------------
#  Each deferred operation refuses
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "operation",
    [
        pytest.param(lambda t: ArgumentTermReducer().reduce(t), id="reduction"),
        pytest.param(lambda t: EtaReducer().reduce(t), id="eta_reduction"),
        pytest.param(lambda t: match_trees(t, t, {}), id="structural_match"),
        pytest.param(lambda t: _same_tree(t.context, t.context), id="shape_comparison"),
        pytest.param(lambda t: AcceptanceColoringVisitor().classify(t), id="colouring"),
        pytest.param(lambda t: DebateTermLabeller().label(t), id="labelling"),
    ],
)
def test_deferred_operations_refuse_first_order_terms(operation):
    with pytest.raises(FirstOrderNotSupported):
        operation(first_order())


@pytest.mark.parametrize(
    "graft",
    [
        pytest.param(lambda r, s: graft_uniform(r, s), id="uniform"),
        pytest.param(lambda r, s: graft_single(r, "1", s), id="single"),
    ],
)
def test_grafting_refuses_a_first_order_root(graft):
    root = first_order("μt:P O.<?1:A||O*t>")
    scion = parse_proof_term("μs:A.<a||s>")
    with pytest.raises(FirstOrderNotSupported):
        graft(root, scion)


# ---------------------------------------------------------------------------
#  Propositional work is untouched
# ---------------------------------------------------------------------------


def test_reduction_still_accepts_a_propositional_term():
    assert ArgumentTermReducer().reduce(propositional()) is not None


def test_colouring_still_accepts_a_propositional_term():
    assert AcceptanceColoringVisitor().classify(propositional()) in {
        "green", "red", "yellow", None,
    }


def test_matching_still_compares_propositional_terms():
    assert match_trees(propositional(), propositional(), {}) is True
