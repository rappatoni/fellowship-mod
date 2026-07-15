from core.ac.alt_structure import (
    match_alt_structure,
    match_alternative_counterexample_structure,
    match_application_structure,
    match_defeasible_warrant_structure,
    match_dual_application_structure,
    match_dual_defeasible_warrant_structure,
)
from core.ac.ast import Cons, Deleg, DI, Geled, ID, Mu, Mutilde, Sonc


def _alt_pair(name="alt", prop="A", left=None, right=None):
    left = left or DI("case1", prop)
    right = right or DI("case2", prop)
    return Mu(
        ID(name, prop),
        prop,
        Mu(ID("_", prop), prop, left, ID(name, prop)),
        Mutilde(DI("_", prop), prop, right, ID(name, prop)),
    )


def test_match_alt_structure_extracts_two_terms():
    left = DI("case1", "A")
    right = DI("case2", "A")
    node = _alt_pair("some_var", "A", left, right)

    alt = match_alt_structure(node)

    assert alt is not None
    assert alt.binder_name == "some_var"
    assert alt.prop == "A"
    assert alt.elements == (left, right)


def test_match_alt_structure_accepts_none_props():
    left = DI("case1")
    right = DI("case2")
    node = _alt_pair("some_var", None, left, right)

    alt = match_alt_structure(node)

    assert alt is not None
    assert alt.prop is None
    assert alt.elements == (left, right)


def test_match_alt_structure_rejects_non_underscore_left_binder():
    node = Mu(
        ID("alt", "A"),
        "A",
        Mu(ID("used", "A"), "A", DI("case1", "A"), ID("alt", "A")),
        Mutilde(DI("_", "A"), "A", DI("case2", "A"), ID("alt", "A")),
    )

    assert match_alt_structure(node) is None


def test_match_alt_structure_rejects_wrong_continuation():
    node = Mu(
        ID("alt", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("case1", "A"), ID("other", "A")),
        Mutilde(DI("_", "A"), "A", DI("case2", "A"), ID("alt", "A")),
    )

    assert match_alt_structure(node) is None


def test_match_alt_structure_flattens_right_nested_tail():
    case1 = DI("case1", "A")
    case2 = DI("case2", "A")
    case3 = DI("case3", "A")
    nested = _alt_pair("tail", "A", case2, case3)
    node = _alt_pair("head", "A", case1, nested)

    alt = match_alt_structure(node)

    assert alt is not None
    assert alt.binder_name == "head"
    assert alt.prop == "A"
    assert alt.elements == (case1, case2, case3)


def _counterexample_pair(name="alt", prop="A", left=None, right=None):
    left = left or ID("cond1", prop)
    right = right or ID("cond2", prop)
    return Mutilde(
        DI(name, prop),
        prop,
        Mu(ID("_", prop), prop, DI(name, prop), left),
        Mutilde(DI("_", prop), prop, DI(name, prop), right),
    )


def test_match_alternative_counterexample_structure_extracts_two_contexts():
    left = ID("cond1", "A")
    right = ID("cond2", "A")
    node = _counterexample_pair("some_var", "A", left, right)

    alt = match_alternative_counterexample_structure(node)

    assert alt is not None
    assert alt.binder_name == "some_var"
    assert alt.prop == "A"
    assert alt.elements == (left, right)


def test_match_alternative_counterexample_structure_flattens_left_nested_tail():
    cond1 = ID("cond1", "A")
    cond2 = ID("cond2", "A")
    cond3 = ID("cond3", "A")
    nested = _counterexample_pair("tail", "A", cond1, cond2)
    node = _counterexample_pair("head", "A", nested, cond3)

    alt = match_alternative_counterexample_structure(node)

    assert alt is not None
    assert alt.binder_name == "head"
    assert alt.prop == "A"
    assert alt.elements == (cond1, cond2, cond3)


def test_match_defeasible_warrant_structure_extracts_support_and_exception():
    support = DI("support", "A")
    exception = ID("exception", "A")
    node = Mu(
        ID("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", support, ID("w", "A")),
        Mutilde(DI("_", "A"), "A", DI("support", "A"), exception),
    )

    warrant = match_defeasible_warrant_structure(node)

    assert warrant is not None
    assert warrant.binder_name == "w"
    assert warrant.prop == "A"
    assert warrant.support is support
    assert warrant.exception is exception


def test_match_dual_defeasible_warrant_structure_extracts_support_and_requirement():
    support = DI("support", "A")
    requirement = ID("requirement", "A")
    node = Mutilde(
        DI("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", support, requirement),
        Mutilde(DI("_", "A"), "A", DI("w", "A"), ID("requirement", "A")),
    )

    warrant = match_dual_defeasible_warrant_structure(node)

    assert warrant is not None
    assert warrant.binder_name == "w"
    assert warrant.prop == "A"
    assert warrant.support is support
    assert warrant.requirement is requirement


def test_match_defeasible_warrant_structure_accepts_reversed_inner_order():
    support = DI("support", "A")
    exception = ID("exception", "A")
    node = Mu(
        ID("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", support, exception),
        Mutilde(DI("_", "A"), "A", DI("support", "A"), ID("w", "A")),
    )

    warrant = match_defeasible_warrant_structure(node)

    assert warrant is not None
    assert warrant.support is support
    assert warrant.exception is exception


def test_match_dual_defeasible_warrant_structure_accepts_reversed_inner_order():
    support = DI("support", "A")
    requirement = ID("requirement", "A")
    node = Mutilde(
        DI("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("w", "A"), requirement),
        Mutilde(DI("_", "A"), "A", support, ID("requirement", "A")),
    )

    warrant = match_dual_defeasible_warrant_structure(node)

    assert warrant is not None
    assert warrant.support is support
    assert warrant.requirement is requirement


def test_match_defeasible_warrant_structure_treats_delegs_equal_up_to_prop():
    node = Mu(
        ID("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", Deleg("1", "A"), ID("w", "A")),
        Mutilde(DI("_", "A"), "A", Deleg("2", "A"), ID("exception", "A")),
    )

    warrant = match_defeasible_warrant_structure(node)

    assert warrant is not None
    assert isinstance(warrant.support, Deleg)
    assert warrant.support.number == "1"


def test_match_dual_defeasible_warrant_structure_treats_geleds_equal_up_to_prop():
    node = Mutilde(
        DI("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("support", "A"), Geled("1", "A")),
        Mutilde(DI("_", "A"), "A", DI("w", "A"), Geled("2", "A")),
    )

    warrant = match_dual_defeasible_warrant_structure(node)

    assert warrant is not None
    assert isinstance(warrant.requirement, Geled)
    assert warrant.requirement.number == "1"


def test_match_application_structure_extracts_function_and_argument():
    function = DI("f", "B->A")
    argument = DI("v", "B")
    node = Mu(
        ID("x", "A"),
        "A",
        function,
        Cons(argument, ID("x", "A")),
    )

    application = match_application_structure(node)

    assert application is not None
    assert application.binder_name == "x"
    assert application.prop == "A"
    assert application.function is function
    assert application.argument is argument
    assert application.argument_prop == "B"


def test_match_dual_application_structure_extracts_warrant_and_condition():
    condition = DI("e", "B")
    warrant = ID("f", "B->A")
    node = Mutilde(
        DI("x", "A"),
        "A",
        Sonc(ID("x", "A"), condition),
        warrant,
    )

    application = match_dual_application_structure(node)

    assert application is not None
    assert application.binder_name == "x"
    assert application.prop == "A"
    assert application.condition is condition
    assert application.warrant is warrant
    assert application.condition_prop == "B"
