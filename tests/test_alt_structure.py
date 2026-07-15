from core.ac.alt_structure import match_alt_structure
from core.ac.ast import DI, ID, Mu, Mutilde


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