from core.ac.ast import DI, ID, Mu, Mutilde
from pres.nl import natural_language_argumentative_rendering, natural_language_rendering, pretty_natural


def _alt_pair(name="alt", prop="A", left=None, right=None):
    left = left or DI("case1", prop)
    right = right or DI("case2", prop)
    return Mu(
        ID(name, prop),
        prop,
        Mu(ID("_", prop), prop, left, ID(name, prop)),
        Mutilde(DI("_", prop), prop, right, ID(name, prop)),
    )


def test_argumentative_rendering_uses_alternative_pattern_block():
    node = _alt_pair("whatever", "A", DI("case1", "A"), DI("case2", "A"))

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Hinreichend für A ist",
            "   Fallgruppe:",
            "      by case1",
            "   oder Fallgruppe:",
            "      by case2",
        ]
    )


def test_argumentative_rendering_flattens_right_nested_alternatives():
    case1 = DI("case1", "A")
    case2 = DI("case2", "A")
    case3 = DI("case3", "A")
    node = _alt_pair("head", "A", case1, _alt_pair("tail", "A", case2, case3))

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Hinreichend für A ist",
            "   Fallgruppe:",
            "      by case1",
            "   oder Fallgruppe:",
            "      by case2",
            "   oder Fallgruppe:",
            "      by case3",
        ]
    )


def test_non_argumentative_rendering_does_not_use_demo_pattern():
    node = _alt_pair("whatever", "A", DI("case1", "A"), DI("case2", "A"))

    out = pretty_natural(node, natural_language_rendering)

    assert "Hinreichend für" not in out
    assert "we need to prove A(whatever)" in out


def test_pattern_renderer_resumes_normal_rendering_at_captured_terms():
    nested_case = Mu(ID("support", "B"), "B", DI("fact", "B"), ID("k", "B"))
    node = _alt_pair("whatever", "A", nested_case, DI("case2", "A"))

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert "Hinreichend für A ist" in out
    assert "      We will argue for B(support)" in out
    assert "         by fact" in out
    assert "      by case2" in out


def _counterexample_pair(name="alt", prop="A", left=None, right=None):
    left = left or ID("cond1", prop)
    right = right or ID("cond2", prop)
    return Mutilde(
        DI(name, prop),
        prop,
        Mu(ID("_", prop), prop, DI(name, prop), left),
        Mutilde(DI("_", prop), prop, DI(name, prop), right),
    )


def test_argumentative_rendering_uses_alternative_counterexample_pattern_block():
    node = _counterexample_pair("whatever", "A", ID("cond1", "A"), ID("cond2", "A"))

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Für A ist notwendigerweise zu prüfen",
            "   Bedingung",
            "      done ",
            "   und Bedingung",
            "      done ",
        ]
    )


def test_argumentative_rendering_flattens_left_nested_counterexamples():
    cond1 = ID("cond1", "A")
    cond2 = ID("cond2", "A")
    cond3 = ID("cond3", "A")
    node = _counterexample_pair("head", "A", _counterexample_pair("tail", "A", cond1, cond2), cond3)

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Für A ist notwendigerweise zu prüfen",
            "   Bedingung",
            "      done ",
            "   und Bedingung",
            "      done ",
            "   und Bedingung",
            "      done ",
        ]
    )


def test_argumentative_rendering_uses_defeasible_warrant_pattern_block():
    node = Mu(
        ID("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("support", "A"), ID("w", "A")),
        Mutilde(DI("_", "A"), "A", DI("support", "A"), ID("exception", "A")),
    )

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Für A spricht wenn",
            "      by support",
            "   aber",
            "      done ",
        ]
    )


def test_argumentative_rendering_uses_dual_defeasible_warrant_pattern_block():
    node = Mutilde(
        DI("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("support", "A"), ID("requirement", "A")),
        Mutilde(DI("_", "A"), "A", DI("w", "A"), ID("requirement", "A")),
    )

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "by support",
            "   oder A erfordert dass",
            "      done ",
        ]
    )


def test_argumentative_rendering_uses_reversed_defeasible_warrant_pattern_block():
    node = Mu(
        ID("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("support", "A"), ID("exception", "A")),
        Mutilde(DI("_", "A"), "A", DI("support", "A"), ID("w", "A")),
    )

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Für A spricht wenn",
            "      by support",
            "   aber",
            "      done ",
        ]
    )


def test_argumentative_rendering_uses_reversed_dual_defeasible_warrant_pattern_block():
    node = Mutilde(
        DI("w", "A"),
        "A",
        Mu(ID("_", "A"), "A", DI("w", "A"), ID("requirement", "A")),
        Mutilde(DI("_", "A"), "A", DI("support", "A"), ID("requirement", "A")),
    )

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "by support",
            "   oder A erfordert dass",
            "      done ",
        ]
    )
