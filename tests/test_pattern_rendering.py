from core.ac.ast import Cons, DI, ID, Mu, Mutilde, Sonc
from pres.nl import natural_language_argumentative_rendering, natural_language_rendering, pretty_natural, pruefschema_rendering


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
            "Die Prüfung, ob A zerfällt in folgende Fallgruppen:",
            "   Fallgruppe:",
            "      by A",
            "   oder Fallgruppe:",
            "      by A",
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
            "Die Prüfung, ob A zerfällt in folgende Fallgruppen:",
            "   Fallgruppe:",
            "      by A",
            "   oder Fallgruppe:",
            "      by A",
            "   oder Fallgruppe:",
            "      by A",
        ]
    )


def test_non_argumentative_rendering_does_not_use_demo_pattern():
    node = _alt_pair("whatever", "A", DI("case1", "A"), DI("case2", "A"))

    out = pretty_natural(node, natural_language_rendering)

    assert "zerfällt in folgende Fallgruppen" not in out
    assert "we need to prove A(whatever)" in out


def test_pattern_renderer_resumes_normal_rendering_at_captured_terms():
    nested_case = Mu(ID("support", "B"), "B", DI("fact", "B"), ID("k", "B"))
    node = _alt_pair("whatever", "A", nested_case, DI("case2", "A"))

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert "Die Prüfung, ob A zerfällt in folgende Fallgruppen:" in out
    assert "      We will argue for B(support)" in out
    assert "         by B" in out
    assert "      by A" in out


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
            "Für A ist notwendigerweise zu prüfen:",
            "   Prüfpunkt:",
            "         done A",
            "   und Prüfpunkt:",
            "         done A",
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
            "Für A ist notwendigerweise zu prüfen:",
            "   Prüfpunkt:",
            "         done A",
            "   und Prüfpunkt:",
            "         done A",
            "   und Prüfpunkt:",
            "         done A",
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
            "Für A spricht ",
            "      by A",
            "   aber",
            "         done A",
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
            "gegen A spricht ",
            "      done A",
            "   aber",
            "         by A",
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
            "Für A spricht ",
            "      by A",
            "   aber",
            "         done A",
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
            "gegen A spricht ",
            "      done A",
            "   aber",
            "         by A",
        ]
    )


def test_argumentative_rendering_uses_application_pattern_block():
    node = Mu(
        ID("x", "A"),
        "A",
        DI("f", "B->A"),
        Cons(DI("v", "B"), ID("x", "A")),
    )

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Für A ist hinreichend, dass B",
            "   weil",
            "      by B -> A",
            "   und",
            "      by B",
        ]
    )


def test_argumentative_rendering_uses_dual_application_pattern_block():
    node = Mutilde(
        DI("x", "A"),
        "A",
        Sonc(ID("e", "B"), DI("x", "A")),
        ID("f", "B->A"),
    )

    out = pretty_natural(node, natural_language_argumentative_rendering)

    assert out == "\n".join(
        [
            "Für A ist notwendig dass B",
            "   weil",
            "         done B -> A",
            "      done B",
        ]
    )


def test_pruefschema_rendering_numbers_patterns_and_uses_connective_defaults():
    node = _alt_pair(
        "head",
        "A",
        Mu(ID("w", "A"), "A", DI("f", "B->A"), Cons(DI("v", "B"), ID("w", "A"))),
        _alt_pair("tail", "A", DI("case2", "A-B"), DI("case3", "A")),
    )

    out = pretty_natural(node, pruefschema_rendering)

    assert "Fallgruppe 1:" in out
    assert "oder Fallgruppe 2:" in out
    assert "oder Fallgruppe 3:" in out
    assert "B impliziert A" in out
    assert "A ohne B" in out
    assert "(@binder)ist" not in out
    assert "Prüfungvon" not in out
