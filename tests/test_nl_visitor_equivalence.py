from pres.nl import (
    pretty_natural,
    natural_language_rendering,
    natural_language_dialectical_rendering,
    natural_language_argumentative_rendering,
)
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Sonc, Goal, ID, DI


def test_nl_simple_mu_cons_goal_di_id_matches_expected():
    # μx:A.<f||?1*x>
    pt = Mu(
        ID("x", "A"),
        "A",
        DI("f", "B->A"),
        Cons(Goal("1", "B"), ID("x", "A")),
    )

    out = pretty_natural(pt, natural_language_rendering)
    # NOTE: natural_language_rendering.Mu is a string, but the renderer indexes it as semantic.Mu[0].
    # So legacy behavior prefixes only the first character ('w') rather than the full phrase.
    assert out == "\n".join(
        [
            "wA(x)",
            "   by f",
            "   and",
            "   ? 1",
            "done ",
        ]
    )


def test_nl_support_shape_does_not_crash():
    # This exercises the special-case 'support' branch in argumentative semantics.
    pt = Mu(
        ID("support", "A"),
        "A",
        Goal("1", "A"),
        ID("k", "A"),
    )
    # Just ensure rendering is stable/non-empty across semantics.
    assert pretty_natural(pt, natural_language_argumentative_rendering)
    assert pretty_natural(pt, natural_language_rendering)
    assert pretty_natural(pt, natural_language_dialectical_rendering)


def test_nl_sonc_preserves_legacy_unhandled_message():
    pt = Sonc(ID("k", "A"), DI("x", "A"))
    out = pretty_natural(pt, natural_language_argumentative_rendering)
    assert out == "Unhandled term type: <class 'core.ac.ast.Sonc'>"
