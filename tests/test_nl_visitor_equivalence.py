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
    assert out == "\n".join(
        [
            "we need to prove A(x)",
            "   by f",
            "   and",
            "   ? 1",
            "done ",
        ]
    )


from pres.nl import vanilla_rendering

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


def test_nl_sonc_is_handled_in_argumentative_rendering():
    pt = Sonc(ID("k", "A"), DI("x", "A"))
    out = pretty_natural(pt, natural_language_argumentative_rendering)
    # Sonc is now traversed (like Cons), so we should see both children.
    assert "and" in out
    assert "done" in out


def test_vanilla_rendering_basic():
    pt = Mu(
        ID("x", "A"),
        "A",
        DI("f", "B->A"),
        Cons(Goal("1", "B"), ID("x", "A")),
    )
    out = pretty_natural(pt, vanilla_rendering)
    # vanilla should preserve full syntax (only whitespace differs)
    assert "μx:A.<" in out
    assert "||" in out
    assert "*" in out
    assert "f:B->A" in out
    assert "1:B" in out
