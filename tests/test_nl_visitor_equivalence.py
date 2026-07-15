from pres.nl import (
    pretty_natural,
    natural_language_rendering,
    natural_language_dialectical_rendering,
    natural_language_argumentative_rendering,
)
from core.ac.ast import Admal, Mu, Mutilde, Lamda, Cons, Sonc, Goal, Laog, Deleg, Geled, ID, DI, Hyp, Pyh


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
            "   by B -> A",
            "   and",
            "   ? B",
            "done x",
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
    # vanilla should preserve full syntax while adding tree guides
    assert "μx:A.<" in out
    assert "├─ f:B->A||" in out
    assert "└─ ?1:B*x:A" in out
    assert "||" in out
    assert "*" in out
    assert "f:B->A" in out
    assert "1:B" in out



def test_vanilla_rendering_nested_uses_continuation_guides():
    pt = Mu(
        ID("x", "A"),
        "A",
        Mu(ID("y", "B"), "B", DI("g", "B"), ID("y", "B")),
        ID("x", "A"),
    )

    out = pretty_natural(pt, vanilla_rendering)

    assert "├─ μy:B.<" in out
    assert "│  ├─ g:B||" in out
    assert "│  └─ y:B" in out
    assert "└─ x:A" in out



def test_vanilla_rendering_mutilde_uses_tree_guides():
    pt = Mutilde(
        DI("k", "A"),
        "A",
        DI("f", "A"),
        ID("alpha", "A"),
    )

    out = pretty_natural(pt, vanilla_rendering)

    assert "μ'k:A.<" in out
    assert "├─ f:A||" in out
    assert "└─ alpha:A" in out


def test_nl_unbound_id_renders_prop_but_bound_id_renders_name():
    unbound = ID("free", "A")
    assert pretty_natural(unbound, natural_language_rendering) == "done A"

    bound = Mu(ID("x", "A"), "A", DI("fact", "A"), ID("x", "A"))
    out = pretty_natural(bound, natural_language_rendering)
    assert out.splitlines()[-1] == "done x"


def test_nl_unbound_di_renders_prop_but_bound_di_renders_name():
    unbound = DI("fact", "A")
    assert pretty_natural(unbound, natural_language_rendering) == "by A"

    bound = Lamda(Hyp(DI("h", "A"), "A"), DI("h", "A"))
    out = pretty_natural(bound, natural_language_rendering)
    assert out.splitlines()[-1] == "by h"


def test_nl_admal_tracks_bound_id_name():
    bound = Admal(Pyh(ID("k", "A"), "A"), ID("k", "A"))
    out = pretty_natural(bound, natural_language_rendering)
    assert out == "done k"


def test_nl_structural_leaves_render_decorated_props():
    declarations = {"Bird": "iota -> bool", "Tweety": "iota"}
    decorations = {"Bird": "@arg1 is a bird", "Tweety": "Tweety"}

    assert pretty_natural(
        Goal("1", "Bird Tweety"),
        natural_language_rendering,
        declarations=declarations,
        decorations=decorations,
    ) == "? Tweety is a bird"
    assert pretty_natural(
        Laog("1", "Bird Tweety"),
        natural_language_rendering,
        declarations=declarations,
        decorations=decorations,
    ) == " ?Tweety is a bird"
    assert pretty_natural(
        Deleg("1", "Bird Tweety"),
        natural_language_rendering,
        declarations=declarations,
        decorations=decorations,
    ) == " !Tweety is a bird"
    assert pretty_natural(
        Geled("1", "Bird Tweety"),
        natural_language_rendering,
        declarations=declarations,
        decorations=decorations,
    ) == "! Tweety is a bird"
