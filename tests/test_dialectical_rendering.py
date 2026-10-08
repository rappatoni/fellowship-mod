"""The dialectical rendering: vanilla with the support and attack scaffolds
abbreviated (SUP/ATT on the term side, PUS/TTA on the context side), and
vanilla's lambda layout, which the dialectical rendering inherits."""

from core.ac.ast import Cons, DI, Deleg, Geled, Goal, Hyp, ID, Lamda, Laog, Mu, Mutilde
from pres.nl import dialectical_rendering, pretty_natural, vanilla_rendering


# The paper shapes (core/dc/debate_graph.py, scaffold catalogue).
def t_sup(alt, beta, prop, t, t2):
    return Mu(ID(alt, prop), prop, t,
              Mutilde(DI(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(beta, prop), ID(alt, prop)),
                      Mutilde(DI("_", prop), prop, t2, ID(alt, prop))))


def t_att(alt, beta, prop, t, e2):
    return Mu(ID(alt, prop), prop, t,
              Mutilde(DI(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(beta, prop), e2),
                      Mutilde(DI("_", prop), prop, DI(beta, prop), ID(alt, prop))))


def c_sup(x, beta, prop, e, e2):
    return Mutilde(DI(x, prop), prop,
                   Mu(ID(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(x, prop), e2),
                      Mutilde(DI("_", prop), prop, DI(x, prop), ID(beta, prop))),
                   e)


def eta(name, prop, body):
    return Mu(ID(name, prop), prop, body, ID(name, prop))


def test_vanilla_lambda_body_is_an_indented_child():
    assert pretty_natural(Lamda(Hyp(DI("h", "A"), "A"), DI("h", "A")), vanilla_rendering) == (
        "λh:A.\n"
        "└─ h:A"
    )


def test_vanilla_lambda_nested_body_uses_continuation_guides():
    body = Mu(ID("a", "B"), "B", Goal("1", "B"), ID("a", "B"))
    out = pretty_natural(Lamda(Hyp(DI("h", "A"), "A"), body), vanilla_rendering)
    assert out == (
        "λh:A.\n"
        "└─ μa:B.<\n"
        "   ├─ ?1:B||\n"
        "   └─ a:B\n"
        "   >"
    )


def test_a_support_stack_reads_as_one_sup():
    # SUP(root, SUP(P1, P2)): the stacked shape the unfolder builds
    p1, p2 = eta("p1", "A", DI("f", "A")), eta("p2", "A", DI("g", "A"))
    term = t_sup("alt1", "b1", "A", Goal("u1", "A"), t_sup("alt2", "b2", "A", p1, p2))
    assert pretty_natural(term, dialectical_rendering) == (
        "SUP(?u1:A) (\n"
        "├─ ++ μp1:A.<\n"
        "│     ├─ f:A||\n"
        "│     └─ p1:A\n"
        "│     >\n"
        "└─ ++ μp2:A.<\n"
        "      ├─ g:A||\n"
        "      └─ p2:A\n"
        "      >\n"
        ")"
    )


def test_supports_nested_through_the_original_read_as_one_sup():
    # SUP(SUP(root, P1), P2): the legacy nesting
    term = t_sup("alt2", "b2", "A", t_sup("alt1", "b1", "A", Deleg("u1", "A"), DI("f", "A")),
                 DI("g", "A"))
    assert pretty_natural(term, dialectical_rendering) == (
        "SUP(!u1:A) (\n"
        "├─ ++ f:A\n"
        "└─ ++ g:A\n"
        ")"
    )


def test_an_attack_by_the_contrarys_debate():
    wing = c_sup("x1", "b3", "A", Laog("u2", "A"), ID("k", "A"))
    term = t_att("alt1", "b1", "A",
                 t_sup("alt2", "b2", "A", Goal("u1", "A"), DI("f", "A")), wing)
    assert pretty_natural(term, dialectical_rendering) == (
        "ATT(\n"
        "   SUP(?u1:A) (\n"
        "   └─ ++ f:A\n"
        "   )\n"
        ") (\n"
        "└─ -- PUS(u2:A?) (\n"
        "      └─ ++ k:A\n"
        "      )\n"
        ")"
    )


def test_the_context_side_attack_reads_tta():
    term = Mutilde(DI("x", "A"), "A",
                   Mu(ID("b", "A"), "A",
                      Mu(ID("_", "A"), "A", DI("x", "A"), ID("b", "A")),
                      Mutilde(DI("_", "A"), "A", DI("t", "A"), ID("b", "A"))),
                   Geled("u1", "A"))
    assert pretty_natural(term, dialectical_rendering) == (
        "TTA(u1:A!) (\n"
        "└─ -- t:A\n"
        ")"
    )


def test_other_terms_render_as_vanilla():
    term = Mu(ID("x", "A"), "A", DI("f", "B->A"), Cons(Goal("1", "B"), ID("x", "A")))
    assert pretty_natural(term, dialectical_rendering) == pretty_natural(term, vanilla_rendering)
