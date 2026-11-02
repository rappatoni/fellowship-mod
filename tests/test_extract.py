"""A registered debate's arguments, cut out of its term (core/dc/extract.py;
tasks.org, aida-debate-reverse-compilation): registered in the debate's
session under the argument discipline, they are its moves, and compiled
again they give the graph the term was compiled to."""

import sys
from copy import deepcopy

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Laog, Geled, Deleg, ID, DI
from core.comp.oracle_terms import alpha_equal
from core.dc.discipline import free_variables, violations
from core.dc.extract import cut, extract_arguments
from core.dc.shapes import modernise_scaffolds
from wrap.cli import setup_prover
from wrap.service import ContentRefused, Service

sys.path.insert(0, "tests")
from test_registered_debate import block, run  # noqa: E402

STRICT = {"pRule", "pAx", "qAx", "nQ"}


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def p_arg(name="pArg", site="1"):
    """An argument for P owing Q."""
    return eta(name, "P", Mu(ID("r", "P"), "P", DI("pRule", "Q->P"), Cons(Goal(site, "Q"), ID("r", "P"))))


def t_sup(orig, scion, prop="P", alt="alt", beta="b"):
    return Mu(ID(alt, prop), prop, orig,
              Mutilde(DI(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(beta, prop), ID(alt, prop)),
                      Mutilde(DI("_", prop), prop, scion, ID(alt, prop))))


def t_att(orig, scion_ctx, prop="P", alt="alt", beta="b"):
    return Mu(ID(alt, prop), prop, orig,
              Mutilde(DI(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(beta, prop), scion_ctx),
                      Mutilde(DI("_", prop), prop, DI(beta, prop), ID(alt, prop))))


def c_att(orig_ctx, scion_t, prop="Q", x="x", beta="b"):
    return Mutilde(DI(x, prop), prop,
                   Mu(ID(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(x, prop), ID(beta, prop)),
                      Mutilde(DI("_", prop), prop, scion_t, ID(beta, prop))),
                   orig_ctx)


def signature(graph):
    return (sorted((e.target_key, e.target_side, e.strict,
                    tuple(sorted((s.key, s.side, s.kind) for s in e.sources))) for e in graph.edges),
            {k: frozenset(v) for k, v in graph.defaults.items()})


class TestCut:
    def test_a_support_scaffold_leaves_its_original(self):
        host = eta("d", "P", t_sup(Deleg("5", "P"), p_arg()))
        args, _ = extract_arguments(host, "d", strict_names=STRICT)
        opening, supporter = args
        assert (opening.rank, opening.conclusion, opening.anti) == (0, "P", False)
        assert alpha_equal(opening.body, eta("d", "P", Deleg("1", "P")))
        assert alpha_equal(supporter.body, p_arg())
        assert supporter.edge == "pArg"

    def test_an_attack_scaffold_leaves_its_original(self):
        attacker = Mutilde(DI("y", "P"), "P", DI("y", "P"), ID("nP", "P"))
        host = eta("d", "P", t_att(p_arg(), attacker))
        args, _ = extract_arguments(host, "d", strict_names=STRICT | {"nP"})
        assert alpha_equal(args[0].body, eta("d", "P", p_arg(site="1")))
        assert [a.anti for a in args] == [False, True]

    def test_a_context_attack_leaves_the_presumed_refutation(self):
        # the shape scasp-aida's _barred is rewritten into
        host = c_att(Geled("3", "Q"), eta("q1", "Q", DI("qAx", "Q")))
        args, _ = extract_arguments(host, "d", strict_names=STRICT)
        assert args[0].anti and alpha_equal(args[0].body, Geled("1", "Q"))
        assert alpha_equal(args[1].body, eta("q1", "Q", DI("qAx", "Q")))

    def test_a_captured_variable_becomes_a_site_of_its_kind(self):
        # the scion hands its result to alt, which the scaffold binds
        scion = Mu(ID("s", "P"), "P", DI("pRule", "Q->P"), Cons(Goal("1", "Q"), ID("alt", "P")))
        presumed = deepcopy(scion)
        presumed.context.context.captured_presumption = True
        for scion, site in ((scion, Laog), (presumed, Geled)):
            body = cut(scion, STRICT)
            assert free_variables(body) <= {("t", "pRule")}
            assert isinstance(body.context.context, site)

    def test_the_refutation_of_false_stays(self):
        # Fellowship's negation elimination ends in the built-in _F_; it is
        # no capture, and the discipline sees through the wrapper only with it
        refutation = Mutilde(DI("x", "Flies"), "Flies", DI("nf", "Penguin->~Flies"),
                             Cons(Deleg("1", "Penguin"),
                                  Mutilde(DI("H", "~Flies"), "~Flies", DI("H", "~Flies"),
                                          Cons(DI("x", "Flies"), ID("_F_", "false")))))
        body = cut(refutation, {"nf"})
        assert body.context.context.context.context.name == "_F_"
        assert not violations(body, True)

    def test_sites_are_numbered_afresh(self):
        body = cut(eta("d", "P", Mu(ID("r", "P"), "P", DI("pRule", "Q->P"),
                                    Cons(Goal("7.2", "Q"), ID("r", "P")))), STRICT)
        assert body.term.context.term.number == "1"


class TestRanks:
    def test_a_stack_bottom_first(self):
        stack = t_sup(p_arg("p1"), p_arg("p2"), alt="alt2", beta="b2")   # SUP(P1, P2)
        host = eta("d", "P", t_sup(Deleg("5", "P"), stack))
        args, _ = extract_arguments(host, "d", strict_names=STRICT)
        assert [(a.edge, a.rank) for a in args] == [("d", 0), ("p1", 1), ("p2", 2)]

    def test_nested_scaffolds_the_outermost_last(self):
        m3 = Mu(ID("alt2", "P"), "P",
                Mu(ID("_", "P"), "P",
                   Mu(ID("alt", "P"), "P", Mu(ID("_", "P"), "P", Deleg("5", "P"), ID("alt", "P")),
                      Mutilde(DI("_", "P"), "P", p_arg("p2"), ID("alt", "P"))),
                   ID("alt2", "P")),
                Mutilde(DI("_", "P"), "P", p_arg("p3"), ID("alt2", "P")))
        host = eta("d", "P", modernise_scaffolds(m3, STRICT))
        args, _ = extract_arguments(host, "d", strict_names=STRICT)
        assert [a.edge for a in args] == ["d", "p2", "p3"]

    def test_a_copy_is_extracted_once(self):
        stack = t_sup(p_arg("p1"), p_arg("p1"), alt="alt2", beta="b2")
        host = eta("d", "P", t_sup(Deleg("5", "P"), stack))
        args, _ = extract_arguments(host, "d", strict_names=STRICT)
        assert [a.edge for a in args] == ["d", "p1"]


@pytest.fixture
def prover():
    p = setup_prover()
    yield p
    p.close()


@pytest.mark.parametrize("fixture, count", [("simple", 5), ("simple_pred", 5), ("orphans", 13)])
def test_an_import_is_rebuilt_from_its_arguments(prover, fixture, count):
    run(prover, block(f"tests/import/{fixture}.fspy", "d"))
    debate = prover.debates["d"]
    child = debate.debate_session.session
    assert len(debate.extracted) == count and debate.refused == {}
    names = [m.argument for m in debate.moves]
    assert names == [f"d__a{k}" for k in range(1, count + 1)]
    for name in names:
        assert not violations(child.arguments[name].body, child.arguments[name].is_anti)
    # compiled again, the arguments give the graph the term was compiled to
    assert signature(child.graph) == signature(debate.record_graph)
    # and they are the moves of the debate in its session
    inner = child.debates["d"]
    assert [m.argument for m in inner.moves] == names and inner.finished
    assert Service.of(child).evaluate("d").nf_class == "value"
    assert Service.of(prover).graph("d", whole=True).graph is child.graph


def test_simple_s_arguments(prover):
    run(prover, block("tests/import/simple.fspy", "simple"))
    child = prover.debates["simple"].debate_session.session
    texts = {name: (a.conclusion, a.is_anti) for name, a in child.arguments.items()}
    assert texts == {"simple__a1": ("A", False), "simple__a2": ("simple__H_A", True),
                     "simple__a3": ("C", False), "simple__a4": ("simple__H_A", True),
                     "simple__a5": ("B", False)}
    # the presumed refutations of B and C are attacked by their proofs
    assert child.declarations["simple__a5"] == "B"            # strict: Fellowship holds it


def test_an_argument_the_session_refuses_is_a_warning(prover, monkeypatch, caplog):
    original = Service.register_body

    def register_body(self, name, conclusion, body, **kw):
        if name.endswith("__a3"):
            raise ContentRefused("refused for the test")
        return original(self, name, conclusion, body, **kw)

    monkeypatch.setattr(Service, "register_body", register_body)
    run(prover, block("tests/import/simple.fspy", "simple"))
    debate = prover.debates["simple"]
    assert debate.refused == {"simple__a3": "refused for the test"}
    assert [m.argument for m in debate.moves] == ["simple__a1", "simple__a2", "simple__a4", "simple__a5"]
    assert "not taken into its session" in caplog.text
