"""The deprecated scaffold shapes rewritten into the paper shapes
(core/dc/shapes.py; tasks.org, aida-legacy-shape-shim).

No fixture argument holds an M3 shape (census of 2026-10-29), so the
equivalence is checked on built terms - the compiler reads the M3 shape
and its rewrite into the same graph - and on scasp-aida's imports in
tests/import/, whose terms Fellowship must still accept after the
rewrite."""

import warnings
from copy import deepcopy

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Laog, Geled, ID, DI
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.resolve import resolve
from core.comp.adf_label import grounded_labels
from core.comp.oracle_terms import alpha_equal
from core.dc.argument import Argument
from core.dc.debate_graph import (
    canonical_prop as K, compile_debate, declaration_kinds, _match_paper_scaffold,
)
from core.dc.shapes import modernise_scaffolds
from core.dc.strict import compile_issue
from wrap.cli import setup_prover
from wrap.service import Service
from wrap.syntax import Command, parse_units

STRICT = {"pRule", "pAx", "qAx", "nP", "nQ", "nQ2"}


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def anti_eta(name, prop, inner):
    return Mutilde(DI(name, prop), prop, DI(name, prop), inner)


def p_arg():
    """An argument for P owing Q: mu pArg:P.< mu r:P.< pRule || ?1:Q * r > || pArg >"""
    return eta("pArg", "P", Mu(ID("r", "P"), "P", DI("pRule", "Q->P"), Cons(Goal("1", "Q"), ID("r", "P"))))


def m3_t_sup(orig, scion, prop="P"):
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, scion, ID("alt", prop)))


def m3_t_att(orig, scion_ctx, prop="P"):
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, Goal("9", prop), scion_ctx))


def m3_c_sup(orig_ctx, scion_ctx, prop="Q"):
    return Mutilde(DI("alt", prop), prop,
                   Mu(ID("_", prop), prop, DI("alt", prop), scion_ctx),
                   Mutilde(DI("_", prop), prop, DI("alt", prop), orig_ctx))


def m3_c_att(orig_ctx, scion_t, prop="Q"):
    return Mutilde(DI("alt", prop), prop,
                   Mu(ID("_", prop), prop, scion_t, Laog("9", prop)),
                   Mutilde(DI("_", prop), prop, DI("alt", prop), orig_ctx))


def exchanged_c_att(scion_t, prop="Q"):
    """scasp-aida's _barred: mu'alt.< mu_.<alt||Q:3?> || mu'_.<SCION||Q:4?> >"""
    return Mutilde(DI("alt", prop), prop,
                   Mu(ID("_", prop), prop, DI("alt", prop), Geled("3", prop)),
                   Mutilde(DI("_", prop), prop, scion_t, Geled("4", prop)))


def signature(graph):
    """The graph up to edge names and roles: a role is diagnostic, and a
    stack the compiler read edge by edge in the M3 shape is one wing in the
    paper shape (the same edges, attacker instead of supporter)."""
    edges = sorted((e.target_key, e.target_side, e.strict,
                    tuple(sorted((s.key, s.side, s.kind) for s in e.sources)))
                   for e in graph.edges)
    return edges, {k: frozenset(v) for k, v in graph.defaults.items()}


CASES = {
    "T-SUP": (m3_t_sup(p_arg(), eta("p2", "P", DI("pAx", "P"))), "supporter", "term"),
    "T-ATT": (m3_t_att(p_arg(), ID("nP", "P")), "attacker", "term"),
    "T-ATT, rerouted scion": (m3_t_att(p_arg(), anti_eta("y", "P", ID("alt", "P"))), "attacker", "term"),
    "C-SUP": (m3_c_sup(ID("nQ", "Q"), ID("nQ2", "Q")), "supporter", "context"),
    "C-ATT": (m3_c_att(ID("nQ", "Q"), eta("q1", "Q", DI("qAx", "Q"))), "attacker", "context"),
    "C-ATT, rerouted scion": (m3_c_att(ID("nQ", "Q"), eta("q2", "Q", DI("alt", "Q"))), "attacker", "context"),
}


class TestM3Shapes:
    @pytest.mark.parametrize("case", CASES)
    def test_rewritten_into_the_paper_shape(self, case):
        term, role, side = CASES[case]
        match = _match_paper_scaffold(modernise_scaffolds(term, STRICT))
        assert match is not None and match[0] == role and match[1] == side
        assert match[5] == "alt"                     # alpha / x keeps its name

    @pytest.mark.parametrize("case", CASES)
    def test_compiles_to_the_same_graph_and_labels(self, case):
        term, _, _ = CASES[case]
        before = compile_debate(term, "d", strict_names=STRICT)
        after = compile_debate(modernise_scaffolds(term, STRICT), "d", strict_names=STRICT)
        assert signature(after) == signature(before)
        assert grounded_labels(after) == grounded_labels(before)

    def test_attack_scion_has_its_reroute_undone(self):
        term, _, _ = CASES["T-ATT, rerouted scion"]
        scion = _match_paper_scaffold(modernise_scaffolds(term, STRICT))[4]
        assert isinstance(scion.context, Laog)        # alt's place: a fresh open site
        assert scion.context.number not in ("1", "9")

    def test_input_is_left_unchanged(self):
        term, _, _ = CASES["T-SUP"]
        kept = deepcopy(term)
        modernise_scaffolds(term, STRICT)
        assert alpha_equal(term, kept)

    def test_fresh_beta_avoids_names_in_the_term(self):
        term = m3_t_sup(p_arg(), eta("b", "P", DI("pAx", "P")))
        out = modernise_scaffolds(term, STRICT)
        assert out.context.di.name not in ("b", "alt", "_")

    def test_a_stack_is_rewritten_throughout(self):
        inner = m3_t_sup(p_arg(), eta("p2", "P", DI("pAx", "P")))
        outer = Mu(ID("alt2", "P"), "P",
                   Mu(ID("_", "P"), "P", inner, ID("alt2", "P")),
                   Mutilde(DI("_", "P"), "P", eta("p3", "P", DI("pAx", "P")), ID("alt2", "P")))
        out = modernise_scaffolds(outer, STRICT)
        assert _match_paper_scaffold(out) is not None
        assert _match_paper_scaffold(out.term) is not None          # the original: the inner stack
        assert alpha_equal(modernise_scaffolds(out, STRICT), out)  # nothing M3 is left
        before = compile_debate(outer, "d", strict_names=STRICT)
        after = compile_debate(out, "d", strict_names=STRICT)
        assert signature(after) == signature(before)


class TestPaperShapesStay:
    def test_paper_scaffolds_are_not_rewritten(self):
        sup = modernise_scaffolds(m3_t_sup(p_arg(), eta("p2", "P", DI("pAx", "P"))), STRICT)
        att = modernise_scaffolds(m3_c_att(ID("nQ", "Q"), eta("q1", "Q", DI("qAx", "Q"))), STRICT)
        for paper in (sup, att):
            assert alpha_equal(modernise_scaffolds(paper, STRICT), paper)

    def test_m3_inside_a_paper_scion_is_rewritten(self):
        m3 = m3_t_sup(p_arg(), eta("p2", "P", DI("pAx", "P")))
        host = modernise_scaffolds(m3_t_sup(p_arg(), m3), STRICT)  # paper, with an M3 scion
        host.context.context.term = m3
        out = modernise_scaffolds(host, STRICT)
        assert _match_paper_scaffold(out.context.context.term) is not None


class TestExchangedContextAttack:
    def test_read_as_the_m3_context_attack(self):
        term = exchanged_c_att(eta("q1", "Q", DI("qAx", "Q")))
        match = _match_paper_scaffold(modernise_scaffolds(term, STRICT))
        assert match[:3] == ("attacker", "context", "Q")
        assert isinstance(match[3], Geled) and match[3].number == "3"   # the presumed refutation

    def test_compiles_to_a_presumption_attacked_by_the_proof(self):
        term = exchanged_c_att(eta("q1", "Q", DI("qAx", "Q")))
        before = compile_debate(term, "d", strict_names=STRICT)
        assert any(s.key == K("Q") and s.side == "context"
                   for e in before.edges if e.target_side == "context" for s in e.sources)
        after = compile_debate(modernise_scaffolds(term, STRICT), "d", strict_names=STRICT)
        assert after.is_acyclic()
        attacker, = after.edges
        assert (attacker.target_key, attacker.target_side, attacker.role) == (K("Q"), "term", "attacker")
        assert after.defaults[(K("Q"), "context")] == {"presumption"}

    def test_left_alone_when_not_asked_for(self):
        term = exchanged_c_att(eta("q1", "Q", DI("qAx", "Q")))
        out = modernise_scaffolds(term, STRICT, exchanged_context_attacks=False)
        assert alpha_equal(out, term)


# ---------------------------------------------------------------------------
# scasp-aida's imports (tests/import/): refused as arguments since the
# argument discipline; their terms must replay in Fellowship once rewritten.
# ---------------------------------------------------------------------------

def imported(path, *, modernise):
    """The import's term, rewritten if asked, replayed through Fellowship
    (the type check and enrichment), and the prover holding its
    declarations."""
    text = open(path, encoding="utf-8").read()
    register = next(u for u in parse_units(text) if isinstance(u, Command) and u.kind == "register")
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        Service.of(p).check(text.replace(register.text, ""))
    body = ProofTermTransformer().transform(
        Grammar().parser.parse(Argument._normalize_pt_to_unicode(register["proof_term"])))
    body = resolve(body, p.declarations)
    if modernise:
        body = modernise_scaffolds(body, p.declarations.keys())
    arg = Argument(p, name="imported", conclusion=register["conclusion"],
                   is_anti=isinstance(body, Mutilde))
    arg.body = body
    arg.execute(preserve_input_body=True)
    graph = compile_issue(arg.body, "imported", strict_names=p.declarations.keys(),
                          strict_kinds=declaration_kinds(p.declarations))
    return p, register["conclusion"], graph


@pytest.mark.parametrize("name", ["simple", "simple_pred", "orphans"])
def test_an_import_replays_and_compiles_acyclic_once_rewritten(name):
    p, conclusion, graph = imported(f"tests/import/{name}.fspy", modernise=True)
    try:
        assert graph.is_acyclic()
        labels = grounded_labels(graph)
        assert labels[(K(conclusion), "term")] == "IN"     # s(CASP)'s answer
        # every premise's presumed refutation is attacked by its proof, and loses
        for (key, side), label in labels.items():
            if side == "context" and (key, "term") in labels:
                assert label == "OUT", graph.nodes.get(key, key)
    finally:
        p.close()


def test_simple_without_the_rewrite_supports_a_premise_by_itself():
    """What the rewrite repairs: _barred matches no shape and compiles to
    B[c] <- B[c], B[t], B[c]."""
    p, _, graph = imported("tests/import/simple.fspy", modernise=False)
    try:
        assert not graph.is_acyclic()
        assert any(e.target_key == K("B") and e.target_side == "context"
                   and (K("B"), "context") in {(s.key, s.side) for s in e.sources}
                   for e in graph.edges)
    finally:
        p.close()


def test_simple_once_rewritten():
    p, _, graph = imported("tests/import/simple.fspy", modernise=True)
    try:
        # A's argument, the two refutations of H_A, the proofs of B and C,
        # and the three declared rules
        assert len(graph.edges) == 8
        assert not any(e.target_side == "context" and e.target_key in (K("B"), K("C"))
                       for e in graph.edges)
        for premise in ("B", "C"):
            attacker = next(e for e in graph.edges if e.target_key == K(premise))
            assert (attacker.target_side, attacker.role, attacker.strict) == ("term", "attacker", True)
            assert graph.defaults[(K(premise), "context")] == {"presumption"}
        attacks_on_h = [e for e in graph.edges if e.target_key == K("H_A") and e.target_side == "context"]
        assert len(attacks_on_h) == 2
    finally:
        p.close()
