"""Every binder captures, the scaffolds' alt and b included (tasks.org,
aida-unfold-scaffold-binders-capture).

On either side a scaffold's alt binds the contrary of its statement and
its b the statement itself; the scion is under both, the supported or
attacked term under alt only:

    SUP(T, X) = mu alt.< T || mu'b.< mu _.<b||alt> || mu'_.< X || alt > > >
    ATT(T, E) = mu alt.< T || mu'b.< mu _.<b||E>   || mu'_.< b || alt > > >

A supporter that needs the statement it supports gets b (after the outer
step, the supported term); the attacker wing's root, the contrary, is the
attack's alt.  The strict phase takes b for its supported term and alt
for "the statement fails"; the rewrites substitute b; the compiler counts
b as binding.
"""

import copy
import logging
import re
import warnings

import pytest

from core.ac.ast import DI, ID, Goal, Laog, Mu, Mutilde
from core.comp.evaluate import evaluate_debate
from core.comp.oracle_terms import _free_names, alpha_equal, normalize_strong
from core.dc.debate_graph import (
    BUILTIN_LEAVES, _match_scaffold, canonical_prop as K, declaration_kinds,
)
from core.dc.strict import (
    compile_issue, keep_attack_wing, keep_scion_support, strict_in_scope, strict_resolve,
)
from core.dc.unfold import argument_edge, unfold, unfold_argument
from pres.gen import pres_str
from wrap.cli import execute_script, setup_prover

from test_unfold_entrypoints import FIXTURES

CYCLE = "tests/minicourse/lesson8a_cycle.fspy"
CIRCULAR = "tests/circular_supporter.fspy"
LESSON9 = "tests/minicourse/lesson9_evaluation.fspy"
SELF_ATTACK = "tests/rationality/self_attack_lk.fspy"


@pytest.fixture
def fresh():
    prover = setup_prover()
    yield prover
    prover.close()


def run(prover, script):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, str(script), strict=False, stop_on_error=False, isolate=False,
                       render_files=False, stop_marker=False)


def options(prover):
    return dict(strict_names=list(prover.declarations.keys()),
                strict_kinds=declaration_kinds(prover.declarations))


def evaluate(prover, term, mode="skeptical"):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return evaluate_debate(copy.deepcopy(term), "x", mode=mode, **options(prover))


def terms_of(prover):
    doc = prover.graph
    for statement in doc.statements():
        yield f"issue {statement}", unfold(doc, statement)
    for edge in doc.edges:
        if edge.role != "subargument":
            yield f"argument {edge.name}", unfold_argument(doc, edge)


# ---------------------------------------------------------------------------
# The unfolder
# ---------------------------------------------------------------------------

def test_a_circular_supporter_rests_on_b(fresh):
    # back needs B inside B's own debate: its demand is the b of B's
    # support scaffold, and after the outer step B's root obligation,
    # shared - no second obligation for B.  Skeptically B is UNDEC and the
    # site an obligation; the credulous witness labels the loop IN, so it
    # is the delegation !IN:B, a value (the coinductive reading, tasks.org
    # aida-unfounded-credulous-witness).
    run(fresh, CYCLE)
    term = unfold(fresh.graph, (K("B"), "term"))
    match = _match_scaffold(term, set(fresh.declarations))
    beta = term.context.di.name
    assert match[0] == "supporter" and "r3:B->A||%s:B*" % beta in pres_str(term)
    assert len(re.findall(r"\?u\d+:B", pres_str(term))) == 1
    nf, cls, *_ = evaluate(fresh, term, "skeptical")
    assert cls == "open" and pres_str(nf) == "?UNDEC:B"
    nf, cls, *_ = evaluate(fresh, term, "credulous")
    assert cls == "value" and "r3:B->A||!IN:B*" in pres_str(nf)


@pytest.mark.parametrize("script", FIXTURES + [CIRCULAR, "tests/entrypoints.fspy"])
def test_no_statement_is_left_without_a_binder(fresh, script, caplog):
    # capture alone ends the unfolding: the defensive fallback never fires
    run(fresh, script)
    with caplog.at_level(logging.WARNING, logger="core.dc.unfold"):
        for _label, _term in terms_of(fresh):
            pass
    assert not [r for r in caplog.records if "no binder in scope" in r.getMessage()]


@pytest.mark.parametrize("script", FIXTURES + [CIRCULAR, "tests/entrypoints.fspy"])
def test_no_scaffold_variable_is_left_free(fresh, script):
    # the rewrites substitute b: after both phases and normalisation only
    # declared names are free
    run(fresh, script)
    allowed = set(fresh.declarations) | set(BUILTIN_LEAVES)
    for label, term in terms_of(fresh):
        for mode in ("skeptical", "credulous"):
            nf, *_ = evaluate(fresh, term, mode)
            free = {name for _kind, name in _free_names(nf)} - allowed
            assert not free, (label, mode, free, pres_str(nf))


def test_the_attacker_wing_root_is_the_attacks_alt(fresh):
    # lesson 9: the debate about "A fails" attacks A; its root is the
    # attack's alt, and con is left to sigma
    run(fresh, LESSON9)
    names = set(fresh.declarations)
    term = unfold(fresh.graph, (K("B"), "term"))
    pro = _match_scaffold(term, names)[4]
    attack = pro.term.context.term                       # the debate about A
    match = _match_scaffold(attack, names)
    assert match[0] == "attacker"
    wing = _match_scaffold(match[4], names)
    assert wing[0] == "supporter" and isinstance(wing[3], ID) and wing[3].name == attack.id.name
    trace = []
    strict_resolve(term, names, trace=trace)
    assert (K("A"), "context") not in [s for s, _ in trace]
    for mode in ("skeptical", "credulous"):
        assert evaluate(fresh, term, mode)[1] == "value"


# ---------------------------------------------------------------------------
# Verdicts
# ---------------------------------------------------------------------------

def reordered(tmp_path):
    """tests/circular_supporter.fspy with other registered last."""
    text = open(CIRCULAR).read()
    other = re.search(r"argument other : \(A\)\..*?dixi\.\n", text, re.S).group(0)
    back = re.search(r"argument back : \(A\)\..*?dixi\.\n", text, re.S).group(0)
    path = tmp_path / "other_last.fspy"
    path.write_text(text.replace(other, "@@").replace(back, other).replace("@@", back))
    return path


@pytest.mark.parametrize("order", ["back last", "other last"])
def test_a_circular_supporter_no_longer_wins_by_strictness(fresh, order, tmp_path):
    # Strictness takes back's captured b for the supported term, which
    # holds an open site: back is not strict and does not beat other.  A
    # is VALUE in both orders (before: OPEN with back last).
    run(fresh, CIRCULAR if order == "back last" else reordered(tmp_path))
    term = unfold(fresh.graph, (K("A"), "term"))
    for mode in ("skeptical", "credulous"):
        assert evaluate(fresh, term, mode)[1] == "value"


def test_b_with_back_on_top_is_a_value_through_the_delegation(fresh, tmp_path):
    # aida-circular-supporter-judged-in: inside B's debate sigma keeps back
    # (its source B is IN), whose B can only be B's root obligation.  B is
    # IN (through other), so that obligation is the delegation !IN:B and B
    # is a value - but other's proof is not in the normal form
    # (aida-labelled-sites-delegation-rewrite; OPEN before).  With other on
    # top, B is a value through other.
    run(fresh, CIRCULAR)
    nf, cls, *_ = evaluate(fresh, unfold(fresh.graph, (K("B"), "term")))
    assert cls == "value" and "r3:B->A||!IN:B*" in pres_str(nf)
    fresh.new_document()
    run(fresh, reordered(tmp_path))
    assert evaluate(fresh, unfold(fresh.graph, (K("B"), "term")))[1] == "value"


def test_an_attack_with_nothing_but_its_fallback_normalises_away(fresh):
    # The self-attack: the root attack's wing is the captured contrary
    # alone.  No scaffold is matched; cbn and cbv agree up to eta, and
    # P[c] keeps its presumption marker.
    run(fresh, SELF_ATTACK)
    names = set(fresh.declarations)
    term = unfold(fresh.graph, (K("P"), "term"))
    assert _match_scaffold(term, names) is None
    alt, wiring = term.id.name, term.context
    assert pres_str(wiring.context) == f"μ'_:P.<{wiring.di.name}:P||{alt}:P>"
    resolved, _ = strict_resolve(term, names)
    forms = {s: eta_normal(normalize_strong(copy.deepcopy(resolved), s)) for s in ("cbn", "cbv")}
    assert alpha_equal(forms["cbn"], forms["cbv"])
    graph = compile_issue(term, "x", **options(fresh))
    assert "presumption" in graph.defaults[(K("P"), "context")]


def eta_normal(term):
    from core.ac.ast import ProofTerm
    from core.comp.oracle_terms import _occurs
    if (isinstance(term, Mu) and isinstance(term.context, ID) and term.context.name == term.id.name
            and not _occurs(term.term, ID, term.id.name)):
        return eta_normal(term.term)
    if (isinstance(term, Mutilde) and isinstance(term.term, DI) and term.term.name == term.di.name
            and not _occurs(term.context, DI, term.di.name)):
        return eta_normal(term.context)
    for slot in ("term", "context"):
        child = getattr(term, slot, None)
        if isinstance(child, ProofTerm):
            setattr(term, slot, eta_normal(child))
    return term


# ---------------------------------------------------------------------------
# The strict phase and the rewrites, on hand-built scaffolds
# ---------------------------------------------------------------------------

def sup(orig, scion, alt="a", beta="b", prop="Q"):
    return Mu(ID(alt, prop), prop, orig,
              Mutilde(DI(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(beta, prop), ID(alt, prop)),
                      Mutilde(DI("_", prop), prop, scion, ID(alt, prop))))


def att(orig, scion, alt="a", beta="b", prop="Q"):
    return Mu(ID(alt, prop), prop, orig,
              Mutilde(DI(beta, prop), prop,
                      Mu(ID("_", prop), prop, DI(beta, prop), scion),
                      Mutilde(DI("_", prop), prop, DI(beta, prop), ID(alt, prop))))


def uses_b(prop="Q"):
    """A supporter of Q that needs Q: mu k.< b || k >."""
    return Mu(ID("k", prop), prop, DI("b", prop), ID("k", prop))


class TestStrictness:
    def test_b_is_as_open_as_the_supported_term(self):
        scope_open = {"b": ("b", DI, Goal("1", "Q"))}
        scope_strict = {"b": ("b", DI, DI("qAx", "Q"))}
        assert not strict_in_scope(uses_b(), scope_open)
        assert strict_in_scope(uses_b(), scope_strict)

    def test_b_follows_outward(self):
        # b2 stands for a term using b1, which stands for a site
        scope = {"b1": ("b", DI, Goal("1", "Q")),
                 "b2": ("b", DI, Mu(ID("j", "Q"), "Q", DI("b1", "Q"), ID("j", "Q")))}
        assert not strict_in_scope(Mu(ID("k", "Q"), "Q", DI("b2", "Q"), ID("k", "Q")), scope)

    def test_alt_is_never_strict(self):
        assert not strict_in_scope(ID("a", "Q"), {"a": ("alt", ID, "Q")})

    def test_an_arguments_own_binder_stays_settled(self):
        assert strict_in_scope(DI("h", "Q"), {"b": ("b", DI, Goal("1", "Q"))})

    def test_a_supporter_needing_its_statement_does_not_beat_the_site(self):
        trace = []
        strict_resolve(sup(Goal("1", "Q"), uses_b()), {"qAx"}, trace=trace)
        assert trace == []

    def test_with_a_strict_original_it_is_decided(self):
        trace = []
        strict_resolve(sup(DI("qAx", "Q"), uses_b()), {"qAx"}, trace=trace)
        assert [what for _, what in trace] == ["supporter strict"]


class TestRewrites:
    def test_keeping_the_supporter_substitutes_b(self):
        node = sup(Goal("1", "Q"), uses_b())
        kept = keep_scion_support(node, node.context.context.term, "a")
        assert "b" not in {n for _k, n in _free_names(kept)}
        assert isinstance(kept.term, Goal)                       # mu k.< ?1 || k >

    def test_keeping_the_attacker_substitutes_b(self):
        wing = Mutilde(DI("y", "Q"), "Q", DI("b", "Q"), Laog("2", "Q"))   # needs Q: b
        node = att(Goal("1", "Q"), wing)
        kept = keep_attack_wing(node, _match_scaffold(node, set()))
        assert "b" not in {n for _k, n in _free_names(kept)}
        assert pres_str(kept) == "μa:Q.<?1:Q||μ'y:Q.<?1:Q||2:Q?>>"

    def test_argument_names_survive_the_substitution(self):
        scion = Mu(ID("pro", "Q"), "Q", DI("b", "Q"), ID("pro", "Q"))
        node = sup(Goal("1", "Q"), scion)
        kept = keep_scion_support(node, node.context.context.term, "a")
        assert kept.id.name == "pro"
