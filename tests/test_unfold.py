"""B3/B4: unfolding a document graph into a debate term, and adequacy
restated over unfolded terms (fragment-followups-plan.org, Phase B;
tasks.org aida-graph-first).

Hand-built graphs check the shapes; the fixture-based tests replay real
.fspy files, build the document graph from every atomic argument, unfold
each issue and check T6 (termination), the labels and the verdicts.
"""

import warnings
from pathlib import Path

import pytest

from core.ac.ast import Mu, Mutilde, Lamda, Hyp, Cons, Goal, Laog, Deleg, Geled, ID, DI
from core.comp.adf_label import grounded_labels, labellings
from core.comp.evaluate import evaluate_debate
from core.comp.oracle_terms import alpha_equal, normalize_strong, classify_nf


def _contains(node, pred):
    if pred(node):
        return True
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if child is not None and _contains(child, pred):
            return True
    return False
from core.dc.debate_graph import (
    compile_debate, compile_document, declaration_kinds, canonical_prop,
    DebateGraph, Edge, Source, _match_scaffold,
)
from core.dc.unfold import unfold, Unfolder, contrary
from core.dc.strict import compile_issue, strict_resolve
from core.dc.typecheck import typecheck, TypeCheckFailed
from wrap.cli import setup_prover, execute_script

K = canonical_prop


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def parg(site):
    return eta("pArg", "P", Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"), Cons(site, ID("rule", "P"))))


STRICT = {"pRule", "qRule", "sq"}


def document(*named):
    return compile_document(list(named), strict_names=STRICT)


class TestShapes:
    def test_lone_argument_supports_the_issue_site(self):
        # The root is a statement like any other: its own site (an
        # obligation, P being presumed nowhere) supported by pArg.
        doc = document(("pArg", parg(Goal("1", "Q"))))
        term = unfold(doc, (K("P"), "term"))
        match = _match_scaffold(term, STRICT)
        assert match is not None and match[0] == "supporter" and not match.legacy
        assert isinstance(match[3], Goal) and match[3].prop == "P"
        assert isinstance(match[4], Mu) and match[4].id.name == "pArg"
        g = compile_debate(term, "u", strict_names=STRICT)
        assert [(e.name, e.role, [(g.nodes[s.key], s.side, s.kind) for s in e.sources]) for e in g.edges] == [
            ("pArg", "supporter", [("Q", "term", "obligation")])]
        assert g.defaults[(K("P"), "term")] == {"obligation"}
        # and it evaluates to pArg once pArg's derivation is judged
        nf, cls, sigma, _ = evaluate_debate(term, "u", strict_names=STRICT, mode="credulous")
        assert cls == "open"           # Q is an obligation nobody meets

    def test_supporter_becomes_a_support_scaffold(self):
        qarg = eta("qArg", "Q", Mu(ID("r", "Q"), "Q", DI("qRule", "R->Q"), Cons(Goal("2", "R"), ID("r", "Q"))))
        doc = document(("pArg", parg(Goal("1", "Q"))), ("qArg", qarg))
        term = unfold(doc, (K("P"), "term"))
        parg_copy = _match_scaffold(term, STRICT)[4]     # the root's supporter
        site = parg_copy.term.context.term               # pArg's Q site
        match = _match_scaffold(site, STRICT)
        assert match is not None and match[0] == "supporter" and match[2] == "Q"
        g = compile_debate(term, "u", strict_names=STRICT)
        assert {e.name for e in g.edges} == {"pArg", "qArg"}

    def test_presumed_contrary_becomes_an_attack_scaffold(self):
        doc = document(("pArg", parg(Deleg("1", "Q"))),
                       ("qAtt", Mutilde(DI("qAtt", "Q"), "Q", DI("qAtt", "Q"), Geled("1", "Q"))))
        term = unfold(doc, (K("P"), "term"))
        parg_copy = _match_scaffold(term, STRICT)[4]
        site = parg_copy.term.context.term
        match = _match_scaffold(site, STRICT)
        assert match is not None and match[0] == "attacker" and not match.legacy
        g = compile_debate(term, "u", strict_names=STRICT)
        labels = grounded_labels(g)
        assert labels[(K("Q"), "term")] == "UNDEC" and labels[(K("Q"), "context")] == "UNDEC"

    def test_demanded_contrary_is_the_trivial_challenge(self):
        # Somebody merely has an open refutation site for Q: an attacker
        # that labels OUT and is discarded.
        doc = document(("pArg", parg(Deleg("1", "Q"))),
                       ("qOpen", Mutilde(DI("qOpen", "Q"), "Q", DI("qOpen", "Q"), Laog("1", "Q"))))
        term = unfold(doc, (K("P"), "term"))
        nf, cls, sigma, _ = evaluate_debate(term, "u", strict_names=STRICT)
        assert sigma[(K("Q"), "context")] == "OUT" and cls == "value"

    def test_cycle_is_cut_at_a_repeated_statement(self):
        # P from Q supported by Q from P: the inner P stays a bare site.
        qfromp = eta("qFromP", "Q", Mu(ID("k", "Q"), "Q", Goal("7", "P"), ID("k", "Q")))
        doc = document(("pArg", parg(Goal("1", "Q"))), ("qFromP", qfromp))
        assert not doc.is_acyclic()
        term = unfold(doc, (K("P"), "term"))          # terminates
        g = compile_debate(term, "u", strict_names=STRICT)
        # a positive cycle with no foundation: grounded undecided; complete
        # admits both-IN and both-OUT (two preferred), but only both-OUT
        # is stable - self-support is unfounded.  The ADF's verdict.
        assert set(grounded_labels(g).values()) == {"UNDEC"}
        assert len(labellings(g, "preferred")) == 2
        assert [set(l.values()) for l in labellings(g, "stable")] == [{"OUT"}]

    def test_hypothesis_is_captured(self):
        # Q from P (obligation) supporting the Q site inside lambda h:P.
        host = eta("s2", "P", Mu(ID("beta", "P"), "P",
                                 Lamda(Hyp(DI("h", "P"), "P"), Goal("1", "Q")), Laog("2", "P->Q")))
        p3 = eta("p3", "Q", Mu(ID("g", "Q"), "Q", Goal("8", "P"), ID("g", "Q")))
        doc = document(("s2", host), ("p3", p3))
        term = unfold(doc, (K("P"), "term"))
        assert _contains(term, lambda n: isinstance(n, DI) and n.name == "h")
        # the only demand for P left is the issue's own root site
        goals_for_p = []
        _contains(term, lambda n: goals_for_p.append(n) if isinstance(n, Goal) and n.prop == "P" else False)
        assert len(goals_for_p) == 1 and goals_for_p[0] is term.term   # the root's own site

    def test_continuation_captures_an_obligation(self):
        # A demand for a refutation of P under a lambda inside a proof of
        # P is the continuation: debates are classical.
        host = eta("s2", "P", Mu(ID("beta", "P"), "P",
                                 Lamda(Hyp(DI("h", "P"), "P"), Goal("1", "Q")), Laog("2", "P->Q")))
        p3 = eta("p3", "Q", Mu(ID("g", "Q"), "Q", Goal("9", "P"), Laog("8", "P")))
        doc = document(("s2", host), ("p3", p3))
        term = unfold(doc, (K("P"), "term"))
        assert _contains(term, lambda n: isinstance(n, ID) and n.name == "beta")
        # p3's demand is gone; the one Laog for P left is the root's trivial
        # challenge (P[c] carries an obligation marker), outside p3
        laogs = []
        _contains(term, lambda n: laogs.append(n) if isinstance(n, Laog) and n.prop == "P" else False)
        assert len(laogs) == 1
        assert not _contains(term, lambda n: isinstance(n, Mu) and n.id.name == "g"
                             and _contains(n, lambda m: isinstance(m, Laog)))

    def test_presumption_is_captured_as_a_presumption_source(self):
        # The cycle representation.  p3 derives Q from a PRESUMED refutation
        # of P (via pq : (P->false)->Q) and supports the Q site inside the
        # proof of P: the presumption is captured by that proof's own
        # continuation (no copy, no bare site), and the compiler keeps it a
        # presumption source of p3's lambda edge - not a discharge - so the
        # labelling still sees the loop P[t] <- Q[t] <- P->false[t] <- P[c].
        host = eta("s2", "P", Mu(ID("beta", "P"), "P",
                                 Lamda(Hyp(DI("h", "P"), "P"), Goal("1", "Q")), Laog("2", "P->Q")))
        p3 = eta("p3", "Q", Mu(ID("g", "Q"), "Q", DI("pq", "(P->false)->Q"),
                                Cons(Lamda(Hyp(DI("x", "P"), "P"),
                                           Mu(ID("_", "false"), "false", DI("x", "P"), Geled("8", "P"))),
                                     ID("g", "Q"))))
        doc = compile_document([("s2", host), ("p3", p3)], strict_names=STRICT | {"pq"})
        term = unfold(doc, (K("P"), "term"))
        # root = ATT(P!, SUP(site, s2)): inside the copy of s2 the presumed
        # refutation of P is gone, captured; the bare P! at the root is the
        # contrary's own default attacking the issue.
        root_attack = _match_scaffold(term, STRICT)
        s2_copy = _match_scaffold(root_attack[3], STRICT)[4]
        assert not _contains(s2_copy, lambda n: isinstance(n, Geled) and n.prop == "P")
        captured = []
        _contains(term, lambda n: captured.append(n) if isinstance(n, ID) and n.name == "beta" else False)
        assert captured and all(getattr(v, "captured_presumption", False) for v in captured)
        g = compile_debate(term, "u", strict_names=STRICT | {"pq"})
        by_name = {e.name: e for e in g.edges}
        assert "p3" in by_name
        lam = [e for e in g.edges if e.name.startswith("p3.")]
        assert lam and ("P", "context", "presumption") in {(g.nodes[s.key], s.side, s.kind) for s in lam[0].sources}
        assert not g.is_acyclic()


# ---------------------------------------------------------------------------
# Fixture-based: real documents
# ---------------------------------------------------------------------------

FIXTURES = {
    # script: (atomic argument names, issue argument)
    "tests/rationality/contested.fspy": (["pArg", "qAtt"], "pArg"),
    "tests/rationality/two_witnesses.fspy": (["pArg", "yQ", "sChal"], "pArg"),
    "tests/rationality/self_attack_lk.fspy": (["SelfAttack"], "SelfAttack"),
    "tests/rationality/cyclic_undercut.fspy": (["argA", "argB", "cQ", "cR"], "argA"),
    "tests/rationality/even_loop_lk.fspy": (["Pdefault", "Qdefault"], "Pdefault"),
    "tests/peirces_law.fspy": (["p1", "s1", "p2", "s2", "p3", "s3"], "p1"),
}


@pytest.fixture
def prover():
    # one prover per test: declarations do not survive a change of script
    p = setup_prover()
    yield p
    p.close()


def load(prover, script):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, script, strict=True, stop_on_error=True, isolate=False)
    names, issue_name = FIXTURES[script]
    sn, sk = list(prover.declarations.keys()), declaration_kinds(prover.declarations)
    doc = compile_document([(n, prover.get_argument(n).body) for n in names], strict_names=sn, strict_kinds=sk)
    arg = prover.get_argument(issue_name)
    issue = (K(arg.conclusion), "context" if arg.is_anti else "term")
    assert prover.logic == "lk", "debates are classical; fixtures for unfolding are lk"
    return doc, issue, sn, sk


def unfold_checked(prover, doc, statement):
    """Unfold and replay through Fellowship - the type oracle.  If the
    arguments type-check, so must their unfolding; a failure here is an
    unsound unfolding."""
    term = unfold(doc, statement)
    typecheck(prover, term, "check", doc.nodes[statement[0]], statement[1] == "context")
    return term


@pytest.mark.parametrize("script", list(FIXTURES))
def test_unfolded_terms_typecheck(prover, script):
    """Every statement of every fixture document unfolds to a term
    Fellowship accepts and reconstructs."""
    doc, issue, sn, sk = load(prover, script)
    for statement in sorted(set(doc.statements())):
        unfold_checked(prover, doc, statement)


@pytest.mark.parametrize("script", list(FIXTURES))
def test_unfolding_terminates_and_compiles(prover, script):
    doc, issue, sn, sk = load(prover, script)
    term = unfold_checked(prover, doc, issue)                        # T6, typed
    g = compile_debate(term, "u", strict_names=sn, strict_kinds=sk)
    assert g.edges or g.defaults


@pytest.mark.parametrize("script", list(FIXTURES))
def test_adequacy_restated(prover, script):
    """IN -> value, OUT -> not a value, for every issue of every fixture,
    with unfolding as the bridge (B4)."""
    doc, issue, sn, sk = load(prover, script)
    for statement in sorted(set(doc.statements())):
        if statement[1] == "context":
            continue                          # term-side issues suffice here
        term = unfold(doc, statement)         # typed in test_unfolded_terms_typecheck
        nf, cls, sigma, g = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk,
                                            mode="skeptical", semantics="grounded")
        label = sigma.get(statement)
        if label == "IN":
            assert cls == "value", (script, statement, cls)
        elif label == "OUT":
            assert cls != "value", (script, statement, cls)


def test_peirce_is_a_theorem(prover):
    doc, issue, sn, sk = load(prover, "tests/peirces_law.fspy")
    assert not doc.is_acyclic()                                     # the document shows the loop
    term = unfold_checked(prover, doc, issue)
    # The framework of the term: the trap as a cycle through captured
    # obligations, the thesis OUT like everything else.
    framework = compile_debate(term, "u", strict_names=sn, strict_kinds=sk)
    assert not framework.is_acyclic()
    assert grounded_labels(framework)[issue] == "OUT"
    # Strictness on the term: every scaffold decided, the whole term closed,
    # one strict edge for the thesis - and nothing else established.
    _, strict = strict_resolve(term, sn)
    assert [(g_key, side) for e in strict for (g_key, side) in [(e.target_key, e.target_side)]] == [issue]
    g = compile_issue(term, "u", strict_names=sn, strict_kinds=sk)
    assert grounded_labels(g)[issue] == "IN"
    assert all(v != "IN" for s, v in grounded_labels(g).items() if s != issue)
    nf, cls, _, _ = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk)
    assert cls == "value"
    # the classical proof: lambda f. mu a.< f || (lambda h. mu _.< h || a >) * a >
    P, Q, T, I = "P", "Q", "((P->Q)->P)->P", "(P->Q)->P"
    classical_proof = eta("t", T, Lamda(Hyp(DI("f", I), I),
                                        Mu(ID("a", P), P, DI("f", I),
                                           Cons(Lamda(Hyp(DI("h", P), P), Mu(ID("_", Q), Q, DI("h", P), ID("a", P))),
                                                ID("a", P)))))
    assert alpha_equal(nf, classical_proof)


def test_even_loop_is_symmetric(prover):
    # The cycle representation: from either side, the other argument's
    # presumed refutation is captured by this side's continuation - one
    # copy of each argument, no bare cut - and the issue graph is the loop
    # itself.  Neither model is accepted under grounded, each is under
    # credulous preferred from its own entry point.
    doc, issue, sn, sk = load(prover, "tests/rationality/even_loop_lk.fspy")
    for name in ("Pdefault", "Qdefault"):
        arg = prover.get_argument(name)
        term = unfold_checked(prover, doc, (K(arg.conclusion), "term"))
        rules = []
        _contains(term, lambda n: rules.append(n) if isinstance(n, DI) and n.name in ("pRule", "qRule") else False)
        assert sorted(r.name for r in rules) == ["pRule", "qRule"]        # each argument once
        assert _contains(term, lambda n: getattr(n, "captured_presumption", False))
        _, cred_grounded, _, _ = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk,
                                                 mode="credulous", semantics="grounded")
        assert cred_grounded != "value"
        g = compile_debate(term, "u", strict_names=sn, strict_kinds=sk)
        assert set(grounded_labels(g).values()) == {"UNDEC"}
        _, skeptical, _, _ = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk, mode="skeptical")
        _, credulous, _, _ = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk, mode="credulous")
        assert skeptical != "value" and credulous == "value"


def test_self_attack_is_not_a_value(prover):
    doc, issue, sn, sk = load(prover, "tests/rationality/self_attack_lk.fspy")
    term = unfold_checked(prover, doc, issue)
    for mode in ("skeptical", "credulous"):
        _, cls, _, _ = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk, mode=mode)
        assert cls != "value"


def test_cyclic_undercut_issue_is_its_presumption(prover):
    # A is IN by cR's presumption; unfolding puts that presumption at the
    # root, so the value is the presumption of A (gap 2 of D0 closed).
    doc, issue, sn, sk = load(prover, "tests/rationality/cyclic_undercut.fspy")
    term = unfold_checked(prover, doc, issue)
    nf, cls, sigma, _ = evaluate_debate(term, "u", strict_names=sn, strict_kinds=sk, mode="credulous")
    assert sigma[issue] == "IN" and cls == "value"
    assert _contains(nf, lambda n: isinstance(n, Deleg) and n.prop == "A")
    assert not _contains(nf, lambda n: isinstance(n, DI) and n.name == "qa")   # argA dropped
