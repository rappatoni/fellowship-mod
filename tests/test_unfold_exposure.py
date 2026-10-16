"""The exposure of horns in the unfolded term (core/dc/unfold.py;
tasks.org, aida-rational-closure step 2, 2026-10-16).

Every horn of an argument - a declared axiom it applies, a lemma it
proves, a lambda - is an exposed node: where it stands in the body, the
debate about its statement is unfolded, with the node's own derivation
in the stack.  The fixtures (tests/exposure/, and three older ones) pin
the unfolded terms as the compact scaffold printer shows them, the
verdicts, that every term replays through Fellowship, and that the three
``subarguments`` settings agree on classes (the differential test of
aida-circular-support-design).
"""

import logging
import warnings
from pathlib import Path

import pytest

from core.dc.typecheck import typecheck
from core.dc.unfold import argument_edge
from pres.gen import pres_scaffolds, pres_str
from wrap.cli import setup_prover
from wrap.service import Service


@pytest.fixture(autouse=True)
def _unfolded_route(monkeypatch):
    """These tests are about the unfolded term, and pin its verdicts; the
    shared route is compared with it where a test says so."""
    monkeypatch.setenv("FSP_PIPELINE", "unfolded")

TESTS = Path(__file__).parent

FIXTURES = [
    "minicourse/lesson8a_attackrule.fspy",    # an axiom exposed, attacked, strict
    "exposure/lemma_captured.fspy",           # a lemma exposed; bound where it stands
    "exposure/site_variant.fspy",             # the same with a site: nothing new
    "exposure/two_sites.fspy",                # an unengaged axiom stays as written
    "exposure/curried_cell.fspy",             # a spine cell is not exposed
    "minicourse/lesson8a_cycle.fspy",         # capture by b, unchanged
    "entrypoints.fspy",                       # primacy extends to the axioms
    "rationality/self_attack_lk.fspy",        # a lambda edge stacked at its issue
]

SETTINGS = ("default", "share", "preserve")


def session(script, subarguments="default"):
    prover = setup_prover()
    svc = Service.of(prover)
    svc.set_subarguments(subarguments)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        report = svc.check((TESTS / script).read_text())
    refused = [(e.span.line, e.message) for e in report.entries if e.status == "refused"]
    assert refused == [], refused
    return prover, svc


def targets(prover):
    """Every argument with an edge and every statement, as target texts."""
    found = [name for name in prover.arguments if argument_edge(prover.graph, name) is not None]
    for key, side in prover.graph.statements():
        prop = prover.graph.nodes.get(key, key)
        found.append(f"issue :{prop}" if side == "term" else f"issue {prop}:")
    return found


def term_of(svc, text):
    return svc.issue_term(svc.target(text), want_term=True)


def replay(prover, found, name="exposure"):
    """The unfolded term through Fellowship's type check."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        if found.argument is not None:
            typecheck(prover, found.term, name, found.argument.conclusion, found.argument.is_anti)
        else:
            key, side = found.issue
            typecheck(prover, found.term, name, prover.graph.nodes[key], side == "context")


def verdict(svc, text):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        ev = svc.evaluate(svc.target(text))
    return ev.nf_class, pres_str(ev.normal_form)


# -- the shapes -----------------------------------------------------------------

KILLR = "μ'killR:R.<killR||μ'x:R.<nR||μ'H1:~R.<H1||x*_F_>>>"
R_DEBATE = f"ATT[alt4,b4](!u3:R, SUP~[alt5,b5](alt4, {KILLR}))"


class TestAnAxiomIsExposed:
    """lesson8a_attackrule.fspy: attackrule refutes A->B, the axiom pro
    applies."""

    def test_the_debate_about_the_axiom_stands_where_the_axiom_stood(self):
        prover, svc = session("minicourse/lesson8a_attackrule.fspy")
        try:
            term = pres_scaffolds(term_of(svc, "pro").term)
            # r1's statement A->B: an obligation root (Q3), r1 on top (pro's
            # primacy extends to it), attackrule in the attacker wing
            assert term.startswith(
                "SUP[alt1,b1](?u1:B, μpro:B.<μth:B.<ATT[alt3,b3](SUP[alt2,b2](?u2:A->B, r1), "
                "SUP~[alt4,b4](alt3, μ'attackrule:A->B.<attackrule||μ'th_2:A->B.<r3||!u3:D*"
                "μ'H1:~(A->B).<H1||th_2*_F_>>>))||ATT[alt5,b5](!u4:A, ")
            assert verdict(svc, "pro") == ("value", "μpro:B.<r1:A->B||!IN:A*pro:B>")
            # attackrule loses to the strict axiom in phase 1
            assert verdict(svc, "attackrule") == ("exception", "μ'alt2:A->B.<r1:A->B||OUT:A->B?>")
            assert pres_scaffolds(term_of(svc, "issue :A->B").term) == (
                "ATT[alt2,b2](SUP[alt1,b1](?u1:A->B, r1), SUP~[alt3,b3](alt2, μ'attackrule:A->B."
                "<attackrule||μ'th:A->B.<r3||!u2:D*μ'H1:~(A->B).<H1||th*_F_>>>))")
            assert verdict(svc, "issue :A->B") == ("value", "r1:A->B")
            # the axioms nobody engages are stacked at their own issue only
            assert pres_scaffolds(term_of(svc, "issue :~C").term) == "SUP[alt1,b1](?u1:~C, nc)"
        finally:
            prover.close()


class TestALemmaIsExposed:
    """lemma_captured.fspy: argQ proves P on the way to Q; argQ.μ1 is a
    second derivation of P beside argP."""

    def test_the_document(self):
        prover, svc = session("exposure/lemma_captured.fspy")
        try:
            g = prover.graph
            assert [(e.name, g._show(e.target_key, e.target_side), e.strict) for e in g.edges] == [
                ("qp", "Q->P[t]", True), ("argP", "P[t]", False), ("pq", "P->Q[t]", True),
                ("rp", "R->P[t]", True), ("argQ.μ1", "P[t]", False), ("argQ", "Q[t]", False),
                ("nR", "~R[t]", True), ("killR", "R[c]", True)]
            labels = svc.label(svc.target("issue :P")).labellings[0]
            assert {g._show(*k): v for k, v in labels.items()} == {
                "P->Q[t]": "IN", "Q->P[t]": "IN", "R->P[t]": "IN", "~R[t]": "IN",
                "P[t]": "IN", "Q[t]": "IN", "R[c]": "IN", "R[t]": "OUT"}
        finally:
            prover.close()

    def test_the_lemma_is_a_stack_element_of_its_statement(self):
        prover, svc = session("exposure/lemma_captured.fspy")
        try:
            # the canonical term of P: argP, then the lemma (registration
            # order); inside argQ the lemma's P is bound by the root's b1
            assert pres_scaffolds(term_of(svc, "issue :P").term) == (
                "SUP[alt1,b1](?u1:P, SUP[alt2,b2](μargP:P.<μth:P.<qp||SUP[alt3,b3](!u2:Q, "
                "μargQ:Q.<μth_2:Q.<pq||b1*th_2>||argQ>)*th>||argP>, "
                f"μth2:P.<rp||{R_DEBATE}*th2>))")
            # judged by what it stands for, argQ's P (the root, inside P's
            # own debate) is no evidence: P from the presumed Q
            assert verdict(svc, "issue :P") == ("value", "μargP:P.<qp:Q->P||!IN:Q*argP:P>")
            # unfolded for argQ: the lemma is in argQ's body, at its own
            # statement's debate, with an obligation root and argP below it
            assert pres_scaffolds(term_of(svc, "argQ").term) == (
                "SUP[alt1,b1](!u1:Q, μargQ:Q.<μth:Q.<pq||SUP[alt2,b2](?u2:P, SUP[alt3,b3]("
                "μargP:P.<μth_2:P.<qp||b1*th_2>||argP>, "
                f"μth2:P.<rp||{R_DEBATE}*th2>))*th>||argQ>)")
            assert verdict(svc, "argQ") == ("value", "μargQ:Q.<pq:P->Q||μb1:P.<qp:Q->P||!IN:Q*b1:P>*argQ:Q>")
        finally:
            prover.close()

    def test_bound_where_it_stands_the_lemma_is_captured(self):
        """Unfolded for argP: argQ's P is argP's own statement, bound by the
        scaffold's b2, and argQ has no primacy, so the lemma is the captured
        variable alone (decision 2); the lemma itself stands below argP."""
        prover, svc = session("exposure/lemma_captured.fspy")
        try:
            assert pres_scaffolds(term_of(svc, "argP").term) == (
                f"SUP[alt1,b1](?u1:P, SUP[alt2,b2](μth2:P.<rp||{R_DEBATE}*th2>, "
                "μargP:P.<μth:P.<qp||SUP[alt3,b3](!u2:Q, μargQ:Q.<μth_2:Q.<pq||b2*th_2>||argQ>)"
                "*th>||argP>))")
            # b2 is the stack below argP - the refuted lemma - so argQ
            # stands OUT here and Q's scaffold keeps its presumption
            # (tasks.org, aida-judge-by-what-it-stands-for; until 2026-10-16
            # an exception: argQ was kept by P's label)
            assert verdict(svc, "argP") == ("value", "μargP:P.<qp:Q->P||!IN:Q*argP:P>")
        finally:
            prover.close()

    def test_preserve_keeps_the_lemma_on_top_of_the_captured_variable(self):
        prover, svc = session("exposure/lemma_captured.fspy", "preserve")
        try:
            term = pres_scaffolds(term_of(svc, "argP").term)
            assert "SUP[alt4,b4](b2, μth2:P.<rp||ATT[alt5,b5](!u3:R, " in term
            assert verdict(svc, "argP") == ("value", "μargP:P.<qp:Q->P||!IN:Q*argP:P>")
        finally:
            prover.close()


class TestTheSiteVariant:
    def test_a_site_and_a_lemma_unfold_alike(self):
        """site_variant.fspy: argQ demands P, argP2 derives P from the
        refuted R.  The terms are those of lemma_captured.fspy with argP2's
        wrapper around the lemma's body, and the verdicts the same."""
        lemma = session("exposure/lemma_captured.fspy")
        site = session("exposure/site_variant.fspy")
        try:
            for text in ("argP", "argQ", "issue :P", "issue :Q"):
                with_site = pres_scaffolds(term_of(site[1], text).term)
                unwrapped = with_site.replace("μargP2:P.<", "").replace("||argP2>", "")
                assert unwrapped == pres_scaffolds(term_of(lemma[1], text).term), text
                assert verdict(site[1], text)[0] == verdict(lemma[1], text)[0], text
            assert verdict(site[1], "argP") == ("value", "μargP:P.<qp:Q->P||!IN:Q*argP:P>")
        finally:
            lemma[0].close()
            site[0].close()


class TestNothingNewWhereNobodyEngages:
    def test_an_unengaged_axiom_stays_as_written(self):
        prover, svc = session("exposure/two_sites.fspy")
        try:
            assert pres_scaffolds(term_of(svc, "two").term) == (
                "SUP[alt1,b1](?u1:B, μtwo:B.<μth:B.<r||SUP[alt2,b2](!u2:A, μaArg:A.<μth_2:A.<rc||"
                "!u3:C*th_2>||aArg>)*SUP[alt3,b3](!u4:A, μaArg_2:A.<μth_3:A.<rc||!u5:C*th_3>||"
                "aArg_2>)*th>||two>)")
            assert verdict(svc, "two") == (
                "value", "μtwo:B.<r:A->A->B||μb1:A.<rc:C->A||!IN:C*b1:A>*μb3:A.<rc:C->A||!IN:C*b3:A>*two:B>")
        finally:
            prover.close()

    def test_a_spine_cell_is_not_exposed(self):
        """curried_cell.fspy: cArg's B->C is a cell carrying its own
        continuation; the counterargument test against B->C is nowhere in
        cArg's term."""
        prover, svc = session("exposure/curried_cell.fspy")
        try:
            term = pres_scaffolds(term_of(svc, "cArg").term)
            assert term == ("SUP[alt1,b1](?u1:C, μcArg:C.<μr:C.<abc||SUP[alt2,b2](?u2:A, "
                            "μaArg:A.<axA||aArg>)*!u3:B*r>||cArg>)")
            assert "test" not in term
            assert verdict(svc, "cArg") == ("value", "μcArg:C.<abc:A->B->C||μb1:A.<axA:A||b1:A>*!IN:B*cArg:C>")
            assert [e.name for e in prover.graph.edges] == ["abc", "cArg", "aArg"]
        finally:
            prover.close()

    def test_the_cycle_fixture_is_unchanged(self):
        prover, svc = session("minicourse/lesson8a_cycle.fspy")
        try:
            assert pres_scaffolds(term_of(svc, "pro").term) == (
                "SUP[alt1,b1](?u1:B, μpro:B.<μth:B.<r1||SUP[alt2,b2](?u2:A, μback:A.<μth_2:A.<r3||"
                "b1*th_2>||back>)*th>||pro>)")
            assert verdict(svc, "pro") == ("open", "?UNDEC:B")
        finally:
            prover.close()


class TestPrimacyAndLambdas:
    def test_primacy_extends_to_the_entrypoints_axioms(self):
        """entrypoints.fspy: c1 and c2 both refute F from an axiom; inside
        c1's term its own axiom n1 is on top of ~F's stack, inside c2's n2."""
        prover, svc = session("entrypoints.fspy")
        try:
            c1 = pres_scaffolds(term_of(svc, "c1").term)
            assert "μ'c1:F.<c1||μ'x:F.<SUP[alt3,b3](?u2:~F, SUP[alt4,b4](n2, n1))" in c1
            assert "μ'c2:F.<c2||μ'x_2:F.<SUP[alt5,b5](?u3:~F, SUP[alt6,b6](n1, n2))" in c1
            assert verdict(svc, "c1") == ("value", "μ'c1:F.<n1:~F||c1:F*_F_>")
            assert verdict(svc, "c2") == ("value", "μ'c2:F.<n2:~F||c2:F*_F_>")
            # the canonical issue: registration order, nobody on top
            assert pres_scaffolds(term_of(svc, "issue :~F").term) == "SUP[alt1,b1](?u1:~F, SUP[alt2,b2](n1, n2))"
            assert verdict(svc, "issue :~F") == ("value", "n2:~F")
        finally:
            prover.close()

    def test_a_lambda_edge_is_stacked_at_its_own_issue(self):
        """self_attack_lk.fspy: P->false is derived only by SelfAttack's
        lambda.  Its issue term stacks that edge (until 2026-10-16 a bare
        site), and SelfAttack inside it is captured by the root's b1."""
        prover, svc = session("rationality/self_attack_lk.fspy")
        try:
            assert pres_scaffolds(term_of(svc, "issue :P->false").term) == (
                "SUP[alt1,b1](?u1:P->false, λh:P.μalpha:false.<h||ATT~[alt2,b2](P:!u2, "
                "SUP[alt3,b3](alt2, μSelfAttack:P.<μrule:P.<pRule||b1*rule>||SelfAttack>))>)")
            assert verdict(svc, "issue :P->false") == ("open", "?UNDEC:P->false")
            assert verdict(svc, "SelfAttack")[0] == "value"
        finally:
            prover.close()


# -- every term replays; the settings agree -------------------------------------

@pytest.mark.parametrize("script", FIXTURES)
def test_every_unfolded_term_replays_through_fellowship(script):
    for setting in SETTINGS:
        prover, svc = session(script, setting)
        try:
            for text in targets(prover):
                replay(prover, term_of(svc, text), name="replay")
        finally:
            prover.close()


#: Where the settings disagree on labels: (fixture, target, setting).  The
#: preserved copy of SelfAttack's lambda is closed by the captured
#: continuation ``rule``, and the strict phase counts it strict (tasks.org,
#: aida-discuss-capture-source-vs-strict: a captured site is no assumption
#: for the strict phase), which adds the strict edge SelfAttack* and turns
#: P->false[t] from UNDEC to OUT.  The class is "open" either way.
LABELS_DIFFER = {("rationality/self_attack_lk.fspy", "issue :P->false", "preserve")}


@pytest.mark.parametrize("script", FIXTURES)
def test_the_settings_agree_on_classes_and_labels(script):
    """The differential test of aida-circular-support-design: default,
    share and preserve give the same class for every argument and issue,
    and the same labels but for LABELS_DIFFER."""
    classes, labels = {}, {}
    for setting in SETTINGS:
        prover, svc = session(script, setting)
        try:
            for text in targets(prover):
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore")
                    classes[(text, setting)] = svc.evaluate(svc.target(text)).nf_class
                    labels[(text, setting)] = svc.label(svc.target(text)).labellings[0]
            names = targets(prover)
        finally:
            prover.close()
    for text in names:
        for setting in SETTINGS[1:]:
            assert classes[(text, setting)] == classes[(text, "default")], (text, setting)
            if (script, text, setting) in LABELS_DIFFER:
                assert labels[(text, setting)] != labels[(text, "default")], (text, setting)
            else:
                assert labels[(text, setting)] == labels[(text, "default")], (text, setting)


def test_the_setting_is_a_session_setting():
    from wrap.service import InvalidRequest
    from wrap.syntax import parse
    assert parse("subarguments preserve").args == {"mode": "preserve"}
    assert parse("subarguments share").kind == "subarguments"
    prover, svc = session("exposure/lemma_captured.fspy")
    try:
        assert prover.subarguments == "default"
        before = svc.session.revision
        svc.set_subarguments("share")
        assert prover.subarguments == "share" and svc.session.revision > before
        with pytest.raises(InvalidRequest):
            svc.set_subarguments("all")
    finally:
        prover.close()


# -- judged by what it stands for (tasks.org, aida-judge-by-what-it-stands-for) --

PRESUMED_Q = "μargP:P.<qp:Q->P||!IN:Q*argP:P>"


def evaluated(svc, text, mode="skeptical"):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        ev = svc.evaluate(svc.target(text), mode=mode)
    return ev.nf_class, pres_str(ev.normal_form)


class TestJudgedByWhatItStandsFor:
    """Sigma judges a captured variable by what it stands for: =b2= is the
    stack below the top, a captured obligation root of the debate being
    decided is unmet, a nested sub-debate counts by how it comes out in
    the term.  The discharge is unchanged: a root that survives is named
    by its statement's label."""

    @pytest.mark.parametrize("script", ["exposure/site_variant.fspy", "exposure/lemma_captured.fspy"])
    def test_the_argument_and_the_issue_give_p_from_the_presumed_q(self, script):
        prover, svc = session(script)
        try:
            for text in ("argP", "issue :P"):
                assert evaluated(svc, text) == ("value", PRESUMED_Q), text
        finally:
            prover.close()

    def test_an_unfounded_loop_is_the_bare_delegation(self):
        """site_obligation.fspy: argP demands Q; credulously P is IN only
        through the loop P from Q from P."""
        prover, svc = session("exposure/site_obligation.fspy")
        try:
            for text in ("argP", "issue :P"):
                assert evaluated(svc, text, "credulous") == ("value", "!IN:P"), text
                assert evaluated(svc, text) == ("open", "?UNDEC:P"), text
        finally:
            prover.close()

    def test_the_lesson_8a_loop_is_the_bare_delegation(self):
        prover, svc = session("minicourse/lesson8a_cycle.fspy")
        try:
            for text in ("pro", "issue :B"):
                assert evaluated(svc, text, "credulous") == ("value", "!IN:B"), text
                assert evaluated(svc, text) == ("open", "?UNDEC:B"), text      # skeptical unchanged
        finally:
            prover.close()

    @pytest.mark.parametrize("script", ["exposure/site_obligation.fspy", "exposure/site_variant.fspy",
                                        "exposure/lemma_captured.fspy", "minicourse/lesson8a_cycle.fspy",
                                        "circular_supporter.fspy"])
    def test_the_shared_route_gives_the_same_classes(self, script):
        """The shared route reads a stub by the standing of its resolved
        instance (core/comp/standing.py): every issue gets the class the
        unfolded route gives, in both modes.  (The normal forms may differ
        by the shapes the routes build, as before.)"""
        classes = {}
        for pipe in ("unfolded", "shared"):
            prover, svc = session(script)
            try:
                svc.set_pipeline(pipe)
                for text in targets(prover):
                    if text.startswith("issue"):
                        for mode in ("skeptical", "credulous"):
                            classes.setdefault((text, mode), {})[pipe] = evaluated(svc, text, mode)[0]
            finally:
                prover.close()
        assert all(v["unfolded"] == v["shared"] for v in classes.values()), classes


# -- an exposed lemma stays in the scope of its hypotheses ------------------------

def closed(prover, term):
    """The variables free in ``term`` that no declaration accounts for."""
    from core.dc.discipline import free_variables
    return {name for _kind, name in free_variables(term)
            if name not in prover.declarations and name not in ("_F_", "_T_")}


class TestALemmaStaysInItsScope:
    """tasks.org, aida-lemma-exposure-scope (the literature evaluation's bug
    2).  A lemma that uses a hypothesis of its own argument (``h`` of
    ``λh:A``) is an edge whose source at A is captured.  Stacked in the
    debate about its statement outside that lambda, the hypothesis is
    unfolded as its statement there - an obligation on A - instead of
    being copied free.  Proof by cases (eval/literature/t_cases.fspy) and
    input/output logic's reasoning by cases (eval/literature2/io_cases.fspy)."""

    @pytest.mark.parametrize("script, target", [("exposure/lemma_scope_cases.fspy", "p1"),
                                                ("exposure/lemma_scope_io.fspy", "claim")])
    def test_the_term_is_closed_replays_and_agrees_with_the_label(self, script, target):
        prover, svc = session(script)
        try:
            for text in targets(prover):
                found = term_of(svc, text)
                assert closed(prover, found.term) == set(), text
                replay(prover, found, name="scope")
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                issue = svc.issue_term(svc.target(target), want_term=False).issue
                for mode in ("skeptical", "credulous"):
                    result = svc.evaluate(svc.target(target), mode=mode)
                    assert result.sigma.get(issue) == "OUT"
                    assert result.nf_class == "open", mode
        finally:
            prover.close()

    def test_outside_the_lambda_the_hypothesis_is_an_obligation(self):
        prover, svc = session("exposure/lemma_scope_cases.fspy")
        try:
            shown = pres_scaffolds(term_of(svc, "p1").term)
            assert "μy:B.<f||?u4:A*y>" in shown          # was <f||h*y>, h free
            assert "λh:A.μk:false.<b2||x>" in shown       # inside, captured as before
        finally:
            prover.close()


@pytest.mark.parametrize("script", sorted({*FIXTURES, "exposure/lemma_scope_cases.fspy",
                                           "exposure/lemma_scope_io.fspy",
                                           "minicourse/lesson9_evaluation.fspy",
                                           "minicourse/lesson15_capture.fspy",
                                           "circular_supporter.fspy"}))
def test_every_unfolded_term_is_closed(script):
    """The premise of the strict phase (tasks.org, the decision of
    2026-09-25): the unfolded term is closed as a whole."""
    prover, svc = session(script)
    try:
        for text in targets(prover):
            assert closed(prover, term_of(svc, text).term) == set(), text
    finally:
        prover.close()
