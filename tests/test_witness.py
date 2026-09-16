"""A3: the semantics parameter and the witness-labelling discipline
(tasks.org, aida-credulous-witness-labelling).

Includes the counterexample the task asked for: a debate on which the
earlier per-scaffold credulous policy was unsound in the acyclic fragment.
"""

import pytest

from core.ac.ast import Mu, Mutilde, Lamda, Hyp, Cons, Goal, Deleg, Geled, ID, DI
from core.comp.adf_label import (
    labellings, intersection_labelling, grounded_labels,
    grounded_labels_via_oracle, graph_to_adf, SEMANTICS,
)
from core.comp.oracle import complete_interpretations, two_valued_models
from core.comp.evaluate import (
    evaluate_debate, witness_labelling, issue_of, resolve_scaffolds,
    EvaluationRefused,
)
from core.dc.debate_graph import compile_debate, canonical_prop

K = canonical_prop
P, Q, S = K("P"), K("Q"), K("S")
STRICT = {"pRule", "aRule", "nsb"}


def eta(n, p, i):
    return Mu(ID(n, p), p, i, ID(n, p))


def parg(site):
    return eta("pArg", "P", Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                                 Cons(site, ID("rule", "P"))))


def t_sup(p, o, s):
    return Mu(ID("alt", p), p, Mu(ID("_", p), p, o, ID("alt", p)),
              Mutilde(DI("_", p), p, s, ID("alt", p)))


def t_att(p, o, sc):
    return Mu(ID("alt", p), p, Mu(ID("_", p), p, o, ID("alt", p)),
              Mutilde(DI("_", p), p, Goal("g", p), sc))


def contested():
    """The contested fixture: presumed Q attacked by a presumed challenge."""
    return parg(t_att("Q", Deleg("1", "Q"), Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))))


def two_supporters_on_contrary_presumptions():
    """P from A and B; A supported by yA resting on !S (term side); B
    supported by yB resting on a delegated REFUTATION of S (context side),
    via  nsb : (S->false)->B  applied to  lam s. mu f:false.< s || S! >.

    Graph: A[t] <- S[t]:pres, B[t] <- S[c]:pres, P[t] <- A[t], B[t].
    S is contested, so everything grounds UNDEC.
    """
    yA = eta("yA", "A", Mu(ID("r", "A"), "A", DI("aRule", "S->A"),
                            Cons(Deleg("dS", "S"), ID("r", "A"))))
    refute_S = Lamda(Hyp(DI("s", "S"), "S"),
                     Mu(ID("f", "false"), "false", DI("s", "S"), Geled("dSc", "S")))
    yB = eta("yB", "B", Mu(ID("r2", "B"), "B", DI("nsb", "(S->false)->B"),
                            Cons(refute_S, ID("r2", "B"))))
    host = eta("pArg", "P", Mu(ID("rule", "P"), "P", DI("pRule", "A->B->P"),
                                Cons(t_sup("A", Goal("a", "A"), yA),
                                     Cons(t_sup("B", Goal("b", "B"), yB), ID("rule", "P")))))
    return host


class TestSemanticsParameter:
    def test_grounded_is_singleton(self):
        g = compile_debate(contested(), "d", strict_names=STRICT)
        assert len(labellings(g, "grounded")) == 1
        assert labellings(g, "grounded")[0] == grounded_labels(g)

    def test_complete_agrees_with_oracle(self):
        g = compile_debate(contested(), "d", strict_names=STRICT)
        ours = {frozenset(l.items()) for l in labellings(g, "complete")}
        from core.comp.adf_label import _to_labels
        theirs = {frozenset(_to_labels(v).items()) for v in complete_interpretations(graph_to_adf(g))}
        assert ours == theirs

    def test_contested_has_two_stable_and_two_preferred(self):
        g = compile_debate(contested(), "d", strict_names=STRICT)
        stable = labellings(g, "stable")
        assert len(stable) == 2
        assert all("UNDEC" not in l.values() for l in stable)
        assert {frozenset(l.items()) for l in labellings(g, "preferred")} == {frozenset(l.items()) for l in stable}
        assert {l[(P, "term")] for l in stable} == {"IN", "OUT"}

    def test_stable_are_two_valued_models(self):
        g = compile_debate(contested(), "d", strict_names=STRICT)
        from core.comp.adf_label import _to_labels
        models = {frozenset(_to_labels(v).items()) for v in two_valued_models(graph_to_adf(g))}
        for l in labellings(g, "stable"):
            assert frozenset(l.items()) in models

    def test_intersection(self):
        assert intersection_labelling([{("a", "term"): "IN"}, {("a", "term"): "OUT"}]) == {("a", "term"): "UNDEC"}
        assert intersection_labelling([{("a", "term"): "IN"}, {("a", "term"): "IN"}]) == {("a", "term"): "IN"}

    def test_unknown_semantics_refused(self):
        g = compile_debate(contested(), "d", strict_names=STRICT)
        with pytest.raises(ValueError):
            labellings(g, "admissible")


class TestWitnessLabelling:
    def test_credulous_picks_a_witness_with_the_issue_in(self):
        body = contested()
        g = compile_debate(body, "d", strict_names=STRICT)
        sigma, tiebreak = witness_labelling(g, issue_of(body), "credulous", "preferred")
        assert sigma[(P, "term")] == "IN"
        assert "UNDEC" not in sigma.values()
        assert tiebreak == "credulous"

    def test_skeptical_is_the_intersection(self):
        body = contested()
        g = compile_debate(body, "d", strict_names=STRICT)
        sigma, tiebreak = witness_labelling(g, issue_of(body), "skeptical", "preferred")
        assert sigma[(P, "term")] == "UNDEC" and sigma[(Q, "term")] == "UNDEC"
        assert tiebreak == "skeptical"

    def test_modes_still_diverge_on_the_contested_fixture(self):
        body = contested()
        _, cred, _, _ = evaluate_debate(body, "d", strict_names=STRICT, mode="credulous")
        _, skep, _, _ = evaluate_debate(body, "d", strict_names=STRICT, mode="skeptical")
        assert cred == "value" and skep != "value"

    def test_two_valued_witness_needs_no_tiebreak(self):
        """Under stable/preferred, every scaffold the resolver consults is
        decided by sigma: the labels evaluation selects are jointly realised
        by that one labelling."""
        body = contested()
        g = compile_debate(body, "d", strict_names=STRICT)
        sigma, tiebreak = witness_labelling(g, issue_of(body), "credulous", "stable")
        trace = []
        resolve_scaffolds(body, sigma, tiebreak, strict_names=STRICT, trace=trace)
        assert trace and all(label != "UNDEC" for _, label in trace)

    def test_grounded_semantics_credulous_falls_back(self):
        """Under grounded there is one labelling; if it leaves the issue
        UNDEC nothing makes it IN, so credulous resolves skeptically."""
        body = contested()
        g = compile_debate(body, "d", strict_names=STRICT)
        sigma, tiebreak = witness_labelling(g, issue_of(body), "credulous", "grounded")
        assert sigma == grounded_labels(g) and tiebreak == "skeptical"


class TestLocalPolicyWasUnsound:
    """The counterexample the task asked for.  Two supporters resting on
    contrary presumptions of S.  The old policy (grounded sigma, credulous
    tiebreak at each scaffold independently) keeps both and returns a
    value - but no labelling has S[t] and S[c] both IN, so the issue P is
    not credulously acceptable under any semantics.  The per-scaffold
    policy manufactured acceptance no extension realises."""

    def test_graph_shape(self):
        g = compile_debate(two_supporters_on_contrary_presumptions(), "d", strict_names=STRICT)
        labels = grounded_labels(g)
        assert labels[(S, "term")] == "UNDEC" and labels[(S, "context")] == "UNDEC"
        assert labels[(P, "term")] == "UNDEC"
        assert grounded_labels_via_oracle(g) == labels

    def test_no_labelling_accepts_the_issue(self):
        g = compile_debate(two_supporters_on_contrary_presumptions(), "d", strict_names=STRICT)
        for semantics in ("complete", "preferred", "stable"):
            assert all(l[(P, "term")] != "IN" for l in labellings(g, semantics)), semantics

    def test_old_local_policy_returned_a_value(self):
        body = two_supporters_on_contrary_presumptions()
        g = compile_debate(body, "d", strict_names=STRICT)
        from core.comp.oracle_terms import normalize_strong, classify_nf
        resolved = resolve_scaffolds(body, grounded_labels(g), "credulous", strict_names=STRICT)
        assert classify_nf(normalize_strong(resolved)) == "value"      # the unsound answer

    def test_witness_discipline_rejects_it(self):
        body = two_supporters_on_contrary_presumptions()
        for semantics in ("preferred", "stable", "complete"):
            _, cls, sigma, _ = evaluate_debate(body, "d", strict_names=STRICT,
                                               mode="credulous", semantics=semantics)
            assert cls != "value", semantics
            assert sigma[(P, "term")] != "IN"


class TestIssueOf:
    def test_term_and_context_roots(self):
        assert issue_of(parg(Deleg("1", "Q"))) == (P, "term")
        ctx = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))
        assert issue_of(ctx) == (Q, "context")


class TestWitnessSelection:
    """Canonical numbering, selection by number, evaluation under all."""

    def graph(self):
        return compile_debate(contested(), "d", strict_names=STRICT)

    def test_numbering_is_canonical_and_shared_across_modes(self):
        from core.comp.adf_label import labelling_key
        for semantics in ("complete", "preferred", "stable"):
            found = labellings(self.graph(), semantics)
            assert [labelling_key(l) for l in found] == sorted(labelling_key(l) for l in found)
        # preferred and stable coincide here and so must their numbering.
        assert labellings(self.graph(), "preferred") == labellings(self.graph(), "stable")

    def test_accepting_witnesses_are_numbered_like_label(self):
        from core.comp.evaluate import accepting_witnesses
        g = self.graph()
        found = labellings(g, "preferred")
        acc = accepting_witnesses(g, (P, "term"), "preferred")
        assert [(i, s) for i, s in acc] == [(i, s) for i, s in enumerate(found, 1) if s[(P, "term")] == "IN"]
        assert len(acc) == 1

    def test_pick_by_number(self):
        g = self.graph()
        found = labellings(g, "preferred")
        accepting = next(i for i, s in enumerate(found, 1) if s[(P, "term")] == "IN")
        rejecting = next(i for i, s in enumerate(found, 1) if s[(P, "term")] != "IN")
        sigma, tiebreak = witness_labelling(g, (P, "term"), "credulous", "preferred", witness=accepting)
        assert sigma == found[accepting - 1] and tiebreak == "credulous"
        with pytest.raises(EvaluationRefused, match="does not accept"):
            witness_labelling(g, (P, "term"), "credulous", "preferred", witness=rejecting)
        with pytest.raises(EvaluationRefused, match="does not exist"):
            witness_labelling(g, (P, "term"), "credulous", "preferred", witness=len(found) + 1)
        with pytest.raises(EvaluationRefused, match="only applies to credulous"):
            witness_labelling(g, (P, "term"), "skeptical", "preferred", witness=1)

    def test_default_is_the_first_accepting_witness(self):
        g = self.graph()
        default, _ = witness_labelling(g, (P, "term"), "credulous", "preferred")
        first_number = next(i for i, s in enumerate(labellings(g, "preferred"), 1) if s[(P, "term")] == "IN")
        picked, _ = witness_labelling(g, (P, "term"), "credulous", "preferred", witness=first_number)
        assert default == picked

    def test_evaluate_debate_accepts_a_witness(self):
        body = contested()
        g = compile_debate(body, "d", strict_names=STRICT)
        n = next(i for i, s in enumerate(labellings(g, "preferred"), 1) if s[(P, "term")] == "IN")
        nf, cls, sigma, _ = evaluate_debate(body, "d", strict_names=STRICT, mode="credulous", witness=n)
        assert cls == "value" and sigma[(P, "term")] == "IN"

    def test_evaluate_witnesses_lists_every_accepting_labelling(self):
        from core.comp.evaluate import evaluate_witnesses
        from core.comp.oracle_terms import alpha_equal
        body = contested()
        results, g = evaluate_witnesses(body, "d", strict_names=STRICT, semantics="complete")
        # complete has three labellings here; exactly one accepts P.
        assert len(labellings(g, "complete")) == 3 and len(results) == 1
        number, nf, cls, sigma = results[0]
        assert cls == "value" and sigma[(P, "term")] == "IN"
        default_nf, *_ = evaluate_debate(body, "d", strict_names=STRICT, mode="credulous", semantics="complete")
        assert alpha_equal(nf, default_nf)

    def test_rejected_issue_has_no_witnesses(self):
        from core.comp.evaluate import evaluate_witnesses
        results, _ = evaluate_witnesses(two_supporters_on_contrary_presumptions(), "d",
                                        strict_names=STRICT, semantics="preferred")
        assert results == []


class TestTwoAcceptingWitnesses:
    """A debate with two accepting witnesses, found while adding witness
    selection (2026-09-16).  P from a presumed Q; Q also supported by yQ
    resting on a presumed S, which a presumed refutation of S contests.
    Preferred gives two labellings, both with P IN, differing on S.

    Witness 1 (S refuted) exposes a gap in the wing-choice rule: the
    resolver consults sigma at the scion's TARGET statement (Q[t], IN by
    presumption) and keeps the supporter yQ although yQ's own derivation
    is defeated (its source S[t] is OUT).  The normal form is then open
    at the S site while sigma says P is IN - an adequacy violation inside
    the acyclic fragment.  Pinned here; the rule change (consult the
    supporter's sources, not just its target) is tasks.org
    aida-supporter-derivation-status."""

    def body(self):
        s_contest = t_att("S", Deleg("s", "S"),
                          Mutilde(DI("x", "S"), "S", DI("x", "S"), Geled("ds", "S")))
        yQ = eta("yQ", "Q", Mu(ID("r", "Q"), "Q", DI("sq", "S->Q"),
                                Cons(s_contest, ID("r", "Q"))))
        return parg(t_sup("Q", Deleg("1", "Q"), yQ))

    STRICT = STRICT | {"sq"}

    def results(self):
        from core.comp.evaluate import evaluate_witnesses
        return evaluate_witnesses(self.body(), "d", strict_names=self.STRICT, semantics="preferred")

    def test_two_accepting_witnesses_differing_on_s(self):
        results, _ = self.results()
        assert [r[0] for r in results] == [1, 2]
        assert {r[3][(S, "term")] for r in results} == {"IN", "OUT"}
        assert all(r[3][(P, "term")] == "IN" for r in results)

    def test_witness_choice_changes_the_normal_form(self):
        from core.comp.oracle_terms import alpha_equal
        results, _ = self.results()
        assert not alpha_equal(results[0][1], results[1][1])
        for n, nf, _, _ in results:
            picked, *_ = evaluate_debate(self.body(), "d", strict_names=self.STRICT,
                                         mode="credulous", witness=n)
            assert alpha_equal(picked, nf)

    @pytest.mark.xfail(strict=True, reason=(
        "aida-supporter-derivation-status: the supporter is kept on the "
        "strength of its target's label although its own derivation is "
        "defeated under this witness; adequacy (IN -> value) fails."))
    def test_every_accepting_witness_yields_a_value(self):
        results, _ = self.results()
        assert all(cls == "value" for _, _, cls, _ in results)

    def test_the_witness_with_s_accepted_yields_a_value(self):
        results, _ = self.results()
        ok = [cls for _, _, cls, sigma in results if sigma[(S, "term")] == "IN"]
        assert ok == ["value"]
