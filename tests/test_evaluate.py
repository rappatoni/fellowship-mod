"""M6: label-guided evaluation (propositional-fragment-plan.org).

Adequacy (V3, fragment form): sigma(root) = IN implies the normal form is
a value; OUT implies it is not a value (open or exception).  The
supported case additionally checks the commuting square concretely: the
evaluator's output alpha-equals the manually instantiated ev_m(sigma)
term - built with the M0 oracle's instantiate_sites, normalized by the
same minimal normalizer.
"""

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Deleg, Geled, ID, DI
from core.comp.evaluate import evaluate_debate, EvaluationRefused
from core.comp.oracle_terms import (
    instantiate_sites, normalize_term, alpha_equal, classify_nf,
)
from core.dc.debate_graph import canonical_prop


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def parg(site):
    return eta("pArg", "P",
               Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                  Cons(site, ID("rule", "P"))))


def qarg(site):
    return eta("qArg", "Q",
               Mu(ID("rule2", "Q"), "Q", DI("qRule", "(R->false)->Q"),
                  Cons(site, ID("rule2", "Q"))))


def t_sup(prop, orig, scion):
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, scion, ID("alt", prop)))


def t_att(prop, orig, scion_ctx):
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, Goal("g2", prop), scion_ctx))


STRICT = {"pRule", "qRule"}
P, Q = canonical_prop("P"), canonical_prop("Q")


def contains(node, predicate):
    if predicate(node):
        return True
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if child is not None and contains(child, predicate):
            return True
    return False


class TestSupport:
    def body(self, leaf):
        return parg(t_sup("Q", Goal("1", "Q"), qarg(leaf)))

    @pytest.mark.parametrize("mode", ["skeptical", "credulous"])
    def test_presumption_backed_supporter_yields_value(self, mode):
        nf, cls, labels, _ = evaluate_debate(
            self.body(Deleg("2", "R->false")), "d", strict_names=STRICT, mode=mode)
        assert labels[(P, "term")] == "IN"
        assert cls == "value"
        # The supporter's presumption survives as an indeterminate.
        assert contains(nf, lambda n: isinstance(n, Deleg))
        assert not contains(nf, lambda n: isinstance(n, Goal))

    @pytest.mark.parametrize("mode", ["skeptical", "credulous"])
    def test_obligation_backed_supporter_is_discarded(self, mode):
        nf, cls, labels, _ = evaluate_debate(
            self.body(Goal("2", "R->false")), "d", strict_names=STRICT, mode=mode)
        assert labels[(P, "term")] == "OUT"
        assert cls == "open"
        # The original open Q site is kept; the supporter is gone.
        assert contains(nf, lambda n: isinstance(n, Goal) and n.prop == "Q")
        assert not contains(nf, lambda n: getattr(n, "prop", None) == "R->false")

    def test_adequacy_square_on_the_supported_case(self):
        # ev_m(sigma): the pre-graft body with the supporter substituted at
        # the supported site - the manual side of the commuting square.
        supporter = qarg(Deleg("2", "R->false"))
        composed = parg(t_sup("Q", Goal("1", "Q"), supporter))
        pre_graft = parg(Goal("1", "Q"))
        manual = instantiate_sites(pre_graft, {"1": supporter})

        nf, _, _, _ = evaluate_debate(composed, "d", strict_names=STRICT)
        assert alpha_equal(nf, normalize_term(manual, strategy="cbn"))


class TestAttack:
    def trivial_challenge(self):
        # mu'qAtt:Q.< qAtt || alt > - the rerouted bare challenge, OUT.
        return Mutilde(DI("qAtt", "Q"), "Q", DI("qAtt", "Q"), ID("alt", "Q"))

    def grounded_challenge(self):
        # mu'x:Q.< x || ?d:Q gel > - a challenge resting on a context-side
        # presumption: grounded IN.
        return Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))

    @pytest.mark.parametrize("mode", ["skeptical", "credulous"])
    def test_out_attacker_is_discarded(self, mode):
        body = parg(t_att("Q", Goal("1", "Q"), self.trivial_challenge()))
        nf, cls, labels, _ = evaluate_debate(body, "d", strict_names=STRICT, mode=mode)
        assert labels[(Q, "context")] == "OUT"
        assert cls == "open"  # the original obligation stays open
        assert contains(nf, lambda n: isinstance(n, Goal) and n.prop == "Q")

    @pytest.mark.parametrize("mode", ["skeptical", "credulous"])
    def test_in_attacker_defeats_the_site(self, mode):
        body = parg(t_att("Q", Goal("1", "Q"), self.grounded_challenge()))
        nf, cls, labels, _ = evaluate_debate(body, "d", strict_names=STRICT, mode=mode)
        assert labels[(Q, "context")] == "IN"
        assert labels[(P, "term")] == "OUT"
        assert cls != "value"  # v1 adequacy: defeated is never a value
        # The attacker's content is what remains at the site.
        assert contains(nf, lambda n: isinstance(n, Geled))


class TestContested:
    def body(self):
        # Presumption for Q attacked by a presumption-backed challenge:
        # both sides UNDEC (the M4 finding).
        challenge = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))
        return parg(t_att("Q", Deleg("1", "Q"), challenge))

    def test_mode_policy_diverges(self):
        nf_c, cls_c, labels, _ = evaluate_debate(
            self.body(), "d", strict_names=STRICT, mode="credulous")
        nf_s, cls_s, _, _ = evaluate_debate(
            self.body(), "d", strict_names=STRICT, mode="skeptical")
        assert labels[(Q, "term")] == "UNDEC"
        assert labels[(Q, "context")] == "UNDEC"
        # Credulous discards the UNDEC attacker: the presumption stands.
        assert cls_c == "value"
        assert contains(nf_c, lambda n: isinstance(n, Deleg))
        # Skeptical discards the UNDEC attacked side: not a value.
        assert cls_s != "value"
        assert contains(nf_s, lambda n: isinstance(n, Geled))


class TestRefusals:
    def test_capture_pattern_refused(self):
        # A supporter wing that reaches for the alt variable is a capture
        # pattern - outside the acyclic fragment.  The compiler refuses it
        # already (free variable in the scion edge); the evaluator's own
        # EvaluationRefused check covers direct resolve_scaffolds use.
        from core.dc.debate_graph import DebateCompileError
        scion = Mu(ID("k", "Q"), "Q", Deleg("2", "Q"), ID("alt", "Q"))
        body = parg(t_sup("Q", Goal("1", "Q"), scion))
        with pytest.raises(DebateCompileError, match="alt"):
            evaluate_debate(body, "d", strict_names=STRICT, mode="credulous")

    def test_capture_refused_by_resolver_directly(self):
        from core.comp.evaluate import resolve_scaffolds
        scion = Mu(ID("k", "Q"), "Q", Deleg("2", "Q"), ID("alt", "Q"))
        body = parg(t_sup("Q", Goal("1", "Q"), scion))
        labels = {(Q, "term"): "IN"}
        with pytest.raises(EvaluationRefused, match="capture"):
            resolve_scaffolds(body, labels, "credulous")

    def test_plain_argument_evaluates_without_scaffolds(self):
        nf, cls, labels, _ = evaluate_debate(
            parg(Deleg("1", "Q")), "d", strict_names=STRICT)
        assert labels[(P, "term")] == "IN"
        assert cls == "value"
