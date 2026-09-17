"""M6: label-guided evaluation (propositional-fragment-plan.org).

Adequacy (V3, fragment form): sigma(root) = IN implies the normal form is
a value; OUT implies it is not a value (open or exception).  The
supported case additionally checks the commuting square concretely: the
evaluator's output alpha-equals the manually instantiated ev_m(sigma)
term - built with the M0 oracle's instantiate_sites, normalized by the
same minimal normalizer.
"""

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Laog, Deleg, Geled, ID, DI
from core.comp.evaluate import evaluate_debate, EvaluationRefused
from core.comp.oracle_terms import (
    instantiate_sites, normalize_strong, alpha_equal, classify_nf,
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
    # paper T-SUP: mu alt.< orig || mu'b.< mu_.<b||alt> || mu'_.<scion||alt> > >
    return Mu(ID("alt", prop), prop, orig,
              Mutilde(DI("b", prop), prop,
                      Mu(ID("_", prop), prop, DI("b", prop), ID("alt", prop)),
                      Mutilde(DI("_", prop), prop, scion, ID("alt", prop))))


def t_att(prop, orig, scion_ctx):
    # paper T-ATT: mu alt.< orig || mu'b.< mu_.<b||scion> || mu'_.<b||alt> > >
    return Mu(ID("alt", prop), prop, orig,
              Mutilde(DI("b", prop), prop,
                      Mu(ID("_", prop), prop, DI("b", prop), scion_ctx),
                      Mutilde(DI("_", prop), prop, DI("b", prop), ID("alt", prop))))


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
        assert alpha_equal(nf, normalize_strong(manual, strategy="cbn"))


class TestAttack:
    def trivial_challenge(self):
        # mu'qAtt:Q.< qAtt || c:Q? > - a bare challenge: an open refutation
        # site, nothing behind it; OUT.  (The paper's attack shape does not
        # reroute the challenger's port to the catch.)
        return Mutilde(DI("qAtt", "Q"), "Q", DI("qAtt", "Q"), Laog("c", "Q"))

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
        from core.comp.adf_label import grounded_labels
        from core.dc.debate_graph import compile_debate
        nf_c, cls_c, sigma_c, g = evaluate_debate(
            self.body(), "d", strict_names=STRICT, mode="credulous")
        nf_s, cls_s, sigma_s, _ = evaluate_debate(
            self.body(), "d", strict_names=STRICT, mode="skeptical")
        grounded = grounded_labels(g)
        assert grounded[(Q, "term")] == "UNDEC"
        assert grounded[(Q, "context")] == "UNDEC"
        # A3: the credulous witness is a preferred labelling with the issue
        # IN, so it decides Q; the skeptical sigma is the intersection.
        assert sigma_c[(P, "term")] == "IN" and sigma_c[(Q, "term")] == "IN"
        assert sigma_s == grounded
        # Credulous keeps the presumption: the attacker is OUT in the witness.
        assert cls_c == "value"
        assert contains(nf_c, lambda n: isinstance(n, Deleg))
        # Skeptical discards the UNDEC attacked side: not a value.
        assert cls_s != "value"
        assert contains(nf_s, lambda n: isinstance(n, Geled))


class TestCaptureIsDecidedByStrictness:
    """A wing that captured a binder is no longer refused, nor absorbed: the
    framework records the capture as a source and the strict phase decides
    the scaffold if strictness can (core/dc/strict.py)."""

    def body(self):
        # the supporter uses the site's continuation, on a presumption
        scion = Mu(ID("k", "Q"), "Q", Deleg("2", "Q"), ID("alt", "Q"))
        return parg(t_sup("Q", Goal("1", "Q"), scion))

    def test_a_defeasible_capturing_supporter_is_judged_by_sigma(self):
        # Q[c] is an obligation nobody meets: the supporter is OUT and the
        # site stays an open obligation.
        nf, cls, sigma, g = evaluate_debate(self.body(), "d", strict_names=STRICT, mode="credulous")
        assert sigma[(Q, "context")] == "OUT" and cls == "open"
        assert contains(nf, lambda n: isinstance(n, Goal) and n.prop == "Q")

    def test_a_strict_capturing_supporter_is_kept_regardless_of_sigma(self):
        # The supporter meets the demand for Q by throwing the site's own
        # continuation an axiom: closed in the debate, so the strict phase
        # keeps it whatever the labelling of Q[c] says.
        from core.dc.strict import strict_resolve
        from core.comp.oracle_terms import _occurs
        scion = Mu(ID("k", "Q"), "Q", DI("qAx", "Q"), ID("alt", "Q"))
        body = parg(t_sup("Q", Goal("1", "Q"), scion))
        trace = []
        resolved, edges = strict_resolve(body, STRICT | {"qAx"}, trace=trace)
        assert trace == [((Q, "term"), "supporter strict")]
        site = resolved.term.context.term
        assert isinstance(site, Mu) and _occurs(site.term, ID, site.id.name)   # binder kept: captured
        nf, cls, sigma, _ = evaluate_debate(body, "d", strict_names=STRICT | {"qAx"}, mode="skeptical")
        assert cls == "value"


class TestRefusals:
    def test_plain_argument_evaluates_without_scaffolds(self):
        nf, cls, labels, _ = evaluate_debate(
            parg(Deleg("1", "Q")), "d", strict_names=STRICT)
        assert labels[(P, "term")] == "IN"
        assert cls == "value"


class TestAdequacyAcrossWitnesses:
    """Adequacy per accepting witness on the two-witness fixture
    (aida-supporter-derivation-status): every credulous witness yields a
    value, and the wing kept is the one whose derivation is live under
    that witness."""

    def body(self):
        s_contest = t_att("S", Deleg("s", "S"),
                          Mutilde(DI("x", "S"), "S", DI("x", "S"), Geled("ds", "S")))
        yq = eta("yQ", "Q", Mu(ID("r", "Q"), "Q", DI("sq", "S->Q"),
                                Cons(s_contest, ID("r", "Q"))))
        return parg(t_sup("Q", Deleg("1", "Q"), yq))

    def test_in_implies_value_for_every_witness(self):
        from core.comp.evaluate import evaluate_witnesses
        results, _ = evaluate_witnesses(self.body(), "d", strict_names=STRICT | {"sq"})
        assert len(results) == 2
        for _, nf, cls, sigma in results:
            assert sigma[(P, "term")] == "IN" and cls == "value"
            s_in = sigma[(canonical_prop("S"), "term")] == "IN"
            # S accepted: Q derived from S (the presumption of S in the
            # term); S refuted: the presumption of Q stands instead.
            assert contains(nf, lambda n: isinstance(n, Deleg) and n.prop == ("S" if s_in else "Q"))
            assert not contains(nf, lambda n: isinstance(n, Deleg) and n.prop == ("Q" if s_in else "S"))
