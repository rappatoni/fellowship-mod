"""A4: the fragment verification residues (tasks.org,
aida-fragment-verification-residues; V1-V3 of
propositional-fragment-plan.org).

V1  graft-order commutation: independent grafts commute up to alpha.
    A law of the universal property of C[X] (simultaneous substitution
    for indeterminates), not agreement with graft.py's current output.
V2  conservativity: a strict, closed term never normalises to one with
    an open obligation; asserted inside Argument.normalize and
    evaluate_debate, exercised here on the checker itself.
V3  the OUT row of the adequacy square: at a site sigma defeats, the
    normal form holds the clash mu _:A.<[:A] || E> and is alpha-equal to
    substituting that clash for the site by hand and normalising.
"""

import random
from copy import deepcopy

import pytest

from core.ac.ast import (
    Mu, Mutilde, Lamda, Hyp, Cons, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.comp.evaluate import evaluate_debate
from core.comp.oracle_terms import (
    alpha_equal, normalize_strong, classify_nf, instantiate_sites,
    check_conservativity, is_strict_closed, ConservativityViolation, _contains,
)
from core.dc.graft import graft_single
from core.dc.debate_graph import canonical_prop


# ---------------------------------------------------------------------------
# V1: a small random generator for the propositional fragment
# ---------------------------------------------------------------------------

PROPS = ("A", "B", "C")


class Gen:
    """Random closed-enough terms: every variable refers to an enclosing
    binder, every site carries a unique number from ``prefix``."""

    def __init__(self, seed, prefix):
        self.rng = random.Random(seed)
        self.prefix = prefix
        self.n = 0

    def fresh(self, base):
        self.n += 1
        return f"{base}{self.prefix}{self.n}"

    def site(self, cls, prop):
        return cls(self.fresh("s"), prop)

    def term(self, depth, tvars, cvars):
        p = self.rng.choice(PROPS)
        options = ["goal", "deleg", "mu", "lam"]
        if any(q == p for _, q in tvars):
            options.append("var")
        choice = self.rng.choice(options) if depth > 0 else self.rng.choice(["goal", "deleg"] + (["var"] if "var" in options else []))
        if choice == "var":
            name = self.rng.choice([n for n, q in tvars if q == p])
            return DI(name, p)
        if choice == "goal":
            return self.site(Goal, p)
        if choice == "deleg":
            return self.site(Deleg, p)
        if choice == "mu":
            k = self.fresh("k")
            return Mu(ID(k, p), p,
                      self.term(depth - 1, tvars, cvars + [(k, p)]),
                      self.context(depth - 1, tvars, cvars + [(k, p)]))
        x = self.fresh("x")
        q = self.rng.choice(PROPS)
        return Lamda(Hyp(DI(x, q), q), self.term(depth - 1, tvars + [(x, q)], cvars))

    def context(self, depth, tvars, cvars):
        p = self.rng.choice(PROPS)
        options = ["laog", "geled", "mutilde", "cons"]
        if any(q == p for _, q in cvars):
            options.append("var")
        choice = self.rng.choice(options) if depth > 0 else self.rng.choice(["laog", "geled"] + (["var"] if "var" in options else []))
        if choice == "var":
            name = self.rng.choice([n for n, q in cvars if q == p])
            return ID(name, p)
        if choice == "laog":
            return self.site(Laog, p)
        if choice == "geled":
            return self.site(Geled, p)
        if choice == "mutilde":
            x = self.fresh("x")
            return Mutilde(DI(x, p), p,
                           self.term(depth - 1, tvars + [(x, p)], cvars),
                           self.context(depth - 1, tvars + [(x, p)], cvars))
        return Cons(self.term(depth - 1, tvars, cvars), self.context(depth - 1, tvars, cvars))

    def scion_for(self, site):
        """A scion whose root binder fits the site (the only thing
        graft_single type-checks)."""
        p = site.prop
        if isinstance(site, Goal):
            k = self.fresh("k")
            return Mu(ID(k, p), p, self.term(2, [], [(k, p)]), self.context(2, [], [(k, p)]))
        x = self.fresh("x")
        return Mutilde(DI(x, p), p, self.term(2, [(x, p)], []), self.context(2, [(x, p)], []))


def obligation_sites(node, acc=None):
    acc = [] if acc is None else acc
    if isinstance(node, (Goal, Laog)):
        acc.append(node)
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if child is not None:
            obligation_sites(child, acc)
    return acc


def site_numbers(node):
    return {s.number for s in obligation_sites(node)}


def _root_of(gen):
    p = gen.rng.choice(PROPS)
    k = gen.fresh("k")
    return Mu(ID(k, p), p, gen.term(3, [], [(k, p)]), gen.context(3, [], [(k, p)]))


class TestV1GraftOrderCommutes:
    """graft(graft(B, n1, S1), n2, S2) =alpha graft(graft(B, n2, S2), n1, S1)
    whenever n1 != n2 are sites of B and the scions' own site numbers are
    disjoint from B's (the independence precondition)."""

    CASES = 150

    @staticmethod
    def two_site_body(seed):
        """Redraw from derived seeds until the body has two sites."""
        for attempt in range(50):
            gen = Gen(seed * 1000 + attempt, "b")
            body = _root_of(gen)
            sites = obligation_sites(body)
            if len(sites) >= 2:
                return gen, body, sites
        raise AssertionError(f"seed {seed}: no two-site body in 50 draws")

    @pytest.mark.parametrize("seed", range(CASES))
    def test_independent_grafts_commute(self, seed):
        gen, body, sites = self.two_site_body(seed)
        n1, n2 = gen.rng.sample(sites, 2)
        sg = Gen(seed, "s")
        s1, s2 = sg.scion_for(n1), sg.scion_for(n2)
        assert site_numbers(body).isdisjoint(site_numbers(s1) | site_numbers(s2))

        left = graft_single(graft_single(body, n1.number, s1), n2.number, s2)
        right = graft_single(graft_single(body, n2.number, s2), n1.number, s1)
        assert alpha_equal(left, right)
        assert n1.number not in site_numbers(left)
        assert n2.number not in site_numbers(left)
        assert alpha_equal(body, body)  # the generator's terms are comparable

    def test_dependent_grafts_need_not_commute(self):
        """Sanity: the precondition is not vacuous.  If S1 carries a site
        numbered like n2, grafting S1 first exposes a second n2 and the
        orders differ."""
        # The root binder has prop C so s1's stray Laog "2":A is not
        # captured on the way in and really is a second site numbered 2.
        body = Mu(ID("k", "C"), "C", Goal("1", "A"), Laog("2", "A"))
        s1 = Mu(ID("k1", "A"), "A", DI("k1x", "A"), Laog("2", "A"))  # carries "2"
        s2 = Mutilde(DI("y", "A"), "A", DI("y", "A"), ID("kk", "A"))
        left = graft_single(graft_single(body, "1", s1), "2", s2)
        right = graft_single(graft_single(body, "2", s2), "1", s1)
        assert not alpha_equal(left, right)


# ---------------------------------------------------------------------------
# V2
# ---------------------------------------------------------------------------

class TestV2Conservativity:
    def test_strict_closed_predicate(self):
        assert is_strict_closed(Mu(ID("k", "A"), "A", DI("ax", "A"), ID("k", "A")))
        assert not is_strict_closed(Mu(ID("k", "A"), "A", Goal("1", "A"), ID("k", "A")))
        assert not is_strict_closed(Mu(ID("k", "A"), "A", Deleg("1", "A"), ID("k", "A")))

    def test_open_leaf_from_strict_input_trips(self):
        strict = Mu(ID("k", "A"), "A", DI("ax", "A"), ID("k", "A"))
        leaky = Mu(ID("k", "A"), "A", Goal("1", "A"), ID("k", "A"))
        with pytest.raises(ConservativityViolation):
            check_conservativity(strict, leaky)
        leaky_ctx = Mu(ID("k", "A"), "A", DI("ax", "A"), Laog("1", "A"))
        with pytest.raises(ConservativityViolation):
            check_conservativity(strict, leaky_ctx)

    def test_closed_output_passes_and_sited_input_is_exempt(self):
        strict = Mu(ID("k", "A"), "A", DI("ax", "A"), ID("k", "A"))
        check_conservativity(strict, deepcopy(strict))
        sited = Mu(ID("k", "A"), "A", Goal("1", "A"), ID("k", "A"))
        check_conservativity(sited, deepcopy(sited))  # exempt: input had a site

    def test_evaluate_debate_runs_the_check(self):
        body = Mu(ID("pArg", "P"), "P",
                  Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                     Cons(DI("qAx", "Q"), ID("rule", "P"))),
                  ID("pArg", "P"))
        nf, cls, _, _ = evaluate_debate(body, "d", strict_names={"pRule", "qAx"})
        assert cls == "value" and not _contains(nf, (Goal, Laog))


# ---------------------------------------------------------------------------
# V3: the OUT row of the square
# ---------------------------------------------------------------------------

P, Q = canonical_prop("P"), canonical_prop("Q")


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def parg(site):
    return eta("pArg", "P", Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                                Cons(site, ID("rule", "P"))))


def t_att(prop, orig, scion_ctx):
    # paper T-ATT: mu alt.< orig || mu'b.< mu_.<b||scion> || mu'_.<b||alt> > >
    return Mu(ID("alt", prop), prop, orig,
              Mutilde(DI("b", prop), prop,
                      Mu(ID("_", prop), prop, DI("b", prop), scion_ctx),
                      Mutilde(DI("_", prop), prop, DI("b", prop), ID("alt", prop))))


def grounded_challenge():
    return Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))


class TestV3OutRowSquare:
    @pytest.mark.parametrize("mode", ["skeptical", "credulous"])
    def test_defeated_site_holds_the_clash(self, mode):
        body = parg(t_att("Q", Goal("1", "Q"), grounded_challenge()))
        nf, cls, sigma, _ = evaluate_debate(body, "d", strict_names={"pRule"}, mode=mode)
        assert sigma[(Q, "context")] == "IN" and sigma[(P, "term")] == "OUT"
        assert cls != "value"
        # nf = mu pArg:P.< pRule || SITE * pArg >: the site of Q holds the
        # clash mu alt.< original || E > under an affine binder - here the
        # original is the site's own obligation, E the winning refutation.
        assert isinstance(nf.context, Cons)
        site = nf.context.term
        assert isinstance(site, Mu) and site.prop == "Q"
        from core.comp.oracle_terms import _occurs
        assert not _occurs(site.term, ID, site.id.name)
        assert not _occurs(site.context, ID, site.id.name)
        assert isinstance(site.term, Goal)
        assert isinstance(site.context, Geled)

    @pytest.mark.parametrize("mode", ["skeptical", "credulous"])
    def test_square_commutes_on_the_out_row(self, mode):
        """ev_m by hand: substitute the clash mu _:Q.< t || E > - the site's
        original t facing the winning refutation E - for the defeated site
        of the ORIGINAL (unattacked) term and normalise; the evaluator's
        normal form is alpha-equal to it."""
        body = parg(t_att("Q", Goal("1", "Q"), grounded_challenge()))
        nf, _, _, _ = evaluate_debate(body, "d", strict_names={"pRule"}, mode=mode)
        clash = Mu(ID("_", "Q"), "Q", Goal("1", "Q"), grounded_challenge())
        by_hand = normalize_strong(instantiate_sites(parg(Goal("1", "Q")), {"1": clash}))
        assert alpha_equal(nf, by_hand)
        # a defeated obligation is an exception, like a defeated
        # presumption (aida-classify-exception-before-open; open until
        # 2026-10-07, when obligations were checked first)
        assert classify_nf(by_hand) == classify_nf(nf) == "exception"
