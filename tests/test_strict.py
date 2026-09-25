"""The strict phase (core/dc/strict.py): the paper's strict redundancy and
strict defeat, read off the concrete debate term."""

from copy import deepcopy

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Laog, Deleg, Geled, ID, DI
from core.comp.oracle_terms import (
    normalize_strong, classify_nf, alpha_equal, contains_uncatchable_clash,
)
from core.dc.debate_graph import canonical_prop, compile_debate
from core.dc.strict import strict_resolve, strict_in_debate, is_closed, compile_issue

A = "A"
STRICT = {"ax", "nx"}


def tsup(orig, t2):
    return Mu(ID("a", A), A, orig,
              Mutilde(DI("b", A), A, Mu(ID("_", A), A, DI("b", A), ID("a", A)),
                      Mutilde(DI("_", A), A, t2, ID("a", A))))


def tatt(orig, e2):
    return Mu(ID("a", A), A, orig,
              Mutilde(DI("b", A), A, Mu(ID("_", A), A, DI("b", A), e2),
                      Mutilde(DI("_", A), A, DI("b", A), ID("a", A))))


axiom = Mu(ID("t1", A), A, DI("ax", A), ID("t1", A))            # strict proof
presumed = Mu(ID("t2", A), A, Deleg("p", A), ID("t2", A))       # presumed proof
refuted = Mutilde(DI("r", A), A, DI("r", A), Geled("q", A))     # presumed refutation
denied = Mutilde(DI("e", A), A, DI("e", A), ID("nx", A))        # strict refutation


class TestPredicates:
    def test_strict_in_debate_means_no_open_site(self):
        assert strict_in_debate(axiom) and not strict_in_debate(presumed)
        assert strict_in_debate(Mu(ID("k", A), A, DI("h", A), ID("k", A)))    # a met demand

    def test_a_captured_presumption_is_not_an_assumption(self):
        """2026-09-25: a captured variable is a commitment the debate made,
        whichever kind of site it replaced.  Classically ~P -> P gives P."""
        captured = DI("h", A); captured.captured_presumption = True
        assert strict_in_debate(Mu(ID("k", A), A, captured, ID("k", A)))

    def test_a_clash_is_strict_but_produces_an_exception(self):
        """A closed clash rests on nothing, so it is strict and decides a
        scaffold; what it PRODUCES is an exception, which bars its strict
        edge, not its decision."""
        clash = Mu(ID("c", A), A, DI("ax", A), ID("nx", A))
        assert strict_in_debate(clash)
        assert contains_uncatchable_clash(clash)
        assert classify_nf(normalize_strong(clash)) == "exception"

    def test_closed_needs_no_free_variable(self):
        assert is_closed(axiom, STRICT)
        assert not is_closed(Mu(ID("k", A), A, DI("h", A), ID("k", A)), STRICT)


class TestDecisions:
    def decide(self, term):
        trace = []
        out, edges = strict_resolve(term, STRICT, trace=trace)
        return [what for _, what in trace], out, edges

    def test_support_strict_original_keeps_it(self):
        what, out, _ = self.decide(tsup(axiom, presumed))
        assert what == ["original strict"] and alpha_equal(out, axiom)

    def test_support_strict_supporter_wins_over_open_site(self):
        what, out, _ = self.decide(tsup(Goal("1", A), axiom))
        assert what == ["supporter strict"] and alpha_equal(out, axiom)

    def test_support_both_defeasible_is_delayed(self):
        what, out, _ = self.decide(tsup(Deleg("1", A), presumed))
        assert what == [] and alpha_equal(out, tsup(Deleg("1", A), presumed))

    def test_attack_strict_attacker_defeats_defeasible_original(self):
        what, out, _ = self.decide(tatt(presumed, denied))
        assert what == ["attacker strict: defeat"]
        assert classify_nf(normalize_strong(out)) == "exception"

    def test_attack_strict_original_survives_defeasible_attacker(self):
        what, out, _ = self.decide(tatt(axiom, refuted))
        assert what == ["original strict"] and alpha_equal(out, axiom)

    def test_attack_both_strict_is_delayed(self):
        what, out, _ = self.decide(tatt(axiom, denied))
        assert what == []          # inconsistent: the labelling says CONTR

    def test_attack_by_a_bare_presumption_is_delayed(self):
        # The regression: the attack's wiring has the legacy C-SUP shape;
        # a bottom-up walk once decided it as "original strict".
        what, out, _ = self.decide(tatt(Deleg("1", A), refuted))
        assert what == []


class TestStrictEdges:
    def test_closed_decided_term_yields_a_strict_edge(self):
        term = tsup(Goal("1", A), axiom)                 # decided, closed: axiom
        wrapped = Mu(ID("d", A), A, term, ID("d", A))    # an argument named d
        _, edges = strict_resolve(wrapped, STRICT)
        assert [(e.name, e.target_key, e.strict, e.sources) for e in edges] == [("d*", canonical_prop(A), True, ())]

    def test_undecided_or_open_yields_none(self):
        _, edges = strict_resolve(Mu(ID("d", A), A, tsup(Deleg("1", A), presumed), ID("d", A)), STRICT)
        assert edges == []

    def test_compile_issue_adds_the_edge(self):
        wrapped = Mu(ID("d", A), A, tsup(Goal("1", A), axiom), ID("d", A))
        g = compile_issue(wrapped, "d", strict_names=STRICT)
        assert any(e.name == "d*" and e.strict for e in g.edges)
        # the framework's own view of d: its site is an obligation marker
        # (an identity edge), its supporter t1 an ordinary strict edge
        assert g.defaults[(canonical_prop(A), "term")] == {"obligation"}
        assert any(e.name == "t1" and e.strict for e in g.edges)


class TestClashesDecideButClaimNoEdge:
    """A clash is strict - it rests on nothing - so it decides a scaffold;
    what it produces is an exception, so it never contributes a strict
    edge and it reaches the root (author's objection, 2026-09-25)."""

    def test_a_defeated_presumption_still_loses_to_a_live_supporter(self):
        # The defeat wraps the presumed proof, so the clash still holds an
        # open SITE and is not strict.  The supporter wins on that ground,
        # not on the removed "a clash is not strict" rule.
        defeated = tatt(presumed, denied)
        term = Mu(ID("d", A), A, tsup(defeated, axiom), ID("d", A))
        trace = []
        out, edges = strict_resolve(term, STRICT, trace=trace)
        assert [w for _, w in trace] == ["attacker strict: defeat", "supporter strict"]
        assert classify_nf(normalize_strong(out)) == "value"
        assert [e.name for e in edges] == ["d*"]

    def test_a_closed_clash_alone_yields_no_strict_edge(self):
        term = Mu(ID("d", A), A, tatt(presumed, denied), ID("d", A))
        _, edges = strict_resolve(term, STRICT)
        assert edges == []

    def test_a_site_free_clash_wins_by_strict_redundancy(self):
        # The case the removed rule used to govern: an original that rests
        # on nothing yet derives a contradiction.  It is strict, so it
        # makes the supporter redundant; the exception then bubbles up and
        # no strict edge is claimed for A.
        clash = Mu(ID("c", A), A, DI("ax", A), ID("nx", A))
        term = Mu(ID("d", A), A, tsup(clash, axiom), ID("d", A))
        trace = []
        out, edges = strict_resolve(term, STRICT, trace=trace)
        assert [w for _, w in trace] == ["original strict"]
        assert classify_nf(normalize_strong(deepcopy(out))) == "exception"
        assert edges == []

    def test_the_exception_survives_compile_issue(self):
        clash = Mu(ID("c", A), A, DI("ax", A), ID("nx", A))
        term = Mu(ID("d", A), A, tsup(clash, axiom), ID("d", A))
        g = compile_issue(term, "d", strict_names=STRICT)
        assert not any(e.strict and e.name.endswith("*") for e in g.edges)
