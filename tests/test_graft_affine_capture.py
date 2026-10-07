"""Repeated support grafting preserves open sites (M1 triage, 2026-08-31).

History: this file was added by a chore(debug) commit (6de510e) while
inspecting repeated support grafting, and pinned the then-observed
behaviour — a ProverError ("This is not trivial") — as its expectation.
Spec-first triage against propositional-fragment-plan.org's trust
hierarchy concluded the error was the bug and the current behaviour is
correct: the composite debate replays through Fellowship (the type
oracle), every open site of every constituent argument survives with its
proposition intact, and the supported site itself stays open, support
being disjunctive (the original alternative is retained, not consumed).
This test now asserts those invariants instead of the debug-era snapshot.
"""

from collections import Counter

from wrap.cli import setup_prover
from core.dc.argument import Argument


def test_repeated_support_grafting_preserves_open_sites():
    prover = setup_prover()
    try:
        prover.send_command('lk.')
        prover.send_command('declare P,Q : bool.')

        peirce = '((P->Q)->P)->P'

        p1 = Argument(prover, 'p1', '((P -> Q)->P)->P', [])
        p1.execute()
        s1 = Argument(prover, 's1', '((P -> Q)->P)->P', ['elim f'])
        s1.execute()

        # s1 supports p1's open Peirce obligation.
        d1 = s1.support(p1, name='d1')
        d1_props = Counter(info['prop'] for info in d1.assumptions.values())
        # p1's site is retained (support keeps the original alternative
        # open) and s1 contributes its own open site of type P.
        assert d1_props == Counter({peirce: 1, 'P': 1})

        # p2 (concluding P, with two open sites of its own) supports d1 on P.
        p2 = Argument(prover, 'p2', 'P', ['cut ((P -> Q)->P) alpha', 'elim g'])
        p2.execute()
        assert Counter(info['prop'] for info in p2.assumptions.values()) == \
            Counter({'P': 1, '(P->Q)->P': 1})

        # The debug-era expectation was a ProverError here; the correct
        # outcome is a well-typed composite that Fellowship replays.
        d2 = p2.support(d1, name='d2')
        assert d2.executed

        # Every constituent's open sites survive, renumbered but with their
        # propositions intact: p1's Peirce site, s1's P site (the supported
        # one, still open), and p2's two sites.
        d2_props = Counter(info['prop'] for info in d2.assumptions.values())
        assert d2_props == Counter({peirce: 1, 'P': 2, '(P->Q)->P': 1})
    finally:
        prover.close()
