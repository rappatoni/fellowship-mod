"""M3 integration: compile real prover-generated debate terms.

These tests rebuild the exact support/attack compositions used to lock the
scaffold shapes (see the M3 log in propositional-fragment-plan.org) and
check that the compiler decomposes prover output - not just the hand-built
replicas in test_debate_graph.py.  The executed Argument path doubles as
the type-oracle validation: every compiled body replayed through
Fellowship on construction.
"""

import pytest

from wrap.cli import setup_prover
from core.dc.argument import Argument
from core.dc.debate_graph import (
    canonical_prop, compile_debate, CyclicDebateNotSupported,
)


@pytest.fixture(scope="module")
def rules_prover():
    prover = setup_prover()
    prover.send_command('lk.')
    prover.send_command('declare P,Q,R:bool.')
    prover.send_command('declare pRule : (Q->P).')
    prover.send_command('declare qRule : ((R->false)->Q).')
    yield prover
    prover.close()


def test_support_composition_compiles(rules_prover):
    prover = rules_prover
    pArg = Argument(prover, 'pArg', 'P',
                    ['cut (Q->P) rule', 'axiom pRule', 'elim', 'next', 'axiom rule'])
    pArg.execute()
    qArg = Argument(prover, 'qArg', 'Q',
                    ['cut ((R->false)->Q) rule', 'axiom qRule', 'elim', 'next', 'axiom rule'])
    qArg.execute()
    d = qArg.support(pArg, name='dsup')

    g = compile_debate(d.body, 'dsup', strict_names=prover.declarations.keys())
    by_role = {e.role: e for e in g.edges}
    assert set(by_role) == {"argument", "supporter"}
    host, scion = by_role["argument"], by_role["supporter"]
    assert host.target_key == canonical_prop("P")
    assert host.target_side == "term"
    assert [s.key for s in host.sources] == [canonical_prop("Q")]
    assert scion.target_key == canonical_prop("Q")
    assert scion.target_side == "term"
    assert [s.key for s in scion.sources] == [canonical_prop("R->false")]
    assert g.defaults[(canonical_prop("Q"), "term")] == {"obligation"}


def test_attack_composition_compiles(rules_prover):
    prover = rules_prover
    pArg = Argument(prover, 'pArg2', 'P',
                    ['cut (Q->P) rule', 'axiom pRule', 'elim', 'next', 'axiom rule'])
    pArg.execute()
    qAtt = Argument(prover, 'qAtt', 'Q', [], is_anti=True)
    qAtt.execute()
    d = qAtt.attack(pArg, name='datt')

    g = compile_debate(d.body, 'datt', strict_names=prover.declarations.keys())
    by_role = {e.role: e for e in g.edges}
    # The attacker is a bare challenge (identity edge): it leaves a
    # context-side obligation marker on Q instead of an edge.
    assert set(by_role) == {"argument"}
    assert by_role["argument"].sources[0].key == canonical_prop("Q")
    assert g.defaults[(canonical_prop("Q"), "context")] == {"obligation"}
    assert g.defaults[(canonical_prop("Q"), "term")] == {"obligation"}


def test_context_side_support_compiles(rules_prover):
    prover = rules_prover
    nP = Argument(prover, 'nP', 'P', [], is_anti=True)
    nP.execute()
    nP2 = Argument(prover, 'nP2', 'P', [], is_anti=True)
    nP2.execute()
    d = nP2.support(nP, name='dctx')

    g = compile_debate(d.body, 'dctx', strict_names=prover.declarations.keys())
    # Both host and scion are bare challenges: everything collapses to
    # default markers on the single P node.
    assert g.edges == []
    assert g.defaults[(canonical_prop("P"), "context")] == {"obligation"}
    assert list(g.nodes.values()) == ["P"]


# ---------------------------------------------------------------------------
# The cyclic fragment (aida-cyclic-fragment): real prover output with
# derivation cycles compiles, and the labellings are what the ADF says.
# ---------------------------------------------------------------------------

from pathlib import Path

from wrap.cli import execute_script
from core.dc.debate_graph import compile_document, declaration_kinds
from core.comp.adf_label import labellings, grounded_labels, grounded_labels_via_oracle


@pytest.fixture(scope="module")
def even_loop_prover():
    prover = setup_prover()
    yield prover
    prover.close()


def _replay_even_loop(prover):
    # Replayed per test: conftest's autouse fixture clears the argument
    # store between tests, so a module-level replay would be lost.
    execute_script(prover, str(Path("tests/rationality/even_loop.fspy")),
                   strict=True, stop_on_error=True, isolate=False)
    return prover.declarations.keys(), declaration_kinds(prover.declarations)


def test_even_loop_document_is_cyclic_and_has_two_stable_labellings(even_loop_prover):
    prover = even_loop_prover
    names, kinds = _replay_even_loop(prover)
    bodies = [(n, prover.get_argument(n).body) for n in ("Pdefault", "Qdefault")]
    g = compile_document(bodies, strict_names=names, strict_kinds=kinds)
    assert not g.is_acyclic()                      # P[t] <- Q[c] ~ Q[t] <- P[c] ~ P[t]
    assert set(grounded_labels(g).values()) == {"UNDEC"}
    assert grounded_labels(g) == grounded_labels_via_oracle(g)
    # Not the two-labelling Dung even loop: the fixture's refutations are
    # PRESUMPTIONS with their own default, so "both refutations stand,
    # neither proof" is a third stable labelling.  P and Q are never both
    # accepted, and each is credulously acceptable.
    stable = labellings(g, "stable")
    assert len(stable) == 3
    P, Q = canonical_prop("P"), canonical_prop("Q")
    verdicts = {(l[(P, "term")], l[(Q, "term")]) for l in stable}
    assert verdicts == {("IN", "OUT"), ("OUT", "IN"), ("OUT", "OUT")}
    assert len(labellings(g, "preferred")) == 3


def test_each_even_loop_argument_alone_is_acyclic(even_loop_prover):
    prover = even_loop_prover
    names, kinds = _replay_even_loop(prover)
    for n in ("Pdefault", "Qdefault"):
        g = compile_debate(prover.get_argument(n).body, n, strict_names=names, strict_kinds=kinds)
        assert g.is_acyclic()
