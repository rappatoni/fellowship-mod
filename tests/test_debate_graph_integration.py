"""M3 integration: compile real prover-generated debate terms.

These tests rebuild the support/attack debates used to lock the scaffold
shapes (see the M3 log in propositional-fragment-plan.org) - since
aida-debate-objects as debates over registered prover output, unfolded
and type-checked like any debate - and check that the compiler
decomposes them, not just the hand-built replicas in
test_debate_graph.py.
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


def _debate_graph(prover, *lines):
    """The issue graph of a closed debate over registered prover output -
    the term the debate compiles to, decomposed by the compiler."""
    from wrap.cli import debate_line, _debate_issue
    from core.dc.strict import compile_issue
    from core.dc.debate_graph import declaration_kinds
    for line in lines:
        assert debate_line(prover, line), line
    debate = prover.debates[prover.recording_debate]
    assert debate_line(prover, "hora est.")
    _arg, _issue, term, _shared = _debate_issue(prover, debate)
    return compile_issue(term, debate.name, strict_names=prover.declarations.keys(),
                         strict_kinds=declaration_kinds(prover.declarations))


def _register(prover, *arguments):
    for argument in arguments:
        argument.execute()
        prover.register_argument(argument)


def test_support_debate_compiles(rules_prover):
    prover = rules_prover
    pArg = Argument(prover, 'pArg', 'P',
                    ['cut (Q->P) rule', 'axiom pRule', 'elim', 'next', 'axiom rule'])
    qArg = Argument(prover, 'qArg', 'Q',
                    ['cut ((R->false)->Q) rule', 'axiom qRule', 'elim', 'next', 'axiom rule'])
    _register(prover, pArg, qArg)
    g = _debate_graph(prover, "debate pro closed dsup : P.", "pArg.", "support qArg pArg.")
    by_name = {e.name: e for e in g.edges}
    assert set(by_name) == {"pArg", "qArg"}
    host, scion = by_name["pArg"], by_name["qArg"]
    assert (host.target_key, host.target_side) == (canonical_prop("P"), "term")
    assert [s.key for s in host.sources] == [canonical_prop("Q")]
    assert (scion.target_key, scion.target_side) == (canonical_prop("Q"), "term")
    assert [s.key for s in scion.sources] == [canonical_prop("R->false")]
    assert g.defaults[(canonical_prop("Q"), "term")] == {"obligation"}


def test_attack_debate_compiles(rules_prover):
    prover = rules_prover
    pArg = Argument(prover, 'pArg2', 'P',
                    ['cut (Q->P) rule', 'axiom pRule', 'elim', 'next', 'axiom rule'])
    qAtt = Argument(prover, 'qAtt', 'Q', [], is_anti=True)
    _register(prover, pArg, qAtt)
    g = _debate_graph(prover, "debate pro closed datt : P.", "pArg2.", "rebut qAtt pArg2.")
    # The attacker is a bare challenge (identity edge): it leaves a
    # context-side obligation marker on Q instead of an edge.
    assert {e.name for e in g.edges} == {"pArg2"}
    assert [s.key for s in g.edges[0].sources] == [canonical_prop("Q")]
    assert g.defaults[(canonical_prop("Q"), "context")] == {"obligation"}
    assert g.defaults[(canonical_prop("Q"), "term")] == {"obligation"}


def test_context_side_support_debate_compiles(rules_prover):
    prover = rules_prover
    nP = Argument(prover, 'nP', 'P', [], is_anti=True)
    nP2 = Argument(prover, 'nP2', 'P', [], is_anti=True)
    _register(prover, nP, nP2)
    g = _debate_graph(prover, "debate con closed dctx : P.", "nP.", "support nP2 nP.")
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
    # Replayed per test, each time into a new document of the module's
    # prover: nothing carries over from the test before.
    execute_script(prover, str(Path("tests/rationality/even_loop.fspy")),
                   strict=True, stop_on_error=True, isolate=False, new_document=True)
    return prover.declarations.keys(), declaration_kinds(prover.declarations)


def test_even_loop_document_is_cyclic_and_has_two_stable_labellings(even_loop_prover):
    prover = even_loop_prover
    names, kinds = _replay_even_loop(prover)
    bodies = [(n, prover.get_argument(n).body) for n in ("Pdefault", "Qdefault")]
    g = compile_document(bodies, strict_names=names, strict_kinds=kinds)
    assert not g.is_acyclic()                      # P[t] <- Q[c] ~ Q[t] <- P[c] ~ P[t]
    assert set(grounded_labels(g).values()) == {"UNDEC"}
    assert grounded_labels(g) == grounded_labels_via_oracle(g)
    # The Dung even loop, two labellings.  The fixture's refutations are
    # bare PRESUMPTIONS, and since the asymmetric guard (2026-09-25) a bare
    # default does not guard back against a derivation, so "both
    # refutations stand, neither proof" is no longer stable: each proof is
    # derived and would have to be IN.  P and Q are never both accepted,
    # and each is credulously acceptable.
    stable = labellings(g, "stable")
    assert len(stable) == 2
    P, Q = canonical_prop("P"), canonical_prop("Q")
    verdicts = {(l[(P, "term")], l[(Q, "term")]) for l in stable}
    assert verdicts == {("IN", "OUT"), ("OUT", "IN")}
    assert len(labellings(g, "preferred")) == 2


def test_each_even_loop_argument_alone_is_acyclic(even_loop_prover):
    prover = even_loop_prover
    names, kinds = _replay_even_loop(prover)
    for n in ("Pdefault", "Qdefault"):
        g = compile_debate(prover.get_argument(n).body, n, strict_names=names, strict_kinds=kinds)
        assert g.is_acyclic()
