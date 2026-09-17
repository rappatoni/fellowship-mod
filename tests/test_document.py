"""Phase C (aida-document-graph): the document graph on the prover and
the debate verbs' assertion (option (i)+(ii) of the recorded decision)."""

import warnings

import pytest

from wrap.cli import setup_prover, execute_script
from wrap.prover import ProverError
from core.dc.argument import Argument
from core.dc.debate_graph import canonical_prop, compile_debate, declaration_kinds
from core.dc.unfold import unfold
from core.dc.typecheck import typecheck
from core.comp.adf_label import grounded_labels

K = canonical_prop


@pytest.fixture
def prover():
    p = setup_prover()
    yield p
    p.close()


def issue_graph(prover, name):
    arg = prover.get_argument(name)
    term = unfold(prover.document, prover.issue_of(arg), classical=prover.logic != "lj")
    typecheck(prover, term, name, arg.conclusion, arg.is_anti)     # the type oracle
    return compile_debate(term, name, strict_names=prover.declarations.keys(),
                          strict_kinds=declaration_kinds(prover.declarations))


def test_attack_registered_elsewhere_counts(prover):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, "tests/rationality/document_attack.fspy",
                       strict=True, stop_on_error=True, isolate=False)
    doc = prover.document
    assert {e.name for e in doc.edges} == {"pArg"}
    assert doc.defaults[(K("Q"), "context")] == {"presumption"}      # qAtt's presumption, no verb
    labels = grounded_labels(issue_graph(prover, "pArg"))
    assert labels[(K("Q"), "term")] == "UNDEC" and labels[(K("P"), "term")] == "UNDEC"


def test_composed_debate_adds_nothing_to_the_document(prover):
    prover.send_command("lk.")
    prover.send_command("declare P,Q:bool.")
    prover.send_command("declare pRule : (Q->P).")
    pArg = Argument(prover, "pArg", "P", ["cut (Q->P) rule", "axiom pRule", "elim", "by default", "next", "axiom rule"])
    pArg.execute(); prover.register_argument(pArg)
    qAtt = Argument(prover, "qAtt", "Q", ["by default"], is_anti=True)
    qAtt.execute(); prover.register_argument(qAtt)
    before = (len(prover.document.edges), dict(prover.document.defaults))
    d = qAtt.attack(pArg, name="datt")
    prover.register_argument(d)
    assert getattr(d, "composed", False)
    assert (len(prover.document.edges), dict(prover.document.defaults)) == before
    assert prover.issue_of(d) == (K("P"), "term")


def test_verb_assertion_refuses_an_unreachable_target(prover):
    prover.send_command("lk.")
    prover.send_command("declare P,Q,R:bool.")
    prover.send_command("declare pRule : (Q->P).")
    pArg = Argument(prover, "pArg", "P", ["cut (Q->P) rule", "axiom pRule", "elim", "by default", "next", "axiom rule"])
    pArg.execute(); prover.register_argument(pArg)
    rArg = Argument(prover, "rArg", "R", ["by default"])
    rArg.execute(); prover.register_argument(rArg)
    with pytest.raises(ProverError, match="not.*reachable|no statement"):
        rArg.support(pArg, name="bad")
    rAtt = Argument(prover, "rAtt", "R", ["by default"], is_anti=True)
    rAtt.execute(); prover.register_argument(rAtt)
    with pytest.raises(ProverError, match="reachable"):
        rAtt.attack(pArg, name="bad2")


def test_logic_mode_is_tracked(prover):
    assert prover.logic == "lk"
    prover.send_command("lj.")
    assert prover.logic == "lj"
    prover.send_command("lk.")
    assert prover.logic == "lk"
