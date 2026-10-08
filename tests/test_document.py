"""Phase C (aida-document-graph): the document graph on the prover, and
the check of the debate verbs (aida-debate-objects)."""

import warnings

import pytest

from wrap.cli import setup_prover, execute_script
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
    term = unfold(prover.document, prover.issue_of(arg))
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


def test_a_verb_refuses_a_target_without_the_node(prover):
    # The debate verbs check their target (core/dc/debate.py); a debate adds
    # nothing to the document (tests/test_debates.py).
    from core.dc.debate import DebateError
    from wrap.cli import debate_line
    prover.send_command("lk.")
    prover.send_command("declare P,Q,R:bool.")
    prover.send_command("declare pRule : (Q->P).")
    pArg = Argument(prover, "pArg", "P", ["cut (Q->P) rule", "axiom pRule", "elim", "by default", "next", "axiom rule"])
    pArg.execute(); prover.register_argument(pArg)
    rArg = Argument(prover, "rArg", "R", ["by default"])
    rArg.execute(); prover.register_argument(rArg)
    rAtt = Argument(prover, "rAtt", "R", ["by default"], is_anti=True)
    rAtt.execute(); prover.register_argument(rAtt)
    assert debate_line(prover, "debate pro closed d : P.") and debate_line(prover, "pArg.")
    with pytest.raises(DebateError, match="no site on :R"):
        debate_line(prover, "support rArg pArg.")
    with pytest.raises(DebateError, match="no site on :R"):
        debate_line(prover, "attack rAtt pArg.")
    assert debate_line(prover, "rArg pArg.")            # no verb: a non sequitur is allowed


def test_logic_mode_is_tracked(prover):
    assert prover.logic == "lk"
    prover.send_command("lj.")
    assert prover.logic == "lj"
    prover.send_command("lk.")
    assert prover.logic == "lk"


def test_debate_commands_are_refused_in_lj(prover):
    from wrap.cli import _issue_term
    prover.send_command("lj.")
    prover.send_command("declare P:bool.")
    pArg = Argument(prover, "pArg", "P", ["by default"])
    pArg.execute(); prover.register_argument(pArg)
    arg, issue, term = _issue_term(prover, "pArg")
    assert term is None                      # debates are classical
    prover.send_command("lk.")
