from pathlib import Path
import logging

from wrap.cli import execute_script
from core.ac.ast import ProofTerm, Mu, Mutilde, ID, DI

logger = logging.getLogger(__name__)


def _contains_axiom(node: ProofTerm, ax_name: str) -> bool:
    if node is None:
        return False
    if isinstance(node, (ID, DI)):
        return node.name == ax_name
    for ch in (getattr(node, "term", None), getattr(node, "context", None)):
        if ch is not None and _contains_axiom(ch, ax_name):
            return True
    return False


def test_multiple_supports_are_lifo_stack(prover, tmp_path):
    """Assert the *syntax tree* (pre-normalization) corresponds to stacked (LIFO) supports.

    Concretely, in the term of the debate base_s1_s2:
    - the outer support's context branch should contain the second supporter leaf (axB)
    - the outer support's term branch should contain the earlier supporter leaf (axA)

    This checks nesting, not just string order, and avoids relying on binder names.
    """

    script = Path(__file__).parent / "multiple_supports_stack_vs_queue.fspy"
    assert script.exists()

    execute_script(prover, str(script), strict=True, isolate=False)

    # The supports are a debate (aida-debate-objects): its term stacks the
    # supporters in the order they were uttered, the last outermost.
    debate = prover.debates["base_s1_s2"]
    term = prover.debate_term(debate)
    logger.info("base_s1_s2 term: %s", getattr(term, "pres", None))
    assert _contains_axiom(term, "axA"), "expected axA in the debate term"
    assert _contains_axiom(term, "axB"), "expected axB in the debate term"

    # LIFO (stack) property: the axB-support subtree must contain the entire axA-support subtree.
    # We test this by finding a Mu subtree that contains axB and also contains axA beneath it.
    def _has_lifo_stack(n: ProofTerm) -> bool:
        if n is None:
            return False
        if isinstance(n, Mu) and _contains_axiom(n, "axB") and _contains_axiom(n, "axA"):
            return True
        for ch in (getattr(n, "term", None), getattr(n, "context", None)):
            if ch is not None and _has_lifo_stack(ch):
                return True
        return False

    assert _has_lifo_stack(term), "expected axB wrapper to enclose axA wrapper (LIFO stack)"

    # Evaluation judges the outermost supporter first: the later one (axB)
    from core.comp.evaluate import evaluate_debate
    from core.dc.debate_graph import declaration_kinds
    from pres.gen import pres_str
    nf, cls, _sigma, _graph = evaluate_debate(
        term, "base_s1_s2", strict_names=prover.declarations.keys(),
        strict_kinds=declaration_kinds(prover.declarations))
    shown = pres_str(nf)
    logger.info("base_s1_s2 normal form: %s (%s)", shown, cls)
    assert cls == "value", shown
    assert "axB" in shown, shown
    assert "axA" not in shown, shown
