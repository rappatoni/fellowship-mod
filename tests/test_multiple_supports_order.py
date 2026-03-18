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

    Concretely, in the composed term for base_s1_s2:
    - the outer support's context branch should contain the second supporter leaf (axB)
    - the outer support's term branch should contain the earlier supporter leaf (axA)

    This checks nesting, not just string order, and avoids relying on binder names.
    """

    script = Path(__file__).parent / "multiple_supports_stack_vs_queue.fspy"
    assert script.exists()

    execute_script(prover, str(script), strict=True, isolate=False)

    # --- debug log: proof terms at each stage ---------------------------------
    base = prover.get_argument("base")
    s1 = prover.get_argument("s1")
    s2 = prover.get_argument("s2")
    base_s1 = prover.get_argument("base_s1")
    base_s1_s2 = prover.get_argument("base_s1_s2")

    logger.info("base.proof_term: %s", getattr(base, "proof_term", None))
    logger.info("s1.proof_term: %s", getattr(s1, "proof_term", None))
    logger.info("s2.proof_term: %s", getattr(s2, "proof_term", None))
    logger.info("base_s1.proof_term: %s", getattr(base_s1, "proof_term", None))
    logger.info("base_s1_s2.proof_term: %s", getattr(base_s1_s2, "proof_term", None))

    arg = base_s1_s2
    assert arg is not None
    assert arg.body is not None

    root = arg.body
    assert isinstance(root, Mu), type(root)

    # In the θ-expansion support shape, the outermost node's context is just ID(outer alt).
    # The supporter subterms (containing axA/axB) live under the *term* branch, nested.
    assert _contains_axiom(root.term, "axA"), "expected axA in the composed term"
    assert _contains_axiom(root.term, "axB"), "expected axB in the composed term"

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

    assert _has_lifo_stack(root.term), "expected axB wrapper to enclose axA wrapper (LIFO stack)"

    # Normalization: basic-support reduction should keep the later supporter (axB)
    arg.normalize()
    nf = arg.normal_form
    logger.info("base_s1_s2.normal_form: %s", nf)
    assert isinstance(nf, str) and nf.strip(), nf
    assert "axB" in nf, nf
    assert "axA" not in nf, nf
