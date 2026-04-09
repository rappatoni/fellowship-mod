from pathlib import Path

from wrap.cli import execute_script


def test_graft_uses_canonicalized_conclusion_after_execute(prover):
    script = Path(__file__).parent / "graft_conclusion_formatting.fspy"
    assert script.exists()

    execute_script(prover, str(script), strict=True, isolate=False)

    root = prover.get_argument("root")
    scion = prover.get_argument("scion")
    chained = prover.get_argument("scion_root")

    assert root is not None
    assert scion is not None
    assert chained is not None

    assert root.conclusion == "A->B->C"
    assert scion.conclusion == "A->B->C"
    assert chained.conclusion == "A->B->C"
    assert chained.body is not None
    assert chained.proof_term is not None and chained.proof_term.strip()
