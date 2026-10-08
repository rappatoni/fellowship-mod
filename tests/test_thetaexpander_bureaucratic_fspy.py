from pathlib import Path

from wrap.cli import execute_script


SCRIPT = Path(__file__).with_name("thetaexpander_bureaucratic_cases.fspy")


def _run_thetaexpander_cases(prover):
    execute_script(prover, str(SCRIPT), isolate=False)
    return {name: prover.get_argument(name) for name in [
        "atomic_term",
        "atomic_context",
        "affine_parent",
        "eta_long_parent",
    ]}


def test_thetaexpander_atomic_term_exposes_once_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["atomic_term"].body

    assert body is not None
    assert getattr(body, "pres", None) == "μatomic_term:P.<?1:P||atomic_term:P>"


def test_thetaexpander_atomic_context_exposes_once_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["atomic_context"].body

    assert body is not None
    assert getattr(body, "pres", None) == "μ'atomic_context:P.<atomic_context:P||1:P?>"

def test_thetaexpander_affine_parent_blocks_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["affine_parent"].body

    assert body is not None
    assert "μ_:P." in body.pres
    assert "alt" not in body.pres


def test_thetaexpander_eta_long_parent_blocks_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["eta_long_parent"].body

    assert body is not None
    assert "alpha" in body.pres
    assert "AltC" not in body.pres
