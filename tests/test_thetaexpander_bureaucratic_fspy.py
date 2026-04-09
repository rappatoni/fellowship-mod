from pathlib import Path

from wrap.cli import execute_script


SCRIPT = Path(__file__).with_name("thetaexpander_bureaucratic_cases.fspy")


def _run_thetaexpander_cases(prover):
    execute_script(prover, str(SCRIPT))
    return {name: prover.get_argument(name) for name in [
        "atomic_term",
        "atomic_context",
        "affine_parent",
        "support_case",
        "term_under_mutilde_case",
        "context_under_mu_case",
        "eta_long_parent",
        "lambda_whole_case",
        "lambda_leaf_case",
    ]}


def test_thetaexpander_atomic_term_exposes_once_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["atomic_term"].body

    assert body is not None
    assert getattr(body, "pres", None) == "μatomic_term:P.<1:P||atomic_term:P>"


def test_thetaexpander_atomic_context_exposes_once_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["atomic_context"].body

    assert body is not None
    assert getattr(body, "pres", None) == "μ'atomic_context:P.<atomic_context:P||1:P>"

def test_thetaexpander_affine_parent_blocks_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["affine_parent"].body

    assert body is not None
    assert "μ_:P." in body.pres
    assert "alt" not in body.pres


def test_thetaexpander_support_shell_blocks_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["support_case"].body

    assert body is not None
    assert "support_shell" in body.pres
    assert "μsupport_shell:P.<" in body.pres
    assert "μbeta:P.<μ_:P.<" not in body.pres
    assert "AltC" not in body.pres


def test_thetaexpander_term_under_mutilde_parent_block_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["term_under_mutilde_case"].body

    assert body is not None
    assert "term_under_mutilde" in body.pres
    assert "AltC" not in body.pres


def test_thetaexpander_context_under_mu_parent_block_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["context_under_mu_case"].body

    assert body is not None
    assert "context_under_mu" in body.pres
    assert "AltT" not in body.pres


def test_thetaexpander_eta_long_parent_blocks_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["eta_long_parent"].body

    assert body is not None
    assert "alpha" in body.pres
    assert "AltC" not in body.pres


def test_thetaexpander_lambda_allows_expansion_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    whole = args["lambda_whole_case"].body
    leaf = args["lambda_leaf_case"].body

    assert whole is not None and leaf is not None
    assert "alt" in whole.pres or "AltC" in whole.pres
    assert "alt" in leaf.pres or "AltC" in leaf.pres


def test_thetaexpander_lambda_whole_case_term_shape_via_fspy(prover):
    args = _run_thetaexpander_cases(prover)
    body = args["lambda_whole_case"].body

    assert body is not None
    assert getattr(body, "pres", None) is not None
    assert "f" in body.pres
    assert "alpha" in body.pres