from wrap.prover import ProverError


def test_affine_elim_right_imp_preserves_underscore_and_keeps_it_out_of_context(prover):
    commands = [
        "lk.",
        "declare P:bool.",
        "theorem t: ((P -> P) -> (P -> P)).",
        "elim _.",
    ]

    state = None
    for cmd in commands:
        state = prover.send_command(cmd, silent=1)

    assert state is not None
    proof = state.get("proof_term") or state.get("proof-term") or ""
    assert "\\_:P->P." in proof or "\\_:P." in proof, proof

    with_prover_error = None
    try:
        prover.send_command("axiom _.", silent=1)
    except ProverError as exc:
        with_prover_error = str(exc)

    assert with_prover_error is not None
    assert "_" in with_prover_error
    assert (
        "neither in your hypothesis nor in your conclusion" in with_prover_error
        or "not in your list of hypothesis" in with_prover_error
    )


def test_affine_elim_left_minus_preserves_underscore_and_keeps_it_out_of_context(prover):
    commands = [
        "lk.",
        "declare P:bool.",
        "theorem t: ((P - P) -> (P - P)).",
        "elim h.",
        "cut (P - P) k.",
        "axiom h.",
        "elim _.",
    ]

    state = None
    for cmd in commands:
        state = prover.send_command(cmd, silent=1)

    assert state is not None
    proof = state.get("proof_term") or state.get("proof-term") or ""
    assert "\\_:P." in proof or "\\_:P->P." in proof, proof

    with_prover_error = None
    try:
        prover.send_command("axiom _.", silent=1)
    except ProverError as exc:
        with_prover_error = str(exc)

    assert with_prover_error is not None
    assert "_" in with_prover_error
    assert "neither in your hypothesis nor in your conclusion" in with_prover_error