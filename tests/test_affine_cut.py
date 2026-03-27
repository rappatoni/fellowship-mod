from pathlib import Path

import pytest
from wrap.prover import ProverError


def test_affine_cut_script_roundtrip(prover):
    script = Path(".cecli/workspace/affine_cut.fsp")
    script.write_text(
        "\n".join(
            [
                "lk.",
                "declare P:bool.",
                "theorem t: (P -> P).",
                "elim h.",
                "cut (P) _.",
                "axiom h.",
                "axiom _.",
                "",
            ]
        )
    )

    state = None
    for line in script.read_text().splitlines():
        try:
            state = prover.send_command(line, silent=1)
        except ProverError as exc:
            assert line == "axiom _."
            msg = str(exc)
            assert "_" in msg
            assert (
                "neither in your hypothesis nor in your conclusion" in msg
                or "not in your list of hypothesis" in msg
            )
            break
    else:
        raise AssertionError("expected axiom _. to fail because '_' must not enter the context")

    assert state is not None


def test_affine_cut_preserves_underscore_in_machine_proof_term(prover):
    commands = [
        "lk.",
        "declare P:bool.",
        "theorem t: (P -> P).",
        "elim h.",
        "cut (P) _.",
        "axiom h.",
    ]

    state = None
    for cmd in commands:
        state = prover.send_command(cmd, silent=1)

    assert state is not None
    proof = state.get("proof_term") or state.get("proof-term") or ""
    assert "_" in proof, proof
    assert ";:_:P." in proof or ";'_:P." in proof