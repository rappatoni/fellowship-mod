# fsp_machine_mode_tests.py – high‑level integration smoke tests
"""Run with `pytest -q fsp_machine_mode_tests.py` once:

1.  *machine mode* is compiled into fsp (env var `FSP_MACHINE=1`).
2.  The Python wrapper (`wrapper.py`) is importable from PYTHONPATH.

We stub the bare minimum – the tests will *xfail* until each stage of the
OCaml side emits the corresponding payload fields.
"""

import os
import sys
import json
import subprocess
from pathlib import Path

import pytest
# No legacy wrapper import; rely on the session-scoped `prover` fixture from conftest

# ---------------------------------------------------------------------------
#  Fixtures
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
#  Tests – will be tightened as payload grows
# ---------------------------------------------------------------------------

def test_snapshot_minimal(prover):
    """`lj.` should succeed without machine errors."""
    state = prover.send_command("lj.", silent=1)
    assert isinstance(state, dict)
    assert not state.get("errors"), f"machine payload errors: {state.get('errors')}"


def test_declare_roundtrip(prover):
    """Ensure `declare` is still recorded in wrapper bookkeeping."""
    prover.send_command("declare A:bool.")
    assert "A" in prover.declarations
    assert prover.declarations["A"] == "bool"


@pytest.mark.xfail(reason="proof-term-hash field may not be emitted yet")
def test_proof_term_hash(prover):
    state = prover.last_state or {}
    assert "proof-term-hash" in state, "payload missing proof-term-hash"
